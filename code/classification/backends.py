"""Model backends for zero-shot misconception classification.

Three interchangeable classifiers, all returning the same cleaned label set
for a comment:

* :class:`GPTClassifier`    - OpenAI Chat Completions API (GPT-4o).
* :class:`GeminiClassifier` - Google Gen AI API (Gemini 2.0 Flash).
* :class:`LlamaClassifier`  - Hugging Face transformers (Llama 3.2, local).

All decoding is greedy / temperature 0 so the labels are reproducible for a
fixed model snapshot. Provider SDKs are imported lazily inside each class, so
importing this module never requires every dependency to be installed - only
the backend you actually instantiate needs its package.
"""

from __future__ import annotations

import os
import time
from abc import ABC, abstractmethod

from taxonomy import build_prompt, parse_output

# A handful of label digits is all the model needs to emit.
MAX_OUTPUT_TOKENS = 16


def _retry(fn, *, attempts: int = 5, base_delay: float = 2.0):
    """Call ``fn`` with exponential backoff on transient errors."""
    last_exc = None
    for i in range(attempts):
        try:
            return fn()
        except Exception as exc:  # noqa: BLE001 - re-raised after final attempt
            last_exc = exc
            if i == attempts - 1:
                break
            time.sleep(base_delay * (2 ** i))
    raise last_exc


class Classifier(ABC):
    """Common interface: turn a comment body into a cleaned label set."""

    name: str = "classifier"

    @abstractmethod
    def generate(self, prompt: str) -> str:
        """Return the raw model text for a fully built prompt."""

    def classify(self, comment_body: object) -> list[str]:
        """Return the cleaned label set (list of '1'..'5') for one comment."""
        raw = self.generate(build_prompt(comment_body))
        return parse_output(raw)


class GPTClassifier(Classifier):
    """OpenAI GPT-4o (the paper's primary classifier).

    Requires ``openai`` and the ``OPENAI_API_KEY`` environment variable.
    """

    name = "gpt"

    def __init__(
        self,
        model: str = "gpt-4o",
        temperature: float = 0.0,
        api_key: str | None = None,
    ):
        from openai import OpenAI  # lazy import

        self.model = model
        self.temperature = temperature
        self.client = OpenAI(api_key=api_key or os.environ.get("OPENAI_API_KEY"))

    def generate(self, prompt: str) -> str:
        def _call():
            resp = self.client.chat.completions.create(
                model=self.model,
                messages=[{"role": "user", "content": prompt}],
                temperature=self.temperature,
                max_tokens=MAX_OUTPUT_TOKENS,
            )
            return resp.choices[0].message.content or ""

        return _retry(_call)


class GeminiClassifier(Classifier):
    """Google Gemini 2.0 Flash via the google-genai SDK.

    Requires ``google-genai`` and ``GEMINI_API_KEY`` (or ``GOOGLE_API_KEY``).
    """

    name = "gemini"

    def __init__(
        self,
        model: str = "gemini-2.0-flash",
        temperature: float = 0.0,
        api_key: str | None = None,
    ):
        from google import genai  # lazy import
        from google.genai import types

        self.model = model
        self._types = types
        key = api_key or os.environ.get("GEMINI_API_KEY") or os.environ.get("GOOGLE_API_KEY")
        self.client = genai.Client(api_key=key)
        self.config = types.GenerateContentConfig(
            temperature=temperature,
            max_output_tokens=MAX_OUTPUT_TOKENS,
        )

    def generate(self, prompt: str) -> str:
        def _call():
            resp = self.client.models.generate_content(
                model=self.model,
                contents=prompt,
                config=self.config,
            )
            return resp.text or ""

        return _retry(_call)


class LlamaClassifier(Classifier):
    """Llama 3.2 Instruct loaded locally from Hugging Face transformers.

    The paper refers to this baseline as "LLaMA-3.2B"; the corresponding
    public checkpoint is ``meta-llama/Llama-3.2-3B-Instruct`` (use the 1B
    variant by passing ``model_id="meta-llama/Llama-3.2-1B-Instruct"``).
    Requires ``transformers``, ``torch`` and (for gated checkpoints) a
    Hugging Face token via ``huggingface-cli login`` or ``HF_TOKEN``.
    """

    name = "llama"

    def __init__(
        self,
        model_id: str = "meta-llama/Llama-3.2-3B-Instruct",
        device_map: str = "auto",
        torch_dtype: str = "auto",
    ):
        import torch  # lazy import
        from transformers import AutoModelForCausalLM, AutoTokenizer

        self._torch = torch
        self.tokenizer = AutoTokenizer.from_pretrained(model_id)
        self.model = AutoModelForCausalLM.from_pretrained(
            model_id,
            torch_dtype=torch_dtype,
            device_map=device_map,
        )
        self.model.eval()

    def generate(self, prompt: str) -> str:
        torch = self._torch
        messages = [{"role": "user", "content": prompt}]
        inputs = self.tokenizer.apply_chat_template(
            messages,
            add_generation_prompt=True,
            return_tensors="pt",
        ).to(self.model.device)

        with torch.inference_mode():
            output_ids = self.model.generate(
                inputs,
                max_new_tokens=MAX_OUTPUT_TOKENS,
                do_sample=False,
                pad_token_id=self.tokenizer.eos_token_id,
            )

        generated = output_ids[0][inputs.shape[-1]:]
        return self.tokenizer.decode(generated, skip_special_tokens=True)


# Registry used by the CLI in run_classification.py.
BACKENDS = {
    "gpt": GPTClassifier,
    "gemini": GeminiClassifier,
    "llama": LlamaClassifier,
}


def build_classifier(model: str, **kwargs) -> Classifier:
    """Instantiate a backend by name ('gpt', 'gemini', or 'llama')."""
    try:
        cls = BACKENDS[model]
    except KeyError:
        raise ValueError(
            f"Unknown model '{model}'. Choose one of: {', '.join(BACKENDS)}"
        ) from None
    return cls(**kwargs)
