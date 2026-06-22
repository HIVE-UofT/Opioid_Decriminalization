# Zero-shot misconception classification

Reproducible pipeline for the multi-label policy-misconception classification
in *Detecting Misconception Surges in Opioid Policy*. Each Reddit comment is
labelled with the paper's five-class taxonomy using the **exact prompt from
the manuscript** (Section 2.2), by three interchangeable models:

| Backend  | Model                              | How it runs                          |
| -------- | ---------------------------------- | ------------------------------------ |
| `gpt`    | GPT-4o (primary classifier)        | OpenAI Chat Completions API          |
| `gemini` | Gemini 2.0 Flash                   | Google Gen AI API                    |
| `llama`  | Llama 3.2 Instruct (baseline)      | Hugging Face transformers, local GPU |

The output is a CSV identical to the input plus a `writing` column holding the
cleaned label set, e.g. `['5']` or `['2', '4']` — the same format consumed by
`robustness.ipynb` and the co-occurrence analysis.

## Files

- `taxonomy.py` — the verbatim prompt, label names, output parsing (Label-5
  mutual-exclusivity rule), and the conservative comment-level consolidation
  (majority vote over repeated export rows).
- `backends.py` — the three model backends behind one `Classifier` interface.
  Provider SDKs are imported lazily, so you only need the dependency for the
  backend you actually run.
- `run_classification.py` — CLI to label a CSV, with incremental checkpointing
  and `--resume`.
- `requirements.txt` — dependencies (the cluster venv already provides
  `transformers`, `torch`, `pandas`, and `tqdm`).

## Setup

```bash
pip install -r requirements.txt          # or install only what a backend needs

export OPENAI_API_KEY=...                 # for --model gpt
export GEMINI_API_KEY=...                 # for --model gemini (or GOOGLE_API_KEY)
huggingface-cli login                     # for --model llama (gated checkpoint)
```

## Usage

```bash
cd code/classification

# GPT-4o over the final analytic corpus
python run_classification.py --model gpt \
    --input ../../dataset/Comments.csv --output ../../dataset/gpt.csv

# Gemini 2.0 Flash
python run_classification.py --model gemini \
    --input ../../dataset/Comments.csv --output ../../dataset/gemini.csv

# Llama 3.2 (local; run on a GPU node)
python run_classification.py --model llama \
    --input ../../dataset/Comments.csv --output ../../dataset/llama.csv
```

Helpful flags:

- `--limit N` — classify only the first `N` rows (quick smoke test).
- `--resume` — continue an interrupted run from the partially written output.
- `--model-name` — override the provider model id / HF checkpoint
  (e.g. `--model-name meta-llama/Llama-3.2-1B-Instruct`).
- `--consolidate` — also write `<output>_consolidated.csv`, collapsing repeated
  export rows into one conservative comment-level label set per the paper's
  majority-vote rule. Use this only when classifying the **raw export** (with
  repeated rows); `Comments.csv` is already deduplicated to 12,537 records.
