"""Run zero-shot misconception classification over the Reddit corpus.

Reproduces the ``writing`` label column for any of the three models used in
the paper. Reads a comment CSV (e.g. ``dataset/Comments.csv``), classifies the
``body`` of each row with the paper's prompt, and writes the same columns plus
a ``writing`` column holding the cleaned label set (a list of '1'..'5').

Examples
--------
    # GPT-4o (needs OPENAI_API_KEY)
    python run_classification.py --model gpt \
        --input ../../dataset/Comments.csv --output ../../dataset/gpt.csv

    # Gemini 2.0 Flash (needs GEMINI_API_KEY / GOOGLE_API_KEY)
    python run_classification.py --model gemini \
        --input ../../dataset/Comments.csv --output ../../dataset/gemini.csv

    # Llama 3.2 from Hugging Face (run on a GPU node)
    python run_classification.py --model llama \
        --input ../../dataset/Comments.csv --output ../../dataset/llama.csv

Use ``--limit N`` for a small smoke test, and ``--resume`` to continue an
interrupted run from the partially written output file.

The script writes incrementally, flushing after every ``--checkpoint-every``
rows, so a crash or pre-emption never loses completed work.
"""

from __future__ import annotations

import argparse
import sys

import pandas as pd
from tqdm import tqdm

from backends import BACKENDS, build_classifier
from taxonomy import consolidate_repeated_rows, parse_output_ints

INPUT_COLUMNS = ["body", "communityName", "createdAt", "numberOfreplies", "upVotes"]
KEY_COLUMNS = ["body", "communityName", "createdAt", "numberOfreplies", "upVotes"]


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--model", required=True, choices=sorted(BACKENDS), help="Which backend to run.")
    p.add_argument("--input", required=True, help="Input CSV with a 'body' column.")
    p.add_argument("--output", required=True, help="Output CSV to write (gains a 'writing' column).")
    p.add_argument("--limit", type=int, default=None, help="Classify only the first N rows (smoke test).")
    p.add_argument("--resume", action="store_true", help="Resume from rows already present in --output.")
    p.add_argument("--checkpoint-every", type=int, default=100, help="Flush output every N rows.")
    # Backend-specific knobs.
    p.add_argument("--model-name", default=None, help="Override the provider model id / HF checkpoint.")
    p.add_argument("--temperature", type=float, default=0.0, help="Sampling temperature for API backends.")
    p.add_argument(
        "--consolidate",
        action="store_true",
        help="After labelling, collapse repeated export rows into one conservative "
        "comment-level label set (paper's majority-vote rule).",
    )
    return p.parse_args(argv)


def backend_kwargs(args: argparse.Namespace) -> dict:
    """Map CLI flags to the chosen backend's constructor arguments."""
    if args.model in ("gpt", "gemini"):
        kwargs = {"temperature": args.temperature}
        if args.model_name:
            kwargs["model"] = args.model_name
        return kwargs
    # llama
    return {"model_id": args.model_name} if args.model_name else {}


def load_done_rows(output_path: str, resume: bool) -> int:
    """Return how many leading rows of the output already have labels."""
    if not resume:
        return 0
    try:
        existing = pd.read_csv(output_path)
    except FileNotFoundError:
        return 0
    return int(existing["writing"].notna().sum()) if "writing" in existing else 0


def consolidate(df: pd.DataFrame) -> pd.DataFrame:
    """Collapse repeated export rows to one comment-level record per group."""
    records = []
    for key, group in df.groupby(KEY_COLUMNS, sort=False):
        per_row = [parse_output_ints(w) for w in group["writing"]]
        record = dict(zip(KEY_COLUMNS, key))
        record["writing"] = consolidate_repeated_rows(per_row)
        records.append(record)
    return pd.DataFrame.from_records(records)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)

    df = pd.read_csv(args.input)
    if "body" not in df.columns:
        sys.exit(f"Input {args.input} has no 'body' column (columns: {list(df.columns)}).")
    if args.limit is not None:
        df = df.head(args.limit).copy()

    keep = [c for c in INPUT_COLUMNS if c in df.columns]
    df = df[keep].copy()

    start = load_done_rows(args.output, args.resume)
    if start:
        print(f"Resuming: {start} rows already labelled in {args.output}.")
    if start >= len(df):
        print("Nothing to do; all rows already labelled.")
    else:
        clf = build_classifier(args.model, **backend_kwargs(args))
        print(f"Loaded backend '{clf.name}'. Classifying rows {start}..{len(df) - 1}.")

        writing = list(load_partial_writing(args.output, start)) if start else []
        writing += [None] * (len(df) - len(writing))

        for i in tqdm(range(start, len(df)), initial=start, total=len(df)):
            writing[i] = clf.classify(df.iloc[i]["body"])
            if (i + 1) % args.checkpoint_every == 0 or i == len(df) - 1:
                out = df.iloc[: i + 1].copy()
                out["writing"] = writing[: i + 1]
                out.to_csv(args.output, index=False)

        df["writing"] = writing
        df.to_csv(args.output, index=False)
        print(f"Wrote {len(df)} labelled rows to {args.output}.")

    if args.consolidate:
        labelled = pd.read_csv(args.output)
        collapsed = consolidate(labelled)
        consolidated_path = args.output.replace(".csv", "_consolidated.csv")
        collapsed.to_csv(consolidated_path, index=False)
        print(f"Wrote {len(collapsed)} consolidated comment records to {consolidated_path}.")

    return 0


def load_partial_writing(output_path: str, n: int) -> list:
    """Read the first ``n`` 'writing' values from an existing output file."""
    existing = pd.read_csv(output_path)
    return list(existing["writing"].head(n))


if __name__ == "__main__":
    raise SystemExit(main())
