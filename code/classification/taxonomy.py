"""Policy-misconception taxonomy, prompt, and label parsing.

This module is the single source of truth for the zero-shot multi-label
classification used in the paper. The prompt below is reproduced verbatim
from the manuscript (Section 2.2, "Policy Misconceptions Prompt"). The
parsing rules implement the label cleaning described in the same section
(Label 5 mutual exclusivity) and the conservative comment-level label
assignment used to collapse repeated export rows.

Labels
------
1. Policy-Target Misinterpretation
2. Legal-Status Confusion
3. Enforcement Misconceptions
4. Impact on Crime or Society
5. Unclear or no verifiable factual claim (no-claim category)
"""

from __future__ import annotations

import re
from collections import Counter

# Human-readable label names, kept for reporting/figures.
LABEL_NAMES = {
    1: "Policy-Target Misinterpretation",
    2: "Legal-Status Confusion",
    3: "Enforcement Misconceptions",
    4: "Impact on Crime or Society",
    5: "Unclear or no verifiable factual claim",
}

SUBSTANTIVE_LABELS = (1, 2, 3, 4)
NO_CLAIM_LABEL = 5

# Verbatim prompt from the paper (Section 2.2). The comment body is appended
# after the trailing "Comment: " marker by ``build_prompt``.
TAXONOMY_PROMPT = """You are a public health policy analyst. The task is to identify up to three factual error types in a Reddit comment about British Columbia's drug decriminalization pilot. Select labels from this list:
- 1. Policy-Target Misinterpretation: The comment misrepresents the purpose of the policy. For example, it claims the policy promotes drug use rather than access to health and social services.
- 2. Legal-Status Confusion: The comment conflates decriminalization with legalization. For example, it claims drugs are fully legal or permitted everywhere.
- 3. Enforcement Misconceptions: The comment misstates police or authority powers. For example, it claims police cannot intervene, confiscate drugs, or act on trafficking.
- 4. Impact on Crime or Society: The comment attributes crime, overdose deaths, or disorder directly to the policy without credible support.
- 5. Unclear or no verifiable factual claim: Use this label only when no other label applies.
Apply these rules:
- The classification should focus on factual claims rather than tone or opinion.
- The output should include one to three labels. Use fewer labels when the evidence is weak.
- Label 5 applies to satire, unverifiable anecdotes, or comments without a checkable factual claim.
Output the label numbers only, separated by commas.
Comment: """


def build_prompt(comment_body: object) -> str:
    """Return the full prompt for one comment.

    The comment text is concatenated rather than ``format``-substituted so
    that braces or other format characters in user text are never interpreted.
    """
    return TAXONOMY_PROMPT + str(comment_body)


def parse_output(raw_output: object) -> list[str]:
    """Parse raw model text into a cleaned label set.

    Implements the codebook rule from the paper: keep substantive labels
    {1,2,3,4} if any are present (dropping Label 5), otherwise return {5}.
    Labels are returned as a sorted list of single-character strings so the
    result matches the ``writing`` column produced by the original pipeline.
    """
    found = sorted({int(x) for x in re.findall(r"[1-5]", str(raw_output))})
    substantive = [x for x in found if x in SUBSTANTIVE_LABELS]
    labels = substantive if substantive else [NO_CLAIM_LABEL]
    return [str(x) for x in labels]


def parse_output_ints(raw_output: object) -> list[int]:
    """Same as :func:`parse_output` but returns integers."""
    return [int(x) for x in parse_output(raw_output)]


def consolidate_repeated_rows(per_row_labels: list[list[int]]) -> list[str]:
    """Collapse repeated export rows into one conservative comment-level label.

    Reproduces the paper's rule for a repeated-row group: a substantive class
    ``c`` is kept when it appears in strictly more than half of the n_g rows
    (majority vote); if no substantive class survives, the group is labelled
    {5}. ``per_row_labels`` is the list of cleaned label sets (one per export
    row) belonging to the same comment record.
    """
    n = len(per_row_labels)
    if n == 0:
        return [str(NO_CLAIM_LABEL)]
    counts = Counter()
    for labels in per_row_labels:
        for c in set(labels):
            if c in SUBSTANTIVE_LABELS:
                counts[c] += 1
    kept = sorted(c for c, k in counts.items() if k > n / 2)
    if not kept:
        return [str(NO_CLAIM_LABEL)]
    return [str(c) for c in kept]
