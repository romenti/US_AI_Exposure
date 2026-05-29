import os
from openai import OpenAI
import os
import time
import re
import pandas as pd
from typing import List, Tuple, Dict, Optional
from huggingface_hub import InferenceClient, login

from trial_file_1 import HF_TOKEN

HF_TOKEN = "insert API Key"
import os
import os, re, time
from typing import Optional, Tuple, List, Dict
import pandas as pd
from openai import OpenAI

# ------------------------- CLIENT -------------------------

client = OpenAI(
    base_url="",
    api_key=HF_TOKEN,
)

MODEL_NAME = "google/gemma-3-27b-it:featherless-ai"

# ------------------------- PROMPTS -------------------------

SYSTEM_PROMPT_BATCH = (
    "You are an evaluator of Generative AI complementarity.\n\n"
    "TASK:\n"
    "For each input task, estimate the probability (0-100) that the task could be meaningfully COMPLEMENTED or AUGMENTED by Generative AI.\n\n"
    "CORE DEFINITION:\n"
    "'Complemented or augmented' means Generative AI can help a human perform the task better, faster, or more creatively by producing useful outputs such as:\n"
    "- text\n"
    "- summaries\n"
    "- ideas\n"
    "- code\n"
    "- structured reasoning\n"
    "- images or video\n"
    "- audio\n\n"
    "The human remains responsible for direction, judgment, and final use of the output.\n"
    "High scores should be assigned when Generative AI can act as a strong force multiplier by reducing cognitive effort, accelerating execution, or improving ideation and communication.\n"
    "Low scores should be assigned when Generative AI would provide little practical value for the task, or when the task depends mainly on physical action, real-world execution, or non-generative capabilities.\n\n"
    "INCLUDE:\n"
    "- text generation\n"
    "- code generation\n"
    "- image/video generation\n"
    "- audio generation\n\n"
    "EXCLUDE:\n"
    "- robotics\n"
    "- physical automation\n"
    "- purely mechanical or manual execution\n\n"
    "IMPORTANT INTERPRETATION RULE:\n"
    "Evaluate whether Generative AI can materially assist a human in completing the task, not whether it can perform the entire task autonomously.\n\n"
    "SCORING GUIDE:\n"
    "0-10   = Very unlikely to be complemented by Generative AI\n"
    "11-30  = Low likelihood\n"
    "31-50  = Moderate-low likelihood\n"
    "51-70  = Moderate likelihood\n"
    "71-90  = High likelihood\n"
    "91-100 = Very high likelihood\n\n"
    "QUALITATIVE LABEL:\n"
    "Map each score to exactly one label using this scale:\n"
    "- 0-20   -> Very Low\n"
    "- 21-40  -> Low\n"
    "- 41-60  -> Moderate\n"
    "- 61-80  -> High\n"
    "- 81-100 -> Very High\n\n"
    "OUTPUT REQUIREMENTS:\n"
    "- Assign a precise integer from 0 to 100\n"
    "- Use the full range when appropriate\n"
    "- Do not default to multiples of 5\n"
    "- Keep the explanation to no more than 2 sentences\n\n"
    "OUTPUT FORMAT (STRICT):\n"
    "Return exactly one TSV line per input task, in the same order as provided.\n"
    "Each line must be:\n"
    "ID<TAB>POINT<TAB>JUDGMENT<TAB>EXP\n\n"
    "FIELD DEFINITIONS:\n"
    "- ID: the given task ID (for example, T000001)\n"
    "- POINT: an integer from 0 to 100\n"
    "- JUDGMENT: exactly one of Very Low, Low, Moderate, High, Very High\n"
    "- EXP: a brief explanation\n\n"
    "STRICT RULES:\n"
    "- No headers\n"
    "- No bullet points in the output\n"
    "- No extra text\n"
    "- No blank lines\n"
)

# ------------------------- TSV LINE REGEX -------------------------

ID_LINE = re.compile(
    r"""
    (?:assistantfinal|assistant)?\s*
    (?P<i>T\d{6})
    \s*[\t ]+
    (?P<p>NA|[0-9]{1,3}(?:\.\d+)?)
    \s*%?
    [\t ]+
    (?P<j>Very\ Low|Low|Moderate|High|Very\ High)
    [\t ]+
    (?P<exp>.*)
    """,
    re.IGNORECASE | re.VERBOSE,
)


# ------------------------- HELPERS -------------------------
def clamp01_or_na(s: Optional[str]) -> Optional[int]:
    if not s:
        return None
    s = s.strip().replace("%", "")
    if s.upper() == "NA":
        return None
    try:
        v = int(float(s))
        return max(0, min(100, v))
    except ValueError:
        return None

def sanitize_task_text(task: str) -> str:
    return re.sub(r"[\t\r\n]+", " ", task).strip()


def normalize_judgment(s: Optional[str]) -> Optional[str]:
    if not s:
        return None
    s = " ".join(s.strip().split()).lower()
    mapping = {
        "very low": "Very Low",
        "low": "Low",
        "moderate": "Moderate",
        "high": "High",
        "very high": "Very High",
    }
    return mapping.get(s)

def judgment_from_score(score: Optional[int]) -> Optional[str]:
    if score is None:
        return None
    if 0 <= score <= 20:
        return "Very Low"
    elif score <= 40:
        return "Low"
    elif score <= 60:
        return "Moderate"
    elif score <= 80:
        return "High"
    else:
        return "Very High"


def sanitize_task_text(task: str) -> str:
    return re.sub(r"[\t\r\n]+", " ", task).strip()


def build_user_content(pair: Tuple[str, str]) -> str:
    tid, task = pair
    row = f"{tid}\t{task}"
    return (
        "Rate the following tasks. For EACH task, output ONE line in this format:\n"
        "ID<TAB>POINT<TAB>JUDGMENT<TAB>EXP\n\n"
        "Where:\n"
        "- ID is the given ID (e.g., T000001)\n"
        "- POINT is an integer from 0 to 100 (no percent sign)\n"
        "- JUDGMENT is exactly one of: Very Low, Low, Moderate, High, Very High\n"
        "- EXP is a short explanation (<= 2 sentences)\n\n"
        "IMPORTANT:\n"
        "- Output EXACTLY one line per task.\n"
        "- Keep the SAME ORDER as the tasks below.\n"
        "- Do NOT add any header, bullets, or extra text.\n"
        "- Do NOT skip or merge tasks.\n\n"
        "TASKS:\n"
        f"{row}"
    )

def call_chat(messages, max_new_tokens: int, temperature: float = 0.0, retries: int = 4):
    delay = 2.0
    for attempt in range(retries):
        try:
            return client.chat.completions.create(
                model=MODEL_NAME,
                messages=messages,
                max_tokens=max_new_tokens,
                temperature=temperature,
            )
        except Exception as e:
            print(f"[Retry {attempt+1}/{retries}] Error: {e}")
            if attempt == retries - 1:
                raise
            time.sleep(delay)
            delay = min(20.0, delay * 1.8)

def parse_tsv_response(text: str, expected_ids: List[str]) -> Dict[str, Dict[str, object]]:
    found: Dict[str, Dict[str, object]] = {}
    lines = [l.rstrip() for l in text.splitlines() if l.strip()]

    for raw in lines:
        m = ID_LINE.search(raw)
        if not m:
            print(f"[PARSE SKIP] {raw!r}")
            continue

        tid = m.group("i")
        if tid not in expected_ids:
            print(f"[PARSE UNKNOWN ID] {raw!r}")
            continue

        point = clamp01_or_na(m.group("p"))
        judgment = None
        if "j" in m.groupdict():
            judgment = normalize_judgment(m.group("j"))

        exp = (m.group("exp") or "").strip()

        if judgment is None and point is not None:
            judgment = judgment_from_score(point)

        found[tid] = {
            "point": point,
            "judgment": judgment,
            "exp": exp,
        }

    return found

# ------------------------- MAIN (NO BATCHING) -------------------------
def evaluate_tasks(
    input_df: pd.DataFrame,
    task_col: str = "task",
    temperature: float = 0.0,
    max_new_tokens: int = 256,
    retries_per_task: int = 2,
) -> pd.DataFrame:

    required_cols = [task_col]
    missing_cols = [c for c in required_cols if c not in input_df.columns]
    if missing_cols:
        raise ValueError(f"Missing required columns: {missing_cols}")

    work_df = input_df[required_cols].copy()
    work_df = work_df[work_df[task_col].notna()].reset_index(drop=True)

    all_rows = []

    for idx, row in work_df.iterrows():
        tid = f"T{idx:06d}"
        clean_task = sanitize_task_text(row[task_col])

        print(f"Evaluating {tid}...")

        parsed = {}
        for attempt in range(retries_per_task + 1):
            user_content = build_user_content((tid, clean_task))
            messages = [
                {"role": "system", "content": SYSTEM_PROMPT_BATCH},
                {"role": "user", "content": user_content},
            ]

            resp = call_chat(
                messages,
                max_new_tokens=max_new_tokens,
                temperature=temperature,
            )
            content = (resp.choices[0].message.content or "").strip()
            parsed = parse_tsv_response(content, [tid])

            if tid in parsed:
                break

            print(f"Warning: missing parse for {tid}, retry {attempt + 1}/{retries_per_task}")

        rec = parsed.get(
            tid,
            {"point": None, "judgment": None, "exp": ""}
        )

        # Fallback: infer judgment from score if the model omitted it
        if rec["judgment"] is None:
            rec["judgment"] = judgment_from_score(rec["point"])

        all_rows.append(
            {
                "task_id": tid,
                "task": clean_task,
                "Estimate": rec["point"],
                "Qualitative_Judgment": rec["judgment"],
                "Explanation": rec["exp"],
            }
        )

    out_df = pd.DataFrame(all_rows)
    return out_df.sort_values("task_id").reset_index(drop=True)

# ------------------------- RUN EXAMPLE -------------------------
if __name__ == "__main__":
    #df = pd.read_csv("missing_complement_gemini_match.csv", sep="\t")
    df = pd.read_excel("Task Statements.xlsx", sheet_name="Task Statements")

    # keep ALL tasks
    tasks = df["task"].dropna().astype(str).unique().tolist()



    print(f"Loaded {len(tasks)} tasks for evaluation...")

    out = evaluate_tasks(
        df,
        task_col="task",
        temperature=0.0,
        max_new_tokens=256,
        retries_per_task=2,
    )

    out.to_csv("complement_gemini.csv", index=False)
    print(f"✅ Saved {len(out)} fully evaluated tasks to replace_gemini_missing.csv")
