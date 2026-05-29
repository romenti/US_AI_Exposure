HF_TOKEN = "Insert API Key here"

import os
from openai import OpenAI
import os
import time
import re
import pandas as pd

from typing import List, Tuple, Dict, Optional
from huggingface_hub import InferenceClient, login
import os
import os, re, time
from typing import Optional, Tuple, List, Dict
import pandas as pd
from openai import OpenAI

# ------------------------- CLIENT -------------------------


HF_TOKEN = "Insert API Key here"
client = OpenAI(
    base_url="",
    api_key=HF_TOKEN,
)

MODEL_NAME = "google/gemma-3-27b-it:featherless-ai"

# ------------------------- PROMPTS -------------------------

SYSTEM_PROMPT_BATCH SYSTEM_PROMPT_BATCH_REP = (
    "You are an automation-risk evaluator.\n\n"
    "TASK:\n"
    "For each input task, estimate the probability (0-100) that the task could be
    FULLY REPLACED by Generative AI.\n\n"
    "DEFINITION:\n"
    "'Fully replaced' means that Generative AI can independently produce the core output of the task without human execution, physical presence, or real-world interaction.\n\n"
     "If Generative AI would not realistically interpret and execute autonomously the task from the input without human oversight, assign a very low probability.\n"
    "Include all major forms of Generative AI:\n"
    "- text generation\n"
    "- code generation\n"
    "- image/video generation\n"
    "- audio generation\n\n"
    "Do NOT consider:\n"
    "- robotics\n"
    "- physical automation\n"
    "- human supervision or hybrid workflows\n\n"
    "SCORING SCALE:\n"
    "0-10   = Very unlikely to be replaced\n"
    "11-30  = Low replacement likelihood\n"
    "31-50  = Moderate-low replacement likelihood\n"
    "51-70  = Moderate replacement likelihood\n"
    "71-90  = High replacement likelihood\n"
    "91-100 = Very high replacement likelihood\n\n"
    "QUALITATIVE JUDGMENT:\n"
    "Map each score to exactly one label:\n"
    "- Very Low\n"
    "- Low\n"
    "- Moderate\n"
    "- High\n"
    "- Very High\n\n"
    "Suggested mapping:\n"
    "- 0-20 -> Very Low\n"
    "- 21-40 -> Low\n"
    "- 41-60 -> Moderate\n"
    "- 61-80 -> High\n"
    "- 81-100 -> Very High\n\n"
    "IMPORTANT:\n"
    "- Assign a precise integer from 0 to 100.\n"
    "- Avoid rounding everything to multiples of 5.\n"
    "- Use the full scale.\n"
    "- Keep the explanation to no more than 2 sentences.\n\n"
    "OUTPUT FORMAT (STRICT):\n"
    "Return EXACTLY one TSV line per input task, in the SAME ORDER.\n"
    "Each line MUST be:\n"
    "ID<TAB>POINT<TAB>JUDGMENT<TAB>EXP\n\n"
    "Where:\n"
    "- ID is the given ID (e.g., T000001)\n"
    "- POINT is an integer from 0 to 100\n"
    "- JUDGMENT is exactly one of: Very Low, Low, Moderate, High, Very High\n"
    "- EXP is a short explanation\n\n"
    "STRICT RULES:\n"
    "- No headers\n"
    "- No bullet points\n"
    "- No extra text\n"
    "- No blank lines\n"
)

# ------------------------- TSV LINE REGEX -------------------------
ID_LINE = re.compile(
    r"""
    (?:assistantfinal|assistant)?   # optional chat prefix
    \s*
    (?P<i>T\d{6})                   # ID like T000001
    \s*[\t ]+                       # separator after ID
    (?P<p>NA|[0-9]{1,3}(?:\.\d+)?)  # NA or 0-100
    \s*%?                           # optional percent sign
    [\t ,;]+                        # separator
    (?P<exp>.*)                     # explanation
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

def build_user_content(pair: Tuple[str, str]) -> str:
    tid, task = pair
    row = f"{tid}\t{task}"
    return (
        "Rate the following tasks. For EACH task, output ONE line in this format:\n"
        "ID<TAB>POINT<TAB>EXP\n"
        "Where:\n"
        "- ID is the given ID (e.g., T000001)\n"
        "- POINT is an integer from 0 to 100 (no percent sign)\n"
        "- EXP is a short explanation (<= 2 sentences)\n\n"
        "IMPORTANT:\n"
        "- Output EXACTLY one line per task.\n"
        "- Keep the SAME ORDER as the tasks below.\n"
        "- Do NOT add any header, bullets, or extra text.\n\n"
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

        P = clamp01_or_na(m.group("p"))
        exp = (m.group("exp") or "").strip()
        found[tid] = {"point": P, "exp": exp}

    return found

# ------------------------- MAIN (NO BATCHING) -------------------------
def evaluate_tasks(
    tasks: List[str],
    temperature: float = 0.0,
    max_new_tokens: int = 256,
    retries_per_task: int = 2,
) -> pd.DataFrame:

    all_rows = []

    for idx, task in enumerate(tasks):
        tid = f"T{idx:06d}"
        clean_task = sanitize_task_text(task)
        print(f"Evaluating {tid}...")

        parsed = {}
        for attempt in range(retries_per_task + 1):
            user_content = build_user_content((tid, clean_task))
            messages = [
                {"role": "system", "content": SYSTEM_PROMPT_BATCH},
                {"role": "user", "content": user_content},
            ]

            resp = call_chat(messages, max_new_tokens=max_new_tokens, temperature=temperature)
            content = (resp.choices[0].message.content or "").strip()
            parsed = parse_tsv_response(content, [tid])

            if tid in parsed:
                break
            else:
                print(f"⚠️ Missing parse for {tid}, retry {attempt+1}/{retries_per_task}")

        rec = parsed.get(tid, {"class": None, "exp": ""})
        all_rows.append(
            {
                "task_id": tid,
                "task": clean_task,
                "Estimate": rec["point"],
                "Explanation": rec["exp"],
            }
        )

    df = pd.DataFrame(all_rows)
    return df.sort_values("task_id").reset_index(drop=True)

# ------------------------- RUN EXAMPLE -------------------------
if __name__ == "__main__":
    df = pd.read_excel("Task Statements.xlsx", sheet_name="Task Statements")

    # keep ALL tasks
    tasks = df["Task"].dropna().astype(str).unique().tolist()


    print(f"Loaded {len(tasks)} tasks for evaluation...")
    out = evaluate_tasks(tasks, temperature=0.0)
    out.to_csv("replacement_gemini.csv", index=False)
    print(f"✅ Saved {len(out)} fully evaluated tasks to complement_gen_ai_gemini_trial_continuous.csv")
