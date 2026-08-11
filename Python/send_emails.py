"""Send the pre-formatted RepNote/PubPeer emails from data/formatted_emails.csv.

Each row of that CSV is one already-rendered email (built by
email_formatting.Rmd):
  - email_addresses: one or more recipient addresses, separated by "; ".
    When a row has several, they are co-authors of the same paper and are
    all addressed on the SAME email, not sent as separate emails.
  - html: the full, ready-to-send HTML body for that email.

This is a one-off send, not a recurring newsletter, so gmail.send_email()
adds no unsubscribe headers.

Sending is rate-limited to at most MAX_PER_DAY emails per calendar day, with
WAIT_SECONDS between each send, and is resumable: every attempt is recorded
in data/send_log.csv, so re-running the script (e.g. once a day until all
rows are sent) skips whatever was already sent and continues from there.
"""
from __future__ import annotations

import csv
import logging
import os
import sys
import time
from datetime import date, datetime

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from dotenv import load_dotenv

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
load_dotenv(os.path.join(PROJECT_ROOT, ".env"))

from gmail import send_email

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
logger = logging.getLogger(__name__)

FORMATTED_EMAILS_PATH = os.path.join(PROJECT_ROOT, "data", "formatted_emails.csv")
SEND_LOG_PATH = os.path.join(PROJECT_ROOT, "data", "send_log.csv")

# Must match the Subject line baked into the html by email_formatting.Rmd's
# make_email_html() — there is no separate subject column in the CSV.
SUBJECT = "Your work has a replication/reproduction — upcoming PubPeer comment"

MAX_PER_DAY = 200
WAIT_SECONDS = 120  # 2 minutes between emails

LOG_FIELDS = ["doi_o", "email_addresses", "status", "timestamp", "message_id", "error"]


def load_sent_dois() -> set[str]:
    """DOIs already sent successfully in a previous run of this script."""
    if not os.path.exists(SEND_LOG_PATH):
        return set()
    with open(SEND_LOG_PATH, newline="", encoding="utf-8") as f:
        return {row["doi_o"] for row in csv.DictReader(f) if row["status"] == "sent"}


def count_sent_today() -> int:
    if not os.path.exists(SEND_LOG_PATH):
        return 0
    today = date.today().isoformat()
    with open(SEND_LOG_PATH, newline="", encoding="utf-8") as f:
        return sum(
            1 for row in csv.DictReader(f)
            if row["status"] == "sent" and row["timestamp"].startswith(today)
        )


def log_attempt(doi_o: str, email_addresses: str, status: str, message_id: str = "", error: str = "") -> None:
    file_exists = os.path.exists(SEND_LOG_PATH)
    with open(SEND_LOG_PATH, "a", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=LOG_FIELDS)
        if not file_exists:
            writer.writeheader()
        writer.writerow({
            "doi_o": doi_o,
            "email_addresses": email_addresses,
            "status": status,
            "timestamp": datetime.now().isoformat(timespec="seconds"),
            "message_id": message_id,
            "error": error,
        })


def main() -> None:
    with open(FORMATTED_EMAILS_PATH, newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))

    already_sent = load_sent_dois()
    pending = [r for r in rows if r["doi_o"] not in already_sent]

    remaining_today = MAX_PER_DAY - count_sent_today()
    if remaining_today <= 0:
        logger.info("Daily quota of %d already reached today — nothing to do.", MAX_PER_DAY)
        return

    if not pending:
        logger.info("Nothing left to send — all %d rows already sent.", len(rows))
        return

    batch = pending[:remaining_today]
    logger.info(
        "%d already sent, %d pending. Sending %d now (daily quota remaining: %d).",
        len(already_sent), len(pending), len(batch), remaining_today,
    )

    for i, row in enumerate(batch, 1):
        doi_o = row["doi_o"]
        recipients = [e.strip() for e in row["email_addresses"].split(";") if e.strip()]

        logger.info("Sending %d/%d: %s -> %s", i, len(batch), doi_o, ", ".join(recipients))
        try:
            result = send_email(to=recipients, subject=SUBJECT, html_body=row["html"])
            log_attempt(doi_o, row["email_addresses"], "sent", message_id=result.get("id", ""))
            logger.info("Sent. Message ID: %s", result.get("id"))
        except Exception as e:
            logger.error("Failed to send for %s: %s", doi_o, e)
            log_attempt(doi_o, row["email_addresses"], "failed", error=str(e))

        if i < len(batch):
            time.sleep(WAIT_SECONDS)

    logger.info("Done — %d emails processed this run.", len(batch))


if __name__ == "__main__":
    main()
