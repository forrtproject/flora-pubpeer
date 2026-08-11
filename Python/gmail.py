from __future__ import annotations

import logging
import os
import smtplib
import time
import uuid
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from typing import Any, Dict, List, Union

logger = logging.getLogger(__name__)

_MAX_RETRIES = 3


def send_email(
    to: Union[str, List[str]],
    subject: str,
    html_body: str,
    *,
    plain_body: str = "",
    sender: str | None = None,
) -> Dict[str, Any]:
    """Send an email via Gmail SMTP with an app password.

    *to* may be a single address or a list of addresses.  When a list is
    given all recipients appear in the To header and the SMTP envelope, i.e.
    they are all addressed on the same email rather than sent individually.

    This is a one-off send, not a recurring newsletter, so no
    List-Unsubscribe headers are added.

    Returns a dict with 'id' (a generated message reference).
    """
    sender_addr = sender or os.environ.get("GMAIL_SENDER_ADDRESS", "flora@replications.forrt.org")
    app_password = os.environ.get("GMAIL_APP_PASSWORD", "")
    if not app_password:
        raise RuntimeError("GMAIL_APP_PASSWORD env var is not set")

    recipients = [to] if isinstance(to, str) else list(to)

    msg = MIMEMultipart("alternative")
    display_name = os.environ.get("SENDER_DISPLAY_NAME", "RepNote Team")
    msg["From"] = f"{display_name} <{sender_addr}>"
    msg["To"] = ", ".join(recipients)
    msg["Subject"] = subject
    msg["Reply-To"] = sender_addr

    # Plain text first (fallback), then HTML (preferred) — per MIME convention
    if plain_body:
        msg.attach(MIMEText(plain_body, "plain"))
    msg.attach(MIMEText(html_body, "html"))

    message_id = uuid.uuid4().hex[:16]

    last_error: Exception | None = None
    for attempt in range(_MAX_RETRIES):
        try:
            with smtplib.SMTP_SSL("smtp.gmail.com", 465) as server:
                server.login(sender_addr, app_password)
                server.sendmail(sender_addr, recipients, msg.as_string())
            logger.info("Email sent", extra={"to": recipients, "message_id": message_id})
            return {"id": message_id}
        except smtplib.SMTPAuthenticationError:
            raise
        except (smtplib.SMTPException, OSError) as e:
            if attempt < _MAX_RETRIES - 1:
                wait = 2 ** attempt
                logger.warning(
                    "SMTP transient error, retrying",
                    extra={"attempt": attempt, "wait": wait, "error": str(e)},
                )
                time.sleep(wait)
                last_error = e
                continue
            raise

    raise last_error  # type: ignore[misc]
