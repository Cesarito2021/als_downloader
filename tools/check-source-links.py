"""Explicit, bounded HTTP check; does not validate dataset identity or download files.

Run from a checkout: python tools/check-source-links.py
Only Python standard-library modules are required. TLS verification stays enabled.
"""
import concurrent.futures
import csv
from datetime import datetime, timezone
import html
from pathlib import Path
import re
import urllib.error
import urllib.parse
import urllib.request

ROOT = Path(__file__).resolve().parents[1]


def redact(url):
    parts = urllib.parse.urlsplit(url)
    if any(key in parts.query.lower() for key in ("signature=", "token=", "sig=", "key-pair-id=")):
        return urllib.parse.urlunsplit(parts._replace(query=""))
    return url


def check(url):
    row = dict(url=url, checked_at_utc=datetime.now(timezone.utc).isoformat(),
               http_status="", final_url="", title="", result="")
    try:
        request = urllib.request.Request(url, headers={"User-Agent": "ALSDownloader-source-review/1.0"})
        with urllib.request.urlopen(request, timeout=30) as response:
            content = response.read(1_500_000).decode("utf-8", errors="replace")
            title = re.search(r"<title[^>]*>(.*?)</title>", content, re.I | re.S)
            row.update(http_status=response.status, final_url=redact(response.url),
                       title=html.unescape(re.sub(r"\s+", " ", title.group(1))).strip() if title else "",
                       result="HTTP reachable; identity and file access are separate checks")
    except urllib.error.HTTPError as error:
        row.update(http_status=error.code, final_url=redact(error.url),
                   result="HTTP error; not evidence of dataset absence")
    except Exception as error:
        row["result"] = type(error).__name__ + ": " + str(error)
    return row


if __name__ == "__main__":
    urls = set()
    for path, columns in [("docs/dataset-candidates.csv", ("source_url", "reviewed_url")),
                          ("inst/extdata/providers.csv", ("info_url",))]:
        with (ROOT / path).open(encoding="utf-8-sig", newline="") as stream:
            for record in csv.DictReader(stream):
                urls.update(record[c] for c in columns if record.get(c))
    # Preserve originally supplied provider URLs even after they are corrected.
    destination = ROOT / "docs/link-checks.csv"
    if destination.exists():
        with destination.open(encoding="utf-8-sig", newline="") as stream:
            urls.update(record["url"] for record in csv.DictReader(stream))
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        rows = list(pool.map(check, sorted(urls)))
    with destination.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    print(f"Checked {len(rows)} URLs; {sum(r['http_status'] == 200 for r in rows)} HTTP 200 responses.")
    print("See docs/link-checks.csv. Reachability is not point-cloud validation.")
