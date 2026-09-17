"""Report observed approval times from public GitHub issues, never a promised SLA."""
import datetime as dt
import json
import os
import statistics
import urllib.request
from pathlib import Path


def days_between(created, approved):
    parse = lambda value: dt.datetime.fromisoformat(value.replace("Z", "+00:00"))
    return (parse(approved) - parse(created)).total_seconds() / 86400


def get_pages(path):
    repo = os.environ["GITHUB_REPOSITORY"]
    for page in range(1, 1001):
        sep = "&" if "?" in path else "?"
        url = f"https://api.github.com/repos/{repo}/{path}{sep}per_page=100&page={page}"
        headers = {"Accept": "application/vnd.github+json", "User-Agent": "ALS-source-approval-report"}
        token = os.environ.get("GH_TOKEN")
        if token:
            headers["Authorization"] = "Bearer " + token
        with urllib.request.urlopen(urllib.request.Request(url, headers=headers), timeout=30) as response:
            items = json.load(response)
        yield from items
        if len(items) < 100:
            return
    raise RuntimeError("Pagination limit reached; report not replaced")


def main():
    durations = []
    for issue in get_pages("issues?state=all&labels=source-approved"):
        if "pull_request" in issue or not issue["title"].startswith("Dataset suggestion:"):
            continue
        approvals = [event["created_at"] for event in get_pages(f"issues/{issue['number']}/events")
                     if event.get("event") == "labeled" and event.get("label", {}).get("name") == "source-approved"]
        if not approvals:
            raise RuntimeError("Approval event unavailable; retain the previous report")
        durations.append(days_between(issue["created_at"], min(approvals)))
    text = "# Source approval times\n\n"
    text += "Measured from GitHub issue creation to the first `source-approved` label, for dataset suggestions currently carrying that label. Email-only proposals are not included.\n\n"
    if durations:
        text += f"Approved requests: **{len(durations)}**. Mean: **{statistics.mean(durations):.1f} calendar days**. Median: **{statistics.median(durations):.1f} calendar days**.\n\n"
    else:
        text += "**No recorded approvals yet; an average is not available.**\n\n"
    text += "Observed turnaround is not a deadline or guarantee. Approval is a maintainer decision; a successful sample test does not approve or publish a source.\n"
    Path("docs/APPROVAL_TIMES.md").write_text(text, encoding="utf-8")


if __name__ == "__main__":
    main()
