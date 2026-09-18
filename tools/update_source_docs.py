"""Generate installed source documentation from the package's canonical metadata."""
import csv
import re
from pathlib import Path

root = Path(__file__).resolve().parents[1]
target = root / "inst" / "sources"
target.mkdir(parents=True, exist_ok=True)

def cell(value):
    return str(value).replace("|", "\\|").replace("\n", " ")

with (root / "inst" / "extdata" / "providers.csv").open(encoding="utf-8-sig", newline="") as f:
    records = list(csv.DictReader(f))
lines = ["# Complete source catalogue", "",
         "Generated from `inst/extdata/providers.csv`; see [access and attribution policy](README.md).",
         "", f"{len(records)} source/product entries, not a count of individual datasets or tiles.",
         "An available adapter is a technical capability, not blanket permission for every asset.", "",
         "| Source | Country | Adapter | Access and limitations | Reviewed |",
         "|---|---|---|---|---|"]
for row in records:
    label = "Available; see conditions" if row["implemented"] == "TRUE" else "Not implemented; reference only"
    lines.append("| " + " | ".join([
        f'[{cell(row["name"])}]({row["info_url"]})', cell(row["country"]),
        label, cell(row["access"]), row["reviewed_on"]]) + " |")
(target / "SOURCES.md").write_text("\n".join(lines) + "\n", encoding="utf-8")

fields = {}
key = None
for line in (root / "DESCRIPTION").read_text(encoding="utf-8").splitlines():
    if line[:1].isspace() and key:
        fields[key] += " " + line.strip()
    elif ":" in line:
        key, value = line.split(":", 1)
        fields[key] = value.strip()
lines = ["# R dependencies", "",
         "Generated from DESCRIPTION. Package records link to their own authors, licences and documentation.",
         "Use `citation(\"packageName\")` and `packageDescription(\"packageName\")` for the installed version.",
         "Dependencies retain their own licences; ALS Downloader does not claim authorship of them.", "",
         "| Dependency | Role | Version requirement |", "|---|---|---|"]
for field, role in [("Depends", "Runtime"), ("Imports", "Required"), ("Suggests", "Optional / development")]:
    for entry in fields.get(field, "").split(","):
        match = re.fullmatch(r"\s*([A-Za-z][A-Za-z0-9.]*)\s*(?:\(([^)]+)\))?\s*", entry)
        if not match:
            raise ValueError(f"Unparsed dependency: {entry}")
        name, constraint = match.groups()
        url = "https://www.r-project.org/" if name == "R" else f"https://CRAN.R-project.org/package={name}"
        lines.append(f"| [{name}]({url}) | {role} | {constraint or 'Not specified'} |")
lines.extend(["", "Browser libraries delivered through Shiny, Leaflet and DT remain subject to",
              "their upstream package notices. Bundled geography and external map services",
              "are documented separately in [NOTICE](../NOTICE)."])
(target / "DEPENDENCIES.md").write_text("\n".join(lines) + "\n", encoding="utf-8")
print(f"Documented {len(records)} catalogue entries and DESCRIPTION dependencies.")
