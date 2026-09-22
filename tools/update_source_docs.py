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

# Country-code -> continent, for the human-readable by-region index below.
# 0 is the generic OpenTopography index service (no single country).
CONTINENTS = {
    0: "Global", 840: "North America", 124: "North America",
    76: "South America", 554: "Oceania",
    756: "Europe", 250: "Europe", 528: "Europe", 578: "Europe", 246: "Europe",
    616: "Europe", 233: "Europe", 276: "Europe", 724: "Europe", 40: "Europe",
    56: "Europe", 100: "Europe", 191: "Europe", 203: "Europe", 208: "Europe",
    348: "Europe", 372: "Europe", 380: "Europe", 428: "Europe", 440: "Europe",
    442: "Europe", 470: "Europe", 620: "Europe", 642: "Europe", 703: "Europe",
    705: "Europe", 752: "Europe",
}
CONTINENT_ORDER = ["Global", "North America", "South America", "Europe", "Oceania"]
missing = {int(r["country_code"]) for r in records} - set(CONTINENTS)
if missing:
    raise ValueError(f"Add a continent mapping for country code(s): {sorted(missing)}")

by_continent = {c: [] for c in CONTINENT_ORDER}
for row in records:
    by_continent[CONTINENTS[int(row["country_code"])]].append(row)

n_available = sum(1 for r in records if r["implemented"] == "TRUE")
lines = ["# Datasets by region", "",
         "Generated from `inst/extdata/providers.csv` by `tools/update_source_docs.py`; "
         "see [SOURCES.md](../inst/sources/SOURCES.md) for the full access/licence text behind "
         "each entry, and the [README](../README.md) for a handful of representative examples.",
         "",
         f"**{n_available} available in-app (in-app search and download) and "
         f"{len(records) - n_available} portal-only (linked official source, no in-app adapter) "
         f"entries, {len(records)} total.**", "",
         "- 🟢 **Available in-app** - search and download inside the app.",
         "- 🔗 **Portal only** - follow the official link and download there; no in-app adapter yet.",
         ""]
for continent in CONTINENT_ORDER:
    rows = by_continent[continent]
    if not rows:
        continue
    lines.append(f"## {continent}")
    lines.append("")
    lines.append("| Country | Source | Status |")
    lines.append("|---|---|---|")
    for row in sorted(rows, key=lambda r: (r["country"], r["name"])):
        status = "🟢 Available in-app" if row["implemented"] == "TRUE" else "🔗 Portal only"
        lines.append("| " + " | ".join([
            cell(row["country"]), f'[{cell(row["name"])}]({row["info_url"]})', status]) + " |")
    lines.append("")
lines.append("[Full evidence and access-check history](../docs/ACTIVE_SOURCES.md) · "
              "[EU-wide coverage detail](../docs/EU_COVERAGE_TRACKER.md).")
(root / "docs" / "DATASETS.md").write_text("\n".join(lines) + "\n", encoding="utf-8")

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
         "Dependencies retain their own licences; ALSdownloadeR does not claim authorship of them.", "",
         "| Dependency | Role | Version requirement |", "|---|---|---|"]
for field, role in [("Depends", "Runtime"), ("Imports", "Required"), ("Suggests", "Optional / development")]:
    for entry in fields.get(field, "").split(","):
        match = re.fullmatch(r"\s*([A-Za-z][A-Za-z0-9.]*)\s*(?:\(([^)]+)\))?\s*", entry)
        if not match:
            raise ValueError(f"Unparsed dependency: {entry}")
        name, constraint = match.groups()
        base_r_packages = {"stats", "tools", "utils", "methods", "graphics", "grDevices", "parallel", "datasets"}
        if name == "R":
            url = "https://www.r-project.org/"
        elif name in base_r_packages:
            url = f"https://stat.ethz.ch/R-manual/R-devel/library/{name}/00Index.html"
        else:
            url = f"https://CRAN.R-project.org/package={name}"
        role = "Bundled with R" if name in base_r_packages else role
        lines.append(f"| [{name}]({url}) | {role} | {constraint or 'Not specified'} |")
lines.extend(["", "Browser libraries delivered through Shiny, Leaflet and DT remain subject to",
              "their upstream package notices. Bundled geography and external map services",
              "are documented separately in [NOTICE](../NOTICE)."])
(target / "DEPENDENCIES.md").write_text("\n".join(lines) + "\n", encoding="utf-8")
print(f"Documented {len(records)} catalogue entries, the by-region dataset index and DESCRIPTION dependencies.")
