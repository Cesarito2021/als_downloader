# ALS downloader web-based shiny app

ALS Downloader is a Shiny web application developed within the NSF-funded OpenForest4D project for searching, exploring, and downloading airborne LiDAR point-cloud datasets (.LAS/.LAZ) from OpenTopography and USGS 3D Elevation Program (3DEP) sources. The application allows users to query LiDAR data by area of interest and acquisition period, view summaries by product, explore data availability and spatial coverage, and download selected LAZ/LAS tiles locally. Multicore parallel processing enables efficient, scalable data retrieval.

🔗 OpenForest4D: https://openforest4d.org

🔗 OpenTopography: https://portal.opentopography.org/datasets

🔗 USGS 3DEP datasets: https://www.usgs.gov/3d-elevation-program

## Application Overview
![Application banner](www/logo/logo_300dpi.png)

## Overview and Access
ALS downloader is a Shiny-based application designed to search, explore, and download Airborne Laser Scanning (ALS / LiDAR) data from the OpenTopography and USGS 3DEP program.

The application allows users to select source, define a study area (AOI), configurate app and identify ALS tile availability across years and projects, and download selected LAZ tiles locally, leveraging multi-core parallel processing.
The interface is organized into the following functional components (see Figure below):

## User Interface (UI) and Architecture
A) Study Area Input (AOI)
B) Local Processing Configuration/ Local Data verification
C) Output Configuration
D) LiDAR Tile Discovery
E) ALS Data Download
F) Application Header and Project Context

### OpenTopograhy User Interface (UI) and Architecture
![Image](https://github.com/user-attachments/assets/86ea4433-b57c-4511-b2ed-ab6a2fcd3531)

### USGS 3DEP User Interface (UI) and Architecture
![Image](https://github.com/user-attachments/assets/1dc96cd2-d00e-4d18-99f8-e03eeb796610)

⚠️ Important
Due to cloud execution constraints, parallel downloads are only available when running the app locally.
The hosted Shiny version is intended only for data discovery and availability assessment.

## Purpose and Use Cases
ALS downloader is designed to support:
- ALS data availability assessment for forest, environmental, and geomatics studies
- Efficient local data acquisition for large AOIs using multi-core CPUs
- The app is not intended for point cloud processing or visualization, but rather for data discovery and acquisition.

## Opportunities and Challenges
The ALS Downloader provides similar core functionality in both online and local execution modes.
However, the associated challenges and limitations differ depending on the execution environment.
- Note: The online version only supports exploration of ALS data availability from the USGS 3DEP source and does not provide access to OpenTopography datasets.

### Opportunities (common to Online and Local versions)

| Opportunity | Description |
|------------|-------------|
| ALS availability assessment | Identify OpenTopography and USGS 3DEP ALS data intersecting a user-defined AOI |
| Metadata exploration | Inspect available acquisition years, project sources, and tile counts |
| Spatial coverage understanding | Understand the spatial distribution of ALS data within the AOI |
| Data acquisition planning | Support informed planning prior to large-scale data downloads |
| Reproducible workflows | Enable consistent AOI-based data discovery across environments |

---

### Challenges / Limitations (by execution mode)

| Execution Mode | Challenges / Limitations |
|---------------|--------------------------|
| **Online (Shiny deployment)** | No parallel downloads;<br>Limited execution time;<br>Not suitable for large-scale LAZ downloads;<br>Restricted access to local file system |
| **Local (desktop execution)** | Download performance depends on local hardware;<br>Parallelization limited by available CPU cores;<br>Requires local setup (R environment and dependencies);<br>Requires sufficient disk space and network bandwidth |

---

**Summary**

- ➡️ Online execution is best suited for exploring ALS data availability and planning data acquisition from USGS 3DEP; it does not support OpenTopography access.
- ➡️ Local execution is recommended for efficient, large-scale ALS (LAZ) downloads and operational workflows, and provides access to both OpenTopography and USGS 3DEP data sources.

## How to Configure the App Locally (Recommended for Downloading ALS Data)

To fully leverage the application, users should run it locally.

### Step 1 – Download the Application

- Download the repository as a ZIP file
- Unzip it on your local machine

### Step 2 – Download and install the data folder

The data/ directory is not included in the GitHub repository and must be downloaded separately.
1. Download the data folder
Download the data folder from Google Drive:https://drive.google.com/drive/folders/1-MyjrJmtrLpQR1Dc0bU227yq9ID1rZoV?usp=drive_link .

After downloading, move or copy the entire data/ folder into the root of the repository:
```r
ot_pc_app/
├── app.R
├── base.R
├── data/
│   └── * TileIndex_all/
│       └── all .zip files
├── www/
│   └── css/
│   │    └── main.css
│   └── logo/
│       └── logo_300dpi.png
├── .gitignore
└── README.md
```

### Step 3 – Open the Project

- Open the project folder in RStudio
- Ensure required R packages are installed (as listed in the project documentation)

### Step 3 – Run the Application

```r
# ============================
# Run lidar_app locally
# ============================

# Set the working directory to the application folder
# Replace the path below with the location where you unzipped the project
setwd("path/to/ot_pc_app")

# Verify that the app files are present
list.files()

# Run the Shiny application
shiny::runApp("app.R")

# The app will now have access to your local CPU resources
```

## Workflow Tutorial

This section describes the **step-by-step workflow** for discovering and downloading
OpenTopography and USGS 3DEP data using `als_downloader`.  
The workflow guides users from **AOI definition** to **local LAZ data acquisition**.

---

### 1. Study Area Input (AOI)

Users define the Area of Interest by uploading one of the following formats:

- a zipped Shapefile
- a GeoPackage (`.gpkg`)
- a GeoJSON file

If multiple polygons are present, users can select one or more features
based on attribute values.

---
### 2. Source 

- OpenTopography
- USGS 3DEP

### 3. Local Processing Configuration/ Local data verification

This step controls download performance:
- user specify the ALS source to be used
- users specify the number of CPU cores to be used
- parallel downloads significantly reduce acquisition time for large AOIs

⚠️ This option is effective **only when running the app locally**.

---

### 3. Output Configuration

Users define:

- the output directory on their local machine
- the project folder name

Downloaded LAZ files are automatically organized in a structured
folder hierarchy to support reproducible workflows.

---

### 4. LiDAR Tile Discovery

After configuring the AOI:

- the app queries USGS 3DEP ALS metadata services
- ALS tiles intersecting the AOI are identified

Results are summarized by:

- acquisition year
- project/source
- number of available tiles

This step allows users to evaluate ALS availability before downloading data.

---

### 5. ALS Data Download

Users can:

- select specific acquisition years
- select specific ALS projects
- download only the required LAZ tiles

Downloaded files are saved locally and can be directly used in:

- PDAL
- lidR
- CloudCompare
- GIS software

## Developers and Maintainers

- Cesar Alvites — University of Florida  
- Carlos Alberto Silva — University of Florida  
- Viswanath Nandigam — San Diego Supercomputer Center, University of California San Diego  
- Chelsea Scott — Arizona State University  
- Inacio Bueno — University of Florida

## Acknowledgements

This application was developed within the OpenForest4D (https://openforest4d.org/) cyberinfrastructure initiative, supported by academic and research institutions focused on next-generation forest mapping and monitoring.
