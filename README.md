# BCSB Recruitment and Retention Data Processing

This repository contains code for an R Shiny dashboard for tracking participants in BCSB.

## Input Files

The pipeline uses the following input files (automatically mathces the prefix in /input):

- `Web Eligibility Survey` - Patient eligibility information
- `OpenSpecimen_collection` - Specimen collection summary data, need to remove ~5 headers
- `BaselineTimeFromDx` - Baseline timepoint measurements (raw)
- `BaselineFTimeFromDx` - Baseline timepoint measurements (raw)
- `ScreeningSummary` - Screening process summary data
- `EnrollmentSummary_DATA` - Enrollment summary data
- `EnrollmentSummaryFol` - Enrollment follow-up information
- `MasterList` - Master registry from CAFÉ DSCi database

## Repository Structure
```
├── input/      # Input data files
├── out/        # Output files
├── plots/      # Generated figures
├── rsconnect/  # R Shiny app deployment
├── [scripts].R # All scripts
├── README.md   # Documentation
```
## Usage

Clone the repository and run the app.R script from the main directory, in Rstudio.
