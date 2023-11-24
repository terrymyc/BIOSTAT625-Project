# Enhancing Missing Data Imputation in Time Series Accelerometry Data

## Project Overview
This project, led by Deji Suolang, Kaidar Nurumov, and Yongchao Ma, focuses on developing and evaluating novel imputation methods for missing time series data in accelerometry studies.
The primary aim is to reduce underreporting bias, thereby enhancing the reliability of wearable device data in health observational and experimental studies.

## Data
The analysis uses data directly from the [Physical Activity and Transit (PAT)](https://www.nyc.gov/site/doh/data/data-sets/physical-activity-and-transit-survey.page) Survey, hosted externally.
The data will not be redistributed through this repository, and scripts will access the data directly from the host website for reproducibility.

## Repository Structure
- `docs/`: Documentation and additional resources.
  - `proposal.pdf`: Project proposal document.
  - `report.pdf`: Final project report.
- `src/`: Source code for the analysis.
  - `data_preprocessing/`: Scripts for data cleaning and preprocessing.
  - `imputation_methods/`: Scripts for different imputation methods.
  - `analysis/`: Scripts for final data analysis and visualization.
- `results/`: Output from the analysis.
  - `figures/`: Graphs and visualizations.
  - `tables/`: Generated tables and summary statistics.

## Reproducing the Analysis
To reproduce the analysis:
1. Clone the repository.
2. Navigate to the `src/` directory.
3. Run the scripts in the following order:
   - Data preprocessing scripts in `data_preprocessing/`.
   - Imputation method scripts in `imputation_methods/`.
   - Analysis scripts in `analysis/`.
   Note: The scripts will access the PAT Survey data directly from the host website.

## Contributors
- Deji Suolang
- Kaidar Nurumov
- Yongchao Ma

---

This project is part of an academic research initiative. For more information or inquiries, please refer to the `docs/proposal.pdf` document or contact the contributors.
