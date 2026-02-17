## Retrospective Evaluation of Trained and Untrained Probabilistic Ensemble Forecasts for Influenza Hospitalizations ??? United States, 2022???2025
#### US CDC FluSight Team

Welcome to the repository for the evaluation of trained and untrained ensemble forecasts of inlfuenza hospital admissions for the 2022/23, 2023/24, and 2024/25 influenza seasons. Here you will find the necessary code to replicate the generation of the models and analysis performed for manuscript.

**Note:** You will need to clone the FluSight Forecast Github to access all teams' forecasts [FluSight-Forecast-Data](https://github.com/cdcepi/flusight-forecast-data "https://github.com/cdcepi/flusight-forecast-data")

**Table of Contents** This repository contains three main folders and several scripts:

-   `/Data`: contains archived target data as of the time of each generated forecast and RDS files containing the scores, weights, and forecasts.

-   `/Function`: contains scripts that contain various functions sourced in other scripts.

-   `/Supplemental_analyses`: contains scripts, CSVs, and figures found in the supplement of the manuscript.

-   `/Test`: 

-   `Figures_Manuscript`: script for generating figures found in the manuscript.

-   `FluSight Weighted Ensemble Comparison 2225`: supplemental analyses.

-   `Weighted Ensemble 2223 Code`: generates the weighted ensembles for the 2022-2023 season.

-   `Weighted Ensemble 2324 Code`: generates the weighted ensembles for the 2023-2024 season.

-   `Weighted Ensemble 2425 Code`: generates the weighted ensembles for the 2024-2025 season.

### Requirements

Most packages used within the contained R scripts are available through `CRAN`. The exceptions to that may be zoltr and covidHubUtils, which can be downloaded using the below commands. See [reichlab/covidHubUtils](https://github.com/reichlab/covidHubUtils) for more details.

`devtools::install_github("reichlab/zoltr")`

`remotes::install_github("reichlab/covidHubUtils")`

### Analysis

The weighted ensembles for  each season can be produced through their respective `Weighted Ensemble XXXX Code` scripts. In order to run these scripts, you must clone the FluSight Forecast Github to access all teams' forecasts [FluSight-Forecast-Data](https://github.com/cdcepi/flusight-forecast-data "https://github.com/cdcepi/flusight-forecast-data"). Each script takes multiple hours to run and produce each ensemble, so the outputs are available in the data/rds folder.

`data_for_figures_2021-2023.R`

This file can be used to generate all relevant data used in the generate_figures_and_tables_2021-2023.R script. The data have been output to the Data_for_Figures folder, so it is not necessary to run this script prior to using the generate_figures_and_tables_2021-2023.R script. All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the data manipulation code related to the figure of the same name.

`generate_figures_and_tables_2021-2023.R`

This file can be used to generate the figures and tables found in the main section of the manuscript using the data output by the data_for_figures_2021-2023.R script. The data have been output to the Data_for_Figures folder, so it is not necessary to run data_for_figures_2021-2023.R prior to using this script. All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the visualization code related to the figure of the same name.

`/Supplemental_analysis/backfill-analysis/data_for_backfill_figures_2021-2023.R`

This file can be used to generate all relevant data used in the generate_backfill_figures_and_tables_2021-2023.R script. The data have been output to the Data for Backfill Figures folder, so it is not necessary to run this script prior to using the generate_backfill_figures_and_tables_2021-2023.R script. All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the data manipulation code related to the figure of the same name.

`/Supplemental_analysis/log-transformed/data_for_log_transformed_figures_2021-2023.R`

This file can be used to generate all relevant data used in the generate_log_transformed_figures_and_tables_2021-2023.R script. The data have been output to the Data for Log-Transformed Figures folder, so it is not necessary to run this script prior to using the generate_log_transformed_figures_and_tables_2021-2023.R script. All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the data manipulation code related to the figure of the same name.

`/Supplemental_analysis/backfill-analysis/generate_backfill_figures_and_tables_2021-2023.R`

This file can be used to generate the figures and tables found in the backfill supplemental section of the manuscript using the data output by the data_for_backfill_figures_2021-2023.R script. The data have been output to the Data for Backfill Figures folder, so it is not necessary to run data_for_backfill_figures_2021-2023.R prior to using this script. All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the visualization code related to the figure of the same name.

`/Supplemental_analysis/log-transformed/generate_log_transformed_figures_and_tables_2021-2023.R`

This file can be used to generate the figures and tables found in the log-transformed supplemental section of the manuscript using the data output by the data_for_log_transformed_figures_2021-2023.R script. The data have been output to the Data for Log-Transformed Figures folder, so it is not necessary to run data_for_log_transformed_figures_2021-2023.R prior to using this script. All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the visualization code related to the figure of the same name.

`/Supplemental_analysis/national_flusight_21-23.qmd`

This file can be used to generate all relevant data and figures for the national supplemental section of the manuscript.

### Data license and reuse

We are grateful to the teams who built models, generated forecast data and submitted forecasts. The groups have made their public data available under different terms and licenses. You will find the licenses (when provided) within each metadata file in the model-specific folders in the [FluSight-Forecast-Data repo](https://github.com/cdcepi/flusight-forecast-data "https://github.com/cdcepi/flusight-forecast-data") . Please consult these licenses before using these data to ensure that you follow the terms under which these data were released.

All source code that is specific to the overall project is available under an open-source MIT license. We note that this license does NOT cover model code from the various teams or model scenario data (available under specified licenses as described above).

### Software Versions

R: 4.3.0
RStudio: 2023.03.1+446
Windows 10: 22H2

Installation of R will vary by machine and main manuscript software run-time may exceed 30 minutes. 
