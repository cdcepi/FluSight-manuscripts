## Retrospective Evaluation of Trained and Untrained Probabilistic Ensemble Forecasts for Influenza Hospitalizations ??? United States, 2022???2025
#### US CDC FluSight Team

Welcome to the repository for the evaluation of trained and untrained ensemble forecasts of inlfuenza hospital admissions for the 2022/23, 2023/24, and 2024/25 influenza seasons. Here you will find the necessary code to replicate the generation of the models and analysis performed for manuscript.

**Note:** You will need to clone the FluSight Forecast Github to access all teams' forecasts [FluSight-Forecast-Data](https://github.com/cdcepi/flusight-forecast-data "https://github.com/cdcepi/flusight-forecast-data")

**Table of Contents** This repository contains three main folders and several scripts:

-   `/Data`: contains archived target data as of the time of each generated forecast and RDS files containing the scores, weights, and forecasts.

-   `/Functiona`: contains scripts that contain various functions sourced in other scripts.

-   `/Supplement`: contains scripts, CSVs, and figures found in the supplement of the manuscript.

-   `/Data`: contains archived target data, eligibility lists, and ensemble model forecasts, weights, and scores.

-   `/Figures`: contains separately saved versions of each figure.

-   `figures_manuscript`: script for generating figures found in the manuscript.

-   `Weighted Ensemble 2223 Code`: generates the weighted ensembles for the 2022-2023 season.

-   `Weighted Ensemble 2324 Code`: generates the weighted ensembles for the 2023-2024 season.

-   `Weighted Ensemble 2425 Code`: generates the weighted ensembles for the 2024-2025 season.


### Analysis

The weighted ensembles for  each season can be produced through their respective `Weighted Ensemble XXXX Code` scripts. In order to run these scripts, you must clone the FluSight Forecast Github to access all teams' forecasts for the 2023-24 and 2024-25 seasons [FluSight-Forecast-Hub](https://github.com/cdcepi/FluSight-forecast-hub "https://github.com/cdcepi/FluSight-forecast-hub"). The forecasts for the 2022-23 season are saved in the repo in the data/2223 folder, and can be found separately in [FluSight-Forecast-Data](https://github.com/cdcepi/flusight-forecast-data "https://github.com/cdcepi/flusight-forecast-data").  Each script takes multiple hours to run and produce each ensemble, so the outputs are available in the data/rds folder.


`figures_manuscript.Rmd`

This file can be used to generate all relevant data comparisons and figures. The figures are output to the Figures folder and the data used can be found in the Data folder.  All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the data manipulation code related to the figure of the same name.




### Data license and reuse

We are grateful to the teams who built models, generated forecast data and submitted forecasts. The groups have made their public data available under different terms and licenses. You will find the licenses (when provided) within each metadata file in the model-specific folders in the [FluSight-Forecast-Hub repo](https://github.com/cdcepi/FluSight-forecast-hub "https://github.com/cdcepi/FluSight-forecast-hub") . Please consult these licenses before using these data to ensure that you follow the terms under which these data were released.

All source code that is specific to the overall project is available under an open-source MIT license. We note that this license does NOT cover model code from the various teams or model scenario data (available under specified licenses as described above).
