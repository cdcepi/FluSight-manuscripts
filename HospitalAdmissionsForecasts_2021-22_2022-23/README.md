# Evaluation of FluSight influenza forecasting in the 2021-22 and 2022-23 seasons with a new target: laboratory-confirmed influenza hospitalizations

### US CDC FluSight Team

Welcome to the repository for FluSight for the 21-22 and 22-23 influenza seasons. Here you will find the necessary code to replicate the analysis performed for the 21-22 and 22-23 FluSight evaluation manuscript.

**Note:** You will need to clone the FluSight Forecast Github to access all teams' forecasts [FluSight-Forecast-Data](https://github.com/cdcepi/flusight-forecast-data "https://github.com/cdcepi/flusight-forecast-data")

Table of Contents

-   Data_for_Figures: This folder contains files to generate the figures found in the manuscript.

-   Supplemental_analyses: This folder contains scripts, CSVs, and figures found in the supplement of the manuscript.

-   data_for_figures_2021-2023: This script generates the CSV files found in the Data_for_Figures folder.

-   flusight21-23: This quarto contains all the code for the main evaluation.

-   functions2022-2023: This script contains functions used in the evaluation.

-   generate_figures_and_tables_2021-2023: This script generates the figures and tables found in the manuscript without having to run all of the evaluation code.

-   Model names and colors: This script contains the names of each model submitted during the 2021-22 and 2022-23 seasons along with the colors associated with that model used in the evaluation reports.


***Not sure if this should be added into ToC or if it should be its own section. Left redundant lines in, in case it stands alone.***

data_for_figures_2021-2023

This file can be used to generate all relevant data used in the generate_figures_and_tables_2021-2023.R script. The data have been output to the Data_for_Figures folder, so it is not necessary to run this script prior to using the generate_figures_and_tables_2021-2023.R script. All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the data manipulation code related to the figure of the same name.

data_for_backfill_figures_2021-2023

This file can be used to generate all relevant data used in the generate_backfill_figures_and_tables_2021-2023.R script. The data have been output to the Data for Backfill Figures folder, so it is not necessary to run this script prior to using the generate_backfill_figures_and_tables_2021-2023.R script. All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the data manipulation code related to the figure of the same name.

data_for_log_transformed_figures_2021-2023

This file can be used to generate all relevant data used in the generate_log_transformed_figures_and_tables_2021-2023.R script. The data have been output to the Data for Log-Transformed Figures folder, so it is not necessary to run this script prior to using the generate_log_transformed_figures_and_tables_2021-2023.R script. All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the data manipulation code related to the figure of the same name.

generate_figures_and_tables_2021-2023

This file can be used to generate the figures and tables found in the main section of the manuscript using the data output by the data_for_figures_2021-2023.R script. The data have been output to the Data_for_Figures folder, so it is not necessary to run data_for_figures_2021-2023.R prior to using this script. All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the visualization code related to the figure of the same name.

generate_backfill_figures_and_tables_2021-2023

This file can be used to generate the figures and tables found in the backfill supplemental section of the manuscript using the data output by the data_for_backfill_figures_2021-2023.R script. The data have been output to the Data for Backfill Figures folder, so it is not necessary to run data_for_backfill_figures_2021-2023.R prior to using this script. All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the visualization code related to the figure of the same name.

generate_log_transformed_figures_and_tables_2021-2023

This file can be used to generate the figures and tables found in the log-transformed supplemental section of the manuscript using the data output by the data_for_log_transformed_figures_2021-2023.R script. The data have been output to the Data for Log-Transformed Figures folder, so it is not necessary to run data_for_log_transformed_figures_2021-2023.R prior to using this script. All necessary libraries, file paths, and objects are listed at the beginning of this file in the "Setup" section below. Each subsequent section contains the visualization code related to the figure of the same name.
