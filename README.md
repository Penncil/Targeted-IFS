# Targeted-IFS: Targeted Learning of Heterogeneous Sources by Informative Feature Sharing 

## Outline

1. Description 
2. Reproducing the simulation studies
3. Reproducing the real data analysis
4. Notes on output and reproducibility
5. Support

## 1. Description

This repository contains code to reproduce the simulation studies and the real data analysis reported in the manuscript. All code is written in R. Results are written to `.rds` files and figures are produced from dedicated plotting scripts.

Fig1.pptx contains the figure corresponding to Figure 1 in the manuscript, which illustrates three different types of sources.

In this repository, we include a synthetic dataset `sample_data_to_run.csv` that was generated to approximate the structure of the real-world COVID-19 pediatric dataset. The original dataset includes 3,990 patients from 34 clinical sites and 243 binary covariates, but due to privacy and data use restrictions, we cannot share the raw patient-level data. To create a reproducible example, we generated a synthetic dataset with 4,000 patients across 35 clinical sites, containing 243 binary covariates and one binary outcome. The marginal distributions and correlation structure of the features were designed to resemble those of the original dataset, ensuring that the synthetic data are representative for testing and reproducing the analysis pipeline, while containing no identifiable patient information.
## 2. Reproducing the simulation studies

1. Open R or RStudio.
2. Set the working directory to the repository root.
3. Run one or more of the simulation scripts listed in the following:
   ```r
   source("simulation_heter[1].R")
   source("simulation_sensitive.R")
   ```
4. Each script writes its outputs as `.rds` files.
6. To recreate the Table 1 in the manuscript, run:
   ```r
   source("Table1.R")
   ```
8. To recreate the figures in the manuscript, run:
   ```r
   source("Fig2.R")
   source("Fig3.R")
   source("Fig4.R")
   source("Fig5.R")
   source("Fig6.R")
   ```

## 3. Reproducing the real data analysis

1. Open R or RStudio.
2. Set the working directory to the folder `use case`.
3. Ensure the sample dataset `sample_data_to_run.csv` is present in the same folder.
4. Run the main script:
   ```r
   source("realdata.R")
   ```
   The file `Fed_simulation_functions.R` is sourced automatically by `Table1.R`.
5. Outputs are written as `.rds` files inside `use case`.
6. To produce the ROC figure from the manuscript, run:
   ```r
   source("use case/Figure7.R")
   ```
   
   

## 4. Notes on output and reproducibility

1. Figures are regenerated from the `.rds` result files. If you delete or relocate those files, recreate them by rerunning the corresponding simulation or analysis script.

## 5. Support

For questions about the code or the study design, please open an issue in the repository.
