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

## 2. Reproducing the simulation studies

1. Open R or RStudio.
2. Set the working directory to the repository root.
3. Run one or more of the simulation scripts listed in the following:
   ```r
   source("simulation_heter[1].R")
   source("simulation_sensitive.R")
   ```
4. Each script writes its outputs as `.rds` files and save to simulation_result file.
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
2. Set the working directory to the repository root.
3. To recreate Figure 7 in the manuscript, run:
   ```r
   source("Fig7.R")
   ```
4. To reproduct the main results in the real data analysis, please contact the authors to get the real dataset "real_data.rds", and then run the script:
   ```r
   source("realdata.R")
   ```
   
## 4. Notes on output and reproducibility

1. Figures are regenerated from the `.rds` result files. If you delete or relocate those files, recreate them by rerunning the corresponding simulation or analysis script.

## 5. Support

For questions about the code or the study design, please open an issue in the repository.
