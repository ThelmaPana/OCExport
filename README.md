# OCExport

Predict ocean carbon export

## Goals

-   predict carbon export from a bunch of environmental data
-   investigate if various proxis of zooplankton diversity are important for predicting carbon export

## TODOs

-   investigate BARTs (<https://cran.r-project.org/web/packages/BART/vignettes/the-BART-R-package.pdf>)

-   generate proxis of diversity for zooplankton (taxonomic, trophic & morphologic)

-   can we predict POC values from a few plankton variables? -> Yes!

-   can these plankton variables be predicted from the environment?

-   can we isolate the effect of plankton on POC:
    
    - compute POC residuals from env
    
    - compute plankton residuals from env (plankton is multivariate, use MBTR)
    
    - predict POC residuals from plankton residuals





## Repo organisation

### Data

Contains data

### Scripts

-   `utils.R`: useful stuffs for all the projects: loading packages, default values, functions…

-   `00.get_carbon.R`: download POC data

-   `01.get_env.qmd`: download and process environmental data

-   `02.get_uvp.R`: download UVP data from Ecotaxa

-   `03.uvp_diversity.qmd`: compute taxonomic/morphological/trophic diversity metrics for UVP data

-   `04.assemble.qmd`: assemble poc, env and plankton data

-   `05.pred_poc_from_all.qmd`: fit a XGBoost model to predict poc from env + plankton data

-   `06.pred_poc_from_env.qmd`: fit a XGBoost model to predict poc from env as in Wang et al., 2023

-   `07.pred_poc_from_plankton.qmd`: fit a XGBoost model to predict poc from all plankton data

-   `08.pred_poc_from_plankton.qmd`: fit a XGBoost model to predict poc from a few plankton variables, identified as the best predictors in `06.pred_poc_from_plankton.qmd`

-   `09.pred_poc_from_pca.qmd`: fit a XGBoost model to predict poc from PCA outputs on all plankton data

-   `10.pred_plankton_from_env.qmd`: fit a XGBoost model to predict best plankton predictors (from `06.pred_poc_from_plankton.qmd`) from env variables


### Notes

Ideas in development / reports. 


### Presentations

Quarto presentations for meetings.


### Old

Old stuff. 
