# OCExport

Predict ocean carbon export

## Goals

-   predict carbon export from a bunch of environmental data
-   investigate if various proxis of zooplankton diversity are important for predicting carbon export

## TODOs

-   get environmental data (<https://data.up.ethz.ch/shared/AtlantECO/>)

    -   SST:

    -   SSS:

    -   MLD:

    -   nutrients + clines

    -   Zeu: computed from surface chl a

    -   O2

    -   PAR (see https://ads.atmosphere.copernicus.eu/cdsapp#!/dataset/cams-solar-radiation-timeseries?tab=form)

-   get carbon export data (<https://zenodo.org/records/8253973> & <https://www.nature.com/articles/s41586-023-06772-4>)

-   investigate BARTs (<https://cran.r-project.org/web/packages/BART/vignettes/the-BART-R-package.pdf>)

-   generate proxis of diversity for zooplankton (taxonomic, trophic & morphologic)

## Repo organisation

### Data

Contains data

### Scripts

-   `00.get_carbon.R`: download POC data

-   `01.get_env.R`: download environmental data

-   `02.assemble.R`: assemble poc and environmental data

-   `03.xgboost.R`: fit a tidymodel XGBoost to predict POC values from env data

-   `04.bart.R`: fit a tidymodel BART to predict POC values from env data

