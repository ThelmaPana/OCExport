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



Regarding data, source has yet to be determined:

- NPP climatologies from Oregon group

- other env data?

- monthly VS yearly?

## Repo organisation

### Data

Contains data

### Scripts

-   `00.get_carbon.R`: download POC data

-   `01.get_env.R`: download environmental data

-   `02.get_uvp.R`: download UVP data from Ecotaxa

-   `03.uvp_diversity.R`: compute diversity metrics for UVP data

-   `04.assemble.R`: assemble poc and environmental data

-   `05.xgboost.R`: fit a tidymodel XGBoost to predict POC values from env data

-   `06.bart.R`: fit a tidymodel BART to predict POC values from env data

