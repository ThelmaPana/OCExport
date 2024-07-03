# OCExport

Predict particulate organic carbon (POC) attenuation from zooplankton diversity.

## Goals

1.  Compute POC attenuation from POC concentration data by Sauzède et al., 2020

2.  Generate metrics of zooplankton diversity from UVP5 data

3.  Predict POC attenuation from various metrics of zooplankton diversity

4.  Investigate which descriptors of zooplankton diversity are important to predict POC attenuation

## Repo organisation

### Data

Contains data

### Scripts

-   `utils.R`: useful stuff for the project: loading packages, default values, functions…

-   `00.get_uvp`: download UVP data from Ecotaxa

-   `01.uvp_diversity`: compute taxonomic/morphological/trophic diversity metrics for UVP data

-   `02.get_carbon`: process POC data and compute attenuation

-   `03.assemble`: assemble POC and plankton data

-   `04.pred_poc_from_plankton`: fit a XGBoost model to predict POC attenuation from all plankton data

-   `05.pred_poc_from_plankton`: fit a XGBoost model to predict POC attenuation from a few plankton variables

### Presentations

Quarto presentations for meetings.
