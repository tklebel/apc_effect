# Code for: "The APC-Effect: Stratification in Open Access Publishing"

This repository holds code and data for the preprint "The APC-Effect:
Stratification in Open Access Publishing".

# Data ingestion pipeline

The main data for this study came from OpenAlex. The basic steps to ingest the
data that we took are described in the preprint. Once the data was on our
Hadoop/Spark cluster, we started preprocessing to filter down the data to our
target set and compute necessary quantities.

The R scripts numbered 00 to 15 were executed in order to achieve this. The
resultant file `papers_with_concepts.parquet` was then used for descriptive
analysis. 

## Descriptive analysis

-   The file `21-sample-for-multilevel-model.R` was used to create the subsample
    for the mixture model.

-   The file `20-APC-analysis.Rmd` holds the main descriptive analysis. A rendered
version is provided in `20-APC-analysis.html` which can be viewed in the browser.
This also contains interactive figures that allow for further exploration.

- The file `22-additional-mini-analyses.Rmd` includes a few further analyses, one
of which is present in the supplement.

## Mixture modelling
The mixture model was built iteratively, through the files 11 to 17. The file
`17-adapted-mixture.R` holds the final R script, which used the stan model present
in `17-adapted-mixture.stan`. 

Analysis of the model (once sampled) was conducted via the notebook 
`20-analyse-hurdle.Rmd`. Note that rendering this notebook takes a long time
(30-60 minutes). 

Running the sampler is much more time consuming - the last iteration ran for 10
days on 4 cores with 2.2Ghz. Because of this, we provide the fully sampled models
for reanalysis:

- `final_models/hm_final_rerun.rds` contains data from the simple hurdle model
(from file `13-final-hurdle-model.R`).
- The full file from the full analysis (`17-brm-large-sample.rds.bz2`) is provided
as an attached binary to the release, and needs to be moved to the correct location
before the notebook can be rendered.

# Licenses

![](license.png){width="194"}

All code and data in this repository is made available under the CC-BY-SA 4.0
license.

The initial data sources had the following licenses:

-   DOAJ data is licensed under a Creative Commons Attribution-ShareAlike 4.0
    International (CC BY-SA 4.0) license)
    (<https://creativecommons.org/licenses/by-sa/4.0/>)
-   The CWTS Leiden Ranking 2021 is licensed under CC-BY. Citation: Van Eck,
    Nees Jan. (2021). CWTS Leiden Ranking 2021 [Data set]. Zenodo.
    <https://doi.org/10.5281/zenodo.4889279>
-   Data from OpenAlex is in the public domain (CCO)
    (<https://docs.openalex.org/license>)
-   Data from the World Bank is CC-BY 4.0 International.
