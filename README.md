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
analysis. This file is available at https://doi.org/10.5281/zenodo.7014802.

# Descriptive analysis

-   The file `21-sample-for-multilevel-model.R` was used to create the subsample
    for the mixture model.

-   The file `20-APC-analysis.Rmd` holds the main descriptive analysis. A rendered
version is provided in `20-APC-analysis.html` which can be viewed in the browser.
This also contains interactive figures that allow for further exploration. The 
dataset that underlies this analysis is available at https://doi.org/10.5281/zenodo.7014802.
Given its size, it can definitely also be analysed without the need for 
Spark/Hadoop.

- The file `22-additional-mini-analyses.Rmd` includes a few further analyses, one
of which is present in the supplement.

# Mixture modelling
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

# Version history
To enable full transparency of our steps, this repository includes all steps we
took when starting with this particular analysis. To avoid clutter, we deleted
many outdated files from the main branch, which now only holds the core files
from the analysis. All earlier files are still available on the `archive` branch.

The paper also contains an analysis of the full set of universities (even those
that are not included in the Leiden Ranking). The analysis for this is available
in the branch `full-country-apc-average`. 

Given the long version history, the full repository is fairly large (~800Mb). It
might therefore be easier to simply download release snapshots which will be 
available on Zenodo.

# Software versions
<details>
  <summary>Software versions on Hadoop Cluster</summary>
  We used Spark2 (v2.3.2) to conduct computations. The full session info is as
  follows:

```r
─ Session info ───────────────────────────────────────────────────────────────
 setting  value
 version  R version 3.6.0 (2019-04-26)
 os       CentOS Linux 7 (Core)
 system   x86_64, linux-gnu
 ui       RStudio
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       Europe/Vienna
 date     2022-10-13
 rstudio  1.1.456 (server)
 pandoc   NA

─ Packages ─────────────────────────────────────────────────────────────────────
 package     * version date (UTC) lib source
 arrow       * 4.0.1   2021-05-28 [1] CRAN (R 3.6.0)
 assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.6.0)
 backports     1.4.1   2021-12-13 [1] CRAN (R 3.6.0)
 base64enc     0.1-3   2015-07-28 [1] CRAN (R 3.6.0)
 bit           4.0.4   2020-08-04 [1] CRAN (R 3.6.0)
 bit64         4.0.5   2020-08-30 [1] CRAN (R 3.6.0)
 brio          1.1.3   2021-11-30 [1] CRAN (R 3.6.0)
 broom         0.7.12  2022-01-28 [1] CRAN (R 3.6.0)
 cachem        1.0.6   2021-08-19 [1] CRAN (R 3.6.0)
 callr         3.7.0   2021-04-20 [1] CRAN (R 3.6.0)
 cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.6.0)
 cli           3.3.0   2022-04-25 [1] CRAN (R 3.6.0)
 colorspace  * 2.0-2   2021-06-24 [1] CRAN (R 3.6.0)
 crayon        1.5.1   2022-03-26 [1] CRAN (R 3.6.0)
 DBI           1.1.2   2021-12-20 [1] CRAN (R 3.6.0)
 dbplyr        2.1.1   2021-04-06 [1] CRAN (R 3.6.0)
 desc          1.4.1   2022-03-06 [1] CRAN (R 3.6.0)
 devtools      2.4.3   2021-11-30 [1] CRAN (R 3.6.0)
 digest        0.6.29  2021-12-01 [1] CRAN (R 3.6.0)
 dplyr       * 1.0.9   2022-04-28 [1] CRAN (R 3.6.0)
 ellipsis      0.3.2   2021-04-29 [1] CRAN (R 3.6.0)
 fansi         1.0.3   2022-03-24 [1] CRAN (R 3.6.0)
 fastmap       1.1.0   2021-01-25 [1] CRAN (R 3.6.0)
 forcats     * 0.5.1   2021-01-27 [1] CRAN (R 3.6.0)
 forge         0.2.0   2019-02-26 [1] CRAN (R 3.6.0)
 fs            1.5.2   2021-12-08 [1] CRAN (R 3.6.0)
 generics      0.1.2   2022-01-31 [1] CRAN (R 3.6.0)
 ggplot2     * 3.3.5   2021-06-25 [1] CRAN (R 3.6.0)
 ggrepel     * 0.9.1   2021-01-15 [1] CRAN (R 3.6.0)
 glue          1.6.2   2022-02-24 [1] CRAN (R 3.6.0)
 gtable        0.3.0   2019-03-25 [1] CRAN (R 3.6.0)
 haven         2.4.3   2021-08-04 [1] CRAN (R 3.6.0)
 hms           1.1.1   2021-09-26 [1] CRAN (R 3.6.0)
 htmltools     0.5.2   2021-08-25 [1] CRAN (R 3.6.0)
 htmlwidgets   1.5.4   2021-09-08 [1] CRAN (R 3.6.0)
 httr          1.4.2   2020-07-20 [1] CRAN (R 3.6.0)
 jsonlite      1.8.0   2022-02-22 [1] CRAN (R 3.6.0)
 knitr         1.37    2021-12-16 [1] CRAN (R 3.6.0)
 lifecycle     1.0.1   2021-09-24 [1] CRAN (R 3.6.0)
 lubridate     1.8.0   2021-10-07 [1] CRAN (R 3.6.0)
 magrittr      2.0.3   2022-03-30 [1] CRAN (R 3.6.0)
 memoise       2.0.1   2021-11-26 [1] CRAN (R 3.6.0)
 modelr        0.1.8   2020-05-19 [1] CRAN (R 3.6.0)
 munsell       0.5.0   2018-06-12 [1] CRAN (R 3.6.0)
 patchwork   * 1.1.1   2020-12-17 [1] CRAN (R 3.6.0)
 pillar        1.7.0   2022-02-01 [1] CRAN (R 3.6.0)
 pkgbuild      1.3.1   2021-12-20 [1] CRAN (R 3.6.0)
 pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 3.6.0)
 pkgload       1.2.4   2021-11-30 [1] CRAN (R 3.6.0)
 prettyunits   1.1.1   2020-01-24 [1] CRAN (R 3.6.0)
 processx      3.5.3   2022-03-25 [1] CRAN (R 3.6.0)
 ps            1.7.0   2022-04-23 [1] CRAN (R 3.6.0)
 purrr       * 0.3.4   2020-04-17 [1] CRAN (R 3.6.0)
 r2d3          0.2.5   2020-12-18 [1] CRAN (R 3.6.0)
 R6            2.5.1   2021-08-19 [1] CRAN (R 3.6.0)
 Rcpp          1.0.8   2022-01-13 [1] CRAN (R 3.6.0)
 readr       * 2.1.2   2022-01-30 [1] CRAN (R 3.6.0)
 readxl        1.3.1   2019-03-13 [1] CRAN (R 3.6.0)
 remotes       2.4.2   2021-11-30 [1] CRAN (R 3.6.0)
 reprex        2.0.1   2021-08-05 [1] CRAN (R 3.6.0)
 RJSONIO       1.3-1.6 2021-09-16 [1] CRAN (R 3.6.0)
 rlang         1.0.2   2022-03-04 [1] CRAN (R 3.6.0)
 rprojroot     2.0.3   2022-04-02 [1] CRAN (R 3.6.0)
 rstudioapi    0.13    2020-11-12 [1] CRAN (R 3.6.0)
 rvest         1.0.2   2021-10-16 [1] CRAN (R 3.6.0)
 scales      * 1.1.1   2020-05-11 [1] CRAN (R 3.6.0)
 sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 3.6.0)
 sparklyr    * 1.7.5   2022-02-02 [1] CRAN (R 3.6.0)
 stringi       1.7.6   2021-11-29 [1] CRAN (R 3.6.0)
 stringr     * 1.4.0   2019-02-10 [1] CRAN (R 3.6.0)
 testthat      3.1.4   2022-04-26 [1] CRAN (R 3.6.0)
 tibble      * 3.1.6   2021-11-07 [1] CRAN (R 3.6.0)
 tidyr       * 1.2.0   2022-02-01 [1] CRAN (R 3.6.0)
 tidyselect    1.1.2   2022-02-21 [1] CRAN (R 3.6.0)
 tidyverse   * 1.3.1   2021-04-15 [1] CRAN (R 3.6.0)
 tzdb          0.2.0   2021-10-27 [1] CRAN (R 3.6.0)
 usethis       2.1.5   2021-12-09 [1] CRAN (R 3.6.0)
 utf8          1.2.2   2021-07-24 [1] CRAN (R 3.6.0)
 vctrs         0.4.1   2022-04-13 [1] CRAN (R 3.6.0)
 WDI         * 2.7.7   2022-07-16 [1] CRAN (R 3.6.0)
 withr         2.5.0   2022-03-03 [1] CRAN (R 3.6.0)
 xfun          0.29    2021-12-14 [1] CRAN (R 3.6.0)
 xml2          1.3.3   2021-11-30 [1] CRAN (R 3.6.0)
 yaml          2.2.2   2022-01-25 [1] CRAN (R 3.6.0)
```

</details>

<details>
  <summary>Software versions for multilevel modelling</summary>
  
```r
─ Session info ─────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.1.2 (2021-11-01)
 os       Ubuntu 22.04.1 LTS
 system   x86_64, linux-gnu
 ui       RStudio
 language (EN)
 collate  de_AT.UTF-8
 ctype    de_AT.UTF-8
 tz       Europe/Vienna
 date     2022-10-13
 rstudio  2022.07.0+548 Spotted Wakerobin (server)
 pandoc   NA

─ Packages ─────────────────────────────────────────────────────────────────────
 package        * version  date (UTC) lib source
 abind            1.4-5    2016-07-21 [1] CRAN (R 4.1.2)
 assertthat       0.2.1    2019-03-21 [1] CRAN (R 4.1.2)
 backports        1.4.1    2021-12-13 [1] CRAN (R 4.1.2)
 base64enc        0.1-3    2015-07-28 [1] CRAN (R 4.1.2)
 bayesplot        1.9.0    2022-03-10 [1] CRAN (R 4.1.2)
 bridgesampling   1.1-2    2021-04-16 [1] CRAN (R 4.1.2)
 brms           * 2.17.0   2022-04-13 [1] CRAN (R 4.1.2)
 Brobdingnag      1.2-7    2022-02-03 [1] CRAN (R 4.1.2)
 broom            1.0.0    2022-07-01 [1] CRAN (R 4.1.2)
 callr            3.7.0    2021-04-20 [1] CRAN (R 4.1.2)
 cellranger       1.1.0    2016-07-27 [1] CRAN (R 4.1.2)
 checkmate        2.1.0    2022-04-21 [1] CRAN (R 4.1.2)
 cli              3.4.1    2022-09-23 [1] CRAN (R 4.1.2)
 cmdstanr       * 0.5.2    2022-07-11 [1] local
 coda             0.19-4   2020-09-30 [1] CRAN (R 4.1.2)
 codetools        0.2-18   2020-11-04 [4] CRAN (R 4.0.3)
 colorspace       2.0-3    2022-02-21 [1] CRAN (R 4.1.2)
 colourpicker     1.1.1    2021-10-04 [1] CRAN (R 4.1.2)
 crayon           1.5.1    2022-03-26 [1] CRAN (R 4.1.2)
 crosstalk        1.2.0    2021-11-04 [1] CRAN (R 4.1.2)
 DBI              1.1.3    2022-06-18 [1] CRAN (R 4.1.2)
 dbplyr           2.2.1    2022-06-27 [1] CRAN (R 4.1.2)
 digest           0.6.29   2021-12-01 [1] CRAN (R 4.1.2)
 distributional   0.3.0    2022-01-05 [1] CRAN (R 4.1.2)
 dplyr          * 1.0.9    2022-04-28 [1] CRAN (R 4.1.2)
 DT               0.23     2022-05-10 [1] CRAN (R 4.1.2)
 dygraphs         1.1.1.6  2018-07-11 [1] CRAN (R 4.1.2)
 ellipsis         0.3.2    2021-04-29 [1] CRAN (R 4.1.2)
 fansi            1.0.3    2022-03-24 [1] CRAN (R 4.1.2)
 farver           2.1.1    2022-07-06 [1] CRAN (R 4.1.2)
 fastmap          1.1.0    2021-01-25 [1] CRAN (R 4.1.2)
 forcats        * 0.5.1    2021-01-27 [1] CRAN (R 4.1.2)
 fs               1.5.2    2021-12-08 [1] CRAN (R 4.1.2)
 generics         0.1.3    2022-07-05 [1] CRAN (R 4.1.2)
 ggplot2        * 3.3.6    2022-05-03 [1] CRAN (R 4.1.2)
 ggridges         0.5.3    2021-01-08 [1] CRAN (R 4.1.2)
 glue             1.6.2    2022-02-24 [1] CRAN (R 4.1.2)
 gridExtra        2.3      2017-09-09 [1] CRAN (R 4.1.2)
 gtable           0.3.0    2019-03-25 [1] CRAN (R 4.1.2)
 gtools           3.9.2.2  2022-06-13 [1] CRAN (R 4.1.2)
 haven            2.5.0    2022-04-15 [1] CRAN (R 4.1.2)
 hms              1.1.1    2021-09-26 [1] CRAN (R 4.1.2)
 htmltools        0.5.2    2021-08-25 [1] CRAN (R 4.1.2)
 htmlwidgets      1.5.4    2021-09-08 [1] CRAN (R 4.1.2)
 httpuv           1.6.5    2022-01-05 [1] CRAN (R 4.1.2)
 httr             1.4.3    2022-05-04 [1] CRAN (R 4.1.2)
 igraph           1.3.2    2022-06-13 [1] CRAN (R 4.1.2)
 inline           0.3.19   2021-05-31 [1] CRAN (R 4.1.2)
 jsonlite         1.8.0    2022-02-22 [1] CRAN (R 4.1.2)
 knitr            1.39     2022-04-26 [1] CRAN (R 4.1.2)
 later            1.3.0    2021-08-18 [1] CRAN (R 4.1.2)
 lattice          0.20-45  2021-09-22 [4] CRAN (R 4.1.1)
 lifecycle        1.0.1    2021-09-24 [1] CRAN (R 4.1.2)
 loo              2.5.1    2022-03-24 [1] CRAN (R 4.1.2)
 lubridate        1.8.0    2021-10-07 [1] CRAN (R 4.1.2)
 magrittr         2.0.3    2022-03-30 [1] CRAN (R 4.1.2)
 markdown         1.1      2019-08-07 [1] CRAN (R 4.1.2)
 Matrix           1.4-0    2021-12-08 [4] CRAN (R 4.1.2)
 matrixStats      0.62.0   2022-04-19 [1] CRAN (R 4.1.2)
 mime             0.12     2021-09-28 [1] CRAN (R 4.1.2)
 miniUI           0.1.1.1  2018-05-18 [1] CRAN (R 4.1.2)
 modelr           0.1.8    2020-05-19 [1] CRAN (R 4.1.2)
 munsell          0.5.0    2018-06-12 [1] CRAN (R 4.1.2)
 mvtnorm          1.1-3    2021-10-08 [1] CRAN (R 4.1.2)
 nlme             3.1-155  2022-01-13 [4] CRAN (R 4.1.2)
 pillar           1.7.0    2022-02-01 [1] CRAN (R 4.1.2)
 pkgbuild         1.3.1    2021-12-20 [1] CRAN (R 4.1.2)
 pkgconfig        2.0.3    2019-09-22 [1] CRAN (R 4.1.2)
 plyr             1.8.7    2022-03-24 [1] CRAN (R 4.1.2)
 posterior        1.2.2    2022-06-09 [1] CRAN (R 4.1.2)
 prettyunits      1.1.1    2020-01-24 [1] CRAN (R 4.1.2)
 processx         3.7.0    2022-07-07 [1] CRAN (R 4.1.2)
 promises         1.2.0.1  2021-02-11 [1] CRAN (R 4.1.2)
 ps               1.7.1    2022-06-18 [1] CRAN (R 4.1.2)
 purrr          * 0.3.4    2020-04-17 [1] CRAN (R 4.1.2)
 R6               2.5.1    2021-08-19 [1] CRAN (R 4.1.2)
 Rcpp           * 1.0.9    2022-07-08 [1] CRAN (R 4.1.2)
 RcppParallel     5.1.5    2022-01-05 [1] CRAN (R 4.1.2)
 readr          * 2.1.2    2022-01-30 [1] CRAN (R 4.1.2)
 readxl           1.4.0    2022-03-28 [1] CRAN (R 4.1.2)
 reprex           2.0.1    2021-08-05 [1] CRAN (R 4.1.2)
 reshape2         1.4.4    2020-04-09 [1] CRAN (R 4.1.2)
 rlang            1.0.6    2022-09-24 [1] CRAN (R 4.1.2)
 rstan            2.21.5   2022-04-11 [1] CRAN (R 4.1.2)
 rstantools       2.2.0    2022-04-08 [1] CRAN (R 4.1.2)
 rstudioapi       0.13     2020-11-12 [1] CRAN (R 4.1.2)
 rvest            1.0.2    2021-10-16 [1] CRAN (R 4.1.2)
 scales           1.2.0    2022-04-13 [1] CRAN (R 4.1.2)
 sessioninfo      1.2.2    2021-12-06 [1] CRAN (R 4.1.2)
 shiny            1.7.1    2021-10-02 [1] CRAN (R 4.1.2)
 shinyjs          2.1.0    2021-12-23 [1] CRAN (R 4.1.2)
 shinystan        2.6.0    2022-03-03 [1] CRAN (R 4.1.2)
 shinythemes      1.2.0    2021-01-25 [1] CRAN (R 4.1.2)
 StanHeaders      2.21.0-7 2020-12-17 [1] CRAN (R 4.1.2)
 stringi          1.7.8    2022-07-11 [1] CRAN (R 4.1.2)
 stringr        * 1.4.0    2019-02-10 [1] CRAN (R 4.1.2)
 tensorA          0.36.2   2020-11-19 [1] CRAN (R 4.1.2)
 threejs          0.3.3    2020-01-21 [1] CRAN (R 4.1.2)
 tibble         * 3.1.7    2022-05-03 [1] CRAN (R 4.1.2)
 tidyr          * 1.2.0    2022-02-01 [1] CRAN (R 4.1.2)
 tidyselect       1.1.2    2022-02-21 [1] CRAN (R 4.1.2)
 tidyverse      * 1.3.1    2021-04-15 [1] CRAN (R 4.1.2)
 tzdb             0.3.0    2022-03-28 [1] CRAN (R 4.1.2)
 utf8             1.2.2    2021-07-24 [1] CRAN (R 4.1.2)
 vctrs            0.4.1    2022-04-13 [1] CRAN (R 4.1.2)
 withr            2.5.0    2022-03-03 [1] CRAN (R 4.1.2)
 xfun             0.31     2022-05-10 [1] CRAN (R 4.1.2)
 xml2             1.3.3    2021-11-30 [1] CRAN (R 4.1.2)
 xtable           1.8-4    2019-04-21 [1] CRAN (R 4.1.2)
 xts              0.12.1   2020-09-09 [1] CRAN (R 4.1.2)
 zoo              1.8-10   2022-04-15 [1] CRAN (R 4.1.2)
```

</details>

<details>
  <summary>Software versions for analysing mixture model</summary>
  The analysis of the mixture model was last rendered with the following versions
  
```r
─ Session info ─────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.2.1 (2022-06-23 ucrt)
 os       Windows 10 x64 (build 19044)
 system   x86_64, mingw32
 ui       RStudio
 language (EN)
 collate  German_Austria.utf8
 ctype    German_Austria.utf8
 tz       Europe/Berlin
 date     2022-10-13
 rstudio  2022.07.1+554 Spotted Wakerobin (desktop)
 pandoc   2.11.0.2 @ C:\\Users\\tklebel\\AppData\\Local\\Pandoc\\pandoc.exe

─ Packages ─────────────────────────────────────────────────────────────────────
 ! package         * version  date (UTC) lib source
   abind             1.4-5    2016-07-21 [1] CRAN (R 4.2.0)
   arrayhelpers      1.1-0    2020-02-04 [1] CRAN (R 4.2.1)
   assertthat        0.2.1    2019-03-21 [1] CRAN (R 4.2.1)
   backports         1.4.1    2021-12-13 [1] CRAN (R 4.2.0)
   base64enc         0.1-3    2015-07-28 [1] CRAN (R 4.2.0)
   bayesplot       * 1.9.0    2022-03-10 [1] CRAN (R 4.2.1)
   bridgesampling    1.1-2    2021-04-16 [1] CRAN (R 4.2.1)
   brms            * 2.18.0   2022-09-19 [1] CRAN (R 4.2.1)
   Brobdingnag       1.2-7    2022-02-03 [1] CRAN (R 4.2.1)
   broom             1.0.1    2022-08-29 [1] CRAN (R 4.2.1)
   callr             3.7.2    2022-08-22 [1] CRAN (R 4.2.1)
   cellranger        1.1.0    2016-07-27 [1] CRAN (R 4.2.1)
   checkmate         2.1.0    2022-04-21 [1] CRAN (R 4.2.1)
   cli               3.4.0    2022-09-08 [1] CRAN (R 4.2.1)
   coda              0.19-4   2020-09-30 [1] CRAN (R 4.2.1)
   codetools         0.2-18   2020-11-04 [2] CRAN (R 4.2.1)
   colorspace      * 2.0-3    2022-02-21 [1] CRAN (R 4.2.1)
   colourpicker      1.1.1    2021-10-04 [1] CRAN (R 4.2.1)
   crayon            1.5.1    2022-03-26 [1] CRAN (R 4.2.1)
   crosstalk         1.2.0    2021-11-04 [1] CRAN (R 4.2.1)
   data.table        1.14.2   2021-09-27 [1] CRAN (R 4.2.1)
   DBI               1.1.3    2022-06-18 [1] CRAN (R 4.2.1)
   dbplyr            2.2.1    2022-06-27 [1] CRAN (R 4.2.1)
   digest            0.6.29   2021-12-01 [1] CRAN (R 4.2.1)
   distributional    0.3.1    2022-09-02 [1] CRAN (R 4.2.1)
   dplyr           * 1.0.10   2022-09-01 [1] CRAN (R 4.2.1)
   DT                0.25     2022-09-12 [1] CRAN (R 4.2.1)
   dygraphs          1.1.1.6  2018-07-11 [1] CRAN (R 4.2.1)
   ellipsis          0.3.2    2021-04-29 [1] CRAN (R 4.2.1)
   extrafont         0.18     2022-04-12 [1] CRAN (R 4.2.0)
   extrafontdb       1.0      2012-06-11 [1] CRAN (R 4.2.0)
   fansi             1.0.3    2022-03-24 [1] CRAN (R 4.2.1)
   farver            2.1.1    2022-07-06 [1] CRAN (R 4.2.1)
   fastmap           1.1.0    2021-01-25 [1] CRAN (R 4.2.1)
   forcats         * 0.5.2    2022-08-19 [1] CRAN (R 4.2.1)
   fs                1.5.2    2021-12-08 [1] CRAN (R 4.2.1)
   gargle            1.2.1    2022-09-08 [1] CRAN (R 4.2.1)
   generics          0.1.3    2022-07-05 [1] CRAN (R 4.2.1)
   ggdist            3.2.0    2022-07-19 [1] CRAN (R 4.2.1)
   ggplot2         * 3.3.6    2022-05-03 [1] CRAN (R 4.2.1)
   ggrepel           0.9.1    2021-01-15 [1] CRAN (R 4.2.1)
   ggridges          0.5.4    2022-09-26 [1] CRAN (R 4.2.1)
   glue              1.6.2    2022-02-24 [1] CRAN (R 4.2.1)
   googledrive       2.0.0    2021-07-08 [1] CRAN (R 4.2.1)
   googlesheets4     1.0.1    2022-08-13 [1] CRAN (R 4.2.1)
   gridExtra         2.3      2017-09-09 [1] CRAN (R 4.2.1)
   gtable            0.3.1    2022-09-01 [1] CRAN (R 4.2.1)
   gtools            3.9.3    2022-07-11 [1] CRAN (R 4.2.1)
   haven             2.5.1    2022-08-22 [1] CRAN (R 4.2.1)
   here              1.0.1    2020-12-13 [1] CRAN (R 4.2.1)
   hms               1.1.2    2022-08-19 [1] CRAN (R 4.2.1)
   htmltools         0.5.3    2022-07-18 [1] CRAN (R 4.2.1)
   htmlwidgets       1.5.4    2021-09-08 [1] CRAN (R 4.2.1)
   httpuv            1.6.6    2022-09-08 [1] CRAN (R 4.2.1)
   httr              1.4.4    2022-08-17 [1] CRAN (R 4.2.1)
   igraph            1.3.4    2022-07-19 [1] CRAN (R 4.2.1)
   inline            0.3.19   2021-05-31 [1] CRAN (R 4.2.1)
   jsonlite          1.8.0    2022-02-22 [1] CRAN (R 4.2.1)
   knitr             1.40     2022-08-24 [1] CRAN (R 4.2.1)
   later             1.3.0    2021-08-18 [1] CRAN (R 4.2.1)
   lattice           0.20-45  2021-09-22 [2] CRAN (R 4.2.1)
   lazyeval          0.2.2    2019-03-15 [1] CRAN (R 4.2.1)
   lifecycle         1.0.2    2022-09-09 [1] CRAN (R 4.2.1)
   loo               2.5.1    2022-03-24 [1] CRAN (R 4.2.1)
   lubridate         1.8.0    2021-10-07 [1] CRAN (R 4.2.1)
   magrittr          2.0.3    2022-03-30 [1] CRAN (R 4.2.1)
   marginaleffects * 0.7.1    2022-09-25 [1] CRAN (R 4.2.1)
   markdown          1.1      2019-08-07 [1] CRAN (R 4.2.1)
   Matrix            1.4-1    2022-03-23 [2] CRAN (R 4.2.1)
   matrixStats       0.62.0   2022-04-19 [1] CRAN (R 4.2.1)
   MetBrewer       * 0.2.0    2022-03-21 [1] CRAN (R 4.2.1)
   mime              0.12     2021-09-28 [1] CRAN (R 4.2.0)
   miniUI            0.1.1.1  2018-05-18 [1] CRAN (R 4.2.1)
   modelr          * 0.1.9    2022-08-19 [1] CRAN (R 4.2.1)
   munsell           0.5.0    2018-06-12 [1] CRAN (R 4.2.1)
   mvtnorm           1.1-3    2021-10-08 [1] CRAN (R 4.2.0)
   nlme              3.1-157  2022-03-25 [2] CRAN (R 4.2.1)
   pillar            1.8.1    2022-08-19 [1] CRAN (R 4.2.1)
   pkgbuild          1.3.1    2021-12-20 [1] CRAN (R 4.2.1)
   pkgconfig         2.0.3    2019-09-22 [1] CRAN (R 4.2.1)
   plotly            4.10.0   2021-10-09 [1] CRAN (R 4.2.1)
   plyr              1.8.7    2022-03-24 [1] CRAN (R 4.2.1)
   posterior         1.3.1    2022-09-06 [1] CRAN (R 4.2.1)
   prettyunits       1.1.1    2020-01-24 [1] CRAN (R 4.2.1)
   processx          3.7.0    2022-07-07 [1] CRAN (R 4.2.1)
   promises          1.2.0.1  2021-02-11 [1] CRAN (R 4.2.1)
   prompt            1.0.1    2021-03-12 [1] CRAN (R 4.2.1)
   ps                1.7.1    2022-06-18 [1] CRAN (R 4.2.1)
   purrr           * 0.3.4    2020-04-17 [1] CRAN (R 4.2.1)
   R6                2.5.1    2021-08-19 [1] CRAN (R 4.2.1)
   Rcpp            * 1.0.9    2022-07-08 [1] CRAN (R 4.2.1)
 D RcppParallel      5.1.5    2022-01-05 [1] CRAN (R 4.2.1)
   readr           * 2.1.2    2022-01-30 [1] CRAN (R 4.2.1)
   readxl            1.4.1    2022-08-17 [1] CRAN (R 4.2.1)
   reprex            2.0.2    2022-08-17 [1] CRAN (R 4.2.1)
   reshape2          1.4.4    2020-04-09 [1] CRAN (R 4.2.1)
   rlang             1.0.5    2022-08-31 [1] CRAN (R 4.2.1)
   rprojroot         2.0.3    2022-04-02 [1] CRAN (R 4.2.1)
   rstan             2.21.7   2022-09-08 [1] CRAN (R 4.2.1)
   rstantools        2.2.0    2022-04-08 [1] CRAN (R 4.2.1)
   rstudioapi        0.14     2022-08-22 [1] CRAN (R 4.2.1)
   Rttf2pt1          1.3.8    2020-01-10 [1] CRAN (R 4.2.1)
   rvest             1.0.3    2022-08-19 [1] CRAN (R 4.2.1)
   scales          * 1.2.1    2022-08-20 [1] CRAN (R 4.2.1)
   sessioninfo       1.2.2    2021-12-06 [1] CRAN (R 4.2.1)
   shiny             1.7.2    2022-07-19 [1] CRAN (R 4.2.1)
   shinyjs           2.1.0    2021-12-23 [1] CRAN (R 4.2.1)
   shinystan         2.6.0    2022-03-03 [1] CRAN (R 4.2.1)
   shinythemes       1.2.0    2021-01-25 [1] CRAN (R 4.2.1)
   StanHeaders       2.21.0-7 2020-12-17 [1] CRAN (R 4.2.1)
   stringi           1.7.8    2022-07-11 [1] CRAN (R 4.2.1)
   stringr         * 1.4.1    2022-08-20 [1] CRAN (R 4.2.1)
   svUnit            1.0.6    2021-04-19 [1] CRAN (R 4.2.1)
   tensorA           0.36.2   2020-11-19 [1] CRAN (R 4.2.0)
   threejs           0.3.3    2020-01-21 [1] CRAN (R 4.2.1)
   tibble          * 3.1.8    2022-07-22 [1] CRAN (R 4.2.1)
   tidybayes       * 3.0.2    2022-01-05 [1] CRAN (R 4.2.1)
   tidyr           * 1.2.1    2022-09-08 [1] CRAN (R 4.2.1)
   tidyselect        1.1.2    2022-02-21 [1] CRAN (R 4.2.1)
   tidyverse       * 1.3.2    2022-07-18 [1] CRAN (R 4.2.1)
   tzdb              0.3.0    2022-03-28 [1] CRAN (R 4.2.1)
   utf8              1.2.2    2021-07-24 [1] CRAN (R 4.2.1)
   vctrs             0.4.1    2022-04-13 [1] CRAN (R 4.2.1)
   viridisLite       0.4.1    2022-08-22 [1] CRAN (R 4.2.1)
   WDI             * 2.7.8    2022-09-25 [1] CRAN (R 4.2.1)
   withr             2.5.0    2022-03-03 [1] CRAN (R 4.2.1)
   xfun              0.33     2022-09-12 [1] CRAN (R 4.2.1)
   xml2              1.3.3    2021-11-30 [1] CRAN (R 4.2.1)
   xtable            1.8-4    2019-04-21 [1] CRAN (R 4.2.1)
   xts               0.12.1   2020-09-09 [1] CRAN (R 4.2.1)
   zoo               1.8-11   2022-09-17 [1] CRAN (R 4.2.1)

 D ── DLL MD5 mismatch, broken installation.
```

</details>


# Licenses

![](license.png)

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
