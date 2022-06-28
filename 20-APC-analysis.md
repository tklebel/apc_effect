---
title: "Relationship between OA publishing, APCs and IF"
author: "Thomas Klebel"
date: "28 June, 2022"
output: 
  html_document:
    keep_md: true
---



# Sample description 
## Institutions per country

```r
universities_per_country <- works %>% 
  distinct(country, University) %>% 
  count(country, name = "n_universities") %>% 
  arrange(desc(n_universities)) %>% 
  collect()

universities_per_country %>% 
  knitr::kable()
```



|country              | n_universities|
|:--------------------|--------------:|
|China                |            215|
|United States        |            199|
|United Kingdom       |             60|
|Japan                |             55|
|Germany              |             54|
|South Korea          |             46|
|Spain                |             42|
|Italy                |             41|
|India                |             38|
|Iran                 |             36|
|Australia            |             32|
|Turkey               |             31|
|Brazil               |             31|
|Poland               |             31|
|Canada               |             30|
|France               |             28|
|Taiwan               |             21|
|Netherlands          |             13|
|Sweden               |             12|
|Austria              |             12|
|Russia               |             10|
|South Africa         |              9|
|Switzerland          |              8|
|Egypt                |              8|
|Greece               |              8|
|Israel               |              8|
|Belgium              |              8|
|Finland              |              7|
|Czech Republic       |              7|
|New Zealand          |              7|
|Malaysia             |              6|
|Hungary              |              6|
|Mexico               |              6|
|Norway               |              6|
|Thailand             |              6|
|Ireland              |              6|
|Portugal             |              6|
|Denmark              |              5|
|Pakistan             |              5|
|Saudi Arabia         |              5|
|Singapore            |              3|
|Colombia             |              3|
|Argentina            |              3|
|Romania              |              3|
|Chile                |              3|
|Serbia               |              3|
|Tunisia              |              3|
|Jordan               |              2|
|Slovenia             |              2|
|Slovakia             |              2|
|Nigeria              |              2|
|United Arab Emirates |              2|
|Algeria              |              1|
|Iceland              |              1|
|Uganda               |              1|
|Lithuania            |              1|
|Oman                 |              1|
|Lebanon              |              1|
|Qatar                |              1|
|Cyprus               |              1|
|Viet Nam             |              1|
|Kuwait               |              1|
|Croatia              |              1|
|Estonia              |              1|
|Luxembourg           |              1|
|Ghana                |              1|
|Morocco              |              1|
|Uruguay              |              1|
|Ethiopia             |              1|


```r
# papers per country
papers_per_country <- works %>% 
  distinct(country, country_code, id, work_frac, author_position, institution_id) %>% 
  group_by(country, country_code) %>% 
  summarise(sum_fractional_works = sum(work_frac) %>% round(digits = 1)) %>% 
  arrange(desc(sum_fractional_works)) %>% 
  collect()
```

```
## `summarise()` has grouped output by 'country'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'country'. You can override using the `.groups` argument.
```


```r
papers_per_country %>% 
  select(-country_code) %>% 
  knitr::kable()
```



|country              | sum_fractional_works|
|:--------------------|--------------------:|
|United States        |             132628.9|
|Brazil               |             103447.9|
|China                |              97179.9|
|United Kingdom       |              38102.9|
|Spain                |              34201.0|
|Germany              |              30103.2|
|Canada               |              27258.0|
|Australia            |              21986.6|
|South Korea          |              20156.7|
|Japan                |              20089.4|
|Italy                |              18318.3|
|Poland               |              14124.0|
|Netherlands          |              11053.1|
|South Africa         |              10701.6|
|Sweden               |              10646.7|
|Taiwan               |               9195.1|
|India                |               8771.8|
|Iran                 |               8511.9|
|France               |               7881.0|
|Switzerland          |               7804.5|
|Mexico               |               6637.2|
|Turkey               |               6554.0|
|Belgium              |               6295.9|
|Portugal             |               5963.7|
|Denmark              |               5321.0|
|Malaysia             |               5107.1|
|Israel               |               4829.3|
|Norway               |               4701.5|
|Austria              |               4549.8|
|Colombia             |               4403.0|
|Russia               |               4289.4|
|Argentina            |               4039.5|
|Saudi Arabia         |               4037.6|
|Chile                |               3713.8|
|Finland              |               3449.6|
|Egypt                |               3348.2|
|New Zealand          |               2945.4|
|Singapore            |               2847.1|
|Thailand             |               2659.1|
|Greece               |               2643.7|
|Czech Republic       |               2603.4|
|Ireland              |               2268.4|
|Serbia               |               1808.1|
|Hungary              |               1641.8|
|Slovenia             |               1635.0|
|Croatia              |               1505.5|
|Pakistan             |               1188.1|
|Nigeria              |                741.9|
|Uruguay              |                735.7|
|Romania              |                732.9|
|Estonia              |                589.6|
|Slovakia             |                498.1|
|Jordan               |                497.1|
|Lithuania            |                485.9|
|United Arab Emirates |                415.1|
|Oman                 |                404.2|
|Lebanon              |                367.5|
|Ethiopia             |                343.1|
|Uganda               |                335.7|
|Ghana                |                330.2|
|Kuwait               |                312.7|
|Tunisia              |                294.8|
|Iceland              |                242.7|
|Qatar                |                201.9|
|Luxembourg           |                200.6|
|Viet Nam             |                136.0|
|Cyprus               |                135.6|
|Morocco              |                129.3|
|Algeria              |                 38.0|


```r
# average apc
average_apc <- works %>%
  # first get rid of duplicates from concepts
  distinct(country, country_code, id, work_frac, author_position, institution_id,
           APC_in_dollar) %>% 
  group_by(country) %>%
  # compute the average APC using fractional authorships as weights
  mutate(sum_frac = sum(work_frac)) %>%
  group_by(country, sum_frac) %>%
  summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac) %>% 
  collect()
```

```
## `summarise()` has grouped output by 'country'. You can override using the
## `.groups` argument.
```


```r
# average APC over time
average_apc_time <- works %>%
  # first get rid of duplicates from concepts
  distinct(country, country_code, id, work_frac, author_position, institution_id,
           APC_in_dollar, publication_year) %>% 
  group_by(country, country_code, publication_year) %>%
  # compute the average APC using fractional authorships as weights
  mutate(sum_frac = sum(work_frac)) %>%
  group_by(country, country_code, sum_frac, publication_year) %>%
  summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac) %>% 
  collect()
```

```
## `summarise()` has grouped output by 'country', 'country_code', 'sum_frac'. You
## can override using the `.groups` argument.
```


```r
average_apc_time %>% 
  left_join(wdi, by = c("country_code" = "iso2c")) %>% 
  ggplot(aes(publication_year, mean_apc)) +
  geom_line(aes(group = country), alpha = .3) +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(region)) +
  scale_x_continuous(breaks = scales::pretty_breaks(6)) +
  coord_cartesian(ylim = c(0, 3000)) +
  labs(x = NULL, y = "Mean APC")
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](20-APC-analysis_files/figure-html/apc-region-time-1.png)<!-- -->



All three joined

```r
universities_per_country %>% 
  left_join(papers_per_country, by = "country") %>% 
  left_join(average_apc, by = "country") %>% 
  select(Country = country, `n universities` = n_universities,
         `n fractional publications` = sum_fractional_works, 
         `Mean APC` = mean_apc) %>% 
  knitr::kable()
```



|Country              | n universities| n fractional publications|  Mean APC|
|:--------------------|--------------:|-------------------------:|---------:|
|China                |            215|                   97179.9| 1860.1009|
|United States        |            199|                  132628.9| 1924.7870|
|United Kingdom       |             60|                   38102.9| 1813.0199|
|Japan                |             55|                   20089.4| 1807.1568|
|Germany              |             54|                   30103.2| 1801.3132|
|South Korea          |             46|                   20156.7| 1705.1027|
|Spain                |             42|                   34201.0|  675.1351|
|Italy                |             41|                   18318.3| 1688.0173|
|India                |             38|                    8771.8|  952.2281|
|Iran                 |             36|                    8511.9|  774.7262|
|Australia            |             32|                   21986.6| 1778.9696|
|Turkey               |             31|                    6554.0|  816.8314|
|Brazil               |             31|                  103447.9|  251.6889|
|Poland               |             31|                   14124.0|  836.2554|
|Canada               |             30|                   27258.0| 1691.5385|
|France               |             28|                    7881.0| 1578.2652|
|Taiwan               |             21|                    9195.1| 1841.0145|
|Netherlands          |             13|                   11053.1| 1816.9257|
|Sweden               |             12|                   10646.7| 1835.1115|
|Austria              |             12|                    4549.8| 1690.2740|
|Russia               |             10|                    4289.4|  467.5514|
|South Africa         |              9|                   10701.6|  939.8288|
|Switzerland          |              8|                    7804.5| 2006.9330|
|Egypt                |              8|                    3348.2|  925.9151|
|Greece               |              8|                    2643.7| 1496.4210|
|Israel               |              8|                    4829.3| 2009.7009|
|Belgium              |              8|                    6295.9| 1707.8655|
|Finland              |              7|                    3449.6| 1570.4558|
|Czech Republic       |              7|                    2603.4| 1045.4562|
|New Zealand          |              7|                    2945.4| 1672.8857|
|Malaysia             |              6|                    5107.1| 1112.8705|
|Hungary              |              6|                    1641.8| 1415.5710|
|Mexico               |              6|                    6637.2|  682.3270|
|Norway               |              6|                    4701.5| 1564.4489|
|Thailand             |              6|                    2659.1| 1521.5156|
|Ireland              |              6|                    2268.4| 1781.7014|
|Portugal             |              6|                    5963.7|  778.8029|
|Denmark              |              5|                    5321.0| 1740.5658|
|Pakistan             |              5|                    1188.1| 1040.8818|
|Saudi Arabia         |              5|                    4037.6| 1432.8707|
|Singapore            |              3|                    2847.1| 1999.8188|
|Colombia             |              3|                    4403.0|  237.3403|
|Argentina            |              3|                    4039.5|  294.1655|
|Romania              |              3|                     732.9|  881.5032|
|Chile                |              3|                    3713.8|  564.0258|
|Serbia               |              3|                    1808.1|  568.2770|
|Tunisia              |              3|                     294.8| 1194.6685|
|Jordan               |              2|                     497.1| 1309.0617|
|Slovenia             |              2|                    1635.0|  856.7217|
|Slovakia             |              2|                     498.1|  653.1366|
|Nigeria              |              2|                     741.9| 1118.2824|
|United Arab Emirates |              2|                     415.1| 1776.7250|
|Algeria              |              1|                      38.0|  732.6798|
|Iceland              |              1|                     242.7| 1425.6030|
|Uganda               |              1|                     335.7| 1784.9360|
|Lithuania            |              1|                     485.9|  793.7925|
|Oman                 |              1|                     404.2|  527.5472|
|Lebanon              |              1|                     367.5| 1694.4625|
|Qatar                |              1|                     201.9| 1542.4601|
|Cyprus               |              1|                     135.6| 1552.3123|
|Viet Nam             |              1|                     136.0| 1314.0193|
|Kuwait               |              1|                     312.7| 1634.8834|
|Croatia              |              1|                    1505.5|  385.2555|
|Estonia              |              1|                     589.6| 1005.0140|
|Luxembourg           |              1|                     200.6| 1751.8165|
|Ghana                |              1|                     330.2| 1564.2878|
|Morocco              |              1|                     129.3|  882.8498|
|Uruguay              |              1|                     735.7|  382.8625|
|Ethiopia             |              1|                     343.1| 1664.7411|


## Papers per continent

```r
# needs to be rechecked once the figures are correct!!!!!!
plot_data <- papers_per_country %>% 
  left_join(wdi, by = c("country_code" = "iso2c")) %>% 
  group_by(region) %>% 
  summarise(sum_fractional_works = sum(sum_fractional_works)) %>% 
  mutate(prop = sum_fractional_works / sum(sum_fractional_works),
         label = glue::glue(
           "{scales::comma(sum_fractional_works)} ({scales::percent(prop, accuracy = .1)})"
         )
  )

plot_data %>% 
  ggplot(aes(sum_fractional_works, fct_reorder(region, prop))) +
  geom_col(width = .5) +
  geom_text(aes(label = label), nudge_x = 2000, hjust = "left") +
  scale_x_continuous(expand = expansion(mult = c(0.05, .25)),
                     labels = scales::comma) +
  labs(x = "Number of fractional publications", y = NULL) +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "grey92"))
```

![](20-APC-analysis_files/figure-html/papers-p-continent-1.png)<!-- -->


## Distribution across topics
How many papers do we have, which also have a topic?

```r
works %>% 
  filter(!is.na(field)) %>% 
  distinct(id) %>% 
  sdf_nrow()
```

```
## [1] 1633554
```
This is our total sample size.


Which topics are represented in our sample?

```r
frac_concept_papers <- works %>% 
  distinct(id, field, concept_frac) %>% 
  group_by(field) %>% 
  summarise(frac_papers = sum(concept_frac)) %>% 
  arrange(desc(frac_papers)) %>% 
  collect()
```



```r
plot_data <- frac_concept_papers %>% 
  drop_na() %>% 
  mutate(prop = frac_papers / sum(frac_papers),
         label = glue::glue(
           "{scales::comma(frac_papers)} ({scales::percent(prop, accuracy = .1)})"
         )
  )

plot_data %>% 
  ggplot(aes(frac_papers, fct_reorder(field, prop))) +
  geom_col(width = .5) +
  geom_text(aes(label = label), nudge_x = 5000, hjust = "left") +
  scale_x_continuous(expand = expansion(mult = c(0.05, .25)),
                     labels = scales::comma) +
  labs(x = "Number of fractional publications", y = NULL) +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "grey92"))
```

![](20-APC-analysis_files/figure-html/concept-overview-1.png)<!-- -->

# Association between P_top10 and APC

```r
get_mean_apc_by_author_position <- function(df) {
  df %>%
    # first get rid of duplicates from concepts
    distinct(id, author_position, work_frac, APC_in_dollar, University, country,
             publication_year, P_top10) %>% 
    group_by(University, publication_year, country, P_top10) %>%
    # compute the average APC using fractional authorships as weights
    mutate(sum_frac = sum(work_frac)) %>%
    group_by(University, publication_year, country, P_top10, sum_frac,
             author_position) %>%
    summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac,
              fractional_works = sum(work_frac))
}

mean_apcs <- works %>% 
  filter(publication_year == last_year_of_period) %>% 
  get_mean_apc_by_author_position()

mean_apcs_local <- mean_apcs %>%
  collect()
```

```
## `summarise()` has grouped output by 'University', 'publication_year', 'country',
## 'P_top10', 'sum_frac'. You can override using the `.groups` argument.
```

```r
mean_apc_16_19 <- works %>% 
  filter(first_year_of_period == 2016) %>% 
  get_mean_apc_by_author_position()

mean_apc_16_19_local <- mean_apc_16_19 %>% 
  collect()
```

```
## `summarise()` has grouped output by 'University', 'publication_year', 'country',
## 'P_top10', 'sum_frac'. You can override using the `.groups` argument.
```


```r
# plot for 2016-19

# taking out the correlation, because they are incorrect given that the figure
# shows a non-linear relationship (x-axis logged), but the correlation is linear
# (and quite unsuitable to the skewed P_top10)
mean_apc_16_19_local %>%
  mutate(author_position = recode(author_position, first = "First authors", 
                                  last = "Last authors")) %>% 
  ggplot(aes(P_top10, mean_apc, colour = fractional_works)) + 
  geom_point(aes(), alpha = .5) +
  scale_colour_viridis_c(option = "B", trans = "log10") +
  geom_smooth(colour = "grey30") +
  facet_wrap(vars(author_position)) +
  scale_x_log10() +
  scale_y_continuous(labels = dollar) +
  labs(caption = "Fractional counting; 2016-2019", y = "Mean APC",
       colour = "Number of papers per institution",
       x = expression(P["top 10%"])) +
  theme(legend.position = "top")
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/apc-first-last-1.png)<!-- -->


```r
mean_apcs_local %>%
  group_by(publication_year, author_position) %>%
  mutate(ptop10_quantiles = cut_quartiles(P_top10)) %>%
  group_by(ptop10_quantiles, publication_year, author_position) %>%
  summarise(mean_apc = weighted.mean(mean_apc, sum_frac, na.rm = TRUE), 
            .groups = "drop_last") %>%
  ggplot(aes(publication_year, mean_apc, colour = ptop10_quantiles,
             group = ptop10_quantiles)) +
  geom_line() +
  facet_wrap(vars(author_position)) +
  scale_x_continuous(breaks = seq(2010, 2018, by = 4)) +
  scale_y_continuous(labels = dollar) +
  theme(legend.position = "top") +
  labs(caption = "Fractional counting", y = "Mean APC",
       colour = expression(P["top 10%"]), x = NULL)
```

![](20-APC-analysis_files/figure-html/apc-first-last-time-1.png)<!-- -->


```r
get_mean_apc_by_concept <- function(df) {
  df %>%
    distinct(id, University, publication_year, P_top10, field, work_frac, 
             APC_in_dollar, author_position) %>% 
    group_by(University, publication_year, P_top10, field) %>%
    # spark is unhappy for some reason, so coerce again to numeric
    mutate(work_frac = as.numeric(work_frac)) %>% 
    # compute the average APC using fractional authorships as weights
    mutate(sum_frac = sum(work_frac)) %>%
    group_by(University, publication_year, P_top10, sum_frac,
             author_position, field) %>%
    summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac)
}

mean_apc_concept <- works %>% 
  filter(publication_year == last_year_of_period) %>% 
  get_mean_apc_by_concept()

mean_apc_concept_local <- mean_apc_concept %>%
  collect()
```

```
## `summarise()` has grouped output by 'University', 'publication_year', 'P_top10',
## 'sum_frac', 'author_position'. You can override using the `.groups` argument.
```

```r
mean_apc_concept_16_19 <- works %>% 
  filter(first_year_of_period == 2016) %>% 
  get_mean_apc_by_concept()

mean_apc_concept_16_19_local <- mean_apc_concept_16_19 %>% 
  collect()
```

```
## `summarise()` has grouped output by 'University', 'publication_year', 'P_top10',
## 'sum_frac', 'author_position'. You can override using the `.groups` argument.
```




```r
# plot for 2016-2019
p <- mean_apc_concept_16_19_local %>%
  drop_na(field) %>% 
  ggplot(aes(P_top10, mean_apc, colour = field)) +
  geom_smooth(alpha = .15) +
  facet_wrap(vars(author_position), nrow = 1) +
  scale_x_log10() +
  scale_y_continuous(labels = dollar) + 
  labs(caption = "2016-2019", y = "Mean APC",
       x = expression(P["top 10%"]))
p
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/apc-concepts-1.png)<!-- -->



```r
plotly::ggplotly(p)
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

preservea4916e2c1170368e



## Over time - first-authors

```r
mean_apc_concept_local %>%
  filter(!is.na(field), author_position == "first") %>% 
  group_by(publication_year, field) %>%
  mutate(ptop10_quantiles = cut_quartiles(P_top10)) %>%
  group_by(ptop10_quantiles, publication_year, field) %>%
  summarise(mean_apc = weighted.mean(mean_apc, sum_frac, na.rm = TRUE), 
            .groups = "drop_last") %>%
  ggplot(aes(publication_year, mean_apc, colour = ptop10_quantiles,
             group = ptop10_quantiles)) +
  geom_line() +
  facet_wrap(vars(field)) +
  scale_x_continuous(breaks = seq(2010, 2018, by = 4)) +
  theme(legend.position = "top") +
  labs(caption = "Fractional counting", y = "Mean APC",
       colour = expression(P["top 10%"]), x = NULL)
```

![](20-APC-analysis_files/figure-html/apc-time-concept-first-1.png)<!-- -->

## Over time - last-authors

```r
mean_apc_concept_local %>%
  filter(!is.na(field), author_position == "last") %>% 
  group_by(publication_year, field) %>%
  mutate(ptop10_quantiles = cut_quartiles(P_top10)) %>%
  group_by(ptop10_quantiles, publication_year, field) %>%
  summarise(mean_apc = weighted.mean(mean_apc, sum_frac, na.rm = TRUE), 
            .groups = "drop_last") %>%
  ggplot(aes(publication_year, mean_apc, colour = ptop10_quantiles,
             group = ptop10_quantiles)) +
  geom_line() +
  facet_wrap(vars(field)) +
  scale_x_continuous(breaks = seq(2010, 2018, by = 4)) +
  theme(legend.position = "top") +
  labs(caption = "Fractional counting", y = "Mean APC",
       colour = expression(P["top 10%"]), x = NULL)
```

![](20-APC-analysis_files/figure-html/apc-time-concept-last-1.png)<!-- -->

# Country comparison

```r
mean_apc_country_16_19 <- works %>%
  filter(first_year_of_period == 2016) %>% 
  # first get rid of duplicates from concepts
  distinct(id, work_frac, APC_in_dollar, University, country, P_top10, country_code) %>% 
  # spark is unhappy for some reason, so coerce again to numeric
  mutate(work_frac = as.numeric(work_frac)) %>% 
  group_by(University, country, P_top10) %>%
  # compute the average APC using fractional authorships as weights
  mutate(sum_frac = sum(work_frac)) %>%
  group_by(University, country, P_top10, sum_frac, country_code) %>%
  summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac)

mean_apc_country_16_19_local <- mean_apc_country_16_19 %>%
  collect()
```

```
## `summarise()` has grouped output by 'University', 'country', 'P_top10',
## 'sum_frac'. You can override using the `.groups` argument.
```


```r
mean_apc_country_16_19_local <- mean_apc_country_16_19_local %>% 
  left_join(wdi, by = c("country_code" = "iso2c"))
```


```r
mean_apc_country_16_19_local %>% 
  ggplot(aes(P_top10, mean_apc, colour = region)) +
  geom_point(alpha = .3, size = 1.2) +
  geom_smooth(alpha = .3) +
  scale_x_log10() +
  scale_y_continuous(labels = dollar) +
  labs(y = "Mean APC", x = expression(P["top 10%"]), colour = NULL) +
  theme(legend.position = "top")
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](20-APC-analysis_files/figure-html/apc-by-country-1.png)<!-- -->



```r
spark_disconnect(sc)
```

