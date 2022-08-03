---
title: "Relationship between OA publishing, APCs and IF"
author: "Thomas Klebel"
date: "03 August, 2022"
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
|Poland               |             31|
|Brazil               |             31|
|Turkey               |             31|
|Canada               |             30|
|France               |             28|
|Taiwan               |             21|
|Netherlands          |             13|
|Austria              |             12|
|Sweden               |             12|
|Russia               |             10|
|South Africa         |              9|
|Greece               |              8|
|Belgium              |              8|
|Switzerland          |              8|
|Israel               |              8|
|Egypt                |              8|
|Czech Republic       |              7|
|Finland              |              7|
|New Zealand          |              7|
|Ireland              |              6|
|Mexico               |              6|
|Hungary              |              6|
|Norway               |              6|
|Malaysia             |              6|
|Thailand             |              6|
|Portugal             |              6|
|Denmark              |              5|
|Pakistan             |              5|
|Saudi Arabia         |              5|
|Argentina            |              3|
|Tunisia              |              3|
|Colombia             |              3|
|Chile                |              3|
|Serbia               |              3|
|Romania              |              3|
|Singapore            |              3|
|Slovakia             |              2|
|Nigeria              |              2|
|United Arab Emirates |              2|
|Jordan               |              2|
|Slovenia             |              2|
|Croatia              |              1|
|Lithuania            |              1|
|Cyprus               |              1|
|Viet Nam             |              1|
|Qatar                |              1|
|Ghana                |              1|
|Uruguay              |              1|
|Algeria              |              1|
|Estonia              |              1|
|Lebanon              |              1|
|Morocco              |              1|
|Luxembourg           |              1|
|Iceland              |              1|
|Oman                 |              1|
|Ethiopia             |              1|
|Kuwait               |              1|
|Uganda               |              1|


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
  geom_smooth(se = FALSE, colour = "#007FA8") +
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
all_three_descriptives <- universities_per_country %>% 
  left_join(papers_per_country, by = "country") %>% 
  left_join(average_apc, by = "country")

all_three_descriptives %>% 
  arrange(desc(n_universities), desc(sum_fractional_works)) %>% 
  mutate(mean_apc = round(mean_apc, digits = 1),
         sum_fractional_works = scales::comma(sum_fractional_works)) %>% 
  select(Country = country, `n universities` = n_universities,
         `n fractional publications` = sum_fractional_works, 
         `Mean APC` = mean_apc) %>% 
  knitr::kable()
```



|Country              | n universities|n fractional publications | Mean APC|
|:--------------------|--------------:|:-------------------------|--------:|
|China                |            215|97,179.90                 |   1860.1|
|United States        |            199|132,628.90                |   1924.8|
|United Kingdom       |             60|38,102.90                 |   1813.0|
|Japan                |             55|20,089.40                 |   1807.2|
|Germany              |             54|30,103.20                 |   1801.3|
|South Korea          |             46|20,156.70                 |   1705.1|
|Spain                |             42|34,201.00                 |    675.1|
|Italy                |             41|18,318.30                 |   1688.0|
|India                |             38|8,771.80                  |    952.2|
|Iran                 |             36|8,511.90                  |    774.7|
|Australia            |             32|21,986.60                 |   1779.0|
|Brazil               |             31|103,447.90                |    251.7|
|Poland               |             31|14,124.00                 |    836.3|
|Turkey               |             31|6,554.00                  |    816.8|
|Canada               |             30|27,258.00                 |   1691.5|
|France               |             28|7,881.00                  |   1578.3|
|Taiwan               |             21|9,195.10                  |   1841.0|
|Netherlands          |             13|11,053.10                 |   1816.9|
|Sweden               |             12|10,646.70                 |   1835.1|
|Austria              |             12|4,549.80                  |   1690.3|
|Russia               |             10|4,289.40                  |    467.6|
|South Africa         |              9|10,701.60                 |    939.8|
|Switzerland          |              8|7,804.50                  |   2006.9|
|Belgium              |              8|6,295.90                  |   1707.9|
|Israel               |              8|4,829.30                  |   2009.7|
|Egypt                |              8|3,348.20                  |    925.9|
|Greece               |              8|2,643.70                  |   1496.4|
|Finland              |              7|3,449.60                  |   1570.5|
|New Zealand          |              7|2,945.40                  |   1672.9|
|Czech Republic       |              7|2,603.40                  |   1045.5|
|Mexico               |              6|6,637.20                  |    682.3|
|Portugal             |              6|5,963.70                  |    778.8|
|Malaysia             |              6|5,107.10                  |   1112.9|
|Norway               |              6|4,701.50                  |   1564.4|
|Thailand             |              6|2,659.10                  |   1521.5|
|Ireland              |              6|2,268.40                  |   1781.7|
|Hungary              |              6|1,641.80                  |   1415.6|
|Denmark              |              5|5,321.00                  |   1740.6|
|Saudi Arabia         |              5|4,037.60                  |   1432.9|
|Pakistan             |              5|1,188.10                  |   1040.9|
|Colombia             |              3|4,403.00                  |    237.3|
|Argentina            |              3|4,039.50                  |    294.2|
|Chile                |              3|3,713.80                  |    564.0|
|Singapore            |              3|2,847.10                  |   1999.8|
|Serbia               |              3|1,808.10                  |    568.3|
|Romania              |              3|732.90                    |    881.5|
|Tunisia              |              3|294.80                    |   1194.7|
|Slovenia             |              2|1,635.00                  |    856.7|
|Nigeria              |              2|741.90                    |   1118.3|
|Slovakia             |              2|498.10                    |    653.1|
|Jordan               |              2|497.10                    |   1309.1|
|United Arab Emirates |              2|415.10                    |   1776.7|
|Croatia              |              1|1,505.50                  |    385.3|
|Uruguay              |              1|735.70                    |    382.9|
|Estonia              |              1|589.60                    |   1005.0|
|Lithuania            |              1|485.90                    |    793.8|
|Oman                 |              1|404.20                    |    527.5|
|Lebanon              |              1|367.50                    |   1694.5|
|Ethiopia             |              1|343.10                    |   1664.7|
|Uganda               |              1|335.70                    |   1784.9|
|Ghana                |              1|330.20                    |   1564.3|
|Kuwait               |              1|312.70                    |   1634.9|
|Iceland              |              1|242.70                    |   1425.6|
|Qatar                |              1|201.90                    |   1542.5|
|Luxembourg           |              1|200.60                    |   1751.8|
|Viet Nam             |              1|136.00                    |   1314.0|
|Cyprus               |              1|135.60                    |   1552.3|
|Morocco              |              1|129.30                    |    882.8|
|Algeria              |              1|38.00                     |    732.7|



```r
gdp <- WDI::WDI(start = 2019, end = 2019)

# plot n papers against average apc
p <- all_three_descriptives %>% 
  left_join(wdi, by = c("country_code" = "iso2c")) %>% 
  ggplot(aes(sum_fractional_works, mean_apc, colour = region, label = country)) +
  geom_point() +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "top") +
  labs(y = NULL, colour = NULL, x = "Sum of fractional publications")
p
```

![](20-APC-analysis_files/figure-html/apc-vs-papers-1.png)<!-- -->


```r
plotly::ggplotly(p)
```

preserve6b03d104e2745b4c


```r
pdata <- all_three_descriptives %>% 
  left_join(gdp, by = c("country_code" = "iso2c")) %>% 
  left_join(wdi, by = c("country_code" = "iso2c"))

labels <- pdata %>% 
  mutate(label = case_when(
    country.x %in% c("China", "India", "Uganda", "United States",
                      "Brazil", "Switzerland", "Israel", "Spain",
                     "Saudi Arabia") ~ country.x,
    TRUE ~ ""))

p <- pdata %>% 
  ggplot(aes(NY.GDP.PCAP.KD, mean_apc, colour = region, label = country.x)) +
  geom_point(aes(alpha = sum_fractional_works)) +
  ggrepel::geom_text_repel(data = labels, aes(label = label),
                           show.legend = FALSE, max.overlaps = Inf,
                           box.padding = 1, min.segment.length = 0) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::comma) +
  scale_alpha_continuous(trans = "log10", range = c(.1, 1),
                         labels = scales::comma) +
  scale_colour_discrete_qualitative(palette = "Dark 3") +
  theme(legend.position = "top", legend.box = "vertical") +
  labs(y = "Average APC", colour = NULL, x = "GDP per capita", 
       alpha = "Number of fractional publications")
p
```

![](20-APC-analysis_files/figure-html/apc-vs-gdp-1.png)<!-- -->


```r
plotly::ggplotly(p)
```

preserve003b2f5e2d294c58


## Papers per continent

```r
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


What is the average apc across topics?

```r
apc_field <- works %>% 
  distinct(id, field, concept_frac, APC_in_dollar) %>% 
  group_by(field) %>%
  mutate(sum_frac = sum(concept_frac)) %>%
  group_by(field, sum_frac) %>%
  summarise(mean_apc = sum(concept_frac * APC_in_dollar) / sum_frac) %>% 
  collect()
```

```
## `summarise()` has grouped output by 'field'. You can override using the
## `.groups` argument.
```


```r
apc_field %>% 
  drop_na() %>% 
  ggplot(aes(mean_apc, fct_reorder(field, mean_apc))) +
  geom_segment(aes(xend = 0, yend = field), colour = "grey70") +
  geom_point() + 
  geom_text(aes(label = scales::comma(round(mean_apc))), nudge_x = 30, 
            hjust = "left") +
  labs(y = NULL, x = "Average APC (in $)") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, .1)),
                     labels = scales::comma)
```

![](20-APC-analysis_files/figure-html/apc-field-overall-1.png)<!-- -->


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
p1 <- mean_apc_16_19_local %>%
  mutate(author_position = recode(author_position, first = "First authors", 
                                  last = "Last authors")) %>% 
  ggplot(aes(P_top10, mean_apc, colour = fractional_works)) + 
  geom_point(aes(), alpha = .5) +
  scale_colour_continuous_sequential(palette = "Mako", trans = "log10",
                                     labels = comma) +
  geom_smooth(colour = "grey30") +
  facet_wrap(vars(author_position)) +
  scale_x_log10() +
  scale_y_continuous(labels = dollar) +
  labs(caption = "Fractional counting; 2016-2019", y = "Mean APC",
       colour = "Number of papers per institution",
       x = expression(P["top 10%"])) +
  theme(legend.position = "top",
        legend.key.width = unit(1.5, 'cm'))
p1
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/apc-first-last-1.png)<!-- -->


```r
p2 <- mean_apcs_local %>%
  mutate(author_position = recode(author_position, first = "First authors", 
                                  last = "Last authors")) %>% 
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
  scale_color_discrete_qualitative(palette = "Dark 3") +
  theme(legend.position = "top") +
  labs(caption = "Fractional counting", y = "Mean APC",
       colour = expression(P["top 10%"]), x = NULL)
p2
```

![](20-APC-analysis_files/figure-html/apc-first-last-time-1.png)<!-- -->


```r
p1 / p2 +
  plot_layout(heights = c(4.5, 4))
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/apc-composite-1.png)<!-- -->


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

preservecda0b0e0244cf005


Using ggrepel

```r
field_selection <- mean_apc_concept_16_19_local %>%
  drop_na(field) %>% 
  filter(field %in% c("Medicine", "History", "Sociology", "Biology",
                      "Materials science", "Physics"))
  
mean_apc_concept_16_19_local %>% 
  anti_join(field_selection) %>% 
  ggplot(aes(P_top10, mean_apc, group = field)) +
  geom_smooth(alpha = .3, colour = "grey80", fill = "grey90") +
  geom_smooth(aes(colour = field), data = field_selection) +
  facet_wrap(vars(author_position), nrow = 1) +
  scale_x_log10() +
  scale_y_continuous(labels = dollar) + 
  scale_color_discrete_qualitative(palette = "Dark 3") +
  labs(caption = "2016-2019", y = "Mean APC", colour = NULL,
       x = expression(P["top 10%"])) +
  # theme(legend.position = "top") +
  guides(colour = guide_legend(override.aes = list(alpha = 0)))
```

```
## Joining, by = c("University", "publication_year", "P_top10", "sum_frac",
## "author_position", "field", "mean_apc")
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/field-selection-1.png)<!-- -->


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
  scale_color_discrete_qualitative(palette = "Dark 3") +
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
  scale_color_discrete_qualitative(palette = "Dark 3") +
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
  scale_color_discrete_qualitative(palette = "Dark 3") +
  labs(y = "Mean APC", x = expression(P["top 10%"]), colour = NULL) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(override.aes = list(alpha = 0)))
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](20-APC-analysis_files/figure-html/apc-by-country-1.png)<!-- -->



```r
spark_disconnect(sc)
```

