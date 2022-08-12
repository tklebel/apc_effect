---
title: "Relationship between OA publishing, APCs and IF"
author: "Thomas Klebel"
date: "12 August, 2022"
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
|China                |            216|
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
|Turkey               |             31|
|Brazil               |             31|
|Canada               |             30|
|France               |             28|
|Taiwan               |             21|
|Netherlands          |             13|
|Austria              |             12|
|Sweden               |             12|
|Russia               |             10|
|South Africa         |              9|
|Belgium              |              8|
|Israel               |              8|
|Egypt                |              8|
|Switzerland          |              8|
|Greece               |              8|
|Czech Republic       |              7|
|New Zealand          |              7|
|Finland              |              7|
|Malaysia             |              6|
|Ireland              |              6|
|Norway               |              6|
|Mexico               |              6|
|Portugal             |              6|
|Thailand             |              6|
|Hungary              |              6|
|Saudi Arabia         |              5|
|Denmark              |              5|
|Pakistan             |              5|
|Argentina            |              3|
|Chile                |              3|
|Colombia             |              3|
|Singapore            |              3|
|Serbia               |              3|
|Romania              |              3|
|Tunisia              |              3|
|Jordan               |              2|
|United Arab Emirates |              2|
|Slovakia             |              2|
|Nigeria              |              2|
|Slovenia             |              2|
|Algeria              |              1|
|Ghana                |              1|
|Uruguay              |              1|
|Estonia              |              1|
|Lebanon              |              1|
|Morocco              |              1|
|Luxembourg           |              1|
|Cyprus               |              1|
|Iceland              |              1|
|Qatar                |              1|
|Kuwait               |              1|
|Lithuania            |              1|
|Uganda               |              1|
|Viet Nam             |              1|
|Croatia              |              1|
|Oman                 |              1|
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
|United States        |             125334.6|
|China                |              95237.8|
|Brazil               |              88014.2|
|United Kingdom       |              35912.8|
|Spain                |              32073.9|
|Germany              |              28500.1|
|Canada               |              25745.8|
|Australia            |              21045.8|
|South Korea          |              19847.4|
|Japan                |              19303.5|
|Italy                |              17540.0|
|Poland               |              13594.0|
|Netherlands          |              10488.8|
|South Africa         |              10264.7|
|Sweden               |              10052.5|
|Taiwan               |               8989.2|
|India                |               8434.9|
|Iran                 |               8281.1|
|France               |               7500.7|
|Switzerland          |               7427.1|
|Mexico               |               6230.1|
|Turkey               |               6126.6|
|Belgium              |               6023.9|
|Portugal             |               5591.3|
|Denmark              |               5011.5|
|Malaysia             |               4901.4|
|Israel               |               4540.2|
|Austria              |               4339.3|
|Norway               |               4298.4|
|Russia               |               4104.9|
|Colombia             |               4057.8|
|Saudi Arabia         |               3992.1|
|Argentina            |               3765.4|
|Chile                |               3329.1|
|Egypt                |               3296.5|
|Finland              |               3241.3|
|New Zealand          |               2795.6|
|Singapore            |               2753.1|
|Thailand             |               2580.2|
|Czech Republic       |               2481.3|
|Greece               |               2421.1|
|Ireland              |               2146.9|
|Serbia               |               1756.1|
|Hungary              |               1574.1|
|Slovenia             |               1563.7|
|Croatia              |               1327.4|
|Pakistan             |               1156.7|
|Nigeria              |                715.8|
|Romania              |                702.2|
|Uruguay              |                668.6|
|Estonia              |                513.3|
|Jordan               |                481.2|
|Slovakia             |                481.2|
|Lithuania            |                450.2|
|United Arab Emirates |                404.1|
|Oman                 |                398.5|
|Lebanon              |                359.1|
|Ethiopia             |                331.9|
|Ghana                |                327.3|
|Uganda               |                324.0|
|Kuwait               |                303.2|
|Tunisia              |                291.1|
|Iceland              |                229.0|
|Qatar                |                200.4|
|Luxembourg           |                192.1|
|Viet Nam             |                136.0|
|Cyprus               |                129.0|
|Morocco              |                127.4|
|Algeria              |                 37.0|


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
|China                |            216|95,237.8                  |   1871.5|
|United States        |            199|125,334.6                 |   1930.2|
|United Kingdom       |             60|35,912.8                  |   1817.8|
|Japan                |             55|19,303.5                  |   1816.7|
|Germany              |             54|28,500.1                  |   1816.5|
|South Korea          |             46|19,847.4                  |   1708.8|
|Spain                |             42|32,073.9                  |    697.5|
|Italy                |             41|17,540.0                  |   1703.4|
|India                |             38|8,434.9                   |    957.3|
|Iran                 |             36|8,281.1                   |    766.6|
|Australia            |             32|21,045.8                  |   1790.0|
|Brazil               |             31|88,014.2                  |    264.5|
|Poland               |             31|13,594.0                  |    854.1|
|Turkey               |             31|6,126.6                   |    823.4|
|Canada               |             30|25,745.8                  |   1700.1|
|France               |             28|7,500.7                   |   1606.3|
|Taiwan               |             21|8,989.2                   |   1841.1|
|Netherlands          |             13|10,488.8                  |   1826.8|
|Sweden               |             12|10,052.5                  |   1842.3|
|Austria              |             12|4,339.3                   |   1708.3|
|Russia               |             10|4,104.9                   |    477.6|
|South Africa         |              9|10,264.7                  |    942.4|
|Switzerland          |              8|7,427.1                   |   2016.4|
|Belgium              |              8|6,023.9                   |   1723.8|
|Israel               |              8|4,540.2                   |   2019.7|
|Egypt                |              8|3,296.5                   |    923.8|
|Greece               |              8|2,421.1                   |   1475.0|
|Finland              |              7|3,241.3                   |   1591.7|
|New Zealand          |              7|2,795.6                   |   1681.2|
|Czech Republic       |              7|2,481.3                   |   1062.1|
|Mexico               |              6|6,230.1                   |    700.6|
|Portugal             |              6|5,591.3                   |    807.6|
|Malaysia             |              6|4,901.4                   |   1134.5|
|Norway               |              6|4,298.4                   |   1604.8|
|Thailand             |              6|2,580.2                   |   1525.6|
|Ireland              |              6|2,146.9                   |   1786.5|
|Hungary              |              6|1,574.1                   |   1428.8|
|Denmark              |              5|5,011.5                   |   1762.0|
|Saudi Arabia         |              5|3,992.1                   |   1434.9|
|Pakistan             |              5|1,156.7                   |   1045.3|
|Colombia             |              3|4,057.8                   |    249.0|
|Argentina            |              3|3,765.4                   |    300.8|
|Chile                |              3|3,329.1                   |    597.7|
|Singapore            |              3|2,753.1                   |   2005.6|
|Serbia               |              3|1,756.1                   |    568.5|
|Romania              |              3|702.2                     |    859.7|
|Tunisia              |              3|291.1                     |   1198.8|
|Slovenia             |              2|1,563.7                   |    865.4|
|Nigeria              |              2|715.8                     |   1102.8|
|Jordan               |              2|481.2                     |   1325.2|
|Slovakia             |              2|481.2                     |    652.9|
|United Arab Emirates |              2|404.1                     |   1770.5|
|Croatia              |              1|1,327.4                   |    423.8|
|Uruguay              |              1|668.6                     |    409.0|
|Estonia              |              1|513.3                     |   1098.9|
|Lithuania            |              1|450.2                     |    818.3|
|Oman                 |              1|398.5                     |    508.1|
|Lebanon              |              1|359.1                     |   1687.3|
|Ethiopia             |              1|331.9                     |   1649.4|
|Ghana                |              1|327.3                     |   1563.2|
|Uganda               |              1|324.0                     |   1785.0|
|Kuwait               |              1|303.2                     |   1642.3|
|Iceland              |              1|229.0                     |   1453.3|
|Qatar                |              1|200.4                     |   1552.0|
|Luxembourg           |              1|192.1                     |   1791.3|
|Viet Nam             |              1|136.0                     |   1314.0|
|Cyprus               |              1|129.0                     |   1583.2|
|Morocco              |              1|127.4                     |    893.9|
|Algeria              |              1|37.0                      |    708.9|



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

preservea48b52b4e3e1ea9a


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

preserve40cdfca2ff85c3cd


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
## [1] 1572417
```
This is our total sample size.


```r
works %>% 
  distinct(id) %>% 
  sdf_nrow()
```

```
## [1] 1572417
```


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
plot_data %>% 
  mutate(frac_papers = scales::comma(frac_papers),
         prop = scales::percent(prop, accuracy = .1)) %>% 
  knitr::kable()
```



|field                 |frac_papers |prop  |label           |
|:---------------------|:-----------|:-----|:---------------|
|Medicine              |330,091     |36.2% |330,091 (36.2%) |
|Biology               |163,396     |17.9% |163,396 (17.9%) |
|Chemistry             |82,019      |9.0%  |82,019 (9.0%)   |
|Computer science      |77,209      |8.5%  |77,209 (8.5%)   |
|Materials science     |65,406      |7.2%  |65,406 (7.2%)   |
|Psychology            |49,027      |5.4%  |49,027 (5.4%)   |
|Physics               |28,484      |3.1%  |28,484 (3.1%)   |
|Environmental science |23,262      |2.6%  |23,262 (2.6%)   |
|Political science     |16,094      |1.8%  |16,094 (1.8%)   |
|Mathematics           |14,337      |1.6%  |14,337 (1.6%)   |
|Geography             |13,944      |1.5%  |13,944 (1.5%)   |
|Sociology             |12,119      |1.3%  |12,119 (1.3%)   |
|Art                   |10,435      |1.1%  |10,435 (1.1%)   |
|Business              |9,167       |1.0%  |9,167 (1.0%)    |
|Geology               |7,362       |0.8%  |7,362 (0.8%)    |
|Philosophy            |4,787       |0.5%  |4,787 (0.5%)    |
|Economics             |2,518       |0.3%  |2,518 (0.3%)    |
|History               |1,330       |0.1%  |1,330 (0.1%)    |
|Engineering           |1,023       |0.1%  |1,023 (0.1%)    |



```r
p_apc_field <- apc_field %>% 
  drop_na() %>% 
  ggplot(aes(mean_apc, fct_reorder(field, mean_apc))) +
  geom_segment(aes(xend = 0, yend = field), colour = "grey70") +
  geom_point() + 
  geom_text(aes(label = scales::comma(round(mean_apc))), nudge_x = 30, 
            hjust = "left") +
  labs(y = NULL, x = "Mean APC (in $)") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, .1)),
                     labels = scales::comma)
p_apc_field
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
  plot_layout(heights = c(4.5, 4)) +
  plot_annotation(tag_levels = "A")
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
mean_apc_concept_16_19_local <- mean_apc_concept_16_19_local %>% 
  mutate(author_position = recode(author_position, first = "First authors", 
                                  last = "Last authors"))
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

preservea486cc67f0e1a714


Using ggrepel

```r
field_selection <- mean_apc_concept_16_19_local %>%
  drop_na(field) %>% 
  filter(field %in% c("Medicine", "History", "Sociology", "Biology",
                      "Materials science", "Physics"))
  
final_ptop_apc_field <- mean_apc_concept_16_19_local %>% 
  anti_join(field_selection) %>% 
  ggplot(aes(P_top10, mean_apc, group = field)) +
  geom_smooth(alpha = .3, colour = "grey80", fill = "grey90") +
  geom_smooth(aes(colour = field), data = field_selection) +
  facet_wrap(vars(author_position), nrow = 1) +
  scale_x_log10() +
  scale_y_continuous(labels = dollar) + 
  scale_color_discrete_qualitative(palette = "Dark 3") +
  labs(caption = "Time period: 2016-2019", y = "Mean APC", colour = NULL,
       x = expression(P["top 10%"])) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(override.aes = list(alpha = 0),
                               nrow = 1))
```

```
## Joining, by = c("University", "publication_year", "P_top10", "sum_frac",
## "author_position", "field", "mean_apc")
```

```r
final_ptop_apc_field
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/field-selection-1.png)<!-- -->



```r
(final_ptop_apc_field + 
   # https://stackoverflow.com/a/65946462/3149349
   theme(axis.title.y = element_text(margin = margin(r = -120, unit = "pt")))
   ) / p_apc_field +
  plot_layout(heights = c(3.5, 5)) +
  plot_annotation(tag_levels = "A")
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/composite-field-1.png)<!-- -->


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

