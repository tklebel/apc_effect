---
title: "Relationship between OA publishing, APCs and IF"
author: "Thomas Klebel"
date: "20 June, 2022"
output: 
  html_document:
    keep_md: true
---




# Distribution across topics
How many papers do we have, which also have a topic?

```r
works %>% 
  filter(!is.na(field)) %>% 
  distinct(id) %>% 
  sdf_nrow()
```

```
## [1] 1572245
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
frac_concept_papers %>% 
  drop_na() %>% 
  ggplot(aes(frac_papers, fct_reorder(field, frac_papers))) +
  geom_col(width = .7) +
  scale_x_continuous(labels = scales::comma) +
  labs(y = NULL, x = "Fractional papers")
```

![](20-APC-analysis_files/figure-html/concept-overview-1.png)<!-- -->

# Association between P_top10 and APC

```r
mean_apcs <- works %>%
  # first get rid of duplicates from concepts
  distinct(id, author_position, work_frac, APC_in_dollar, University, country,
           publication_year, P_top10) %>% 
  group_by(University, publication_year, country, P_top10) %>%
  # compute the average APC using fractional authorships as weights
  mutate(sum_frac = sum(work_frac)) %>%
  group_by(University, publication_year, country, P_top10, sum_frac,
           author_position) %>%
  summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac)

mean_apcs_local <- mean_apcs %>%
  collect()
```

```
## `summarise()` has grouped output by 'University', 'publication_year', 'country',
## 'P_top10', 'sum_frac'. You can override using the `.groups` argument.
```


```r
# plot for 2019
apc_16_19 <- mean_apcs_local %>%
  filter(publication_year > 2015 & publication_year < 2020)

# taking out the correlation, because they are incorrect given that the figure
# shows a non-linear relationship (x-axis logged), but the correlation is linear
# (and quite unsuitable to the skewed P_top10)
apc_16_19 %>%
  mutate(author_position = recode(author_position, first = "First authors", 
                                  last = "Last authors")) %>% 
  ggplot(aes(P_top10, mean_apc)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(vars(author_position)) +
  scale_x_log10() +
  scale_y_continuous(labels = dollar) +
  labs(caption = "2016-2019", y = "Mean APC",
       x = expression(P["top 10%"]))
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
mean_apcs_by_concept <- works %>%
  distinct(id, University, publication_year, P_top10, field, work_frac, 
           APC_in_dollar, author_position) %>% 
  group_by(University, publication_year, P_top10, field) %>%
  # compute the average APC using fractional authorships as weights
  mutate(sum_frac = sum(work_frac)) %>%
  group_by(University, publication_year, P_top10, sum_frac,
           author_position, field) %>%
  summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac)

apcs_by_concept_local <- mean_apcs_by_concept %>%
  collect()
```

```
## `summarise()` has grouped output by 'University', 'publication_year', 'P_top10',
## 'sum_frac', 'author_position'. You can override using the `.groups` argument.
```



```r
# plot for 2016-2019
apc_concept_16_19 <- apcs_by_concept_local %>%
  filter(publication_year > 2015 & publication_year < 2020,
         !is.na(field))

p <- apc_concept_16_19 %>%
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

preserve8eeec7408abf0b91



## Over time - first-authors

```r
apcs_by_concept_local %>%
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
apcs_by_concept_local %>%
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


```r
spark_disconnect(sc)
```

