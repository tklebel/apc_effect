---
title: "Relationship between OA publishing, APCs and IF"
author: "Thomas Klebel"
date: "14 June, 2022"
output: 
  html_document:
    keep_md: true
---




# Distribution across topics
How many papers do we have, which also have a topic?

```r
works %>% 
  filter(author_position == "first", !is.na(display_name)) %>% 
  sdf_nrow()
```

```
## [1] 1546985
```
This is only slightly lower than the total number of papers we have.



Which topics are represented in our sample?

```r
frac_concept_papers <- works %>% 
  filter(author_position == "first") %>% 
  group_by(display_name) %>% 
  summarise(frac_papers = sum(concept_frac)) %>% 
  arrange(desc(frac_papers)) %>% 
  collect()
```



```r
frac_concept_papers %>% 
  drop_na() %>% 
  ggplot(aes(frac_papers, fct_reorder(display_name, frac_papers))) +
  geom_col(width = .7) +
  scale_x_continuous(labels = scales::comma) +
  labs(y = NULL, x = "Fractional papers")
```

![](20-APC-analysis_files/figure-html/concept-overview-1.png)<!-- -->


```r
mean_apcs <- works %>%
  # first get rid of duplicates from concepts
  distinct(id, author_position, work_frac, APC_in_dollar, University, country,
           publication_year, PP_top10) %>% 
  group_by(University, publication_year, country, PP_top10) %>%
  # compute the average APC using fractional authorships as weights
  mutate(sum_frac = sum(work_frac)) %>%
  group_by(University, publication_year, country, PP_top10, sum_frac,
           author_position) %>%
  summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac)

mean_apcs_local <- mean_apcs %>%
  collect()
```

```
## `summarise()` has grouped output by 'University', 'publication_year', 'country',
## 'PP_top10', 'sum_frac'. You can override using the `.groups` argument.
```


```r
# plot for 2019
apc_16_19 <- mean_apcs_local %>%
  filter(publication_year > 2015 & publication_year < 2020)

labels <- apc_16_19 %>%
  group_by(author_position) %>%
  summarise(cor = cor(mean_apc, PP_top10, use = "pairwise.complete")) %>%
  mutate(cor = glue::glue("r = {format(cor, nsmall = 2, digits = 2)}"))

apc_16_19 %>%
  ggplot(aes(PP_top10, mean_apc)) +
  geom_point(aes(colour = sum_frac),
             alpha = .5) +
  geom_smooth() +
  facet_wrap(vars(author_position)) +
  geom_text(data = labels, aes(label = cor, x = .25, y = 2250)) +
  scale_colour_viridis_c(trans = "sqrt") +
  labs(caption = "2016-2019")
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/apc-first-last-1.png)<!-- -->

Something went wrong here from splitting into concepts



```r
mean_apcs_by_concept <- works %>%
  group_by(University, publication_year, country, PP_top10, display_name) %>%
  # compute the average APC using fractional authorships as weights
  mutate(sum_frac = sum(work_frac)) %>%
  group_by(University, publication_year, country, PP_top10, sum_frac,
           author_position, display_name) %>%
  summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac)

apcs_by_concept_local <- mean_apcs_by_concept %>%
  collect()
```

```
## `summarise()` has grouped output by 'University', 'publication_year', 'country',
## 'PP_top10', 'sum_frac', 'author_position'. You can override using the `.groups`
## argument.
```



```r
# plot for 2016-2019
apc_concept_16_19 <- apcs_by_concept_local %>%
  filter(publication_year > 2015 & publication_year < 2020,
         !is.na(display_name))

labels <- apc_concept_16_19 %>%
  group_by(author_position, display_name) %>%
  summarise(cor = cor(mean_apc, PP_top10, use = "pairwise.complete.obs")) %>%
  mutate(cor = glue::glue("r = {format(cor, nsmall = 2, digits = 0)}"))
```

```
## `summarise()` has grouped output by 'author_position'. You can override using
## the `.groups` argument.
```

```r
apc_concept_16_19 %>%
  ggplot(aes(PP_top10, mean_apc)) +
  geom_point(aes(colour = sum_frac),
             alpha = .7) +
  geom_smooth() +
  facet_grid(cols = vars(author_position),
             rows = vars(str_wrap(display_name, 10))) +
  geom_text(data = labels, aes(label = cor, x = .3, y = 4000)) +
  scale_colour_viridis_c(trans = "log10") +
  labs(caption = "2016-2019")
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](20-APC-analysis_files/figure-html/apc-concepts-1.png)<!-- -->



```r
spark_disconnect(sc)
```

