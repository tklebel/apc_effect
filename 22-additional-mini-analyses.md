---
title: "Additional mini analyses"
author: "Thomas Klebel"
date: "11 July, 2022"
output: 
  html_document:
    keep_md: true
---




# How many of the journals in DOAJ charge APCs?

```r
doaj %>% 
  count(APC) %>% 
  mutate(prop = n/sum(n))
```

```
## # A tibble: 2 × 3
##   APC       n  prop
##   <lgl> <int> <dbl>
## 1 FALSE 12321 0.695
## 2 TRUE   5397 0.305
```

# Multimodal distribution of APC across fields

```r
apcs <- works %>% 
  select(id, APC_in_dollar)

field_apcs <- works %>% 
  distinct(id, field) %>% 
  left_join(apcs)
```

```
## Joining, by = "id"
```



```r
pdata <- multilevel_sample %>% 
  # remove duplicates from multiple institutions for single papers 
  distinct(id, field, APC_in_dollar) %>% 
  drop_na() # remove null APCs
```


```r
pdata %>% 
  ggplot(aes(APC_in_dollar, fct_reorder(field, APC_in_dollar), fill = stat(x))) +
  geom_density_ridges_gradient(rel_min_height = .01, scale = 1.3, alpha = .7,
                               show.legend = FALSE) + 
  scale_fill_continuous_sequential(palette = "Mako") +
  scale_x_continuous(labels = scales::comma) +
  theme_clean +
  coord_cartesian(clip = "off") +
  labs(y = NULL, x = "APC in dollar") 
```

```
## Picking joint bandwidth of 181
```

![](22-additional-mini-analyses_files/figure-html/apc-by-field-1.png)<!-- -->

# What are the levels of P top 10% in India?

```r
institutions <- works %>% 
  filter(publication_year == 2019) %>% 
  distinct(country, institution_id, P_top10) %>% 
  collect()
  
institutions %>% 
  summarise(q20 = quantile(P_top10, .2),
            q80 = quantile(P_top10, .8))
```

```
## # A tibble: 1 × 2
##     q20   q80
##   <dbl> <dbl>
## 1  86.3  541.
```



```r
institutions %>% 
  filter(country == "India") %>% 
  summarise(min = min(P_top10),
            max = max(P_top10))
```

```
## # A tibble: 1 × 2
##     min   max
##   <dbl> <dbl>
## 1  43.0  372.
```




```r
spark_disconnect(sc)
```

