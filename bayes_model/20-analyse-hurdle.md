---
title: "Analyse Hurdle model"
author: "Thomas Klebel"
date: "21 Juni, 2022"
output: 
  html_document:
    keep_md: true
---




```r
hm3 <- brm(file = "final_models/hm3.rds", file_refit = "never")
```

# Posterior predictive check


```r
pp_check(hm3, ndraws = 50)
```

![](20-analyse-hurdle_files/figure-html/pp_check-1.png)<!-- -->


```r
pred_vis <- function(df, model, country_selection, alpha = 1, ndraws = 1000) {
  get_back <- function(df) mutate(df, P_top10 = exp(6.09 + P_top10))
  
  df %>%
    filter(country == country_selection) %>%
    data_grid(P_top10, country, field) %>%
    add_predicted_draws(model, ndraws = ndraws, re_formula = NULL) %>%
    get_back() %>% 
    ggplot(aes(P_top10, .prediction)) +
    stat_interval() +
    scale_color_manual(values = colorspace::lighten(clrs[4], c(.8, .67, .42))) +
    scale_y_continuous(labels = dollar) +
    geom_point(aes(y = APC_in_dollar), alpha = alpha,
               data = filter(df, country == country_selection) %>% get_back()) +
    facet_wrap(vars(field)) +
    labs(y = "Predicted vs. actual APC", x = expression(P["top 10%"]),
         color = "Credible interval") +
    # theme_minimal(base_family = "Hind") +
    theme_clean() +
    theme(legend.position = "bottom")
}
```


```r
pred_vis(base, hm3, "Austria")
```

![](20-analyse-hurdle_files/figure-html/pp_austria-1.png)<!-- -->


```r
pred_vis(base, hm3, "Brazil", alpha = .4)
```

![](20-analyse-hurdle_files/figure-html/pp_brazil-1.png)<!-- -->


```r
pred_vis(base, hm3, "China", alpha = .15)
```

![](20-analyse-hurdle_files/figure-html/pp_china-1.png)<!-- -->


```r
pred_vis(base, hm3, "United States", alpha = .2)
```

![](20-analyse-hurdle_files/figure-html/pp_us-1.png)<!-- -->


```r
pred_vis(base, hm3, "Turkey", alpha = .7)
```

![](20-analyse-hurdle_files/figure-html/pp_turkey-1.png)<!-- -->


# Model variances and covariances

```r
summary(hm3)
```

```
##  Family: hurdle_lognormal 
##   Links: mu = identity; sigma = identity; hu = logit 
## Formula: APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10 | country) + (1 + P_top10 | field) 
##          hu ~ 1 + P_top10 + (1 + P_top10 | country) + (1 + P_top10 | field)
##    Data: base (Number of observations: 23656) 
##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup draws = 4000
## 
## Group-Level Effects: 
## ~country (Number of levels: 68) 
##                              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
## sd(Intercept)                    0.29      0.03     0.23     0.36 1.00     1680
## sd(P_top10)                      0.08      0.02     0.05     0.13 1.00     1313
## sd(hu_Intercept)                 1.06      0.15     0.81     1.38 1.00     1200
## sd(hu_P_top10)                   0.10      0.07     0.00     0.27 1.01     1277
## cor(Intercept,P_top10)          -0.73      0.17    -0.96    -0.33 1.00     1412
## cor(hu_Intercept,hu_P_top10)    -0.27      0.39    -0.86     0.61 1.00     4780
##                              Tail_ESS
## sd(Intercept)                    2534
## sd(P_top10)                      1450
## sd(hu_Intercept)                 2413
## sd(hu_P_top10)                   1437
## cor(Intercept,P_top10)           2156
## cor(hu_Intercept,hu_P_top10)     3177
## 
## ~field (Number of levels: 19) 
##                              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
## sd(Intercept)                    0.31      0.07     0.20     0.47 1.00     1178
## sd(P_top10)                      0.05      0.02     0.01     0.11 1.00     1520
## sd(hu_Intercept)                 1.44      0.27     1.02     2.06 1.00     1154
## sd(hu_P_top10)                   0.09      0.06     0.00     0.22 1.00     2003
## cor(Intercept,P_top10)          -0.27      0.35    -0.81     0.47 1.00     5300
## cor(hu_Intercept,hu_P_top10)     0.24      0.40    -0.61     0.87 1.00     5137
##                              Tail_ESS
## sd(Intercept)                    1813
## sd(P_top10)                      1934
## sd(hu_Intercept)                 1924
## sd(hu_P_top10)                   2072
## cor(Intercept,P_top10)           3086
## cor(hu_Intercept,hu_P_top10)     2776
## 
## Population-Level Effects: 
##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept        7.18      0.09     7.00     7.36 1.00     1011     1669
## hu_Intercept    -0.11      0.36    -0.81     0.60 1.01      909     1490
## P_top10          0.11      0.03     0.05     0.16 1.00     2728     2484
## hu_P_top10      -0.05      0.06    -0.17     0.07 1.00     4055     3056
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma     0.61      0.01     0.59     0.62 1.00     8314     2891
## 
## Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```


# Marginal effects
This guide seemed promising: https://vincentarelbundock.github.io/marginaleffects/articles/transformation.html#back-transform-lognormal-hurdle
However, all of the marginal effects do not work, because we have a logged exposure.
The slopes are correct for the logged exposure, but not correct when we
rescale (exponentiate and un-center) the exposure. Don't know how to correctly
compute those marginal effects in this instance.

This is best shown in the below: the predicted values taper off, as P_top10
increases. But the logged P_top10 has positive AMEs that increase.


```r
eps <- .001
predictions_data <- predictions(
  hm3,
  newdata = datagrid(country = "Brazil",
                     P_top10 = seq(-3, 3, .1),
                     field = "Biology"),
  dpar = "mu",
  transform_post = exp
) 
predictions_data %>% 
  mutate(P_top10 = exp(6 + P_top10)) %>% 
  ggplot(aes(x = P_top10, y = predicted)) +
  geom_line(size = 1)
```

![](20-analyse-hurdle_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
slopes_data <- comparisons(
  hm3,
  dpar = "mu",
  variables = list(P_top10 = eps),
  newdata = datagrid(country = "Brazil",
                     P_top10 = c(-2, 0, 2),
                     field = "Biology"),
  # rescale the elements of the slope
  # (exp(40.001) - exp(40)) / exp(0.001)
  transform_pre = function(hi, lo) ((exp(hi) - exp(lo)) / exp(eps)) / eps
) %>% 
  left_join(predictions_data, by = "P_top10") %>%
  # Point-slope formula: (y - y1) = m(x - x1)
  mutate(intercept = comparison * (-P_top10) + predicted)

ggplot(predictions_data, aes(x = P_top10, y = predicted)) +
  geom_line(size = 1) + 
  geom_abline(data = slopes_data, aes(slope = comparison, intercept = intercept), 
              size = 0.5, color = "red") +
  geom_point(data = slopes_data) +
  geom_label(data = slopes_data, aes(label = paste0("Slope: ", round(comparison, 1))),
             nudge_x = -1, hjust = 1) +
  theme_minimal()
```

![](20-analyse-hurdle_files/figure-html/unnamed-chunk-4-2.png)<!-- -->


## Revert back to directly plotting effects
### Effect of ptop10 on APC

```r
hm3 %>%
  spread_draws(b_P_top10, r_field[field,term]) %>%
  filter(term == "P_top10") %>%
  mutate(total_effect = b_P_top10 + r_field) %>%
  ggplot(aes(x = total_effect, y = reorder(field, total_effect))) +
  stat_halfeye() + 
  scale_x_continuous(labels = percent) +
  labs(x = "Change in APC for 1% increase in P_top10", y = NULL) +
  theme_clean()
```

![](20-analyse-hurdle_files/figure-html/mu-effect-field-1.png)<!-- -->


```r
hm3 %>%
  spread_draws(b_P_top10, r_country[country,term]) %>%
  filter(term == "P_top10") %>%
  mutate(total_effect = b_P_top10 + r_country) %>%
  ggplot(aes(x = total_effect, y = reorder(country, total_effect))) +
  stat_halfeye() + 
  scale_x_continuous(labels = percent) +
  labs(x = "Change in APC for 1% increase in P_top10", y = NULL) +
  theme_clean()
```

![](20-analyse-hurdle_files/figure-html/mu-effect-country-1.png)<!-- -->


## Effect on hurdle
I'm not entirely certain about the interpretation of the below coefficients.

```r
hm3 %>%
  spread_draws(b_hu_P_top10, r_field__hu[field,term]) %>%
  filter(term == "P_top10") %>%
  mutate(total_effect = b_hu_P_top10 + r_field__hu) %>%
  ggplot(aes(x = total_effect, y = reorder(field, total_effect))) +
  stat_halfeye() + 
  scale_x_continuous(labels = percent) +
  labs(x = "Change in probability of no APC for 1% increase in P_top10", 
       y = NULL) +
  theme_clean()
```

![](20-analyse-hurdle_files/figure-html/hu-effect-field-1.png)<!-- -->


```r
hm3 %>%
  spread_draws(b_hu_P_top10, r_country__hu[country,term]) %>%
  filter(term == "P_top10") %>%
  mutate(total_effect = b_hu_P_top10 + r_country__hu) %>%
  ggplot(aes(x = total_effect, y = reorder(country, total_effect))) +
  stat_halfeye() + 
  scale_x_continuous(labels = percent) +
  labs(x = "Change in probability of no APC for 1% increase in P_top10", 
       y = NULL) +
  coord_cartesian(xlim = c(-.8, .8)) +
  theme_clean()
```

![](20-analyse-hurdle_files/figure-html/hu-effect-country-1.png)<!-- -->

# Marginal effects for selected countries
US, China, Brazil, Austria, France, India, South africa
show the lines here
