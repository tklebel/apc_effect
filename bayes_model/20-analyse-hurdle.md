---
title: "Analyse Hurdle model"
author: "Thomas Klebel"
date: "14 September, 2022"
output: 
  html_document:
    keep_md: true
---




```r
# function for fixing legend
make_nice_effect_quantiles <- function(df, invar, outvar) {
  mutate(
    df,
    {{ outvar }} := recode({{ invar }}, 
                           effect_20 = "Effect at 20% quantile",
                            effect_50 = "Effect at 50% quantile",
                            effect_80 = "Effect at 80% quantile")
    )
}

effect_scale <- list(
  scale_color_manual(values = c(
    "Effect at 20% quantile" = colorspace::lighten("#007FA8", .7),
    "Effect at 50% quantile" = colorspace::lighten("#007FA8", .4),
    "Effect at 80% quantile" = "#007FA8"
  ))
)
```



```r
hm <- read_rds("final_models/17-brm-large-sample.rds.bz2")

hm_simple <- brm(file = "final_models/hm_final_rerun.rds", file_refit = "never")
```



# Posterior predictive check

```r
pp_check(hm, ndraws = 10, cores = mc_cores) +
  coord_cartesian(xlim = c(0, 8000))
```
![](20-analyse-hurdle_files/figure-html/pp_check-1.png)


```r
pred_vis <- function(df, model, country_selection,
                     var_for_offset = base$P_top10, alpha = 1, ndraws = 1000) {
  scale_offset <- attributes(var_for_offset)[["scaled:center"]]
  get_back <- function(df) mutate(df, P_top10 = exp(scale_offset + P_top10))
  
  df %>%
    filter(country == country_selection) %>%
    modelr::data_grid(P_top10, country, field) %>%
    add_predicted_draws(model, ndraws = ndraws, re_formula = NULL, 
                        cores = mc_cores) %>%
    get_back() %>% 
    ggplot(aes(P_top10, .prediction)) +
    stat_interval() +
    scale_color_manual(values = colorspace::lighten(clrs[4], c(.8, .67, .42))) +
    scale_y_continuous(labels = dollar) +
    geom_jitter(aes(y = APC_in_dollar), alpha = alpha, 
                position = position_jitter(width = 5, height = 50),
                data = filter(df, country == country_selection) %>% get_back()) +
    facet_wrap(vars(field)) +
    labs(y = "Predicted vs. actual APC", x = expression(P["top 10%"]),
         color = "Credible interval") +
    # theme_minimal(base_family = "Hind") +
    theme_clean() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}
```


```r
pred_vis(base, hm, "Austria", alpha = .5)
```

![](20-analyse-hurdle_files/figure-html/pp_austria-1.png)<!-- -->


```r
pred_vis(base, hm, "Brazil", alpha = .2)
```

![](20-analyse-hurdle_files/figure-html/pp_brazil-1.png)<!-- -->
This updated model fares much better for Brazil. The predictions are still not
ideal (underestimating higher APCs of ~2000), but overall much better than the
previous model. 


```r
pred_vis(base, hm, "China", alpha = .15)
```

![](20-analyse-hurdle_files/figure-html/pp_china-1.png)<!-- -->


```r
pred_vis(base, hm, "United States", alpha = .2)
```

![](20-analyse-hurdle_files/figure-html/pp_us-1.png)<!-- -->


```r
pred_vis(base, hm, "Turkey", alpha = .7)
```

![](20-analyse-hurdle_files/figure-html/pp_turkey-1.png)<!-- -->


## PP check for simple model 

```r
pp_check(hm_simple, ndraws = 10, cores = mc_cores) +
  coord_cartesian(xlim = c(0, 8000))
```

```
## Warning: The following arguments were unrecognized and ignored: cores
```

```
## Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

![](20-analyse-hurdle_files/figure-html/pp_check_simple_hm-1.png)<!-- -->


```r
pred_vis(base, hm_simple, "Brazil", alpha = .2)
```

![](20-analyse-hurdle_files/figure-html/pp_brazil_simple_hm-1.png)<!-- -->



# Model variances and covariances

```r
summary(hm) 
```

```
##  Family: mixture(hurdle_lognormal, hurdle_lognormal) 
##   Links: mu1 = identity; sigma1 = identity; hu1 = logit; mu2 = identity; sigma2 = identity; hu2 = logit; theta1 = identity; theta2 = identity 
## Formula: APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10 | field) + (1 + P_top10 | country) 
##          hu1 ~ 1 + P_top10 + (1 + P_top10 | field) + (1 + P_top10 | country)
##          hu2 ~ 1 + P_top10 + (1 + P_top10 | field) + (1 + P_top10 | country)
##          theta1 ~ 1 + (1 | field)
##    Data: base (Number of observations: 188614) 
##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup draws = 4000
## 
## Group-Level Effects: 
## ~country (Number of levels: 69) 
##                                Estimate Est.Error l-95% CI u-95% CI Rhat
## sd(mu1_Intercept)                  0.45      0.05     0.36     0.56 1.00
## sd(mu1_P_top10)                    0.03      0.02     0.00     0.08 1.00
## sd(mu2_Intercept)                  0.03      0.00     0.02     0.04 1.00
## sd(mu2_P_top10)                    0.00      0.00     0.00     0.01 1.00
## sd(hu1_Intercept)                  1.39      0.22     1.01     1.88 1.00
## sd(hu1_P_top10)                    0.11      0.08     0.01     0.32 1.00
## sd(hu2_Intercept)                  1.41      0.18     1.10     1.78 1.00
## sd(hu2_P_top10)                    0.17      0.11     0.01     0.40 1.01
## cor(mu1_Intercept,mu1_P_top10)    -0.12      0.30    -0.65     0.51 1.00
## cor(mu2_Intercept,mu2_P_top10)     0.05      0.32    -0.57     0.65 1.00
## cor(hu1_Intercept,hu1_P_top10)    -0.18      0.35    -0.77     0.52 1.00
## cor(hu2_Intercept,hu2_P_top10)    -0.03      0.27    -0.56     0.48 1.00
##                                Bulk_ESS Tail_ESS
## sd(mu1_Intercept)                  1365     1847
## sd(mu1_P_top10)                    1381     2230
## sd(mu2_Intercept)                  1369     2293
## sd(mu2_P_top10)                    2059     2300
## sd(hu1_Intercept)                  1478     2314
## sd(hu1_P_top10)                    1465     2330
## sd(hu2_Intercept)                  1165     1960
## sd(hu2_P_top10)                     596     1609
## cor(mu1_Intercept,mu1_P_top10)     5914     3058
## cor(mu2_Intercept,mu2_P_top10)     5158     3106
## cor(hu1_Intercept,hu1_P_top10)     4486     3248
## cor(hu2_Intercept,hu2_P_top10)     4139     2525
## 
## ~field (Number of levels: 19) 
##                                Estimate Est.Error l-95% CI u-95% CI Rhat
## sd(mu1_Intercept)                  0.26      0.06     0.17     0.40 1.00
## sd(mu1_P_top10)                    0.07      0.03     0.02     0.13 1.00
## sd(mu2_Intercept)                  0.28      0.06     0.18     0.43 1.00
## sd(mu2_P_top10)                    0.01      0.01     0.00     0.03 1.00
## sd(hu1_Intercept)                  1.61      0.27     1.17     2.23 1.00
## sd(hu1_P_top10)                    0.11      0.06     0.01     0.25 1.00
## sd(hu2_Intercept)                  1.99      0.37     1.33     2.76 1.00
## sd(hu2_P_top10)                    0.43      0.11     0.27     0.68 1.00
## sd(theta1_Intercept)               0.94      0.28     0.54     1.61 1.00
## cor(mu1_Intercept,mu1_P_top10)     0.05      0.27    -0.46     0.57 1.00
## cor(mu2_Intercept,mu2_P_top10)    -0.10      0.31    -0.67     0.51 1.00
## cor(hu1_Intercept,hu1_P_top10)    -0.26      0.30    -0.76     0.39 1.00
## cor(hu2_Intercept,hu2_P_top10)     0.23      0.24    -0.27     0.64 1.00
##                                Bulk_ESS Tail_ESS
## sd(mu1_Intercept)                  1511     2541
## sd(mu1_P_top10)                    1932     2225
## sd(mu2_Intercept)                   914     1563
## sd(mu2_P_top10)                    1123     1742
## sd(hu1_Intercept)                  1229     2190
## sd(hu1_P_top10)                    1837     1538
## sd(hu2_Intercept)                  1034     1240
## sd(hu2_P_top10)                    1812     2479
## sd(theta1_Intercept)                854     1385
## cor(mu1_Intercept,mu1_P_top10)     4179     3157
## cor(mu2_Intercept,mu2_P_top10)     4872     3276
## cor(hu1_Intercept,hu1_P_top10)     5363     2940
## cor(hu2_Intercept,hu2_P_top10)     3031     3177
## 
## Population-Level Effects: 
##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## mu1_Intercept        6.79      0.09     6.60     6.97 1.01      985     1857
## hu1_Intercept       -0.58      0.39    -1.42     0.14 1.01      602     1008
## mu2_Intercept        7.51      0.07     7.37     7.63 1.01      845     1360
## hu2_Intercept        0.30      0.43    -0.47     1.20 1.00      869     1277
## theta1_Intercept    -0.28      0.29    -0.79     0.36 1.00      731      830
## mu1_P_top10          0.18      0.03     0.12     0.24 1.00     3116     2850
## hu1_P_top10         -0.02      0.09    -0.18     0.16 1.00     2864     2389
## mu2_P_top10          0.00      0.01    -0.01     0.02 1.00     2782     2579
## hu2_P_top10         -0.18      0.15    -0.48     0.11 1.00     1854     2162
## 
## Family Specific Parameters: 
##        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma1     0.85      0.01     0.83     0.87 1.00     6525     3222
## sigma2     0.20      0.00     0.20     0.20 1.00     6132     3272
## 
## Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```


# "Marginal effects"
Here we compute marginal effects manually, by making predictions for a given
x (P_top10) and then the same x * 1.01, i.e., increasing x (on the original 
scale) by 1%, and then comparing the predictions.

## Fields

```r
scale_offset <- attributes(base$P_top10)[["scaled:center"]]
x1_identity <- 1500
x2_identity <- x1_identity * 1.1
x1_log <- log(x1_identity) - scale_offset
x2_log <- log(x2_identity) - scale_offset


contrast <- predictions(
  hm,
  newdata = datagrid(P_top10 = c(x1_log, x2_log),
                     country = "Brazil", field = unique(base$field))) %>% 
  posteriordraws()
  
contrast_recomputed <- contrast %>% 
  mutate(x = factor(P_top10, labels = c("base", "step"))) %>% 
  pivot_wider(-c(predicted, conf.low, conf.high, P_top10, rowid), 
              names_from = x, values_from = draw) %>% 
  mutate(contrast = step / base - 1)
```


```r
contrast_recomputed %>% 
  ggplot(aes(contrast, fct_reorder(field, contrast))) +
  stat_halfeye() +
  scale_x_continuous(labels = percent)
```

![](20-analyse-hurdle_files/figure-html/fields-brazil-1.png)<!-- -->

This is very sensitive to the respective country. Maybe we can recompute an
average marginal effect after all?


```r
average_draws <- function(model, orig_var, q = .5, 
                                var_for_offset = base$P_top10) {
  scale_offset <- attributes(var_for_offset)[["scaled:center"]]
  
  x1_identity <- quantile(orig_var, q)
  x2_identity <- x1_identity * 1.01
  x1_log <- log(x1_identity) - scale_offset
  x2_log <- log(x2_identity) - scale_offset
  
  
  contrast_all <- predictions(
    model,
    newdata = datagrid(P_top10 = c(x1_log, x2_log),
                       country = unique(base$country), 
                       field = unique(base$field))) %>% 
    posteriordraws()
    
  contrast_all %>% 
    mutate(x = factor(P_top10, labels = c("base", "step"))) %>% 
    pivot_wider(-c(predicted, conf.low, conf.high, P_top10, rowid), 
                names_from = x, values_from = draw) %>% 
    mutate(contrast = step / base - 1)
}

summarise_by <- function(contrast_df, var = field) {
  contrast_df %>% 
    group_by({{ var }}, drawid) %>% 
    summarise(effect = mean(contrast))
}
```


```r
plot_effect <- function(contrast_df, location = "the median") {
  contrast_df %>% 
    ggplot(aes(effect, fct_reorder(field, effect))) +
    stat_halfeye(.width = c(.5, .9), point_interval = "median_hdi") +
    scale_x_continuous(labels = percent) +
    labs(
      y = NULL, 
      x = glue::glue("% change of APC for 1% change of P_top10% at {location}"),
      caption = "Averaged predictions over all countries.") +
    theme_clean() +
    coord_cartesian(xlim = c(-0.005, 0.005))
}
```


```r
contrast_20 <- average_draws(hm, df$P_top10, q = .2)
contrast_50 <- average_draws(hm, df$P_top10, q = .5)
contrast_80 <- average_draws(hm, df$P_top10, q = .8)
```



```r
contrast_20_field <- summarise_by(contrast_20, field)
```

```
## `summarise()` has grouped output by 'field'. You can override using the
## `.groups` argument.
```


```r
contrast_20_field %>% 
  plot_effect("the 20% quantile")
```

![](20-analyse-hurdle_files/figure-html/fields-20-1.png)<!-- -->

This seems reasonable, but would need to further validate.

At 50%

```r
contrast_50_field <- summarise_by(contrast_50, field)
```

```
## `summarise()` has grouped output by 'field'. You can override using the
## `.groups` argument.
```


```r
contrast_50_field %>% 
  plot_effect()
```

![](20-analyse-hurdle_files/figure-html/fields-50-1.png)<!-- -->



```r
contrast_80_field <- summarise_by(contrast_80, field)
```

```
## `summarise()` has grouped output by 'field'. You can override using the
## `.groups` argument.
```


```r
contrast_80_field %>% 
  plot_effect()
```

![](20-analyse-hurdle_files/figure-html/fields-80-1.png)<!-- -->


Compare all three

```r
all_joined <- bind_rows(
  rename(contrast_50_field, effect_50 = effect),
  rename(contrast_80_field, effect_80 = effect),
  rename(contrast_20_field, effect_20 = effect)
)
```


```r
p <- all_joined %>% 
  pivot_longer(contains("effect"), values_to = "effect") %>% 
  drop_na() %>% 
  make_nice_effect_quantiles(name, name) %>% 
  ggplot(aes(effect, fct_reorder(field, effect), colour = name)) +
  geom_vline(xintercept = 0, colour = "grey55", linetype = 2) +
  stat_pointinterval(position = position_dodge(width = .5),
                     .width = c(.5, .9)) +
  scale_x_continuous(labels = percent) +
  labs(
    y = NULL, 
    x = expression(paste("% change of APC for 1% change of ", P["top 10%"], 
                         " at given quantiles")),
    caption = "Predictions averaged over all countries.") +
  theme_clean() +
  coord_cartesian(xlim = c(-0.005, 0.005)) +
  guides(colour = guide_legend(reverse = FALSE))
```


```r
p + 
  effect_scale +
  theme(legend.position = "top", legend.justification = c(1, 0)) + 
  labs(colour = NULL) 
```

![](20-analyse-hurdle_files/figure-html/fields-combined-1-1.png)<!-- -->


Questions that arise:
- why in some fields stronger/weaker effect for larger/smaller P_top10? Is this
also associated with hurdle component?
- Especially: why physics and mathematics negative? because of hurdle? -> Yes


Need to put into context of overall averages: give average per field (from model
or full set)



### Intercept at median

```r
contrast_50 %>% 
  group_by(field, drawid) %>% 
  summarise(intercept = mean(base)) %>% 
  ggplot(aes(intercept, fct_reorder(field, intercept))) +
  stat_halfeye(.width = c(.5, .9), fill = colorspace::lighten("#007FA8"),
               point_interval = "median_hdi") +
  scale_x_continuous(labels = scales::dollar) +
  theme_clean() +
  labs(y = NULL, x = expression(paste("Estimated APC at median of ",
                                      P["top 10%"])))
```

```
## `summarise()` has grouped output by 'field'. You can override using the
## `.groups` argument.
```

![](20-analyse-hurdle_files/figure-html/field-intercept-1.png)<!-- -->



## Countries

```r
contrast_20_country <- summarise_by(contrast_20, country)
```

```
## `summarise()` has grouped output by 'country'. You can override using the
## `.groups` argument.
```

```r
contrast_50_country <- summarise_by(contrast_50, country)
```

```
## `summarise()` has grouped output by 'country'. You can override using the
## `.groups` argument.
```

```r
contrast_80_country <- summarise_by(contrast_80, country)
```

```
## `summarise()` has grouped output by 'country'. You can override using the
## `.groups` argument.
```

```r
all_countries <- bind_rows(
  rename(contrast_20_country, effect_20 = effect),
  rename(contrast_50_country, effect_50 = effect),
  rename(contrast_80_country, effect_80 = effect),
) %>% 
  pivot_longer(contains("effect"), values_to = "effect") %>% 
  drop_na()
```



```r
p_country <- all_countries %>% 
  ggplot(aes(effect, fct_reorder(country, effect), colour = name)) +
  geom_vline(xintercept = 0, colour = "grey55", linetype = 2) +
  stat_pointinterval(position = position_dodge(width = .5),
                     .width = c(.5, .9), point_interval = "median_hdi") +
  scale_x_continuous(labels = percent) +
  labs(
    y = NULL, 
    x = glue::glue("% Change of APC for 1% change of P_top10% at given quantiles"),
    caption = "Averaged predictions over all countries.") +
  theme_clean() +
  coord_cartesian(xlim = c(-0.001, 0.005)) +
  guides(colour = guide_legend(reverse = TRUE))
```


```r
p_country + scale_color_manual(values = c(
    effect_20 = colorspace::lighten("#007FA8", .7),
    effect_50 = colorspace::lighten("#007FA8", .4),
    effect_80 = "#007FA8"
  ))
```

![](20-analyse-hurdle_files/figure-html/countries-all-1.png)<!-- -->

### Group by continent

```r
country_identifier <- base %>% 
  distinct(country, region, country_code) %>% 
  drop_na()

all_countries_w_region <- all_countries %>% 
  left_join(country_identifier)
```

```
## Joining, by = "country"
```


```r
plot_countries_by_region <- function(df) {
  df %>% 
    make_nice_effect_quantiles(name, color_label) %>% 
    ggplot(aes(effect, fct_reorder(country, effect), colour = color_label)) +
    geom_vline(xintercept = 0, colour = "grey55", linetype = 2) +
    stat_pointinterval(position = position_dodge(width = .5),
                       .width = c(.5, .9), point_interval = "median_hdi") +
    scale_x_continuous(labels = percent) +
    labs(
      y = NULL, 
      x = expression(paste("% Change of APC for 1% change of ", P["top 10%"], 
                         " at given quantiles")),
      caption = "Predictions averaged over all fields.",
      colour = NULL) +
    theme_clean() +
    facet_grid(rows = vars(str_wrap(region, 9)), space = "free_y", scales = "free_y") +
    coord_cartesian(xlim = c(-0.001, 0.005)) +
    guides(colour = guide_legend(reverse = FALSE, override.aes = list(size = 2))) +
    theme(legend.position = "top") +
    effect_scale
}

p_europe_subsahara <- all_countries_w_region %>% 
  filter(str_detect(region, "Europe|Sahara")) %>% 
  plot_countries_by_region()

p_rest <- all_countries_w_region %>% 
  filter(!str_detect(region, "Europe|Sahara")) %>% 
  plot_countries_by_region()
```


```r
p_europe_subsahara
```

![](20-analyse-hurdle_files/figure-html/countries-region-europe-subsahara-1.png)<!-- -->

```r
p_rest
```

![](20-analyse-hurdle_files/figure-html/countries-region-non-europe-subsahara-1.png)<!-- -->



Relate to gdp

```r
gdp <- WDI::WDI(start = 2019, end = 2019)

country_effect_with_gdp <- all_countries_w_region %>% 
  left_join(gdp, by = c("country_code" = "iso2c"))
country_effect_with_gdp
```

```
## # A tibble: 828,000 x 9
##    country.x drawid name     effect region country_code country.y NY.GDP.PCAP.KD
##    <chr>     <fct>  <chr>     <dbl> <chr>  <chr>        <chr>              <dbl>
##  1 Algeria   1      effect~ 0.00297 Middl~ DZ           Algeria            4115.
##  2 Algeria   2      effect~ 0.00103 Middl~ DZ           Algeria            4115.
##  3 Algeria   3      effect~ 0.00362 Middl~ DZ           Algeria            4115.
##  4 Algeria   4      effect~ 0.00302 Middl~ DZ           Algeria            4115.
##  5 Algeria   5      effect~ 0.00224 Middl~ DZ           Algeria            4115.
##  6 Algeria   6      effect~ 0.00217 Middl~ DZ           Algeria            4115.
##  7 Algeria   7      effect~ 0.00162 Middl~ DZ           Algeria            4115.
##  8 Algeria   8      effect~ 0.00148 Middl~ DZ           Algeria            4115.
##  9 Algeria   9      effect~ 0.00125 Middl~ DZ           Algeria            4115.
## 10 Algeria   10     effect~ 0.00122 Middl~ DZ           Algeria            4115.
## # ... with 827,990 more rows, and 1 more variable: year <int>
```


```r
p <- country_effect_with_gdp %>% 
  filter(name == "effect_50") %>% 
  rename(gdp = NY.GDP.PCAP.KD) %>% 
  group_by(country_code, country.x, gdp, region) %>% 
  point_interval(effect, .width = .5, .point = median, .interval = hdi) %>% 
  ggplot(aes(gdp, effect, colour = region, label = country.x)) +
  geom_pointrange(aes(ymin = .lower, ymax = .upper)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  labs(colour = NULL, x = "GDP per capita", 
       y = "% increase in APC for 1% increase of P_top10 at the median") +
  theme_clean() +
  theme(legend.position = "top")  +
  coord_cartesian(ylim = c(0, .003))
```


```r
p
```

```
## Warning: Removed 1 rows containing missing values (geom_pointrange).
```

![](20-analyse-hurdle_files/figure-html/countries-gdp-1.png)<!-- -->


```r
plotly::ggplotly(p)
```

```{=html}
<div id="htmlwidget-9b367b5fd350d02ce859" style="width:700px;height:500px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-9b367b5fd350d02ce859">{"x":{"data":[{"x":[58781.0466570626,10155.4929447876,36081.0653118215,31640.2146297141,11414.5783998545,40599.0315482907,61340.1661723862,6612.22739107057,null,3250.56747993197],"y":[0.000731546809528524,0.000525618716687662,0.0010143434521899,0.000662947682132609,0.00161069491675028,0.00109913188337563,0.00107154968917013,0.00104355023961137,0.000733034476735257,0.000986454939177264],"text":["gdp:  58781.0467<br />effect: 0.0007315468<br />region: East Asia & Pacific<br />country.x: Australia<br />.lower: 0.0004371785<br />.upper: 0.0010135189","gdp:  10155.4929<br />effect: 0.0005256187<br />region: East Asia & Pacific<br />country.x: China<br />.lower: 0.0003279944<br />.upper: 0.0006777332","gdp:  36081.0653<br />effect: 0.0010143435<br />region: East Asia & Pacific<br />country.x: Japan<br />.lower: 0.0007959986<br />.upper: 0.0012526695","gdp:  31640.2146<br />effect: 0.0006629477<br />region: East Asia & Pacific<br />country.x: South Korea<br />.lower: 0.0003416752<br />.upper: 0.0009666421","gdp:  11414.5784<br />effect: 0.0016106949<br />region: East Asia & Pacific<br />country.x: Malaysia<br />.lower: 0.0011014165<br />.upper: 0.0018856255","gdp:  40599.0315<br />effect: 0.0010991319<br />region: East Asia & Pacific<br />country.x: New Zealand<br />.lower: 0.0007804047<br />.upper: 0.0014495472","gdp:  61340.1662<br />effect: 0.0010715497<br />region: East Asia & Pacific<br />country.x: Singapore<br />.lower: 0.0007672434<br />.upper: 0.0013690940","gdp:   6612.2274<br />effect: 0.0010435502<br />region: East Asia & Pacific<br />country.x: Thailand<br />.lower: 0.0007638857<br />.upper: 0.0013374321","gdp:          NA<br />effect: 0.0007330345<br />region: East Asia & Pacific<br />country.x: Taiwan<br />.lower: 0.0004086662<br />.upper: 0.0010354005","gdp:   3250.5675<br />effect: 0.0009864549<br />region: East Asia & Pacific<br />country.x: Viet Nam<br />.lower: 0.0005965736<br />.upper: 0.0013503587"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.00028197204284567,0.000152114440722597,0.000238326000274692,0.000303694454150723,0.000274930569815912,0.000350415337461397,0.000297544263660337,0.000293881893497272,0.000302366007799298,0.000363903726547239],"arrayminus":[0.000294368290225666,0.00019762435544355,0.000218344832282789,0.000321272439546184,0.000509278460975818,0.000318727133704066,0.000304306267606668,0.000279664511181563,0.000324368233560313,0.000389881324645712],"type":"data","width":0,"symmetric":false,"color":"rgba(225,106,134,1)"},"name":"East Asia & Pacific","legendgroup":"East Asia & Pacific","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(225,106,134,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(225,106,134,1)"}},"hoveron":"points","frame":null},{"x":[46669.7512148406,43065.5150654148,88413.1917082485,28211.064453125,20202.1515915067,43329.0506893066,57553.1312399488,20408.4362440973,28101.5270745864,46135.0776632627,38912.3312648593,47750.8796619598,19003.829047083,14068.0445267591,15041.0985793785,75143.0184709957,57818.8594639095,32119.7444107572,17241.2553476195,108570.027704794,48443.7320540283,76005.224786526,15016.6732968109,21617.4115165155,11221.7083913628,6567.90956383791,9958.4609375,53490.3518198746,24071.2824401949,18167.4836571189,11955.433463876],"y":[0.00101947229509623,0.000899668013130359,0.00101886583036953,0.00119886016661439,0.00162282094995114,0.00104731094979721,0.00104441127994434,0.00124325851000307,0.000848264361213678,0.00099759005619928,0.00108820481147679,0.000830253228359188,0.000920533545724081,0.00196589110459028,0.000977559593056048,0.00105856053241848,0.00126756753097814,0.0007864586754572,0.00124952119326019,0.00100791264151629,0.000994744182634332,0.00110476668694445,0.00117634333331612,0.00123697648912185,0.00117560854620416,0.00186707660858986,0.00178155224564711,0.000983143783955745,0.00133924349341748,0.00199340003458258,0.00172961299852458],"text":["gdp:  46669.7512<br />effect: 0.0010194723<br />region: Europe & Central Asia<br />country.x: Austria<br />.lower: 0.0007366690<br />.upper: 0.0013798343","gdp:  43065.5151<br />effect: 0.0008996680<br />region: Europe & Central Asia<br />country.x: Belgium<br />.lower: 0.0006295627<br />.upper: 0.0012582692","gdp:  88413.1917<br />effect: 0.0010188658<br />region: Europe & Central Asia<br />country.x: Switzerland<br />.lower: 0.0007269543<br />.upper: 0.0013033834","gdp:  28211.0645<br />effect: 0.0011988602<br />region: Europe & Central Asia<br />country.x: Cyprus<br />.lower: 0.0007545916<br />.upper: 0.0015330812","gdp:  20202.1516<br />effect: 0.0016228209<br />region: Europe & Central Asia<br />country.x: Czech Republic<br />.lower: 0.0011661800<br />.upper: 0.0020016436","gdp:  43329.0507<br />effect: 0.0010473109<br />region: Europe & Central Asia<br />country.x: Germany<br />.lower: 0.0008057171<br />.upper: 0.0013363470","gdp:  57553.1312<br />effect: 0.0010444113<br />region: Europe & Central Asia<br />country.x: Denmark<br />.lower: 0.0007477213<br />.upper: 0.0013661147","gdp:  20408.4362<br />effect: 0.0012432585<br />region: Europe & Central Asia<br />country.x: Estonia<br />.lower: 0.0008063706<br />.upper: 0.0016852580","gdp:  28101.5271<br />effect: 0.0008482644<br />region: Europe & Central Asia<br />country.x: Spain<br />.lower: 0.0005516010<br />.upper: 0.0013212347","gdp:  46135.0777<br />effect: 0.0009975901<br />region: Europe & Central Asia<br />country.x: Finland<br />.lower: 0.0006754741<br />.upper: 0.0013024737","gdp:  38912.3313<br />effect: 0.0010882048<br />region: Europe & Central Asia<br />country.x: France<br />.lower: 0.0007811518<br />.upper: 0.0014749909","gdp:  47750.8797<br />effect: 0.0008302532<br />region: Europe & Central Asia<br />country.x: United Kingdom<br />.lower: 0.0005651334<br />.upper: 0.0010559747","gdp:  19003.8290<br />effect: 0.0009205335<br />region: Europe & Central Asia<br />country.x: Greece<br />.lower: 0.0006123371<br />.upper: 0.0013546888","gdp:  14068.0445<br />effect: 0.0019658911<br />region: Europe & Central Asia<br />country.x: Croatia<br />.lower: 0.0014541237<br />.upper: 0.0024297255","gdp:  15041.0986<br />effect: 0.0009775596<br />region: Europe & Central Asia<br />country.x: Hungary<br />.lower: 0.0005303569<br />.upper: 0.0013615892","gdp:  75143.0185<br />effect: 0.0010585605<br />region: Europe & Central Asia<br />country.x: Ireland<br />.lower: 0.0008114976<br />.upper: 0.0014159203","gdp:  57818.8595<br />effect: 0.0012675675<br />region: Europe & Central Asia<br />country.x: Iceland<br />.lower: 0.0007337924<br />.upper: 0.0016741015","gdp:  32119.7444<br />effect: 0.0007864587<br />region: Europe & Central Asia<br />country.x: Italy<br />.lower: 0.0005199254<br />.upper: 0.0011023492","gdp:  17241.2553<br />effect: 0.0012495212<br />region: Europe & Central Asia<br />country.x: Lithuania<br />.lower: 0.0007856010<br />.upper: 0.0017226364","gdp: 108570.0277<br />effect: 0.0010079126<br />region: Europe & Central Asia<br />country.x: Luxembourg<br />.lower: 0.0005122799<br />.upper: 0.0013180497","gdp:  48443.7321<br />effect: 0.0009947442<br />region: Europe & Central Asia<br />country.x: Netherlands<br />.lower: 0.0006228951<br />.upper: 0.0012273035","gdp:  76005.2248<br />effect: 0.0011047667<br />region: Europe & Central Asia<br />country.x: Norway<br />.lower: 0.0008163760<br />.upper: 0.0014067817","gdp:  15016.6733<br />effect: 0.0011763433<br />region: Europe & Central Asia<br />country.x: Poland<br />.lower: 0.0009282650<br />.upper: 0.0016886691","gdp:  21617.4115<br />effect: 0.0012369765<br />region: Europe & Central Asia<br />country.x: Portugal<br />.lower: 0.0006952122<br />.upper: 0.0018291980","gdp:  11221.7084<br />effect: 0.0011756085<br />region: Europe & Central Asia<br />country.x: Romania<br />.lower: 0.0008486838<br />.upper: 0.0017292232","gdp:   6567.9096<br />effect: 0.0018670766<br />region: Europe & Central Asia<br />country.x: Serbia<br />.lower: 0.0013072446<br />.upper: 0.0022689750","gdp:   9958.4609<br />effect: 0.0017815522<br />region: Europe & Central Asia<br />country.x: Russia<br />.lower: 0.0012206042<br />.upper: 0.0023465103","gdp:  53490.3518<br />effect: 0.0009831438<br />region: Europe & Central Asia<br />country.x: Sweden<br />.lower: 0.0006862078<br />.upper: 0.0012028221","gdp:  24071.2824<br />effect: 0.0013392435<br />region: Europe & Central Asia<br />country.x: Slovenia<br />.lower: 0.0009518130<br />.upper: 0.0018020613","gdp:  18167.4837<br />effect: 0.0019934000<br />region: Europe & Central Asia<br />country.x: Slovakia<br />.lower: 0.0014296351<br />.upper: 0.0024870884","gdp:  11955.4335<br />effect: 0.0017296130<br />region: Europe & Central Asia<br />country.x: Turkey<br />.lower: 0.0013581508<br />.upper: 0.0020172920"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.000360361974067078,0.000358601138490753,0.000284517570102137,0.000334221051168695,0.000378822608561941,0.000289036016272921,0.000321703439480011,0.000441999484444522,0.000472970326250853,0.000304883679419256,0.000386786094602512,0.000225721440820749,0.000434155224154228,0.000463834410216629,0.00038402956048256,0.000357359799336859,0.000406533935755086,0.000315890559505176,0.000473115202378144,0.000310137029966979,0.00023255928867838,0.000302014998481236,0.000512325770820046,0.000592221503458089,0.000553614683859736,0.000401898361933401,0.000564958091364423,0.000219678304061315,0.000462817797059168,0.000493688408149757,0.000287679038831219],"arrayminus":[0.000282803277325173,0.000270105268054298,0.000291911489292091,0.000444268569866603,0.000456640945796473,0.00024159386627794,0.00029668997254978,0.000436887879745317,0.000296663321066829,0.000322115921083148,0.00030705298210096,0.000265119826001295,0.000308196433201706,0.000511767420458924,0.000447202726687053,0.000247062937185101,0.000533775140394967,0.000266533262653678,0.000463920193676856,0.000495632713065706,0.000371849056419819,0.000288390711378253,0.000248078318240185,0.000541764290125666,0.00032692476345359,0.000559832040522018,0.00056094803484964,0.00029693599654954,0.000387430491783732,0.000563764884694004,0.000371462196119627],"type":"data","width":0,"symmetric":false,"color":"rgba(193,133,0,1)"},"name":"Europe & Central Asia","legendgroup":"Europe & Central Asia","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(193,133,0,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(193,133,0,1)"}},"hoveron":"points","frame":null},{"x":[12712.9707379001,8622.06659859425,13828.6343625026,6384.53577007881,9819.53291395011,16036.2964025995],"y":[0.00195227725158481,0.00180165229252681,0.00175165342620963,0.0021190750457427,0.00131187549043123,0.00178972202285467],"text":["gdp:  12712.9707<br />effect: 0.0019522773<br />region: Latin America & Caribbean<br />country.x: Argentina<br />.lower: 0.0013338487<br />.upper: 0.0024742625","gdp:   8622.0666<br />effect: 0.0018016523<br />region: Latin America & Caribbean<br />country.x: Brazil<br />.lower: 0.0015308668<br />.upper: 0.0021806381","gdp:  13828.6344<br />effect: 0.0017516534<br />region: Latin America & Caribbean<br />country.x: Chile<br />.lower: 0.0012923704<br />.upper: 0.0021682247","gdp:   6384.5358<br />effect: 0.0021190750<br />region: Latin America & Caribbean<br />country.x: Colombia<br />.lower: 0.0015258891<br />.upper: 0.0026855667","gdp:   9819.5329<br />effect: 0.0013118755<br />region: Latin America & Caribbean<br />country.x: Mexico<br />.lower: 0.0008895093<br />.upper: 0.0018388942","gdp:  16036.2964<br />effect: 0.0017897220<br />region: Latin America & Caribbean<br />country.x: Uruguay<br />.lower: 0.0011440084<br />.upper: 0.0023160120"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.000521985249219618,0.000378985829013663,0.000416571253381181,0.000566491682165621,0.000527018744624256,0.000526289948623465],"arrayminus":[0.0006184285435323,0.000270785448006108,0.000459283073427706,0.000593185908034834,0.000422366218846086,0.00064571360620365],"type":"data","width":0,"symmetric":false,"color":"rgba(121,157,0,1)"},"name":"Latin America & Caribbean","legendgroup":"Latin America & Caribbean","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(121,157,0,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(121,157,0,1)"}},"hoveron":"points","frame":null},{"x":[40438.3257070934,4115.39553970687,3964.98712904952,38995.2304525406,5308.91985073419,4133.54980467597,27207.1318242823,6815.90897612173,3044.90625,16694.1322691229,59149.339093965,19817.8107097641,4208.06617473945],"y":[0.000923449859508853,0.00140065656644636,0.00175829929730625,0.00100544925665237,0.00187120393783151,0.00100806183400705,0.000968863218582749,0.000980539307066074,0.00158358198141291,0.00189962288045216,0.0010786466611567,0.00106553462067697,0.00161050720366163],"text":["gdp:  40438.3257<br />effect: 0.0009234499<br />region: Middle East & North Africa<br />country.x: United Arab Emirates<br />.lower: 0.0006026663<br />.upper: 0.0012069944","gdp:   4115.3955<br />effect: 0.0014006566<br />region: Middle East & North Africa<br />country.x: Algeria<br />.lower: 0.0007630787<br />.upper: 0.0017515747","gdp:   3964.9871<br />effect: 0.0017582993<br />region: Middle East & North Africa<br />country.x: Egypt<br />.lower: 0.0014241304<br />.upper: 0.0020870066","gdp:  38995.2305<br />effect: 0.0010054493<br />region: Middle East & North Africa<br />country.x: Israel<br />.lower: 0.0007162475<br />.upper: 0.0012607804","gdp:   5308.9199<br />effect: 0.0018712039<br />region: Middle East & North Africa<br />country.x: Iran<br />.lower: 0.0014357526<br />.upper: 0.0021405438","gdp:   4133.5498<br />effect: 0.0010080618<br />region: Middle East & North Africa<br />country.x: Jordan<br />.lower: 0.0007363970<br />.upper: 0.0013919996","gdp:  27207.1318<br />effect: 0.0009688632<br />region: Middle East & North Africa<br />country.x: Kuwait<br />.lower: 0.0006528783<br />.upper: 0.0014353910","gdp:   6815.9090<br />effect: 0.0009805393<br />region: Middle East & North Africa<br />country.x: Lebanon<br />.lower: 0.0006845902<br />.upper: 0.0013206603","gdp:   3044.9062<br />effect: 0.0015835820<br />region: Middle East & North Africa<br />country.x: Morocco<br />.lower: 0.0011445903<br />.upper: 0.0020205909","gdp:  16694.1323<br />effect: 0.0018996229<br />region: Middle East & North Africa<br />country.x: Oman<br />.lower: 0.0015129480<br />.upper: 0.0024004147","gdp:  59149.3391<br />effect: 0.0010786467<br />region: Middle East & North Africa<br />country.x: Qatar<br />.lower: 0.0007184262<br />.upper: 0.0014515197","gdp:  19817.8107<br />effect: 0.0010655346<br />region: Middle East & North Africa<br />country.x: Saudi Arabia<br />.lower: 0.0007479728<br />.upper: 0.0013754333","gdp:   4208.0662<br />effect: 0.0016105072<br />region: Middle East & North Africa<br />country.x: Tunisia<br />.lower: 0.0011722979<br />.upper: 0.0020456946"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.000283544556736237,0.000350918139976856,0.000328707350765476,0.000255331167938183,0.000269339896919077,0.000383937799310823,0.000466527743518843,0.000340120952386856,0.00043700893795554,0.000500791854162919,0.000372873063925729,0.000309898709625921,0.000435187359231368],"arrayminus":[0.000320783522470806,0.000637577908411972,0.000334168891315603,0.000289201792008595,0.000435451361902852,0.000271664820134939,0.000315984913972091,0.000295949107535188,0.000438991707216387,0.000386674853415823,0.000360220484504456,0.000317561786485413,0.000438209285824428],"type":"data","width":0,"symmetric":false,"color":"rgba(0,171,110,1)"},"name":"Middle East & North Africa","legendgroup":"Middle East & North Africa","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(0,171,110,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(0,171,110,1)"}},"hoveron":"points","frame":null},{"x":[45109.2444855151,60687.2322580937],"y":[0.000870415607827249,0.00110114638133031],"text":["gdp:  45109.2445<br />effect: 0.0008704156<br />region: North America<br />country.x: Canada<br />.lower: 0.0005634570<br />.upper: 0.0010991718","gdp:  60687.2323<br />effect: 0.0011011464<br />region: North America<br />country.x: United States<br />.lower: 0.0009359735<br />.upper: 0.0012872719"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.000228756153794328,0.000186125554284477],"arrayminus":[0.000306958641418983,0.000165172872322074],"type":"data","width":0,"symmetric":false,"color":"rgba(0,169,190,1)"},"name":"North America","legendgroup":"North America","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(0,169,190,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(0,169,190,1)"}},"hoveron":"points","frame":null},{"x":[1965.53932972188,1497.98683209944],"y":[0.00206897718142011,0.00149772094602019],"text":["gdp:   1965.5393<br />effect: 0.0020689772<br />region: South Asia<br />country.x: India<br />.lower: 0.0016354506<br />.upper: 0.0024094521","gdp:   1497.9868<br />effect: 0.0014977209<br />region: South Asia<br />country.x: Pakistan<br />.lower: 0.0010609315<br />.upper: 0.0017964461"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.000340474878129452,0.000298725166460894],"arrayminus":[0.0004335266177329,0.000436789490960353],"type":"data","width":0,"symmetric":false,"color":"rgba(108,142,230,1)"},"name":"South Asia","legendgroup":"South Asia","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(108,142,230,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(108,142,230,1)"}},"hoveron":"points","frame":null},{"x":[799.795134257737,2053.58673106375,2502.652281009,894.520366857752,6125.73532475494],"y":[0.000872498859149923,0.000931719231233953,0.000938777379413447,0.000872319910385532,0.00130175708952323],"text":["gdp:    799.7951<br />effect: 0.0008724989<br />region: Sub-Saharan Africa<br />country.x: Ethiopia<br />.lower: 0.0005517167<br />.upper: 0.0011744039","gdp:   2053.5867<br />effect: 0.0009317192<br />region: Sub-Saharan Africa<br />country.x: Ghana<br />.lower: 0.0006192150<br />.upper: 0.0012350271","gdp:   2502.6523<br />effect: 0.0009387774<br />region: Sub-Saharan Africa<br />country.x: Nigeria<br />.lower: 0.0006874310<br />.upper: 0.0012966112","gdp:    894.5204<br />effect: 0.0008723199<br />region: Sub-Saharan Africa<br />country.x: Uganda<br />.lower: 0.0004865469<br />.upper: 0.0010964888","gdp:   6125.7353<br />effect: 0.0013017571<br />region: Sub-Saharan Africa<br />country.x: South Africa<br />.lower: 0.0010145302<br />.upper: 0.0015266654"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.000301904992689272,0.000303307843828903,0.000357833859393262,0.000224168937109464,0.00022490833459844],"arrayminus":[0.000320782159272416,0.000312504201869772,0.000251346385788115,0.000385772974803469,0.000287226932552313],"type":"data","width":0,"symmetric":false,"color":"rgba(209,105,208,1)"},"name":"Sub-Saharan Africa","legendgroup":"Sub-Saharan Africa","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(209,105,208,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(209,105,208,1)"}},"hoveron":"points","frame":null}],"layout":{"margin":{"t":25.2984640929846,"r":6.6417600664176,"b":39.252801992528,"l":48.285595682856},"font":{"color":"rgba(0,0,0,1)","family":"Hind","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-4588.71649426908,113958.539333321],"tickmode":"array","ticktext":["0","30,000","60,000","90,000"],"tickvals":[0,30000,60000,90000],"categoryorder":"array","categoryarray":["0","30,000","60,000","90,000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"Hind","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"GDP per capita","font":{"color":"rgba(0,0,0,1)","family":"Hind","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.00015,0.00315],"tickmode":"array","ticktext":["0.0%","0.1%","0.2%","0.3%"],"tickvals":[0,0.001,0.002,0.003],"categoryorder":"array","categoryarray":["0.0%","0.1%","0.2%","0.3%"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"Hind","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"% increase in APC for 1% increase of P_top10 at the median","font":{"color":"rgba(0,0,0,1)","family":"Hind","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"Hind","size":11.689497716895},"title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"Hind","size":14.6118721461187}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"33381f7c5aa7":{"x":{},"y":{},"colour":{},"label":{},"ymin":{},"ymax":{},"type":"scatter"}},"cur_data":"33381f7c5aa7","visdat":{"33381f7c5aa7":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

This could still be improved by adding number of universities (or papers) as 
a size variable.


```r
unis_p_country <- df %>% 
  distinct(country, University) %>% 
  count(country, name = "n_universities") 
```


```r
pdata <- country_effect_with_gdp %>% 
  filter(name == "effect_50") %>% 
  rename(gdp = NY.GDP.PCAP.KD) %>% 
  group_by(country_code, country.x, gdp, region) %>% 
  point_interval(effect, .width = .5, .point = median, .interval = hdi) %>% 
  left_join(unis_p_country, by = c("country.x" = "country"))

labels <- pdata %>% 
  mutate(label = case_when(
    country.x %in% c("China", "India", "Iran", "Germany", "United States",
                      "Brazil", "Luxembourg", "Czech Republic") ~ country.x,
    TRUE ~ ""))

pdata %>% 
  ggplot(aes(gdp, effect, colour = region, label = "")) +
  geom_linerange(aes(ymin = .lower, ymax = .upper, alpha = n_universities)) +
  geom_point(aes(alpha = n_universities), size = 2) +
  ggrepel::geom_text_repel(data = labels, aes(label = label),
                            show.legend = FALSE, max.overlaps = Inf,
                           box.padding = 1, min.segment.length = 0) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  # scale_size_continuous(trans = "sqrt") +
  scale_alpha_continuous(range = c(.2, 1), trans = "log10") +
  labs(colour = "Region", x = "GDP per capita", alpha = "Number of universities",
       y = expression(
         paste("% increase in APC for 1% increase of ", P["top 10%"], 
               " at the median")
         )) +
  theme_clean() +
  theme(legend.position = "top", legend.box = "vertical")  +
  coord_cartesian(ylim = c(0, .003))
```

```
## Warning: Removed 1 rows containing missing values (geom_segment).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

```
## Warning: Removed 1 rows containing missing values (geom_text_repel).
```

![](20-analyse-hurdle_files/figure-html/countries-gdp-fancy-1.png)<!-- -->



```r
contrast_50 %>% 
  group_by(country, drawid) %>% 
  summarise(intercept = mean(base)) %>% 
  ggplot(aes(intercept, fct_reorder(country, intercept))) +
  stat_halfeye(.width = c(.5, .9), fill = colorspace::lighten("#007FA8"),
               point_interval = "median_hdi") +
  scale_x_continuous(labels = scales::dollar) +
  theme_clean() +
  labs(y = NULL, x = expression(paste("Estimated APC at median of ",
                                      P["top 10%"])))
```

```
## `summarise()` has grouped output by 'country'. You can override using the
## `.groups` argument.
```

![](20-analyse-hurdle_files/figure-html/country-intercept-1.png)<!-- -->
Not sure whether this is helpful. What we show is quite abstract (APC at 
hypothetical level of P_top10, averaged across all fields (weighting equally). 

The same would apply to the field intercepts above.
