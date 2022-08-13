---
title: "Analyse Hurdle model"
author: "Thomas Klebel"
date: "13 August, 2022"
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
hm <- brm(file = "final_models/17-wo-south-africa_brm-large-sample.rds", 
          file_refit = "never")
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
##    Data: subsample (Number of observations: 185888) 
##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup draws = 4000
## 
## Group-Level Effects: 
## ~country (Number of levels: 68) 
##                                Estimate Est.Error l-95% CI u-95% CI Rhat
## sd(mu1_Intercept)                  0.48      0.05     0.38     0.60 1.00
## sd(mu1_P_top10)                    0.05      0.03     0.00     0.12 1.00
## sd(mu2_Intercept)                  0.03      0.00     0.02     0.04 1.00
## sd(mu2_P_top10)                    0.01      0.00     0.00     0.01 1.00
## sd(hu1_Intercept)                  1.56      0.28     1.09     2.16 1.01
## sd(hu1_P_top10)                    0.12      0.09     0.00     0.35 1.00
## sd(hu2_Intercept)                  1.37      0.15     1.10     1.70 1.01
## sd(hu2_P_top10)                    0.32      0.09     0.14     0.51 1.00
## cor(mu1_Intercept,mu1_P_top10)    -0.23      0.25    -0.67     0.31 1.00
## cor(mu2_Intercept,mu2_P_top10)    -0.13      0.30    -0.67     0.50 1.00
## cor(hu1_Intercept,hu1_P_top10)     0.04      0.32    -0.58     0.65 1.00
## cor(hu2_Intercept,hu2_P_top10)     0.06      0.20    -0.33     0.45 1.00
##                                Bulk_ESS Tail_ESS
## sd(mu1_Intercept)                  1104     2058
## sd(mu1_P_top10)                     720     1398
## sd(mu2_Intercept)                  1859     2648
## sd(mu2_P_top10)                    1415     2281
## sd(hu1_Intercept)                   469     1494
## sd(hu1_P_top10)                    1566     1935
## sd(hu2_Intercept)                  1170     1793
## sd(hu2_P_top10)                     697      573
## cor(mu1_Intercept,mu1_P_top10)     4147     2725
## cor(mu2_Intercept,mu2_P_top10)     5603     2842
## cor(hu1_Intercept,hu1_P_top10)     7303     3105
## cor(hu2_Intercept,hu2_P_top10)     2345     2622
## 
## ~field (Number of levels: 19) 
##                                Estimate Est.Error l-95% CI u-95% CI Rhat
## sd(mu1_Intercept)                  0.25      0.05     0.17     0.38 1.00
## sd(mu1_P_top10)                    0.07      0.02     0.03     0.12 1.00
## sd(mu2_Intercept)                  0.25      0.06     0.17     0.39 1.04
## sd(mu2_P_top10)                    0.02      0.01     0.01     0.04 1.00
## sd(hu1_Intercept)                  1.42      0.26     1.01     2.01 1.00
## sd(hu1_P_top10)                    0.13      0.07     0.01     0.27 1.00
## sd(hu2_Intercept)                  2.19      0.32     1.64     2.87 1.00
## sd(hu2_P_top10)                    0.39      0.09     0.25     0.59 1.00
## sd(theta1_Intercept)               0.43      0.11     0.25     0.69 1.00
## cor(mu1_Intercept,mu1_P_top10)     0.18      0.24    -0.31     0.62 1.00
## cor(mu2_Intercept,mu2_P_top10)    -0.20      0.29    -0.71     0.39 1.00
## cor(hu1_Intercept,hu1_P_top10)    -0.33      0.29    -0.81     0.32 1.00
## cor(hu2_Intercept,hu2_P_top10)     0.37      0.22    -0.10     0.72 1.00
##                                Bulk_ESS Tail_ESS
## sd(mu1_Intercept)                  1513     2199
## sd(mu1_P_top10)                    1527     2504
## sd(mu2_Intercept)                   113      305
## sd(mu2_P_top10)                    1639     2326
## sd(hu1_Intercept)                  1528     2406
## sd(hu1_P_top10)                    1295     1332
## sd(hu2_Intercept)                  1549     2220
## sd(hu2_P_top10)                    1581     2543
## sd(theta1_Intercept)               1275     2029
## cor(mu1_Intercept,mu1_P_top10)     3876     3255
## cor(mu2_Intercept,mu2_P_top10)     3300     3193
## cor(hu1_Intercept,hu1_P_top10)     3925     2865
## cor(hu2_Intercept,hu2_P_top10)     2181     2683
## 
## Population-Level Effects: 
##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## mu1_Intercept        6.75      0.09     6.56     6.93 1.01      718     1555
## hu1_Intercept       -1.16      0.45    -2.08    -0.31 1.01      417     1605
## mu2_Intercept        7.52      0.06     7.40     7.64 1.01      368     1091
## hu2_Intercept        0.63      0.47    -0.26     1.56 1.00      658     1368
## theta1_Intercept    -0.87      0.14    -1.13    -0.57 1.01      409     1103
## mu1_P_top10          0.17      0.03     0.11     0.23 1.00     3060     3116
## hu1_P_top10          0.03      0.09    -0.13     0.24 1.00     3111     2800
## mu2_P_top10          0.01      0.01    -0.01     0.02 1.00     1685     2040
## hu2_P_top10         -0.04      0.12    -0.27     0.22 1.00     1746     2673
## 
## Family Specific Parameters: 
##        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma1     0.85      0.01     0.83     0.87 1.01      615     2494
## sigma2     0.21      0.00     0.20     0.21 1.00     8897     3130
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
  theme(legend.position = "top") + 
  labs(colour = NULL)
```

![](20-analyse-hurdle_files/figure-html/fields-combined-1-1.png)<!-- -->



```r
custom_pal <- sequential_hcl(5, palette = "Mako") %>% 
  lighten(amount = .1)
p + scale_color_manual(values = c(
    effect_20 = custom_pal[3],
    effect_50 = custom_pal[3],
    effect_80 = custom_pal[4]
  ))
```

![](20-analyse-hurdle_files/figure-html/fields-combined-2-1.png)<!-- -->

Questions that arise:
- why in some fields stronger/weaker effect for larger/smaller P_top10? Is this
also associated with hurdle component?
- Especially: why physics and mathematics negative? because of hurdle?

- Effect is diminishing for all fields with sufficient data. 

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
## # A tibble: 816,000 × 9
##    country.x drawid name     effect region country_code country.y NY.GDP.PCAP.KD
##    <chr>     <fct>  <chr>     <dbl> <chr>  <chr>        <chr>              <dbl>
##  1 Algeria   1      effec… -1.84e-4 Middl… DZ           Algeria            4115.
##  2 Algeria   2      effec…  1.01e-3 Middl… DZ           Algeria            4115.
##  3 Algeria   3      effec… -3.53e-4 Middl… DZ           Algeria            4115.
##  4 Algeria   4      effec…  1.04e-3 Middl… DZ           Algeria            4115.
##  5 Algeria   5      effec…  9.67e-4 Middl… DZ           Algeria            4115.
##  6 Algeria   6      effec… -8.70e-4 Middl… DZ           Algeria            4115.
##  7 Algeria   7      effec… -1.98e-3 Middl… DZ           Algeria            4115.
##  8 Algeria   8      effec…  2.01e-3 Middl… DZ           Algeria            4115.
##  9 Algeria   9      effec…  1.71e-3 Middl… DZ           Algeria            4115.
## 10 Algeria   10     effec…  1.99e-4 Middl… DZ           Algeria            4115.
## # … with 815,990 more rows, and 1 more variable: year <int>
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
<div id="htmlwidget-8a3d855662b350799335" style="width:700px;height:500px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-8a3d855662b350799335">{"x":{"data":[{"x":[58781.0466570626,10155.4929447876,36081.0653118215,31640.2146297141,11414.5783998545,40599.0315482907,61340.1661723862,6612.22739107057,null,3250.56747993197],"y":[0.000657259392189562,0.000340757882349768,0.000752729049946306,0.000430279683000684,0.00214084454706255,0.000925320194622492,0.000712895889628821,0.00140692130013545,-0.000297358085030298,0.000489677185237628],"text":["gdp:  58781.0467<br />effect:  6.572594e-04<br />region: East Asia & Pacific<br />country.x: Australia<br />.lower:  4.257567e-04<br />.upper: 0.0009635424","gdp:  10155.4929<br />effect:  3.407579e-04<br />region: East Asia & Pacific<br />country.x: China<br />.lower:  2.859429e-06<br />.upper: 0.0005830555","gdp:  36081.0653<br />effect:  7.527290e-04<br />region: East Asia & Pacific<br />country.x: Japan<br />.lower:  5.567459e-04<br />.upper: 0.0009534004","gdp:  31640.2146<br />effect:  4.302797e-04<br />region: East Asia & Pacific<br />country.x: South Korea<br />.lower:  1.183179e-04<br />.upper: 0.0008351501","gdp:  11414.5784<br />effect:  2.140845e-03<br />region: East Asia & Pacific<br />country.x: Malaysia<br />.lower:  1.464967e-03<br />.upper: 0.0026494122","gdp:  40599.0315<br />effect:  9.253202e-04<br />region: East Asia & Pacific<br />country.x: New Zealand<br />.lower:  6.720163e-04<br />.upper: 0.0012943725","gdp:  61340.1662<br />effect:  7.128959e-04<br />region: East Asia & Pacific<br />country.x: Singapore<br />.lower:  3.363803e-04<br />.upper: 0.0011522126","gdp:   6612.2274<br />effect:  1.406921e-03<br />region: East Asia & Pacific<br />country.x: Thailand<br />.lower:  1.022643e-03<br />.upper: 0.0018072824","gdp:          NA<br />effect: -2.973581e-04<br />region: East Asia & Pacific<br />country.x: Taiwan<br />.lower: -7.147161e-04<br />.upper: 0.0002106358","gdp:   3250.5675<br />effect:  4.896772e-04<br />region: East Asia & Pacific<br />country.x: Viet Nam<br />.lower:  4.195183e-05<br />.upper: 0.0009647479"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.000306282970996689,0.000242297593081697,0.000200671333189473,0.000404870460745338,0.000508567678569359,0.000369052324712758,0.000439316725034547,0.00040036113116867,0.000507993843533903,0.000475070744882017],"arrayminus":[0.00023150268063409,0.000337898453452329,0.000195983148588012,0.000311961795879959,0.000675877233631836,0.000253303909226151,0.000376515604156454,0.000384278792669402,0.000417358012892245,0.000447725353119313],"type":"data","width":0,"symmetric":false,"color":"rgba(225,106,134,1)"},"name":"East Asia & Pacific","legendgroup":"East Asia & Pacific","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(225,106,134,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(225,106,134,1)"}},"hoveron":"points","frame":null},{"x":[46669.7512148406,43065.5150654148,88413.1917082485,28211.064453125,20202.1515915067,43329.0506893066,57553.1312399488,20408.4362440973,28101.5270745864,46135.0776632627,38912.3312648593,47750.8796619598,19003.829047083,14068.0445267591,15041.0985793785,75143.0184709957,57818.8594639095,32119.7444107572,17241.2553476195,108570.027704794,48443.7320540283,76005.224786526,15016.6732968109,21617.4115165155,11221.7083913628,6567.90956383791,9958.4609375,53490.3518198746,24071.2824401949,18167.4836571189,11955.433463876],"y":[0.00052051815692508,0.000863127302279828,0.000502956951262086,0.000611745224295094,0.00155380304250123,5.79273599161276e-05,0.000562548091002994,0.00101066393934328,0.000377086915294925,0.000769078057690761,0.000699327303744025,0.000611022893645759,0.000302273884789504,0.00180246510016284,0.000541143910956431,0.00078849292479872,0.000972316344966663,0.000521892743542415,0.00114731383450729,0.000564321218712854,0.000523780216711707,0.00067845623611556,0.000167457387943925,-0.00010650481953015,0.00121039515666912,0.00137404791152103,-3.77698719445286e-05,0.000794295818475679,0.00106607598133713,0.00196177671312631,0.00163262952371149],"text":["gdp:  46669.7512<br />effect:  5.205182e-04<br />region: Europe & Central Asia<br />country.x: Austria<br />.lower:  2.129551e-04<br />.upper: 0.0010215180","gdp:  43065.5151<br />effect:  8.631273e-04<br />region: Europe & Central Asia<br />country.x: Belgium<br />.lower:  4.775291e-04<br />.upper: 0.0012827206","gdp:  88413.1917<br />effect:  5.029570e-04<br />region: Europe & Central Asia<br />country.x: Switzerland<br />.lower:  1.384197e-04<br />.upper: 0.0008450983","gdp:  28211.0645<br />effect:  6.117452e-04<br />region: Europe & Central Asia<br />country.x: Cyprus<br />.lower:  3.240886e-05<br />.upper: 0.0011775819","gdp:  20202.1516<br />effect:  1.553803e-03<br />region: Europe & Central Asia<br />country.x: Czech Republic<br />.lower:  9.563786e-04<br />.upper: 0.0020082752","gdp:  43329.0507<br />effect:  5.792736e-05<br />region: Europe & Central Asia<br />country.x: Germany<br />.lower: -2.735391e-04<br />.upper: 0.0003786064","gdp:  57553.1312<br />effect:  5.625481e-04<br />region: Europe & Central Asia<br />country.x: Denmark<br />.lower:  1.980282e-04<br />.upper: 0.0010087880","gdp:  20408.4362<br />effect:  1.010664e-03<br />region: Europe & Central Asia<br />country.x: Estonia<br />.lower:  4.774704e-04<br />.upper: 0.0016503893","gdp:  28101.5271<br />effect:  3.770869e-04<br />region: Europe & Central Asia<br />country.x: Spain<br />.lower:  5.319441e-05<br />.upper: 0.0008197473","gdp:  46135.0777<br />effect:  7.690781e-04<br />region: Europe & Central Asia<br />country.x: Finland<br />.lower:  3.633485e-04<br />.upper: 0.0012004571","gdp:  38912.3313<br />effect:  6.993273e-04<br />region: Europe & Central Asia<br />country.x: France<br />.lower:  3.646895e-04<br />.upper: 0.0011847862","gdp:  47750.8797<br />effect:  6.110229e-04<br />region: Europe & Central Asia<br />country.x: United Kingdom<br />.lower:  4.622053e-04<br />.upper: 0.0008039663","gdp:  19003.8290<br />effect:  3.022739e-04<br />region: Europe & Central Asia<br />country.x: Greece<br />.lower: -5.036535e-05<br />.upper: 0.0008057102","gdp:  14068.0445<br />effect:  1.802465e-03<br />region: Europe & Central Asia<br />country.x: Croatia<br />.lower:  1.174663e-03<br />.upper: 0.0026431805","gdp:  15041.0986<br />effect:  5.411439e-04<br />region: Europe & Central Asia<br />country.x: Hungary<br />.lower:  1.148672e-04<br />.upper: 0.0012411793","gdp:  75143.0185<br />effect:  7.884929e-04<br />region: Europe & Central Asia<br />country.x: Ireland<br />.lower:  4.894141e-04<br />.upper: 0.0011849265","gdp:  57818.8595<br />effect:  9.723163e-04<br />region: Europe & Central Asia<br />country.x: Iceland<br />.lower:  4.533808e-04<br />.upper: 0.0017143696","gdp:  32119.7444<br />effect:  5.218927e-04<br />region: Europe & Central Asia<br />country.x: Italy<br />.lower:  2.449568e-04<br />.upper: 0.0009544567","gdp:  17241.2553<br />effect:  1.147314e-03<br />region: Europe & Central Asia<br />country.x: Lithuania<br />.lower:  6.333637e-04<br />.upper: 0.0017125476","gdp: 108570.0277<br />effect:  5.643212e-04<br />region: Europe & Central Asia<br />country.x: Luxembourg<br />.lower:  2.299756e-04<br />.upper: 0.0011936464","gdp:  48443.7321<br />effect:  5.237802e-04<br />region: Europe & Central Asia<br />country.x: Netherlands<br />.lower:  1.783182e-04<br />.upper: 0.0009566015","gdp:  76005.2248<br />effect:  6.784562e-04<br />region: Europe & Central Asia<br />country.x: Norway<br />.lower:  4.121582e-04<br />.upper: 0.0011694826","gdp:  15016.6733<br />effect:  1.674574e-04<br />region: Europe & Central Asia<br />country.x: Poland<br />.lower: -2.919397e-04<br />.upper: 0.0006387559","gdp:  21617.4115<br />effect: -1.065048e-04<br />region: Europe & Central Asia<br />country.x: Portugal<br />.lower: -6.060014e-04<br />.upper: 0.0008734443","gdp:  11221.7084<br />effect:  1.210395e-03<br />region: Europe & Central Asia<br />country.x: Romania<br />.lower:  7.055798e-04<br />.upper: 0.0018394733","gdp:   6567.9096<br />effect:  1.374048e-03<br />region: Europe & Central Asia<br />country.x: Serbia<br />.lower:  8.933775e-04<br />.upper: 0.0021077666","gdp:   9958.4609<br />effect: -3.776987e-05<br />region: Europe & Central Asia<br />country.x: Russia<br />.lower: -6.613931e-04<br />.upper: 0.0009849299","gdp:  53490.3518<br />effect:  7.942958e-04<br />region: Europe & Central Asia<br />country.x: Sweden<br />.lower:  4.860866e-04<br />.upper: 0.0011807114","gdp:  24071.2824<br />effect:  1.066076e-03<br />region: Europe & Central Asia<br />country.x: Slovenia<br />.lower:  4.811906e-04<br />.upper: 0.0016136537","gdp:  18167.4837<br />effect:  1.961777e-03<br />region: Europe & Central Asia<br />country.x: Slovakia<br />.lower:  1.269780e-03<br />.upper: 0.0025642205","gdp:  11955.4335<br />effect:  1.632630e-03<br />region: Europe & Central Asia<br />country.x: Turkey<br />.lower:  1.126055e-03<br />.upper: 0.0020381177"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.000500999892942348,0.000419593254255732,0.00034214134415048,0.000565836682532712,0.000454472161218881,0.000320679003968725,0.000446239955921725,0.000639725362900771,0.000442660347933613,0.000431379031330524,0.000485458918344237,0.000192943397055623,0.000503436299816669,0.000840715359080392,0.000700035409681534,0.000396433576044697,0.00074205326928089,0.000432563935990078,0.000565233720224332,0.000629325168303888,0.000432821308798506,0.000491026353771579,0.000471298471350357,0.00097994911937877,0.000629078148972345,0.00073371873435293,0.00102269978482559,0.000386415601814431,0.000547577708281487,0.00060244374661453,0.000405488192527682],"arrayminus":[0.000307563091424387,0.000385598193459706,0.000364537245646455,0.000579336363102078,0.000597424432249349,0.000331466417246535,0.000364519904394283,0.000533193574432109,0.000323892501510878,0.000405729538820691,0.00033463777613492,0.000148817633760486,0.000352639236800663,0.00062780258536219,0.000426276720198486,0.000299078846601957,0.000518935580432688,0.000276935989946197,0.000513950156672201,0.000334345586380697,0.000345461987279352,0.000266298030230988,0.000459397108381496,0.000499496591807389,0.000504815375124575,0.000480670384805219,0.000623623269052906,0.000308209250819591,0.000584885363415681,0.000691996728226908,0.000506574216242962],"type":"data","width":0,"symmetric":false,"color":"rgba(193,133,0,1)"},"name":"Europe & Central Asia","legendgroup":"Europe & Central Asia","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(193,133,0,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(193,133,0,1)"}},"hoveron":"points","frame":null},{"x":[12712.9707379001,8622.06659859425,13828.6343625026,6384.53577007881,9819.53291395011,16036.2964025995],"y":[0.00137740736201462,0.00141855687065617,0.00150652905700698,0.00176585711384329,0.000923872644659803,0.00152497986916141],"text":["gdp:  12712.9707<br />effect:  1.377407e-03<br />region: Latin America & Caribbean<br />country.x: Argentina<br />.lower:  4.691970e-04<br />.upper: 0.0021901069","gdp:   8622.0666<br />effect:  1.418557e-03<br />region: Latin America & Caribbean<br />country.x: Brazil<br />.lower:  1.174888e-03<br />.upper: 0.0016866827","gdp:  13828.6344<br />effect:  1.506529e-03<br />region: Latin America & Caribbean<br />country.x: Chile<br />.lower:  7.047476e-04<br />.upper: 0.0020686024","gdp:   6384.5358<br />effect:  1.765857e-03<br />region: Latin America & Caribbean<br />country.x: Colombia<br />.lower:  9.984029e-04<br />.upper: 0.0026285327","gdp:   9819.5329<br />effect:  9.238726e-04<br />region: Latin America & Caribbean<br />country.x: Mexico<br />.lower:  4.598979e-04<br />.upper: 0.0014803183","gdp:  16036.2964<br />effect:  1.524980e-03<br />region: Latin America & Caribbean<br />country.x: Uruguay<br />.lower:  8.791435e-04<br />.upper: 0.0023139634"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.000812699490177897,0.000268125801382065,0.000562073319526635,0.000862675590085644,0.000556445670617739,0.000788983564009211],"arrayminus":[0.000908210400136333,0.000243668977841765,0.000801781452608335,0.000767454192632311,0.000463974711671221,0.000645836376425887],"type":"data","width":0,"symmetric":false,"color":"rgba(121,157,0,1)"},"name":"Latin America & Caribbean","legendgroup":"Latin America & Caribbean","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(121,157,0,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(121,157,0,1)"}},"hoveron":"points","frame":null},{"x":[40438.3257070934,4115.39553970687,3964.98712904952,38995.2304525406,5308.91985073419,4133.54980467597,27207.1318242823,6815.90897612173,3044.90625,16694.1322691229,59149.339093965,19817.8107097641,4208.06617473945],"y":[0.0003635788019477,0.000624415593372419,0.00136291486255773,0.000508594844918039,0.00195240876363225,0.00101791382029358,0.000666017152384402,0.000528132782817983,0.000803777370227287,0.00167562944480277,0.000840021867136244,0.0009105639116961,0.000856904370408266],"text":["gdp:  40438.3257<br />effect:  3.635788e-04<br />region: Middle East & North Africa<br />country.x: United Arab Emirates<br />.lower:  2.772593e-05<br />.upper: 0.0009590763","gdp:   4115.3955<br />effect:  6.244156e-04<br />region: Middle East & North Africa<br />country.x: Algeria<br />.lower: -1.274446e-05<br />.upper: 0.0012536606","gdp:   3964.9871<br />effect:  1.362915e-03<br />region: Middle East & North Africa<br />country.x: Egypt<br />.lower:  8.693568e-04<br />.upper: 0.0017575668","gdp:  38995.2305<br />effect:  5.085948e-04<br />region: Middle East & North Africa<br />country.x: Israel<br />.lower:  2.741369e-04<br />.upper: 0.0009591893","gdp:   5308.9199<br />effect:  1.952409e-03<br />region: Middle East & North Africa<br />country.x: Iran<br />.lower:  1.566817e-03<br />.upper: 0.0023035025","gdp:   4133.5498<br />effect:  1.017914e-03<br />region: Middle East & North Africa<br />country.x: Jordan<br />.lower:  6.569067e-04<br />.upper: 0.0015364303","gdp:  27207.1318<br />effect:  6.660172e-04<br />region: Middle East & North Africa<br />country.x: Kuwait<br />.lower:  2.896993e-04<br />.upper: 0.0012461517","gdp:   6815.9090<br />effect:  5.281328e-04<br />region: Middle East & North Africa<br />country.x: Lebanon<br />.lower:  2.244327e-04<br />.upper: 0.0011083980","gdp:   3044.9062<br />effect:  8.037774e-04<br />region: Middle East & North Africa<br />country.x: Morocco<br />.lower:  3.135546e-04<br />.upper: 0.0012354888","gdp:  16694.1323<br />effect:  1.675629e-03<br />region: Middle East & North Africa<br />country.x: Oman<br />.lower:  1.259898e-03<br />.upper: 0.0023272862","gdp:  59149.3391<br />effect:  8.400219e-04<br />region: Middle East & North Africa<br />country.x: Qatar<br />.lower:  4.020611e-04<br />.upper: 0.0013221401","gdp:  19817.8107<br />effect:  9.105639e-04<br />region: Middle East & North Africa<br />country.x: Saudi Arabia<br />.lower:  6.129999e-04<br />.upper: 0.0014379704","gdp:   4208.0662<br />effect:  8.569044e-04<br />region: Middle East & North Africa<br />country.x: Tunisia<br />.lower:  4.711859e-04<br />.upper: 0.0015337741"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.00059549754257275,0.000629245028071166,0.000394651927404791,0.000450594456267071,0.000351093729544069,0.000518516480658315,0.000580134505082334,0.000580265221930133,0.000431711467215639,0.000651656777356794,0.000482118234465543,0.000527406501128562,0.000676869751289245],"arrayminus":[0.000335852872576046,0.000637160052073783,0.000493558074776672,0.00023445793374701,0.000385592178712947,0.000361007116069442,0.00037631783755341,0.000303700061129488,0.00049022276061474,0.00041573175654524,0.000437960761925976,0.00029756405871249,0.000385718444965482],"type":"data","width":0,"symmetric":false,"color":"rgba(0,171,110,1)"},"name":"Middle East & North Africa","legendgroup":"Middle East & North Africa","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(0,171,110,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(0,171,110,1)"}},"hoveron":"points","frame":null},{"x":[45109.2444855151,60687.2322580937],"y":[0.000320427411875549,0.000833258196989043],"text":["gdp:  45109.2445<br />effect:  3.204274e-04<br />region: North America<br />country.x: Canada<br />.lower:  1.489817e-05<br />.upper: 0.0005571839","gdp:  60687.2323<br />effect:  8.332582e-04<br />region: North America<br />country.x: United States<br />.lower:  6.894572e-04<br />.upper: 0.0009577350"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.000236756487212906,0.000124476761705256],"arrayminus":[0.000305529238901636,0.000143800978516061],"type":"data","width":0,"symmetric":false,"color":"rgba(0,169,190,1)"},"name":"North America","legendgroup":"North America","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(0,169,190,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(0,169,190,1)"}},"hoveron":"points","frame":null},{"x":[1965.53932972188,1497.98683209944],"y":[0.00267477060777411,0.00178651424884],"text":["gdp:   1965.5393<br />effect:  2.674771e-03<br />region: South Asia<br />country.x: India<br />.lower:  2.263061e-03<br />.upper: 0.0029441457","gdp:   1497.9868<br />effect:  1.786514e-03<br />region: South Asia<br />country.x: Pakistan<br />.lower:  1.219041e-03<br />.upper: 0.0024476317"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.000269375094684983,0.000661117428935507],"arrayminus":[0.000411709362095406,0.000567472802174778],"type":"data","width":0,"symmetric":false,"color":"rgba(108,142,230,1)"},"name":"South Asia","legendgroup":"South Asia","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(108,142,230,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(108,142,230,1)"}},"hoveron":"points","frame":null},{"x":[799.795134257737,2053.58673106375,2502.652281009,894.520366857752],"y":[0.000570801261520555,0.000447199665882919,0.000995931701018287,0.000194761530567721],"text":["gdp:    799.7951<br />effect:  5.708013e-04<br />region: Sub-Saharan Africa<br />country.x: Ethiopia<br />.lower:  2.312692e-04<br />.upper: 0.0010906652","gdp:   2053.5867<br />effect:  4.471997e-04<br />region: Sub-Saharan Africa<br />country.x: Ghana<br />.lower:  5.288839e-05<br />.upper: 0.0009870353","gdp:   2502.6523<br />effect:  9.959317e-04<br />region: Sub-Saharan Africa<br />country.x: Nigeria<br />.lower:  6.071432e-04<br />.upper: 0.0013898673","gdp:    894.5204<br />effect:  1.947615e-04<br />region: Sub-Saharan Africa<br />country.x: Uganda<br />.lower: -2.087633e-04<br />.upper: 0.0006659466"],"type":"scatter","mode":"lines+markers","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.000519863905832071,0.000539835648173813,0.000393935627870788,0.000471185061490862],"arrayminus":[0.000339532109151041,0.000394311274901234,0.000388788476966457,0.000403524850026564],"type":"data","width":0,"symmetric":false,"color":"rgba(209,105,208,1)"},"name":"Sub-Saharan Africa","legendgroup":"Sub-Saharan Africa","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(209,105,208,1)","opacity":1,"size":1.88976377952756,"symbol":"circle","line":{"width":3.77952755905512,"color":"rgba(209,105,208,1)"}},"hoveron":"points","frame":null}],"layout":{"margin":{"t":25.2984640929846,"r":6.6417600664176,"b":39.252801992528,"l":48.285595682856},"font":{"color":"rgba(0,0,0,1)","family":"Hind","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-4588.71649426908,113958.539333321],"tickmode":"array","ticktext":["0","30,000","60,000","90,000"],"tickvals":[0,30000,60000,90000],"categoryorder":"array","categoryarray":["0","30,000","60,000","90,000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"Hind","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"GDP per capita","font":{"color":"rgba(0,0,0,1)","family":"Hind","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.00015,0.00315],"tickmode":"array","ticktext":["0.0%","0.1%","0.2%","0.3%"],"tickvals":[0,0.001,0.002,0.003],"categoryorder":"array","categoryarray":["0.0%","0.1%","0.2%","0.3%"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"Hind","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"% increase in APC for 1% increase of P_top10 at the median","font":{"color":"rgba(0,0,0,1)","family":"Hind","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"Hind","size":11.689497716895},"title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"Hind","size":14.6118721461187}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"721251c74f573":{"x":{},"y":{},"colour":{},"label":{},"ymin":{},"ymax":{},"type":"scatter"}},"cur_data":"721251c74f573","visdat":{"721251c74f573":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
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
