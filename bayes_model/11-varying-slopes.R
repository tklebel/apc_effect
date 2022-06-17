library(tidyverse)
library(brms)
library(brmstools)

df <- read_csv("data/processed/multilevel_sample.csv")

# make model df
base <- df %>%
  select(institution_id, country, author_position, PP_top10, display_name,
         APC_in_dollar) %>%
  filter(!is.na(APC_in_dollar))


mdf <- base %>%
  # calculate indicator variables
  mutate(across(c(institution_id, country, author_position, display_name),
                ~as.factor(.x) %>% as.numeric(), .names = "{.col}_indicator"))

countries <- mdf %>%
  select(contains("country")) %>%
  distinct() %>%
  arrange(country_indicator)
countries

fields <- mdf %>%
  select(contains("display")) %>%
  distinct() %>%
  arrange(display_name_indicator)

dlist <- list(
  country = mdf$country_indicator,
  institute = mdf$institution_id_indicator,
  field = mdf$display_name,
  author_position = mdf$author_position_indicator,
  inst_res = scale(mdf$PP_top10),
  apc_price = scale(mdf$APC_in_dollar)
)

# start with model that incorporates all varying intercepts
m1 <- brm(apc_price ~ 1 + inst_res + (1|country) + (1|institute) +
            (1|author_position) + (1|field),
          family = "gaussian",
          prior = c(prior(normal(0, .5), class = Intercept),
                    prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma)),
          data = dlist,
          chains = 4, cores = 4,
          control = list(adapt_delta = .85),
          save_model = "bayes_model/m1.stan",
          file = "bayes_model/m1")
summary(m1)
ranef(m1)
launch_shinystan(m1)


# and now let intercepts vary by country and field
m2 <- brm(apc_price ~ 1 + inst_res + (1 + inst_res|country) + (1|institute) +
            (1|author_position) + (1 + inst_res|field),
          family = "gaussian",
          prior = c(prior(normal(0, .5), class = Intercept),
                    prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma),
                    prior(lkj(2), class = cor)),
          data = dlist,
          warmup = 1000, iter = 2000, chains = 4, cores = 4,
          control = list(adapt_delta = .95),
          save_model = "bayes_model/m2.stan",
          file = "bayes_model/m2")
# 76 divergent transitions,
# maybe increase adapt delta even a little further, and potentially warm up
# longer
summary(m2)
ranef(m2)

# very interesting: negative correlation between intercept and pptop10 in
# countries: so for countries paying high APCs, pptop has a negative effect
# but positive correlation for fields: low APC, low effect, high APC, high
# impact of inst_res
#
# Overall much more variance between fields than countries.
#
# overall positive effect of inst_res. "negative" values within fields and
# countries need to be taken into context here
#
# todos:
# - rerun with higher adapt_delta and longer warm-up
# - visualise draws for fields and for countries -> there maybe link to regions
# (income or continent)
# - check with a naive model that models inst_res simply per country (no pooling)
# -

# look at visualising
res <- as_draws_df(m2, variable = "inst_res", regex = TRUE)

forest(m2, "field") # very interesting
ggsave("bayes_model/plots/random_effect_fields.png", width = 14, height = 6)
# this shows the effects after combining all parts, i.e., including group and
# population level effects
forest(m2, "country")

# general parameter
res %>%
  ggplot(aes(b_inst_res)) +
  geom_density()

conditional_fields <- fields %>%
  select(field = display_name_indicator)

conditional_effects(m2, conditions = conditional_fields)

waic(m1, m2)
