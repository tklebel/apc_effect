library(tidyverse)
library(brms)
library(brmstools) # see how this can be replaced by tidybayes
library(tidybayes)

df <- read_csv("data/processed/multilevel_sample.csv")

# make model df
base <- df %>%
  select(institution_id, country, author_position, PP_top10,
         field = display_name,
         APC_in_dollar) %>%
  filter(!is.na(APC_in_dollar))

model_base <- base %>%
  mutate(PP_top10 = scale(log(PP_top10)),
         APC_in_dollar = scale(APC_in_dollar))

# start with model that incorporates all varying intercepts
m1 <- brm(APC_in_dollar ~ 1 + PP_top10 + (1|country) + (1|institution_id) +
            (1|author_position) + (1|field),
          family = "gaussian",
          prior = c(prior(normal(0, .5), class = Intercept),
                    prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma)),
          data = base,
          chains = 4, cores = 4,
          control = list(adapt_delta = .95),
          save_model = "bayes_model/m1.stan",
          file = "bayes_model/m1")
summary(m1)
ranef(m1)
launch_shinystan(m1)


# and now let intercepts vary by country and field
m2 <- brm(APC_in_dollar ~ 1 + PP_top10 + (1 + PP_top10|country) + (1|institution_id) +
            (1|author_position) + (1 + PP_top10|field),
          family = "gaussian",
          prior = c(prior(normal(0, .5), class = Intercept),
                    prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma),
                    prior(lkj(2), class = cor)),
          data = base,
          warmup = 2000, iter = 3000, chains = 4, cores = 4,
          control = list(adapt_delta = .98),
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
# impact of PP_top10
#
# Overall much more variance between fields than countries.
#
# overall positive effect of PP_top10. "negative" values within fields and
# countries need to be taken into context here
#
# todos:
# - rerun with higher adapt_delta and longer warm-up
# - visualise draws for fields and for countries -> there maybe link to regions
# (income or continent)
# - check with a naive model that models PP_top10 simply per country (no pooling)
# -

# look at visualising
res <- as_draws_df(m2, variable = "PP_top10", regex = TRUE)

forest(m2, "field") # very interesting
ggsave("bayes_model/plots/random_effect_fields.png", width = 14, height = 6)
# this shows the effects after combining all parts, i.e., including group and
# population level effects
forest(m2, "country")

# here try to find an equivalent with tidybayes

# general parameter
res %>%
  ggplot(aes(b_PP_top10)) +
  geom_density()

conditional_fields <- fields %>%
  select(field = display_name_indicator)

conditional_effects(m2, conditions = conditional_fields)

waic(m1, m2)


# issues with the data: high numbers of 0 values
# this might help a lot: https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/
