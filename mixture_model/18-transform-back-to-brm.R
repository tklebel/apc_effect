library(tidyverse)
library(cmdstanr)
library(brms)

# read in csv files from fit
wo_sa_files <- list.files("bayes_model/output_files/", pattern = "3c6c",
                          full.names = TRUE)
wo_sa <- rstan::read_stan_csv(wo_sa_files)

# prepare empty brms model
df <- read_csv("data/processed/multilevel_sample.csv")
wdi <- WDI::WDI_data$country %>%
  as_tibble() %>%
  select(iso2c, region, income)

base <- df %>%
  left_join(wdi, by = c("country_code" = "iso2c")) %>%
  select(id, institution_id, University, country, region, author_position,
         P_top10, field,
         APC_in_dollar, total_weight) %>%
  mutate(APC_in_dollar = case_when(is.na(APC_in_dollar) ~ 0,
                                   TRUE ~ APC_in_dollar)) %>%
  mutate(P_top10 = scale(log(P_top10), scale = FALSE))

# family
mix <- mixture(hurdle_lognormal(), hurdle_lognormal(), order = TRUE)

# formula
model_formula  <- bf(
  APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10|field) + (1 + P_top10|country),
  hu1 ~ 1 + P_top10 + (1 + P_top10|field) + (1 + P_top10|country),
  hu2 ~ 1 + P_top10 + (1 + P_top10|field) + (1 + P_top10|country),
  theta1 ~ 1 + (1|field)
)

# priors
priors_narrower <- c(
  prior(normal(5, .5), class = Intercept, dpar = "mu1"),
  prior(normal(7.5, .2), class = Intercept, dpar = "mu2"),
  prior(normal(0, 1), class = Intercept, dpar = "theta1"),
  prior(normal(-.5, 1), class = Intercept, dpar = "hu1"),
  prior(normal(.5, 1), class = Intercept, dpar = "hu2"),
  prior(normal(0, 1), class = b, dpar = "mu1"),
  prior(normal(0, 1), class = b, dpar = "mu2"),
  prior(normal(0, 1), class = b, dpar = "hu1"),
  prior(normal(0, 1), class = b, dpar = "hu2"),
  prior(normal(0, 1), class = sd, dpar = "mu1"),
  prior(normal(0, 1), class = sd, dpar = "mu2"),
  prior(normal(0, 1), class = sd, dpar = "hu1"),
  prior(normal(0, 1), class = sd, dpar = "hu2"),
  prior(normal(0, 1), class = sd, dpar = "theta1"),
  prior(normal(0, 1), class = sigma1),
  prior(normal(0, 1), class = sigma2),
  prior(lkj(4), class = cor)
)

subsample <- base %>%
  filter(country != "South Africa")

# create empty brm object
wo_sa_brm <- brms::brm(model_formula,
                            family = mix,
                            prior = priors_narrower,
                            data = subsample,
                            empty = TRUE)
# add back data from fitting
wo_sa_brm$fit <- wo_sa
wo_sa_brm <- rename_pars(wo_sa_brm)
summary(wo_sa_brm)

write_rds(wo_sa_brm, "bayes_model/final_models/17-wo-south-africa_brm.rds")

