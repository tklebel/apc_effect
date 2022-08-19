library(tidyverse)
library(brms)
library(cmdstanr)

options(mc.cores = 4)

df <- read_csv("data/processed/multilevel_sample_large.csv")
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

fitting_data <- make_standata(model_formula,
                              data = base,
                              family = mix,
                              prior = priors_narrower,
                              internal = TRUE)

empty_mod <- brm(model_formula,
                 family = mix,
                 prior = priors_narrower,
                 data = base,
                 empty = TRUE)

message("Compiling model code.")
mod <- cmdstan_model(stan_file = "bayes_model/17-adapted-mixture.stan",
                     stanc_options = list("O1"))

message("Start sampling.")
fit <- mod$sample(
  data = fitting_data,
  seed = 789,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 20,
  init = 0,
  adapt_delta = .9
)

message("Saving model to file.")

# saving to brm model directly
stanfit <- rstan::read_stan_csv(fit$output_files())
empty_mod$fit <- stanfit
wo_sa_brm <- rename_pars(empty_mod)
write_rds(wo_sa_brm,
          "bayes_model/final_models/17-brm-large-sample-3.rds")
message("File saved.")

