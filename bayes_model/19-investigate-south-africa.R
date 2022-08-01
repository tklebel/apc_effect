# The issue: without data from SA, the model fits fine, but with, chains do not
# converge.
# We therefore investigate the data from SA, to find any anomalies that could
# lead to this behaviour.
# A first suspicion from checking the model paramters from the failed model
# point towards different rates of APCs that are zero, which affect the hurdle
# component.
# Alternativel, the key crux of the model (theta) could be an issue, with a
# substantially different distribution of high and low APCs in SA than in other
# countries.


library(tidyverse)
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
  mutate(P_top10 = scale(log(P_top10), scale = FALSE),
         is_sa = country == "South Africa")

# rate of zero APC -----
base %>%
  group_by(is_sa) %>%
  summarise(n = n(),
            n_zero = sum(APC_in_dollar == 0),
            frac = n_zero / n)
# no difference without taking weights into account

base %>%
  group_by(is_sa) %>%
  summarise(n = sum(total_weight),
            n_zero = sum((APC_in_dollar == 0) * total_weight),
            frac = n_zero / n)
# some difference, but this does not seem substantial

base %>%
  group_by(country) %>%
  summarise(n = sum(total_weight),
            n_zero = sum((APC_in_dollar == 0) * total_weight),
            frac = n_zero / n)
# this confirms the above finding: there is a lot of variation across countries
# in the rate of zero-APCs - SA is in the lower region, but in no way an outlier,
# ergo this cannot be the root of the issue.


# zero APC by field in SA compared to rest ----
base %>%
  group_by(is_sa, field) %>%
  summarise(n = sum(total_weight),
            n_zero = sum((APC_in_dollar == 0) * total_weight),
            frac = n_zero / n) %>%
  mutate(origin = if_else(is_sa, "SA", "nonSA")) %>%
  ungroup() %>%
  select(-is_sa, -n, -n_zero) %>%
  pivot_wider(names_from = "origin", values_from = "frac") %>%
  mutate(diff = nonSA - SA)
# these rates differ quite a bit to the overall average. however, do they differ
# more than in other countries?

pdata <- base %>%
  group_by(country, field) %>%
  summarise(n = sum(total_weight),
            n_zero = sum((APC_in_dollar == 0) * total_weight),
            frac = n_zero / n)

pdata %>%
  ggplot(aes(frac, fct_reorder(field, frac))) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(alpha = .5, height = .1) +
  geom_point(data = filter(pdata, country == "South Africa"), colour = "red")
# as seen above, SA is quite low in some fields (philosophy, history, art),
# which is not the norm, but it is in no case the only outlier.


# distribution of APC by field in SA vs rest -----
# given that the above did not give clear indication that SA is a strong outlier
# we now probe the distribution of nonzero APCs
base %>%
  filter(APC_in_dollar > 0) %>%
  ggplot(aes(APC_in_dollar, field, fill = is_sa)) +
  ggridges::geom_density_ridges(alpha = .5)
# again, there are some differences, but nothing outrageous. Engineering,
# Business, Physics, Philosophy, Psychology and computer science are those
# fields with the biggest differences. But: is this really the cause?
# Hard to say why these differences should disturb the model that much.

# The script "17-adapted-mixture_only_sa.R" runs the model only for SA -
# to investigate whether this converges, and if yes/no, how model parameters
# look like (which have high r-hat, low ESS)
