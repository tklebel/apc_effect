library(tidyverse)
library(rethinking)

set.seed(123)

# simulate 1000 researchers
n <- 1000

# start by drawing randomly distributed resources
df <- tibble(
  inst_resources = rnorm(n)
)

# sample institutional prestige as strongly dependant on resources
df <- df %>%
  mutate(inst_prestige = rnorm(n, inst_resources * .8))


# case 1: weak relationship between inst_resources and paper quality
# case 2: strong relationship between inst_resources and paper quality
df <- df %>%
  mutate(paper_quality_case_1 = rnorm(n, inst_resources * .1),
         paper_quality_case_2 = rnorm(n, inst_resources * .6))


# APC price: model with two journals, high and low
df <- df %>%
  mutate(
    apc_high_case_1 = rbernoulli(
      n,
      1/(1 + exp(0 - .5 * inst_resources + .3 * paper_quality_case_1))
    ),
    apc_high_case_2 = rbernoulli(
      n,
      1/(1 + exp(0 - .5 * inst_resources + .3 * paper_quality_case_2))
    )
  )

# model directly with resources
m1.1  <- quap(
  alist(
    apc_high_case_1 ~ dbinom(1, p),
    logit(p) <- a + br_r*inst_resources,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1)
  ), data = df)
precis(m1.1)

m1.2  <- quap(
  alist(
    apc_high_case_2 ~ dbinom(1, p),
    logit(p) <- a + br_r*inst_resources,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1)
  ), data = df)
precis(m1.2)
# a stronger effect of resources on paper quality leads to bias in measurement

# measurement error model

