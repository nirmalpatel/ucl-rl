library(tidyverse)
library(lattice)
library(stringr)
load("mc.Rdata")
load("sarsa.Rdata")

sapply(sarsa_action_vfuns, function(x) {
  c(n_ep = x$n_ep,
    lambda = x$lambda,
    err = mean((x$sarsa_action_vfun - mc_action_vfun) ^ 2))
}) %>%
  t() %>%
  as_tibble() %>%
  ggplot(aes(lambda, err)) +
  geom_line(aes(color = as.factor(n_ep))) +
  geom_point(aes(color = as.factor(n_ep)))
