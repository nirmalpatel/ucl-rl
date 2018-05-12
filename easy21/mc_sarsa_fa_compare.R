library(tidyverse)
library(lattice)
library(stringr)
load("mc.Rdata")
load("sarsa_fa.Rdata")

sapply(sarsa_action_vfuns_fa, function(x) {
  
  all_nm <- names(mc_action_vfun)
  
  c(n_ep = x$n_ep,
    lambda = x$lambda,
    err = mean((x$sarsa_action_vfun_fa[all_nm] - mc_action_vfun[all_nm]) ^ 2))
}) %>%
  t() %>%
  as_tibble() %>%
  ggplot(aes(lambda, err)) +
  geom_line(aes(color = as.factor(n_ep))) +
  geom_point(aes(color = as.factor(n_ep)))
