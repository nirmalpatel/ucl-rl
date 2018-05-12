library(tidyverse)
library(lattice)
library(stringr)
source("env.R")

# initialize value function

all_codes_df <- expand.grid(
  dcode = 1:3,
  pcode = 1:6,
  acode = 1:2
) %>%
  mutate(feat = row_number())

feature_vec <- function(dealer_sum, player_sum, act) {
  
  stopifnot(between(dealer_sum, 1, 10),
            between(player_sum, 1, 21),
            act %in% c("stick", "hit"))
  
  dcodevals <- c(if_else(between(dealer_sum, 1, 4), 1, 0),
              if_else(between(dealer_sum, 4, 7), 2, 0),
              if_else(between(dealer_sum, 7, 10), 3, 0))
  
  pcodevals <- c(if_else(between(player_sum, 1, 6), 1, 0),
              if_else(between(player_sum, 4, 9), 2, 0),
              if_else(between(player_sum, 7, 12), 3, 0),
              if_else(between(player_sum, 10, 15), 4, 0),
              if_else(between(player_sum, 13, 18), 5, 0),
              if_else(between(player_sum, 16, 21), 6, 0))
  
  acodevals <- c(if_else(act == "stick", 1, 0),
              if_else(act == "hit", 2, 0))
  
  featvals <- filter(all_codes_df, dcode %in% dcodevals, pcode %in% pcodevals, acode %in% acodevals)$feat
  
  x_s <- rep(0, 36)
  x_s[featvals] <- 1
  x_s
}

feature_vec(7, 10, "hit")

alphaval <- 0.01
epsval <- 0.05

# theta here is the big theta or the parameter vector
sarsa_action_vfuns_fa <- list()

for (n_ep in c(1000, 5000, 10000)) {
  for (lambda in seq(.1, 1, by = .1)) {
    
    action_vfun_theta <- rep(0, 36)

    for (ep in 1:n_ep) {
      
      # run an episode
      ep_feats <- rep(0, 36)
      ep_eligibs <- rep(0, 36)
      
      # initialize state
      ep_state <- list(
        dealer_sum = sum_of_cards(draw_black()),
        player_sum = sum_of_cards(draw_black()),
        terminal = FALSE
      )
      
      # initialize action
      # fetch action values
      ep_act_values <- c("stick" = sum(feature_vec(ep_state$dealer_sum, ep_state$player_sum, "stick") * action_vfun_theta),
                         "hit" = sum(feature_vec(ep_state$dealer_sum, ep_state$player_sum, "hit") * action_vfun_theta))
      
      # pick random action with prob epsilon_t
      if (runif(1) < epsval) {
        ep_act <- sample(c("stick", "hit"), 1)
      } else {
        ep_act <- c("stick", "hit")[nnet::which.is.max(ep_act_values)]  
      }
      
      repeat {
        # build features
        ep_feats <- feature_vec(ep_state$dealer_sum, ep_state$player_sum, ep_act)
        
        # take a step
        outcome <- step(ep_state, ep_act)
        
        # look at outcomes  
        ep_next_state <- outcome$next_state
        
        if (ep_next_state$terminal == TRUE) {
          # for terminal case
          
          # update delta
          # delta <- R + Q(S', A') - Q(S, A)
          # for terminal S', Q(S', x) = 0
          delta <- outcome$reward + 0 - sum(ep_feats * action_vfun_theta)
          
          # update eligib
          # E_t <- gamma * lambda * E_(t-1) + grad(Q(S,A))
          ep_eligibs <- lambda * ep_eligibs + ep_feats
          
        } else {
          # decide next action
          # fetch action values
          ep_next_act_values <- c("stick" = sum(feature_vec(ep_next_state$dealer_sum, ep_next_state$player_sum, "stick") * action_vfun_theta),
                                  "hit" = sum(feature_vec(ep_next_state$dealer_sum, ep_next_state$player_sum, "hit") * action_vfun_theta))
          
          # pick random action with prob epsilon_t
          if (runif(1) < epsval) {
            ep_next_act <- sample(c("stick", "hit"), 1)
          } else {
            ep_next_act <- c("stick", "hit")[nnet::which.is.max(ep_next_act_values)]  
          }
          
          ep_next_state_act_feats <- feature_vec(ep_next_state$dealer_sum, ep_next_state$player_sum, ep_next_act)
          
          # update delta
          # delta <- R + Q(S', A') - Q(S, A)
          delta <- outcome$reward + sum(ep_next_state_act_feats * action_vfun_theta) - sum(ep_feats * action_vfun_theta)
          
          # update eligib
          # E_t <- gamma * lambda * E_(t-1) + grad(Q(S,A))
          ep_eligibs <- lambda * ep_eligibs + ep_feats
        }
        
        # update theata
        action_vfun_theta <- action_vfun_theta + (alphaval * delta * ep_eligibs)
        
        if (ep_next_state$terminal == TRUE) {
          break
        } else {
          ep_state <- ep_next_state
          ep_act <- ep_next_act
        }
      }
    }
    
    sarsa_action_vfun_theta <- action_vfun_theta
    
    sarsa_action_vfun_fa <- vector("numeric")
    
    for (dealer_sum in 1:10) {
      for (player_sum in 1:21) {
        for (player_act in c("stick", "hit")) {
          sarsa_action_vfun_fa[paste0("D", dealer_sum, "_", "P", player_sum, "_", player_act)] <-
            sum(feature_vec(dealer_sum, player_sum, player_act) * sarsa_action_vfun_theta)
        }
      }
    }
    
    sarsa_action_vfun_fa_rdf <- tibble(
      state_act = names(sarsa_action_vfun_fa),
      val = unname(sarsa_action_vfun_fa)
    )
    
    sarsa_action_vfun_fa_df <-  sarsa_action_vfun_fa_rdf %>%
      mutate(dealer = as.integer(str_match(state_act, "D([0-9]{1,2})_")[, 2]),
             player = as.integer(str_match(state_act, "_P([0-9]{1,2})_")[, 2]),
             act = if_else(str_detect(state_act, "stick"), "stick", "hit")) %>%
      group_by(dealer, player) %>%
      summarise(maxval = max(val)) %>%
      ungroup()
    
    sarsa_action_vfuns_fa[[length(sarsa_action_vfuns_fa) + 1]] <- list(
      n_ep = n_ep,
      lambda = lambda,
      sarsa_action_vfun_fa = sarsa_action_vfun_fa,
      sarsa_action_vfun_fa_rdf = sarsa_action_vfun_fa_rdf,
      sarsa_action_vfun_fa_df = sarsa_action_vfun_fa_df
    )
  }
}

save(sarsa_action_vfuns_fa, file = "sarsa_fa.Rdata")