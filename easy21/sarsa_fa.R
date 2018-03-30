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
            act %in% c("hit", "stick"))
  
  dcodevals <- c(if_else(between(dealer_sum, 1, 4), 1, 0),
              if_else(between(dealer_sum, 4, 7), 2, 0),
              if_else(between(dealer_sum, 7, 10), 3, 0))
  
  pcodevals <- c(if_else(between(dealer_sum, 1, 6), 1, 0),
              if_else(between(dealer_sum, 4, 9), 2, 0),
              if_else(between(dealer_sum, 7, 12), 3, 0),
              if_else(between(dealer_sum, 10, 15), 4, 0),
              if_else(between(dealer_sum, 13, 18), 5, 0),
              if_else(between(dealer_sum, 16, 21), 6, 0))
  
  acodevals <- c(if_else(act == "hit", 1, 0),
              if_else(act == "stick", 2, 0))
  
  featvals <- filter(all_codes_df, dcode %in% dcodevals, pcode %in% pcodevals, acode %in% acodevals)$feat
  
  x_s <- rep(0, 36)
  x_s[featvals] <- 1
  x_s
}

feature_vec(7, 10, "hit")

alphaval <- 0.01
epsval <- 0.05

# theta here is the big theta or the parameter vector
sarsa_action_vfun_thetas <- list()

for (n_ep in c(10000)) {
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
      
      # ep_step_feats <- features for the current step of the episodes
      ep_feats <- feature_vec(ep_state$dealer_sum, ep_state$player_sum, ep_act)
      
      repeat {
        
        # take a step
        outcome <- step(ep_state, ep_act)
        
        # look at outcomes  
        ep_next_state <- outcome$next_state
        
        if (ep_next_state$terminal == TRUE) {
          # for terminal case
          
          # update delta
          # delta <- R + Q(S', A') - Q(S, A)
          # for terminal S', Q(S', x) = 0
          delta <- outcome$reward + 0 - action_vfun[ep_state_act_str]
          
          # update eligib
          # E(S, A) <- E(S, A) + 1
          ep_eligibs[ep_state_act_str] <- ep_eligibs[ep_state_act_str] + 1
          
        } else {
          # decide next action
          ep_next_state_str <- state_str(ep_next_state)
          
          # fetch action values
          ep_next_act_values <- c("stick" = unname(action_vfun[paste0(ep_next_state_str, "_stick")]),
                                  "hit" = unname(action_vfun[paste0(ep_next_state_str, "_hit")]))
          
          # pick random action with prob epsilon_t
          if (runif(1) < (n0 / (n0 + n_state[ep_next_state_str]))) {
            ep_next_act <- sample(c("stick", "hit"), 1)
          } else {
            ep_next_act <- c("stick", "hit")[nnet::which.is.max(ep_next_act_values)]  
          }
          
          ep_next_state_act_str <- paste0(ep_next_state_str, "_", ep_next_act)
          
          # update delta
          # delta <- R + Q(S', A') - Q(S, A)
          delta <- outcome$reward + action_vfun[ep_next_state_act_str] - action_vfun[ep_state_act_str]
          
          # update eligib
          # E(S, A) <- E(S, A) + 1
          ep_eligibs[ep_state_act_str] <- ep_eligibs[ep_state_act_str] + 1
        }
        
        # update action values for all state action pairs
        for (state_act_pair in unique(ep_state_acts)) {
          
          # Q(s, a) <- Q(s,a) + alpha * delta * E(s,a)
          action_vfun[state_act_pair] <- action_vfun[state_act_pair] +
            (1 / n_state_action[state_act_pair]) * delta * ep_eligibs[state_act_pair]
          
          # E(s,a) <- gamma * lambda * E(s, a)
          ep_eligibs[state_act_pair] <- lambda * ep_eligibs[state_act_pair]
        }
        
        if (ep_next_state$terminal == TRUE) {
          break
        } else {
          ep_state <- ep_next_state
          ep_act <- ep_next_act
        }
      }
    }
    
    sarsa_action_vfun <- action_vfun
    
    sarsa_action_vfun_rdf <- tibble(
      state_act = names(action_vfun),
      val = unname(action_vfun)
    )
    
    sarsa_action_vfun_df <-  sarsa_action_vfun_rdf %>%
      mutate(dealer = as.integer(str_match(state_act, "D([0-9]{1,2})_")[, 2]),
             player = as.integer(str_match(state_act, "_P([0-9]{1,2})_")[, 2]),
             act = if_else(str_detect(state_act, "stick"), "stick", "hit")) %>%
      group_by(dealer, player) %>%
      summarise(maxval = max(val)) %>%
      ungroup()
    
    sarsa_action_vfuns[[length(sarsa_action_vfuns) + 1]] <- list(
      n_ep = n_ep,
      lambda = lambda,
      sarsa_action_vfun = sarsa_action_vfun,
      sarsa_action_vfun_rdf = sarsa_action_vfun_rdf,
      sarsa_action_vfun_df = sarsa_action_vfun_df
    )
  }
}

save(sarsa_action_vfuns, file = "sarsa.Rdata")
