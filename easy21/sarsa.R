library(tidyverse)
library(lattice)
library(stringr)
source("env.R")

# initialize value function

all_state_acts_df <- expand.grid(
  dsum = paste0("D", 1:10),
  psum = paste0("P", 1:21),
  act = c("stick", "hit")
)
all_state_acts <- paste0(all_state_acts_df$dsum, "_", all_state_acts_df$psum, "_", all_state_acts_df$act)

all_states_df <- expand.grid(
  dsum = paste0("D", 1:10),
  psum = paste0("P", 1:21)
)
all_states <- paste0(all_states_df$dsum, "_", all_states_df$psum)

n0 <- 100

state_str <- function(s) {
  paste0("D", s$dealer_sum, "_", "P", s$player_sum)
}

sarsa_action_vfuns <- list()

for (n_ep in c(1000, 5000, 10000)) {
  for (lambda in seq(.1, 1, by = .1)) {
    
    action_vfun <- setNames(rep(0.0, length(all_state_acts)), all_state_acts)
    state_vfun <- setNames(rep(0.0, length(all_states)), all_states)
    
    n_state_action <- setNames(rep(0, length(all_state_acts)), all_state_acts)
    n_state <- setNames(rep(0, length(all_states)), all_states)
    
    for (ep in 1:n_ep) {
      
      # run an episode
      ep_states <- c()
      ep_state_acts <- c()
      ep_eligibs <- setNames(rep(0, length(all_state_acts)), all_state_acts)
      
      # initialize state
      ep_state <- list(
        dealer_sum = sum_of_cards(draw_black()),
        player_sum = sum_of_cards(draw_black()),
        terminal = FALSE
      )
      
      # initialize action
      # fetch action values
      ep_act_values <- c("stick" = unname(action_vfun[paste0(state_str(ep_state), "_stick")]),
                         "hit" = unname(action_vfun[paste0(state_str(ep_state), "_hit")]))
      
      # pick random action with prob epsilon_t
      if (runif(1) < (n0 / (n0 + n_state[state_str(ep_state)] + 1))) {
        ep_act <- sample(c("stick", "hit"), 1)
      } else {
        ep_act <- c("stick", "hit")[nnet::which.is.max(ep_act_values)]  
      }
      
      # ep_state <- initial state
      # ep_act <- initial action
      
      repeat {
        
        # update N(s_t)
        ep_state_str <- state_str(ep_state)
        n_state[ep_state_str] <- n_state[ep_state_str] + 1
        # record state
        ep_states <- c(ep_states, ep_state_str)
        
        # update N(s_t, a_t) and store the state action pair
        ep_state_act_str <- paste0(ep_state_str, "_", ep_act)
        n_state_action[ep_state_act_str] <- n_state_action[ep_state_act_str] + 1
        # record state action pair
        ep_state_acts <- c(ep_state_acts, ep_state_act_str)
        
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
