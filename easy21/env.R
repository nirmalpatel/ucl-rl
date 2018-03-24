draw <- function(n = 1) {
  card_col <- sample(c('R', 'B'), size = n, replace = TRUE, prob = c(1/3, 2/3))
  card_num <- sample(1:10, size = n, replace = TRUE)
  
  paste0(card_col, card_num)
}

draw_black <- function(n = 1) {
  card_col <- 'B'
  card_num <- sample(1:10, size = n, replace = TRUE)
  
  paste0(card_col, card_num)
}

sum_of_cards <- function(x) {
  card_signs <- ifelse(substr(x, 1, 1) == "B", 1, -1)
  card_vals <- as.integer(substr(x, 2, nchar(x)))
  
  sum(card_signs * card_vals)
}

step <- function(game_state, player_act) {
  
  if (game_state$terminal == FALSE) {
    
    player_sum <- game_state$player_sum
    dealer_sum <- game_state$dealer_sum
    
    # player sticks
    if (player_act == "stick") {
      
      if (dealer_sum >= 1 & dealer_sum <= 16) {
        
        # dealer repeatedly hits if her sum is between 1 and 16
        # dealer hits until either her sum is between 17 and 21 or she goes bust
        
        repeat {
          
          dealer_sum <- dealer_sum + sum_of_cards(draw())
          
          if (dealer_sum >= 17 & dealer_sum <= 21) {
            
            # both player and dealer are sticking so we need to decide
            # who has won based on their sums
            
            if (player_sum > dealer_sum) {
              game_state$player_sum <- player_sum
              game_state$dealer_sum <- dealer_sum
              game_state$terminal <- TRUE
              return(list(next_state = game_state, reward = 1))
            } else if (player_sum < dealer_sum) {
              game_state$player_sum <- player_sum
              game_state$dealer_sum <- dealer_sum
              game_state$terminal <- TRUE
              return(list(next_state = game_state, reward = -1))
            } else if (player_sum == dealer_sum) {
              game_state$player_sum <- player_sum
              game_state$dealer_sum <- dealer_sum
              game_state$terminal <- TRUE
              return(list(next_state = game_state, reward = 0))
            } else {
              stop("Player sum and dealer sum are not getting compared!")
            }
            
          } else if (dealer_sum > 21 | dealer_sum < 1) {
            
            # dealer went bust
            
            game_state$player_sum <- player_sum
            game_state$dealer_sum <- dealer_sum
            game_state$terminal <- TRUE
            return(list(next_state = game_state, reward = 1))
          }
        }
        
      } else {
        
        # throw error for this unusual condition
        if (game_state$terminal != TRUE) {
          stop("If it is dealer's turn, her sum can't already be < 1 or > 16!")
        }
      }
      
    } else if (player_act == "hit") {
      
      # draw a card and check if player went bust
      player_sum <- player_sum + sum_of_cards(draw())
      
      if (player_sum > 21 | player_sum < 1) {
        game_state$player_sum <- player_sum
        game_state$dealer_sum <- dealer_sum
        game_state$terminal <- TRUE
        return(list(next_state = game_state, reward = -1))
      } else {
        game_state$player_sum <- player_sum
        game_state$dealer_sum <- dealer_sum
        game_state$terminal <- FALSE
        return(list(next_state = game_state, reward = 0))
      }
    } else {
      
      # throw error if invalid action
      stop("Invalid player action, it must be either stick or hit.")
    }
    
  } else if (game_state$terminal == TRUE) {
    
    # return same state if it is terminal
    return(list(next_state = game_state, reward = 0))
  } else {
    
    # throw error if terminal variable not set
    stop("terminal variable not set in the state.")
  }
}
