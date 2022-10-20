library(parallel)
# define the parameters for the learning

# this parameters are for Q Learning
qparams <- c(
  "epsilon" = 0.01,
  "epsilon_greedy?" = TRUE
  
)

# this parameters are for the game board settings
bparams <- c(
  "width" = 7,
  "height" = 5,
  "cancelation" = TRUE,
  "rewards" = c(9, -1, -51),
  "edge" = 1,
  "start_pos" = 1,
  "end_pos" = 5,
  "obj_height" = 2,
  "obj_width" = 3,
  "num_trial" = 1000,
  "num_step" = 100000,
  "num_simulation" = 1000
)


init_agent <- function(num_of_states) {
  return(array(unif(1, -1, 1), dim=c(num_of_states, 5)))
}

simulation <- function(qparams, bparams) {
  # create a board of the given size.
  board_size <- bparams["height"] * bparams["width"];
  
  # initialize all q tables for different agents.
  num_of_states <- (bparams["height"] - bparams["obj_height"] + 1) * (bparams["width"] - bparams["obj_width"] + 1);
  agents <- replicate(board_size, init_agent(num_of_states));
  
  steps <- bparams["num_step"];
  trials <- bparams["num_trial"];

  for step in c(1:trials) {
    
  }
}