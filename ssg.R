library(parallel)

source("board.R")
source("agent.R")
# define the parameters for the learning

# this parameters are for Q Learning
qparams <- c(
  "epsilon" = 0.01,
  "epsilon_greedy?" = TRUE,
  "gamma" = 0.9,
  "alpha" = 0.1
  
)

# this parameters are for the game board settings
bparams <- c(
  "width" = 9,
  "height" = 9,
  "cancelation" = T,
  "edge" = 1,
  "start_pos" = 4,
  "end_pos" = 53,
  "obj_height" = 2,
  "obj_width" = 3,
  "num_trial" = 1000,
  "num_step" = 1000,
  "num_simulation" = 1000
)

rewards <- c(100,-1,-50);

init_agent <- function(num_of_states) {
  return(matrix(runif(num_of_states * 5, -1, 1), nrow=num_of_states, ncol=5))
}

simulation <- function(qparams, bparams) {
  # create a board of the given size.
  board_size <- bparams["height"] * bparams["width"];
  
  # initialize all q tables for different agents.
  num_of_states <- (bparams["height"] - bparams["obj_height"] + 1) * (bparams["width"] - bparams["obj_width"] + 1);
  agents <- replicate(bparams["width"], replicate(bparams["height"], init_agent(num_of_states)));
  steps <- bparams["num_step"];
  trials <- bparams["num_trial"];
  for (trial in 1:trials) {
    state <- bparams["start_pos"];
    prev_state <- state;
    for (step in 1:steps) {
      # get all actions
      # print(step);
      actions <- epsilon_greedy_selection(agents, state, qparams["epsilon"]);
      # print(dim(actions))
      decision <- selectAction(bparams, actions, state, rewards);
      reward <- decision[2];
      prev_state <- state;
      state <- decision[1];
      # print(state)
      is_ended <- decision[3];
      
      agents <- update_q(agents, reward, actions, prev_state, state, qparams)
      # update 
      if (is_ended) {
        print(step);
        # print(agents - agent2);
        # update the q table, break to the next iteration
        break;
      }
    }
  }
}

