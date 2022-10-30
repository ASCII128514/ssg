source("board.R")
source("agent.R")


# TODO:
#   1. Add techniques for storing data while running the simulation
#   2. Add Softmax
#   3. If the runtime is slow, add parallel code
#   4. If it is still slow, port everything to python using tenserflow

# define the parameters for the learning

# this parameters are for Q Learning
qparams <- c(
  "epsilon" = 0.01,
  "epsilon_greedy?" = TRUE,
  "gamma" = 0.9,
  "alpha" = 0.1
  
)

state_trace <- data.frame(matrix(ncol = 10000, nrow = 0))

# this parameters are for the game board settings
bparams <- c(
  "width" = 7,
  "height" = 7,
  "cancelation" = T,
  "edge" = 1,
  "start_pos" = 3,
  "end_pos" = 28,
  "obj_height" = 2,
  "obj_width" = 3,
  "num_trial" = 1000,
  "num_step" = 10000,
  "num_simulation" = 10
)

rewards <- c(100,-1,-50);

init_agent <- function(num_of_states) {
  return(matrix(runif(num_of_states * 5, -1, 1), nrow=num_of_states, ncol=5))
}

# return a list of heat map, reinforcement, steps needed
simulation <- function(qparams, bparams, sim, sim_num) {
  # create a board of the given size.
  board_size <- bparams["height"] * bparams["width"];
  
  cnt <- nrow(sim)+1;
  
  # initialize all q tables for different agents.
  num_of_states <- (bparams["height"] - bparams["obj_height"] + 1) * (bparams["width"] - bparams["obj_width"] + 1);
  agents <- replicate(bparams["width"], replicate(bparams["height"], init_agent(num_of_states)));
  steps <- bparams["num_step"];
  trials <- bparams["num_trial"];
  
  for (trial in 1:trials) {
    print(trial)
    heats <- matrix(0, nrow = bparams["height"], ncol = bparams["width"]);
    states <- rep(-1, 10000);
    state <- bparams["start_pos"];
    prev_state <- state;
    reinforcement <- 0;
    step_cnt <- -1
    for (step in 1:steps) {
      # get all actions
      # print(step);
      actions <- epsilon_greedy_selection(agents, state, qparams["epsilon"]);
      # print(dim(actions))
      prev_state <- state;
      # list[state, reward, is_ended, heats]
      decision<- selectAction(bparams, actions, state, rewards, heats);
      # reward <- decision[2];
      
      state <- decision$state;
      reward <- decision$reward;
      reinforcement <- reinforcement + reward;
      is_ended <- decision$is_ended;
      # update the correct heat values.
      heats <- decision$heats;
      
      agents <- update_q(agents, reward, actions, prev_state, state, qparams)
      # update 
      if (is_ended) {
        step_cnt <- step
        # print(agents - agent2);
        # update the q table, break to the next iteration
        break;
      }
    }
    
    print(c(trial, step_cnt));
    dim(heats) <- board_size;
    sim[cnt,] <- c(sim_num, trial, step_cnt, reinforcement, heats);
    cnt <- cnt + 1;
  }
  return(sim)
}


run_sim <- function(qparams, bparams){
  # one for recording all the heats, update after a trial is over.
  # /boardsizei/heats_k.csv
  # 1+ refers to the simulation number and trial number, and reinforcement
  
  for (i in 1:bparams["num_simulation"]) {
    cat("Simulation:", i);
    sim <- data.frame(matrix(0, nrow = 1,ncol = 4+bparams["height"] * bparams["width"]))
    sim <- simulation(qparams, bparams, sim, i);
    write.table(sim, paste("./data/",bparams["height"], "_" ,bparams["width"], ".csv", sep=""), row.names = FALSE, append=TRUE, col.names=FALSE);
  }
  
  
}

run <- function() {
  if (!dir.exists("./data")) {
    dir.create("data")
  }
  run_sim(qparams, bparams)
}
