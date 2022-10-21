library(purrr)
# epsilon greedy action selection
# returns an array of actions of the agents
epsilon_greedy_selection <- function(agents, state, epsilon){
  num_of_agents = dim(agents)[3];
  actions = replicate(num_of_agents, -1);
  
  for(i in 1:num_of_agents){
    pos <- runif(1);
    if(pos >= epsilon){
      actions[i] = choose_best_action(agents, i, state);
    }
    else{
      actions[i]= choose_random_action(agents);
    }
  }
  return (actions);
}

# choosing random action based on the probability
choose_random_action <- function(agents){
  return (sample(1:dim(agents)[2], 1));
}

# choosing the best action based on the q table
choose_best_action <- function(agents, agent, state){
  q_table <- c(agents[state,,agent])
  return (sample(which(q_table==max(q_table)),1));
}

# updating q table using q learning
update_q <- function(agents, reward, actions, prev_state, state, qparams){
  num_of_agents = dim(agents)[3];
  max_q = get_max_q(agents, state); # get the array of max values of the q table
  for(i in 1:num_of_agents){
    current_q = agents[prev_state,actions[i],i];
    # print(c(typeof(qparams["alpha"]), typeof(qparams["gamma"]), dim(max_q)));
    agents[prev_state,actions[i],i] <- (current_q + (qparams["alpha"] *(reward + qparams["gamma"]*max_q[1] - current_q)));
  }
  return(agents)
}

# get the array of max values of the q table
get_max_q <- function(agents, state){
  num_of_agents = dim(agents)[3];
  # max_q = replicate(num_of_agents, 1);
  max_q = map_dbl(1:num_of_agents, function (x) max(agents[state,,x])); 
  # print(max_q);
  return (max_q);
}
