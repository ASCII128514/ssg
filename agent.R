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
  return (floor(runif(1, min=0, max=dim(agents)[2])));
}

# choosing the best action based on the q table
choose_best_action <- function(agents, agent, state){
  q_table <- c(agents[state,,agent])
  return (which(q_table==max(q_table)));
}

# updating q table using q learning
update_q <- function(agents, reward, action, prev_state, state, qparams){
  num_of_agents = dim(agents)[3];
  max_q = get_max_q(agents, state); # get the array of max values of the q table
  for(i in 1:num_of_agents){
    current_q = agents[prev_state,action,i];
    agents[prev_state,action,i] = current_q + qparams["alpha"] *(reward + qparams["gamma"]*max_q[i] - current_q);
  }
}

# get the arraya of max values of the q table
get_max_q <- function(agents, state){
  num_of_agents = dim(agents)[3];
  max_q = replicate(num_of_agents, NULL);
  for(i in 1:num_of_agents){
    max_q[i] = max(agents[state,,i]); 
  }
  return (max_q);
}
