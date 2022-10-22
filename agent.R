library(purrr)
# epsilon greedy action selection
# returns an array of actions of the agents
epsilon_greedy_selection <- function(agents, state, epsilon){
  h <- dim(agents)[3];
  w <- dim(agents)[4];
  # num_of_agents = dim(agents)[3];
  actions = replicate(h*w, -1);
  

  temp <- runif(w*h);
  greedy <- which(temp >= epsilon);
  rand <- which(temp < epsilon);
  gr <- (greedy-1) %/% w + 1;
  gc <- (greedy - 1) %% w + 1;
  actions[greedy] <- map2_int(gr, gc, function(i, j) choose_best_action(agents, i,j, state));
  actions[rand] <- choose_random_action(length(rand));
  
  # for(i in 1:num_of_agents){
  #   pos <- runif(1);
  #   if(pos >= epsilon){
  #     actions[i] = choose_best_action(agents, i, state);
  #   }
  #   else{
  #     actions[i]= choose_random_action(agents);
  #   }
  # }
  dim(actions) <- c(h, w);
  return (actions);
}

# choosing random action based on the probability
choose_random_action <- function(n){
  return (sample(1:5, n, replace = TRUE));
}

# choosing the best action based on the q table
choose_best_action <- function(agents, i, j, state){
  q_table <- c(agents[state,,i,j])
  
    # print(c(i,j));
  return (sample(which(q_table==max(q_table)), 1));
}

# updating q table using q learning
update_q <- function(agents, reward, actions, prev_state, state, qparams){
  w <- dim(agents)[4];
  h <- dim(agents)[3];
  num_of_agents = dim(agents)[3] * dim(agents)[4];
  max_q = get_max_q(agents, state, h, w); # get the array of max values of the q table
  for(i in 1:h){
    for (j in 1:w) {
      current_q = agents[prev_state,actions[i,j],i, j];
      # print(c(typeof(qparams["alpha"]), typeof(qparams["gamma"]), dim(max_q)));
      agents[prev_state,actions[i, j],i, j] <- (current_q + (qparams["alpha"] *(reward + qparams["gamma"]*max_q[i,j] - current_q)));
    }
  }
  return(agents)
}

# get the array of max values of the q table
get_max_q <- function(agents, state, h,w){
  # max_q = replicate(num_of_agents, 1);
  seg <- agents[state,,,];
  dim(seg) <- c(5, h*w);
  max_q <- apply(seg,2, max);
  # max_q = map2_dbl(1:h, 1:w, function (i,j) max(agents[state,,1:end,1:end])); 
  dim(max_q) <- c(h,w);
  return (max_q);
}
