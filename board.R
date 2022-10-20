

selectAction <- function(bparams, actions, state) {
  
  # get the current position of the object
  n <- bparams["width"];
  m <- bparams["height"];
  r <- ((state-1) %/% n)+1;
  column <- (state-1) %% n + 1;
  
  
  if (bparams["cancelation"]) {
    
  } else {
    
  }
  
  if (action == 1) {
    # move up
    if (r < m) {
      r <- r + 1
    }
    
  } else if (action == 2) {
    # move down
    if (r > 1) {
      r <- r - 1
    }
  } else if (action == 3) {
    # move left
    if (column > 1) {
      column <- column - 1
    }
  } else if (action == 4) {
    # move right
    if (column < n) {
      column <- column + 1
    }
  }
  state <- column + (r - 1) * n;
  
  # determine the reward
  
  rewards <- bparams["rewards"];
  reward <- rewards[2];
  reached_goal <- FALSE;
  # check if it is at the goal state
  if (state == bparams["end_pos"]) {
    reward <- rewards[1];
    reached_goal <- TRUE;
  } else if ((column <= bparams["edge"]) || (column + bparams["obj_width"]-1 >= (n-r+1))) {
    reward <- rewards[3];
  }
  
  return(c(state, reward, reached_goal));
}