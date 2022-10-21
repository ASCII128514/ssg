library(foreach)

selectAction <- function(bparams, actions, state, rewards) {
  
  # get the current position of the object
  n <- bparams["width"]-bparams["obj_width"] + 1;
  m <- bparams["height"]-bparams["obj_height"] + 1;
  r <- ((state-1) %/% n)+1;
  column <- (state-1) %% n + 1;

  
  
  actions_cnt <- c(0,0,0,0,0);
  width <- bparams["obj_width"];
  height <- bparams["obj_height"];
  for (i in actions) {
    
    # only record actions of the agents under or next to the object
    d <- i - state;
    ri <- ((i-1) %/% n)+1;
    ci <- (i-1) %% n + 1;
    d1 <- ri - r;
    d2 <- ci - column;
    if (d1 >= 0 && d1 <= height && d2 >= 0 && d2 <= width) {
      actions_cnt[i] <- actions_cnt[i] + 1
    } else if ((d1 == -1 || d1 == 1) && d2 >= 0 && d2 <= width) {
      actions_cnt[i] <- actions_cnt[i] + 3
    } else if ((d2 == -1 || d2 == 1) && d1 >= 0 && d1 <= height) {
      actions_cnt[i] <- actions_cnt[i] + 3
    }
    
  }
  
  if (bparams["cancelation"]) {
    actions_cnt <- c(actions_cnt[1] - actions_cnt[2], actions_cnt[2] - actions_cnt[1],
                     actions_cnt[3] - actions_cnt[4], actions_cnt[4] - actions_cnt[3],
                     actions_cnt[5]);
  }
  
  vote <- max(actions_cnt);
  actions_highest_vote <- c(0,0,0,0,0);
  cnt <- 0;
  for (i in 1:5) {
    if (actions_cnt[i] == vote) {
      cnt <- cnt + 1;
      actions_highest_vote[cnt] <- i;
    }
  }
  
  actions_highest_vote <- actions_highest_vote[1:cnt];
  if (cnt == 1) {
    action <- actions_highest_vote[1];
  } else {
    action <- sample(actions_highest_vote,1);
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


  reward <- rewards[2];
  reached_goal <- FALSE;
  # check if it is at the goal state
  if (state == bparams["end_pos"]) {
    reward <- rewards[1];
    reached_goal <- TRUE;
  } else if ((column <= bparams["edge"]) || (column > (n-bparams["edge"]))) {
    reward <- rewards[3];
  }
  
  return(c(state, reward, reached_goal));
}