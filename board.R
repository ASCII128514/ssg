# library(foreach)

selectAction <- function(bparams, actions, state, rewards, heats) {
  
  # get the current position of the object
  width <- bparams["obj_width"];
  height <- bparams["obj_height"];
  
  bw <- bparams["width"];
  bh <- bparams["height"];
  n <- bw - width + 1;
  m <- bh - height + 1;
  
  r <- ((state-1) %/% n)+1;
  column <- (state-1) %% n + 1;
  
  # change the dim of the actions
  
  # dim(actions) <- c(bh, bw);
  
  under <- actions[r:(r+height-1), column:(column+width-1)];
  under_cnt <- table(under);
  by <- actions[max(r-1, 1):min(r+height, bh), max(column-1,1):min(column + width, bw)]
  heats[max(r-1, 1):min(r+height, bh), max(column-1,1):min(column + width, bw)] <- 
    heats[max(r-1, 1):min(r+height, bh), max(column-1,1):min(column + width, bw)] + 1
  by_cnt = table(by);
  
  rlast <- dim(by)[1];
  clast <- dim(by)[2];
  # remove the edge cells.
  if (r > 1 && column > 1) {
    by_cnt[by[1,1]] <- by_cnt[by[1,1]] - 1;
    heats[r-1, column-1] <- heats[r-1, column-1] - 1;
  }
  if (r > 1 && column+ width <= bw) {
    by_cnt[by[1, clast]] <- by_cnt[by[1, clast]] - 1;
    heats[r-1, column+width] <- heats[r-1, column+width] - 1;
  }
  
  if (r + height <= bh && column > 1) {
    by_cnt[by[rlast,1]] <- by_cnt[by[rlast,1]] - 1;
    heats[r + height, column-1] <- heats[r + height, column-1] - 1;
  }
  if (r + height <= bh && column+ width <= bw) {
    by_cnt[by[rlast, clast]] <- by_cnt[by[rlast, clast]] - 1;
    heats[r + height, column+width] <- heats[r + height, column+width] - 1;
  }
  
  # add entries to under_cnt that is na
  actions_cnt <- map_int(1:5, function(x) {
    if (is.na(under_cnt[x])) {
      under_cnt[x] <- 0;
      if (is.na(by_cnt[x])) {
        by_cnt[x] <- 0;
      }
    }
    return(as.integer(3 * by_cnt[x] - 2* under_cnt[x]));
  } )
  #  
  # 
  # for (i in actions) {
  #   
  #   # only record actions of the agents under or next to the object
  #   d <- i - state;
  #   ri <- ((i-1) %/% n)+1;
  #   ci <- (i-1) %% n + 1;
  #   d1 <- ri - r;
  #   d2 <- ci - column;
  #   if (d1 >= 0 && d1 <= height && d2 >= 0 && d2 <= width) {
  #     actions_cnt[i] <- actions_cnt[i] + 1
  #   } else if ((d1 == -1 || d1 == 1) && d2 >= 0 && d2 <= width) {
  #     actions_cnt[i] <- actions_cnt[i] + 3
  #   } else if ((d2 == -1 || d2 == 1) && d1 >= 0 && d1 <= height) {
  #     actions_cnt[i] <- actions_cnt[i] + 3
  #   }
  #   
  # }
  
  if (bparams["cancelation"]) {
    actions_cnt <- c(actions_cnt[1] - actions_cnt[2], actions_cnt[2] - actions_cnt[1],
                     actions_cnt[3] - actions_cnt[4], actions_cnt[4] - actions_cnt[3],
                     actions_cnt[5]);
  }
  
  # ignore the stay option.
  
  vote <- which(actions_cnt[1:4] == max(actions_cnt[1:4]));
  # actions_highest_vote <- c(0,0,0,0,0);
  # cnt <- 0;
  # for (i in 1:5) {
  #   if (actions_cnt[i] == vote) {
  #     cnt <- cnt + 1;
  #     actions_highest_vote[cnt] <- i;
  #   }
  # }
  # 
  # actions_highest_vote <- actions_highest_vote[1:cnt];
  # if (cnt == 1) {
  #   action <- actions_highest_vote[1];
  # } else {
  #   action <- sample(actions_highest_vote,1);
  # }
  action <- sample(vote, 1);
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
  
  return(list("state"= state, "reward"= reward,"is_ended"= reached_goal, "heats"= heats));
}