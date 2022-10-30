library(ggplot2)
# fls <- c(
#   7,7,9,7,11,7,13,7,15,7,17,7,19,7,21,7,23,7,25,7
# )

fls <- c(
  7,7
)


dim(fls) <- c(2, length(fls)/2);


# reinforcement over steps

# number of trials to converge



convergence <- data.frame(matrix(0, nrow = 0, ncol = 250))
reinforcements <- data.frame(matrix(0, nrow = 0, ncol = 1000))
for (ind in 1:dim(fls)[2]) {
  fname <- paste("./data/", fls[1,ind],"_" , fls[2,ind], '.csv', sep="");
  df <- read.table(file = fname, header=F, sep=" ");
  rf <- subset(df, V1 > 0 & V1 < 251, select = c(V1, V2, V3, V4));
  
  optimal <- fls[1, ind] - 2;
  convs <- replicate(250, 0);
  reinforcement <- vector("integer", 1000);
  for (i in 1:250) {
    # for every simulation, compute the steps needed for convergence
    s <- subset(rf, V1 == i);
    reinforcement <- reinforcement + s[, 4];
    
    conv <- 1;
    for (j in 1000:1) {
      if (s[j, 3] != optimal) {
        conv <-  j + 1;
        break;
      }
    }
    
    convs[i] = conv;
  }
  
  convergence[dim(convergence)[1]+1, ] <- convs;
  reinforcement <- reinforcement/250;
  reinforcements[dim(reinforcements)[1]+1, ] <- reinforcement;
}


df2 <- data.frame(size=integer(), re=double());

# dim(convergence) <- length(convergence);

s <- replicate(250, seq(5, 25, 2)); 
dim(s) <- length(c);
df2$re <- array(convergence);
df2$size <- s;
# create a violin graph for the convergence graph

p <- ggplot(df2, aes(x=size, y=re)) + 
  geom_violin()


