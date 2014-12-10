 # 1. open the data (when filepath is given)

data1 <- read.table(filepath,header=TRUE)
filepath <- "filepath"


# 2. gives a binary vector of column index of Xs that must be included
must.include.index <- c(2,3) #example

MustInclude <- function(must.include.index, dataframe){
  must.include <- rep(0,ncol(dataframe))
  must.include[must.include.index] <- 1
  
  # bind must.include and original data together, so won't loose track of index 
  dataframe <- rbind(must.include, dataframe)
  
  # Return a dataframe with 1st row is must.include, not all integer anymore
  return(dataframe)
}

data2 <- MustInclude(must.include.index, dataframe=data1)


#3. Regression model and AIC with given Xs and y

GetAIC <- function(data, must.include.index, y.index){
  x.vars <- colnames(data)[must.include.index]
  y.var <- colnames(data)[y.index]
  reg.fmla <- as.formula(paste(y.var,"~",paste(x.vars,collapse="+")))
  resid <- summary(lm(reg.fmla, data=data))$residuals
  rss <- sum(resid^2)
  aic <- nrow(data1)*log(rss/nrow(data1))+2*(length(x.vars)+2)  
  return(aic)
}

#example (must.include.index:c(2, 3), y.index:6)
y.index <- 6

AIC <- GetAIC(data=data1, must.include.index, y.index)


# (1) Note that I used data1 for this step, 
#     which means that I don't need #2. steps when 'must.include.index' is given.
# (2) I have changed step 1 because I don't think we need function for this step.
#     But please let me know if we need function, then I will make it back.
# (3) I also deleted 'getY' and 'getX' steps from Tianyi's 
#     because I could make regression model and AIC with the data from step 1.











>>>>>>> a25ebec82a27ad4337286361577065103cca00c5
# suppose have a dataframe of individuals and a vector or list of scores

# function ranks individuals by their score, and return a dataframe of individuals with scores
# default less score the better, since default scores are AICs, the less the better

Ranking <- function(individuals.daraframe, scores, greatest.better=F){
  # combine individuals and their scores into a dataframe
  individuals.score.dataframe <- data.frame(scores, individuals.dataframe)
  # rank them by their scores
  if (greatest.better==T){
    ranked.individuals <- individuals.score.dataframe[order(scores),]
  }
  else { if (greatest.better==F)
    ranked.individuals <- individuals.score.dataframe[order(-scores),]
    else { 
      cat("Argument greatest.better must be a logical value")
    }
  }
  # return the scores with individual rank in 
  return(ranked.individuals)
}

ranked.individuals <- Ranking(individuals.daraframe, scores, greatest.better=F)

number.of.individuals <- length(individuals.dataframe[,1])
number.of.variables <- length(individuals.dataframe[1,])

# create ab empty dataframe to store the best individual of each generation
# default values are NA
best.individuals <- data.frame(matrix(rep(rep(NA,number.of.variables+1), number.of.individuals),nrow=number.of.individuals))

i=1 # suppose at the 1st generation
# a function to keep track of the best individual of each generation
KeepTheBest <- function(ranked.individuals,i){
  best.individuals[i,] <- tail(ranked.individuals, n=1)
  return(best.individuals)
}


# a function to assign weight for each individual

dataframe = individuals.dataframe
# a functon to do the mutation after the next generation is created, befor asses and ranking
# default mutation rate is 1/# of individuals, from the chapter
Mutation <- function(individuals.dataframe, mutation.rate=2/number.of.individuals){
  if (mutation.rate > 1) {
    cat("Mutation rate should be less than 1.")
  }
  else {
    # interger number of individuals to be mutated, make sure at least 1
    mutation.number <- ceiling(number.of.individuals*mutation.rate)
    # random pick index with replacement
    mutate.index <- sample(1:number.of.individuals, mutation.number, replace=F)
    # only mutate 1 random place of selected individuals
    mutate.location <- sapply(1:mutation.number, FUN=function(x) sample(1:number.of.variables,1))
    # mutate selected individual(s) 
    for (i in 1:mutation.number){
      individuals.dataframe[mutate.index[i],mutate.location[i]] <- abs(individuals.dataframe[mutate.index[i],mutate.location[i]]-1)
    }
    return(individuals.dataframe)
  }
  
}

