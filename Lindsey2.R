# 1. open the data (when filepath is given)

filepath <- "http://www.stat.cmu.edu/~larry/all-of-statistics/=data/fijiquakes.dat"
data1 <- read.table(filepath,header=TRUE)


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


# 3. Given the column index, extract Y
y.index <- 6 # example

GetY <- function(y.index){
  y <- data1[, y.index]
  return(y)
}

y <- GetY(y.index)


# 4.Given index of Xs that should be considered, including the must included columns
x.index <- c(2, 3, 4, 5) #example

GetX <- function(x.index){
  x.data <- data2[x.index]
  return(x.data)
}

x.data <- GetX(x.index)
x <- x.data[-1, ]
rownames(x)=NULL

must.include <- x.data[1,]


# 5. Randomly make a vector of individuals that has "mustinclude X" predictors.

indiv.mat <- function(must.include, n) {
  matrix <- matrix(0, n, length(x))      # null matrix with n individuals
  
  for (i in 1:length(x)) {
    matrix[, i] <- must.include[1, i]    # fill null matrix with must.include row
    
    for (j in 1:n) {
      if (must.include[1, i]==0) {         # for the not must.include column,
        matrix[j, i] <- rbinom(1, 1, 1/2)  # fill with random binary number
      }
    }
  }
  
  return(matrix)
}

individuals <- indiv.mat(must.include, n=length(x))
# n is basically the number of Xs, but it can be up to 2n by users.


#6. Regression model and AIC with given Xs and y

get.score <- function(x, data, individuals, y.index) {
  AIC.vec <- c()
  
  for (i in 1:n) {
    x.index <- which(individuals[i, ]==1)
    x.var <- colnames(x)[x.index]
    y.var <- colnames(data1)[y.index]
    
    reg.fmla <- as.formula(paste(y.var, "~", paste(x.var, collapse="+")))
    linmod <- lm(reg.fmla, data=data1)
    coef <- summary(linmod)$coefficients[, 1]
    
    resid <- summary(linmod)$residuals
    rss <- sum(resid^2)
    aic <- nrow(data1)*log(rss/nrow(data1))+2*(length(x.vars)+2)
    
    AIC.vec[i] <- aic
    
  }
  return(AIC.vec)
}

scores <- get.score(x=x, data=data1, individuals, y.index)




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

