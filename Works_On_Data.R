# I'm really tired of my fix keeps getting changed back by somebody
# there has been so many commits that I cannot look into every commit and find when it went wrong
# I'll demonstrate this version WORKs for my made up data set, as a back up for myself

# I made up a data set with 1000 observations
a1 <- rnorm(1000, mean=15, sd =20)
a2 <- rpois(1000,1)
a3 <- runif(1000, min = 100, max = 150)
a4 <- rbinom(1000,1,0.7)
y <- a1+a3-5*a2+100*a4+rnorm(1000)
b1 <- runif(1000, min=100, max=200)
b2 <- rnorm(1000, mean=200, sd=10)
b3 <- rgamma(1000, 250, 1)
lm(y ~ a1+a2+a3+a4+b1+b2)

# y is a linear combination of a1,a2,a3,a4, with some noise
# b1,b2,b3 are not related variables
# depends on the seed, b2 might be included by select function sometimes

# all inputs needed from user

# data frame with x,y mixed together
data <- data.frame(a1,y,a4,b2,b3,a3,a2,b1)
y.index <- 2
x.index <- c(1,3,4,5,6,7,8)
alleles <- c(1,6) #a1,a3
n <- 30
gen.gap <- 1
mutation.rate <- 2/20
iteration <- 20

# how I called select

select(data, y.index, x.index, alleles, n, gen.gap, mutation.rate, iteration, fitness="AIC", descending = F)


# following is the code I fixed from some commit, probably not the current one
# I cleared my workspace and ran the code with above variables 5+ times , it picks a1,a2,a3,a4 as variables 
# but depends on the seed, and number of iterations, b2 might be included sometimes
# testing not included, this is only a set of working code

library(foreach)
library(doParallel)
library(testthat)

######## 1) select function
# this function calls all functions(step 1~15) using inputs of users

select <- function(data, y.index, x.index, alleles, n, gen.gap, mutation.rate, iteration,
                   fitness, descending) {
  
  # store all inputs in a list to use them
  user.inputs <- list(Data=data, Y.index=y.index, X.index=x.index, Must.include.index=alleles,
                      Num.of.indiv=n, Gen.gap=gen.gap, Mutation.rate=mutation.rate,
                      Num.of.iter=iteration, Fitness=fitness, Descending=as.logical(descending))
  
  # calls step 1. add a binary vector on the top of the dataframe
  data2 <- AddMustInclude(user.inputs[[4]], user.inputs[[1]])
  
  # calls step 2. extract vector of Y, and dataframe of X, and must.include vector
  y <- GetY(user.inputs[[2]], data2)
  
  X.data <- XData(user.inputs[[3]], data2)
  X <- GetX(X.data)
  
  must.include <- MustInclude(X.data)
  
  individuals.dataframe <- IndivMat(must.include, user.inputs[[5]], X)
  
  # calls step 14 that operates step from 4 to 13
  best.individual <- Loop(X, user.inputs[[1]], individuals.dataframe, user.inputs[[2]], user.inputs[[10]],
                          user.inputs[[6]], user.inputs[[7]], user.inputs[[8]], user.inputs[[5]], must.include)
  
  # calls step 15
  Report(best.individual, X)
  
}



# Usage #
# select(data, y, x, alleles, n, gen.gap, mutation.rate, iteration,
#        fitness="AIC", descending = FALSE)
#
# Arguments (more explanation needed like help page)#
# data: a data frame
# y: a single integer of index of y column in data frame
# x: a vector consisting of indeces of x columns in data frame
# alleles: a vector consisting of indeces of clumns that must be included for predictors
# n: a single integer. number of individuals. it should be in the range (number of Xs ~ 2 * number of Xs)
# gen.gap: a real number from 0 to 1. 
# mutation.rate: a real number from 0 to 1.
# iteration: an integer. number of iterations.
# fitness: a function that assesses model. default is "AIC".
# descending: logical. if FALSE, ascending.

#############################################################
# following are steps of creating functions that is needed for select function


################


# step 1. make a dataframe that has a binary row on the top

# the must include columns are specified by index
# if extract y and x the index number might change, and hard to keep track
# make a function to change the index to a vector of boolean, will be easier to keep track

# Add a binary vector to the top of dataframe, 1 corresponding to must include that column
# After running this function, Y and unnecessary Xs will be gone
# but must include columns, still have 1 in their 1st row
# this is a way to keep track of the must include columns
AddMustInclude <- function(must.include.index=NULL, dataframe){
  
  # create a vector of 0 as default value for must.include
  must.include <- rep(0,length(dataframe))
  
  # change 0 to 1 at the place of the must include index 
  must.include[must.include.index] <- 1
  
  # bind must.include and original data together, so won't loose track of index 
  # when extracting y and x from the original dataframe
  dataframe <- rbind(must.include, dataframe)
  
  # Return a dataframe with 1st row is must.include, types not all integer anymore
  return(dataframe)
}

# data2 <- AddMustInclude(must.include.index, dataframe=data)
# we will have a dataframe with must.include as the 1st row
# in the form of 1 and 0's, with 1 indicate the corresponding column has to be included

# View(dataframe)


################


# step 2. extract vector of Y, and dataframe of X, and must.include vector

# Given the column index of Y, extract Y
GetY <- function(column.index.Y, data2){
  # we have the 1st row as must.include, that we don't want to be included in y
  y <- data2[-1, column.index.Y]
  return(y)
}

#y <- GetY(column.index.Y, data2)

# View(y)


# Given index of Xs that should be considered, including the must included columns
# if the user did not include a index for x at this point
# that column will not be considered at all
## this is a way for the user to exclude the variables that are not desired
XData <- function(column.index.X, data2){
  
  # extract desired columns for x from the dataframe with 1st row as must include
  X.data <- data2[column.index.X]
  
  # clear the row names
  rownames(X.data)=NULL
  
  # returns only variables that desired, and 1st to be row must.include
  return(X.data)
}
# apply the functon will get a dataframe of x, 1st row must.include
# X.data <- XData(column.index.X, data2)


# to extract X, extract the X.data without the 1st row
GetX <- function(X.data){
  X <- X.data[-1, ]
  return(X)
}

# X <- GetX(X.data)

# to extract the must.include that will be used in breeding later.
# which is the 1st row of the X.data
MustInclude <- function(X.data){
  must.include <- X.data[1,]
  return(must.include)
}

# must.include <- MustInclude(X.data)

# View(X)
# View(must.include)

# So far should have y as vector, cleaned data X as data frame, and a binary vector must.include


################


# step 3. generate an initial population of individuals

IndivMat <- function(must.include, n, X) {
  
  # null matrix with n individuals
  matrix <- matrix(0, n, length(X))
  
  for (i in 1:length(X)) {
    
    # fill null matrix with must.include row
    matrix[, i] <- must.include[1, i]
    
    for (j in 1:n) {
      
      # for the not must.include column, fill with random binary number
      if (must.include[1, i]==0) {         
        matrix[j, i] <- rbinom(1, 1, 1/2)
      }
    }
  }
  
  return(matrix)
}

# individuals.dataframe <- IndivMat(must.include, n, X)


# So we have a vector of individuals that has "mustinclude X" predictors.
# n is basically the number of X(which is length(x)), but it can be up to 2n by users.


################


# step 4. Regression model and AIC with given Xs and y
# "assess" is an object that is stored results of parallelizing fitness function
# need package: foreach, doParallel

GetScore <- function(X, data, individuals.dataframe, y.index, n) {
  # setup parallel backend to use 8 processors
  cl <- makeCluster(8)
  registerDoParallel(cl)
  
  #make an empty vector to store AIC values
  AIC.vec <- c()
  
  assess <- foreach(i=1:n, .combine=rbind) %dopar% {
    # get the locations(column index) of X that individuals.dataframe has 1
    x.index <- which(individuals.dataframe[i, ]==1)
    
    # get a column name for X locations that we got, and for y
    x.var <- colnames(X)[x.index]
    y.var <- colnames(data)[y.index]
    
    # make a regression formula by pasting y variable name and x variable names
    reg.fmla <- as.formula(paste(y.var, "~", paste(x.var, collapse="+")))
    
    # run the regression model for each individuals, and get each regression coefficients
    linmod <- lm(reg.fmla, data)
    coef <- summary(linmod)$coefficients[, 1]
    
    # get residuals, rss, and aic by using summary of lm
    resid <- summary(linmod)$residuals
    rss <- sum(resid^2)
    aic <- nrow(data)*log(rss/nrow(data))+2*(length(x.var)+2)
    
    # store each AIC scores in AIC.vec to get each AIC scores
    AIC.vec[i] <- aic  
  }
  stopCluster(cl)
  # since variable assess is a matrix, make this as a vector for output of this function
  assess.vec <- as.vector(assess)
  
  return(assess.vec)
}

# scores <- GetScore(X, data, individuals.dataframe, y.index, n)

# so far, we have AIC scores for each individuals to assess models.
# we will use this assess function in loop to iterate.


################


# step 5. Weight function
# a function to assign weight for each individual, given from the reference PDF

Weight <- function(number.of.individuals){
  
  # weighted are ordered to match individuals
  weight <- sapply(1:number.of.individuals, 
                   FUN=function(i){2*i/(number.of.individuals*(number.of.individuals+1))})
  
  # to get the accumulative weight for sampling
  weight.acc <- cumsum(weight)
  # weight.acc can be considered as CDF for weight
  return(weight.acc)
}
# weight.acc <- Weight(number.of.individuals)
# weight.acc will be in increasing order, we will need this in sampling for parents


################

# step 6. Ranking function

# function ranks individuals by their score, and return a dataframe of individuals with scores in the first column

# rank individuals by their scores

# greatest.better : an argument for user to specify if higher score means better fit
# default greatest.better=F , since default scores are AICs, the less the better


Ranking <- function(individuals.dataframe, scores, greatest.better=F){
  
  # combine individuals and their scores into a dataframe, scores match individuals
  # data.frame(a,b) will bind a to the left of b, if their dimensions match
  individuals.scores.dataframe <- data.frame(scores, individuals.dataframe)
  
  # rank them by their scores
  if (greatest.better==T){
    
    # if higher score is better, high score individuals will be at the bottom, and have larger row index
    ranked.individuals <- individuals.scores.dataframe[order(scores),]
  }
  else { if (greatest.better==F)
    
    # if lower score is better, low score individuals will be at the bottom, and have larger row index
    ranked.individuals <- individuals.scores.dataframe[order(-scores),]
    else {
      
      # report a problem to user if the input is neither T nor F
      stop("Argument greatest.better must be a logical value")
    }
  }
  rownames(ranked.individuals)=NULL
  # return a dataframe, socores and individuals, ranked
  return(ranked.individuals)
}

# greatest.better=T
# ranked.individuals <- Ranking(individuals.daraframe, scores, greatest.better)
# ranked.individuals <- Ranking(individuals.daraframe, scores)

#View(ranked.individuals)


################


# step 7. Create the dataframe store the best individuals

# create an empty dataframe to store the best individual of each generation
# need number of generations : number.of.gen to be the length of the dataframe

# number.of.gen=10 # for example



# default values are NA

CreateTheBest <- function(number.of.variables, number.of.gen){
  best.individuals <- data.frame(matrix(rep(rep(NA,number.of.variables+1), number.of.gen),nrow=number.of.gen))
  return(best.individuals)
}
# best.individuals <- CreateTheBest(number.of.variables, number.of.gen)

#View(best.individuals)

# since we are doing iterative selection, need a generation number
# generation=1
# suppose at the 1st generation

# this function to keep track of the best individual 
# by storing the best ones into the dataframe created by CreateTheBest
KeepTheBest <- function(ranked.individuals,generation, best.individuals){
  i <- generation
  # since the best individual is ranked at the bottom
  # take the last individual after ranking and put it in the ith row of best.individuals
  # 1st column will be scores, and the rest is the individual
  best.individuals[i,] <- tail(ranked.individuals, n=1)
  return(best.individuals)
}
# each generation, KeepTheBest will replace one row of best.individuals
# i th row stores the best of generation i

# best.individuals <- KeepTheBest(ranked.individuals,generation, best.individuals)

#View(best.individuals)


# at this point we don't need the scores anymore, get rid of the scores
# this step is done in Loop function
# individuals.dataframe <- ranked.individuals[,-1]


################


# step 8. Pair index function

# sample a pair of individuals based on the weight calculated by Weight 
# sample one index first, keep sampling until get a different index

PairIndex <- function(number.of.individuals, weight.acc){
  
  # sample a index by sampling from uniform(0,1), see which interval it falls in weight.acc
  # intervals are [0,weight.acc[1]),...,[weight.acc[i-1],weight.acc[i]), ...
  # index will be the coressponding i
  # this is done by taking the first index of which the sample is less than the value in weight.acc
  index.a <- (1:number.of.individuals)[runif(1)<weight.acc][1]
  
  # since parents cannot be the same individual, but 2 random numbers might fall into the same interval
  # keep sampling til have a different index
  repeat {index.b <- (1:number.of.individuals)[runif(1)<weight.acc][1]
          if (index.b!=index.a) break}
  # return the index pair as a vecrtor
  # this is equavalent to given weights, sample a pair of parents , without replacement
  return(c(index.a,index.b))
}

# pair.index <- PairIndex(number.of.individuals, weight.acc)


################


# step 9. Cross over function
# a function to do cross over, takes in the index pair of the parents
# produce one individual from a pair of parents as a vector

CrossOver <- function(individuals.dataframe, pair.index, number.of.variables){
  
  # assign parents by the index pair
  parent.a <- individuals.dataframe[pair.index[1],]
  parent.b <- individuals.dataframe[pair.index[2],]
  
  # randomly choose a point to break and crossover
  break.point <- sample(1:(number.of.variables-1),1)
  
  # new individual will be the 1st half of parent.a and the second half of parent.b
  new.individual <- unlist(c(parent.a[1:break.point],parent.b[(break.point+1):number.of.variables]))
  # return the new individual as a vector
  return(new.individual)
}


################


# step 10. Make a new generation
# a function to replace less fit individuals by new individuals created by cross over
# replace from the top, since top ones are less fit by the Ranking function
# number of replacement is determined by the generation gap
# a ratio of (to be replaced)/(total number of individuals)

NewGen <- function(individuals.dataframe, number.of.individuals, number.of.variables, gen.gap=1,
                   weight.acc){
  if (gen.gap>1 || gen.gap<0)
    # incase generation gap is greater than 1
    stop("Generation gap : gen.gap is a rate between 0 and 1")
  else{
    # determine the number of individuals to be replaces, default is to replace all
    # if specify a generation gap, make sure number of replacement is integer and at least 1
    number.new <- ceiling(gen.gap* number.of.individuals)
    
    # replace individuals from the top by repeating 
    # the procedure to get a pair of parents and produce an offspring
    for (j in 1:number.new){
      
      # use the PairIndex to get the parents for a new individual
      pair.index <- PairIndex(number.of.individuals, weight.acc)
      
      # replace a individual by the offspring from crossover
      # replace the individuals from the top, i.e. replace the least fit ones
      individuals.dataframe[j,] <- CrossOver(individuals.dataframe, pair.index, number.of.variables)
    }
    # return a nedataframe with top number.new rows new individuals, and other rows unchanged
    return(individuals.dataframe)
  }
}

# individuals.dataframe <- NewGen(individuals.dataframe, number.of.individuals, number.of.variables, 1/101, weight.acc)


################


# step 11. Mutation function
# a functon to do the mutation after the next generation is created, befor asses and ranking
# default mutation rate is 1/the number of individuals, from the chapter

Mutation <- function(individuals.dataframe, mutation.rate=(1/number.of.individuals), number.of.individuals, number.of.variables){
  if (mutation.rate > 1 || mutation.rate <0 ) {
    # report a problem if mutation rate if greater than 1
    stop("Mutation rate should be less than 1 and greater than 0.")
  }
  else {
    # interger number of individuals to be mutated, make sure at least 1
    mutation.number <- ceiling(number.of.individuals*mutation.rate)
    
    # random pick index with no replacement, to select individuals to be mutated
    mutate.index <- sample(1:number.of.individuals, mutation.number, replace=F)
    
    # random pick a place to be mutated of selected individuals, 1 per selected individual
    mutate.location <- sapply(1:mutation.number, FUN=function(x) sample(1:number.of.variables,1))
    
    # mutate selected individual(s) , one by one if have multiple
    for (i in 1:mutation.number){
      # for the place need to be mutated, have to change 1 to, 0 and 0 to 1
      # done by subtract 1 and take the absolute value, i.e.
      # |0-1|=1, |1-1|=0
      individuals.dataframe[mutate.index[i],mutate.location[i]] <- abs(individuals.dataframe[mutate.index[i],mutate.location[i]]-1)
    }
    return(individuals.dataframe)
  }
  
}


################


# step 12. Fix must.include function
# to make sure must include columns has 1 

FixMustInclude <- function(individuals.dataframe, must.include){
  # change the entire column to 1 where must include has 1
  # may undo Mutation, but is necessary
  individuals.dataframe[,must.include==1] <- 1 
  return(individuals.dataframe)
}
# individuals.dataframe <- FixMustInclude(individuals.dataframe, must.include)

# at this point will have a new generation to be evaluated to pass to Lindsey's functions


################


# step 13. get best individual
# suppose have a all fill best.individuals

GetTheBest <- function(best.individuals, greatest.better=F){
  best.scores <- best.individuals[,1]
  
  if (greatest.better==T){
    
    # if higher score is better, high score individuals will be at the bottom, and have larger row index
    best.individual <- tail(best.individuals[order(best.scores),],1)
  }
  else { if (greatest.better==F)
    
    # if lower score is better, low score individuals will be at the bottom, and have larger row index
    best.individual <- tail(best.individuals[order(-best.scores),],1)
    else {
      # report a problem to user if the input is neither T nor F
      stop("Argument greatest.better must be a logical value")
    }
    return(best.individual)
  }
}


################


# step 14. Loop function that operates step 4~13

Loop <- function(X, data, individuals.dataframe, y.index, greatest.better, 
                 gen.gap, mutation.rate, num.of.gen, n, must.include){
  
  # get the number of individuals and number of variables, which will be used frequently later
  number.of.individuals <- length(individuals.dataframe[,1])
  number.of.variables <- length(individuals.dataframe[1,])
  
  # create the empty dataframe to store best individuals with their score
  best.individuals <- CreateTheBest(number.of.variables, num.of.gen)
  
  # calculate CDF for weight
  weight.acc <- Weight(number.of.individuals)
  i <- 1
  for (i in 1:(num.of.gen-1)){
    # get scores needed for creating new generation
    scores <- GetScore(X, data, individuals.dataframe, y.index, n)
    # return a vector of scores
    
    # attach score from Assess to individuals from input og Loop, then rank
    ranked.individuals <- Ranking(individuals.dataframe, scores, greatest.better)
    # store the best one in best.individuals
    best.individuals <- KeepTheBest(ranked.individuals,i, best.individuals)
    # get rid of the scores from the ranked individuals
    individuals.dataframe <- ranked.individuals[,-1]
    # create new generation 
    individuals.dataframe <- NewGen(individuals.dataframe, number.of.individuals, 
                                    number.of.variables, gen.gap, weight.acc)
    # mutate
    individuals.dataframe <- Mutation(individuals.dataframe, mutation.rate, number.of.individuals, number.of.variables)
    
    # fix must include
    individuals.dataframe <- FixMustInclude(individuals.dataframe, must.include)
    i <- 1+i
  }
  # after the last generation is evaluated, we only evaluate and find the best one
  
  # get scores needed for creating new generation
  scores <- GetScore(X, data, individuals.dataframe, y.index, n)
  # attach score then rank 
  ranked.individuals <- Ranking(individuals.dataframe, scores, greatest.better)
  # take the best one store it
  best.individuals <- KeepTheBest(ranked.individuals,num.of.gen, best.individuals)
  best.individual <- GetTheBest(best.individuals, greatest.better)
  
  return(best.individual)
}

# best.individual <- Loop(X, data, individuals.dataframe, y.index, greatest.better, 
#                         gen.gap, mutation.rate, num.of.gen, n, weight.acc)


################


# step 15. report function
# then pick the individual with the best score
# return the column names, and the score

Report <- function(best.individual, X){
  # the best.individual will be a vector, with the 1st element score, and rest is the best binary individual
  # extrack the best binary individual
  best.index <- best.individual[-1]
  # extract the score of the best individual
  score <- unlist(best.individual[1])
  names(score)=NULL
  # finde the column name from X where the best binary individual has 1
  variable.names <- colnames(X)[best.index==1]
  cat("The best model should include", variable.names, ", with fitness score", score)
}
## now Loop and Report is ready to be called by select function


################
