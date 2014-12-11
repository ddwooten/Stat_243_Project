# step 1. Input function

# 1) make a function that says hello to users
#I used cat function to start with some messages

Start <- function () {
  cat("Hello Users! I am a program.", "\n")
  cat("My task is selecting variables with genetic algorithm in regression.", "\n")
}

# 2) make a function that gets informations from users
# In this function, by using readline, get necessary informations from users and store them in a list
Input <- function() {
  
  #1 filepath[string]
  file.path <- quote(path <- readline("Type a filepath of data: "))
  eval(file.path)
  
  #2 index of y[int]
  y.index <- quote(y1 <- readline("Type a column index of y in your data: "))
  eval(y.index)
  y2 <- as.integer(unlist(y1))
  
  #3 indeces of X[vec]
  x.index <- quote(x1 <- readline("Type column indeces of x in your data (space seperated): "))
  eval(x.index)
  x2 <- as.integer(unlist(strsplit(x1, " ")))
  
  #4 indeces of must.include[vec]
  must.include.index <- quote(must1 <- readline("Type column indeces of must include allels (space seperated): "))
  eval(must.include.index)
  must2 <- as.integer(unlist(strsplit(must1, " ")))
  
  #5 number of individuals[int]
  num.of.indiv <- quote(indiv1 <- readline("Type a number of individuals that you want to creat (should be #x to #2x): "))
  eval(num.of.indiv)
  indiv2 <- as.integer(unlist(indiv1))
  
  #6 generation gap[real]
  gen.gap <- quote(gap1 <- readline("Type a generation gap (from 0 to 1): "))
  eval(gen.gap)
  gap2 <- as.numeric(unlist(gap1))
  
  #7 mutation rate[real]
  mu.rate <- quote(rate1 <- readline("Type a mutation rate (from 0 to 1): "))
  eval(mu.rate)
  rate2 <- as.numeric(unlist(rate1))
  
  #8 number of iterations[int]
  num.of.iter <- quote(iter1 <- readline("Type a number of iterations: "))
  eval(num.of.iter)
  iter2 <- as.integer(unlist(iter1))
  
  #9 fitness function[string]
  fitness.function <- quote(model <- readline("Type a name of fitness function that assesses models (default:AIC): "))
  eval(fitness.function)
  
  #10 ascending/descending[boolean]
  ascending <- quote(ascend1 <- readline("Type 'TRUE' if you want ascending, if not, type 'FALSE': "))
  eval(ascending)
  
  # make a list that includes 10 informations of users
  user.inputs <- list(Filepath=path, Y.index=y2, X.index=x2, Must.include.index=must2,
                      Num.of.individuals=indiv2, Generation.gap=gap2, Mutation.rate=rate2,
                      Num.of.iterations=iter2, Fitness.function=model, Ascending=ascend1)
  
  return(user.inputs)
}


# A final input function that says hello and get infos from users
StartGetInput <- function() {
  cat(Start())
  return(Input())
}

#View StartGetInput()


# After input(), get user.input as list, wrapper will tell which component to be used for each function


################


# step 2. read in the data as a dataframe, and make a dataframe that has a binary row on the top

# filepath should be specified by user
ReadInData <- function (filepath){
  dataframe <- read.table(filepath, header=TRUE)
  return(dataframe)
}

# data1 <- ReadInData(filepath)



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

# data2 <- AddMustInclude(must.include.index, dataframe=data1)
# we will have a dataframe with must.include as the 1st row
# in the form of 1 and 0's, with 1 indicate the corresponding column has to be included

# View(dataframe)


################


# step 3. extract vector of Y, and dataframe of X, and must.include vector

# Given the column index of Y, extract Y
GetY <- function(column.index.Y){
  # we have the 1st row as must.include, that we don't want to be included in y
  y <- data2[-1, column.index.Y]
  return(y)
}

#y <- GetY(column.index.Y)

# View(y)


# Given index of Xs that should be considered, including the must included columns
# if the user did not include a index for x at this point
# that column will not be considered at all
## this is a way for the user to exclude the variables that are not desired
XData <- function(column.index.X){
  
  # extract desired columns for x from the dataframe with 1st row as must include
  X.data <- data2[column.index.X]
  
  # clear the row names
  rownames(X.data)=NULL
  
  # returns only variables that desired, and 1st to be row must.include
  return(X.data)
}
# apply the functon will get a dataframe of x, 1st row must.include
# X.data <- XData(column.index.X)


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


# step 4. generate an initial population of individuals

IndivMat <- function(must.include, n) {

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

# individuals.dataframe <- IndivMat(must.include, n)


# So we have a vector of individuals that has "mustinclude X" predictors.
# n is basically the number of X(which is length(x)), but it can be up to 2n by users.


################


# step 5. Regression model and AIC with given Xs and y

GetScore <- function(X, data, individuals.dataframe, y.index) {
  
  # make a null vector that will be filled and return as an output
  AIC.vec <- c()
  
  for (i in 1:n) {
    
    # get the locations(column index) of X that individuals.dataframe has 1
    x.index <- which(individuals.dataframe[i, ]==1)
    
    # get a column name for X locations that we got, and for y
    x.var <- colnames(X)[x.index]
    y.var <- colnames(y)
    
    # make a regression formula by pasting y variable name and x variable names
    reg.fmla <- as.formula(paste(y.var, "~", paste(x.var, collapse="+")))
    
    # run the regression model for each individuals, and get each regression coefficients
    linmod <- lm(reg.fmla, data=data1)
    coef <- summary(linmod)$coefficients[, 1]
    
    # get residuals, rss, and aic by using summary of lm
    resid <- summary(linmod)$residuals
    rss <- sum(resid^2)
    aic <- nrow(data1)*log(rss/nrow(data1))+2*(length(x.vars)+2)
    
    # store each AIC scores in AIC.vec to return each AIC scores
    AIC.vec[i] <- aic
    
  }
  return(AIC.vec)
}

# scores <- GetScore(X=X, data=data1, individuals.dataframe, y.index)

# so far, we have AIC scores for each individuals to assess models.
# we will use this assess function in loop to iterate.


################


# step 6. Weight function
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

# step 7. Ranking function

# function ranks individuals by their score, and return a dataframe of individuals with scores in the first column

# rank individuals by their scores

# greatest.better : an argument for user to specify if higher score means better fit
# default greatest.better=F , since default scores are AICs, the less the better


Ranking <- function(individuals.daraframe, scores, greatest.better=F){
  
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


# step 8. Create the dataframe stores the best individuals

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
KeepTheBest <- function(ranked.individuals,generation){
  i <- generation
  # since the best individual is ranked at the bottom
  # take the last individual after ranking and put it in the ith row of best.individuals
  # 1st column will be scores, and the rest is the individual
  best.individuals[i,] <- tail(ranked.individuals, n=1)
  return(best.individuals)
}
# each generation, KeepTheBest will replace one row of best.individuals
# i th row stores the best of generation i

# best.individuals <- KeepTheBest(ranked.individuals,generation)

#View(best.individuals)


# at this point we don't need the scores anymore, get rid of the scores
# this step is done in Loop function
# individuals.dataframe <- ranked.individuals[,-1]


################


# step 9. Piar index function

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


# step 10. Cross over function
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


# step 11. Make a new generation
# a function to replace less fit individuals by new individuals created by cross over
# replace from the top, since top ones are less fit by the Ranking function
# number of replacement is determined by the generation gap
# a ratio of (to be replaced)/(total number of individuals)

NewGen <- function(individuals.dataframe, number.of.individuals, number.of.variables, gen.gap=1){
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

# individuals.dataframe <- NewGen(individuals.dataframe, number.of.individuals, number.of.variables, 1/101)


################


# step 12. Mutation function
# a functon to do the mutation after the next generation is created, befor asses and ranking
# default mutation rate is 1/the number of individuals, from the chapter

Mutation <- function(individuals.dataframe, mutation.rate=1/number.of.individuals){
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


# step 13. Fix must.include function
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


# step 14. get best individual
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


# step 15. Loop function that operates step 5~14

Loop <- function(X, data, individuals.dataframe, y.index, greatest.better, 
                 gen.gap, mutation.rate, num.of.gen){
                 
  # get the number of individuals and number of variables, which will be used frequently later
  number.of.individuals <- length(individuals.dataframe[,1])
  number.of.variables <- length(individuals.dataframe[1,])
  
  # create the empty dataframe to store best individuals with their score
  best.individuals <- CreateTheBest(number.of.variables, number.of.gen)
  
  # calculate CDF for weight
  weight.acc <- Weight(number.of.individuals)
  i <- 1
  for (i in 1:(number.of.gen-1)){
    # get scores needed for creating new generation
    scores <- GetScore(X, data, individuals.dataframe, y.index)
    # return a vector of scores
    
    # attach score from Assess to individuals from input og Loop, then rank
    ranked.individuals <- Ranking(individuals.dataframe, scores, greatest.better)
    # store the best one in best.individuals
    best.individuals <- KeepTheBest(ranked.individuals,i)
    # get rid of the scores from the ranked individuals
    individuals.dataframe <- ranked.individuals[,-1]
    # create new generation 
    individuals.dataframe <- NewGen(individuals.dataframe, number.of.individuals, 
                                    number.of.variables, gen.gap)
    # mutate
    individuals.dataframe <- Mutation(individuals.dataframe, mutation.rate)
    
    # fix must include
    individuals.dataframe <- FixMustInclude(individuals.dataframe, must.include)
    i <- 1+i
  }
  # after the last generation is evaluated, we only evaluate and find the best one
  
  # get scores needed for creating new generation
  scores <- GetScore(X, data, individuals.dataframe, y.index)
  # attach score then rank 
  ranked.individuals <- Ranking(individuals.dataframe, scores, greatest.better)
  # take the best one store it
  best.individuals <- KeepTheBest(ranked.individuals,number.of.gen)
  best.individual <- GetTheBest(best.individuals, greatest.better)
  return(best.individual)
}

# best.individual <- Loop(X, data1, individuals.dataframe, y.index, greatest.better, 
#                         weight.acc, gen.gap, mutation.rate, num.of.gen)


################


# step 16. report function
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
## now Loop and Report is ready to be called by Wrapper


################


# step 17. select function
# this wrapper function calls all functions(step 1~16) using inputs of users

select <- function() {
  
  # calls step 1 and store informations in a new variable to use them
  user.inputs <- StartGetInput()
  
  # calls step 2. read the data and add a binary vector on the top of the dataframe
  data1 <- ReadInData(user.inputs[[1]])
  data2 <- AddMustInclude(user.inputs[[4]], data1)

  # calls step 3. extract vector of Y, and dataframe of X, and must.include vector
  y <- GetY(user.inputs[[2]])
  
  X.data <- XData(user.inputs[[3]])
  X <- GetX(X.data)
  
  must.include <- MustInclude(X.data)
  
  # calls step 4. generate an initial population of individuals
  individuals.dataframe <- IndivMat(must.include, user.inputs[[5]])
  
  
  # calls step 15 that operates step from 5 to 14
  best.individual <- Loop(X, data1, individuals.dataframe,
                          user.inputs[[6]], user.inputs[[7]], user.inputs[[8]])
  
  # calls step 16
  report <- Report(best.individual, X)
  
  return(report)
}


#############################################################




#############################################################

test <- function() {
    test_that('

cat('The Program is Over!\n')
cat('*******************************************************\n')

