# we should be given the path to read the file, column index for y & x, x's that must be included

# regression model, type, function to evaluate the fitness, size of each generation,

# generation gap, mutation rate, number of generations


# read in the data as a dataframe
dataframe <- read.table(filepath, header=TRUE)



# the must include columns are specified by index
# if extract y and x the index number might change, and hard to keep track
# make a function to change the index to a vector of boolean, will be easier to keep track


# gives a binary vector of column index of Xs that must be included
MustInclude <- function(must.include.index=NULL){
  
  # create a vector of 0 as default value for must.include
  must.include <- rep(0,length(dataframe))
  
  # change 0 to 1 at the must include index 
  must.include[must.include.index] <- 1
  
  # bind must.include and original data together, so won't loose track of index 
  # when extracting y and x from the original dataframe
  dataframe <- rbind(must.include, dataframe)
  
  # Return a dataframe with 1st row is must.include, types not all integer anymore
  return(dataframe)
}

dataframe <- MustInclude(must.include.index)
# we will have a dataframe with must.include as the 1st row
# in the form of 1 and 0's, with 1 indicate the corresponding column has to be included

# View(dataframe)


# Given the column index of Y, extract Y
GetY <- function(column.index.Y){
  # we have the 1st row as must.include, that we don't want to be included in y
  y <- dataframe[-1, column.index.Y]
  return(y)
}

y <- GetY(column.index.Y)

# View(y)


# Given index of Xs that should be considered, including the must included columns
# if the user did not include a index for x at this point
# that column will not be considered at all
## this is a way for the user to exclude the variables that are not desired
GetX <- function(column.index.X){
  
  # extract desired columns for x from the dataframe with 1st row as must include
  X.data <- dataframe[column.index.X]
  
  # clear the row names
  rownames(X)=NULL
  
  # returns only variables that desired, and 1st to be row must.include
  return(X.data)
}

# apply the functon will get a dataframe of x, 1st row must.include
X.data <- GetX(column.index.X)

# to extract X 
X <- X.data[-1, ]

# to extract the must.include that will be used in breeding later.
must.include <- X.data[1,]

# View(X)
# View(must.include)

# So far should have y as vector, cleaned data X as data frame, and a binary vector must.include

###############

# Lindsey's part to give me a dataframe of individuals named individuals.daraframe,
# and a vector or list of scores named scores

# individuals.daraframe: binary individuals as rows of a dataframe
# scores : score for corresponding row of individuals.daraframe, should be a vector

##############

# function ranks individuals by their score, and return a dataframe of individuals with scores
# to rank individuals by their scores

# greatest.better : an argument for user to specify if higher score means better fit
# default greatest.better=F , since default scores are AICs, the less the better

Ranking <- function(individuals.daraframe, scores, greatest.better=F){
  
  # combine individuals and their scores into a dataframe, scores match individuals
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
      cat("Argument greatest.better must be a logical value")
    }
  }
  rownames(ranked.individuals)=NULL
  # return the scores and individual rank in 
  return(ranked.individuals)
}

greatest.better=T
ranked.individuals <- Ranking(individuals.daraframe, scores, greatest.better)
ranked.individuals <- Ranking(individuals.daraframe, scores)

#View(ranked.individuals)

# get the number of individuals and number of variables, which will be used frequently later
number.of.individuals <- length(individuals.dataframe[,1])
number.of.variables <- length(individuals.dataframe[1,])

# create an empty dataframe to store the best individual of each generation
# need number of generations : number.of.gen
number.of.gen=10 # for example
# default values are NA
best.individuals <- data.frame(matrix(rep(rep(NA,number.of.variables+1), number.of.gen),nrow=number.of.gen))

#View(best.individuals)

# since we are doing iterative selection, need a generation number
generation=1
# suppose at the 1st generation


# a function to keep track of the best individual of each generation
KeepTheBest <- function(ranked.individuals,generation){
  i <- generation
  # since the best individual is ranked at the bottom
  # take the last individual after ranking and put it in the ith row of best.individuals
  # 1st column will be scores, and the rest is the individual
  best.individuals[i,] <- tail(ranked.individuals, n=1)
  return(best.individuals)
}

best.individuals <- KeepTheBest(ranked.individuals,generation)

#View(best.individuals)


# at this point we don't need the scores anymore, get rid of the scores
individuals.dataframe <- ranked.individuals[,-1]

# a function to assign weight for each individual, given from reference
weight <- sapply(1:number.of.individuals, 
                 FUN=function(i){2*i/(number.of.individuals*(number.of.individuals+1))})
# weight will be in increasing order, each weight corresponds to rows of individuals.dataframe

# to get the accumulative weight for sampling
weight.acc <- cumsum(weight)

# sample a pair of individuals based on the weight
PairIndex <- function(number.of.individuals, weight.acc){
  index.a <- (1:number.of.individuals)[runif(1)<weight.acc][1]
  repeat {index.b <- (1:number.of.individuals)[runif(1)<weight.acc][1]
          if (index.b!=index.a) break}
  return(c(index.a,index.b))
}


# a function to do cross over
# produce one individual from a pair of parents, given the index of the parents
CrossOver <- function(individuals.dataframe, pair.index, number.of.variables){
  parent.a <- individuals.dataframe[pair.index[1],]
  parent.b <- individuals.dataframe[pair.index[2],]
  break.point <- sample(1:(number.of.variables-1),1)
  new.individual <- unlist(c(parent.a[1:break.point],parent.b[(break.point+1):number.of.variables]))
  return(new.individual)
}

NewGen <- function(individuals.dataframe, number.of.individuals, number.of.variables, gen.gap=1){
  if (gen.gap>1)
    cat("Generation gap : gen.gap cannot exceed 1")
  else{
    # decide the number of individuals to be replaces, default replace all
    # if specify a generation gap, make sure number is integer and at least 1
    number.new <- ceiling(gen.gap* number.of.individuals)
  }
  for (j in 1:number.new){
    # use the PairIndex to get the parents for a new individual
    pair.index <- PairIndex(number.of.individuals, weight.acc)
    # replace a individual by the offspring from crossover
    # replace the individuals from the top, i.e. replace the least fit ones
    individuals.dataframe[j,] <- CrossOver(individuals.dataframe, pair.index, number.of.variables)
  }
  return(individuals.dataframe)
}

individuals.dataframe <- NewGen(individuals.dataframe, number.of.individuals, number.of.variables, 1/101)

# a functon to do the mutation after the next generation is created, befor asses and ranking
# default mutation rate is 1/the number of individuals, from the chapter
Mutation <- function(individuals.dataframe, mutation.rate=1/number.of.individuals){
  if (mutation.rate > 1) {
    # report a problem if mutation rate if greater than 1
    cat("Mutation rate should be less than 1.")
  }
  else {
    # interger number of individuals to be mutated, make sure at least 1
    mutation.number <- ceiling(number.of.individuals*mutation.rate)
    # random pick index with no replacement, to select individuals to be mutated
    mutate.index <- sample(1:number.of.individuals, mutation.number, replace=F)
    # random pick a place to be mutated of selected individuals, 1 per selected individual
    mutate.location <- sapply(1:mutation.number, FUN=function(x) sample(1:number.of.variables,1))
    # mutate selected individual(s) 
    for (i in 1:mutation.number){
      
      individuals.dataframe[mutate.index[i],mutate.location[i]] <- abs(individuals.dataframe[mutate.index[i],mutate.location[i]]-1)
    }
    return(individuals.dataframe)
  }
  
}


# to make sure must include columns has 1 
individuals.dataframe[1,must.include] <- 1 

# at this point will have a new generation to be evaluated to pass to Lindsey's functions

##########
# after the last generation is evaluated, we do not breed anymore, only do KeepTheBest()
# then pick the individual with the best score
# return the column names 


# suppose have a all fill best.individuals
ReportTheBest <- function(best.individuals, greatest.better=F){
  if (greatest.better==T){
    
    # if higher score is better, high score individuals will be at the bottom, and have larger row index
    best.individuals <- tail(individuals.scores.dataframe[order(scores),]]
  }
  else { if (greatest.better==F)
    
    # if lower score is better, low score individuals will be at the bottom, and have larger row index
    best.individuals <- tail(individuals.scores.dataframe[order(-scores),])
    else {
      # report a problem to user if the input is neither T nor F
      cat("Argument greatest.better must be a logical value")
    }
    return(best.individual)
}