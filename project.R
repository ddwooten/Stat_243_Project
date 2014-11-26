# Given a path, read in data as dataframe
ReadInData <- function (filepath){
  dataframe <- read.table(filepath, header=TRUE)
  return(dataframe)
}

dataframe <- ReadInData(filepath)

# gives a binary vector of column index of Xs that must be included
MustInclude <- function(must.include.index=NULL){
  must.include <- rep(0,length(dataframe))
  must.include[must.include.index] <- 1
  
  # bind must.include and original data together, so won't loose track of index 
  dataframe <- rbind(must.include, dataframe)
  
  # Return a dataframe with 1st row is must.include, not all integer anymore
  return(dataframe)
}

dataframe <- MustInclude(must.include.index)

# View(dataframe)

# Given the column index, extract Y
GetY <- function(column.index.Y){
  y <- dataframe[-1, column.index.Y]
  return(y)
}

y <- GetY(column.index.Y)

# View(y)

# Given index of Xs that should be considered, including the must included columns
GetX <- function(column.index.X){
  X.data <- dataframe[column.index.X]
  return(X.data)
}

X.data <- GetX(column.index.X)
X <- X.data[-1, ]
rownames(X)=NULL
must.include <- X.data[1,]

# View(X)
# View(must.include)

# So far should have y as vector, cleaned data X as data frame, and a binary vector must.include
#######
#######
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
  