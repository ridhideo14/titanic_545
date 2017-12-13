mydata <- read.csv("~/Desktop/train.csv")
#mydata <- as.data.frame(mydata)
# mydata<-as.matrix(mydata)
library(ggplot2) # visualization
#library('tidyverse')
library(magrittr)
dim(mydata)
library(gdata)
##Data Visualization
ggplot(mydata, aes(Age,fill = factor(Survived))) +
  geom_histogram(stat = "count")
ggplot(mydata, aes(Fare,fill = factor(Survived))) +
  geom_histogram(stat = "density")

##Enumerate Function : https://gist.github.com/kevinushey/7538142b5e16dd3b7200
enumerate <- function(X, FUN, ...) {
  result <- vector("list", length(X))
  for (i in seq_along(result)) {
    tmp <- FUN(X[[i]], i, ...)
    if (is.null(tmp))
      result[i] <- list(NULL)
    else
      result[[i]] <- tmp
  }
  result
}


##Convert String column to integer
str_column_to_int <- function(dataset, column){
  for (row in 1: length(dataset))
  {
  class_values <- row[column]
  }
  Unique <- unique(class_values)
  lookup = dict_Python()
  ##################CHECK THIS #############
  for (i in 1:length(Unique) ){
    lookup[enumerate(Unique)] <- i
  }
  ############################################
  for (row in 1:length(dataset)){
    row[column] <- lookup[row[column]]
  }
  
  return(lookup)
  
}





###



###
library(plyr)
##Terminal Node
to_terminal <- function(group){
    outcomes_left <- groups[[1]][2,]
    ##Check this again
    outcomes_left <- as.matrix(outcomes_left)
    outcomes_left <- t(outcomes_left)
    count_zero_left = 0
    count_one_left = 0
    for (j in 2:dim(outcomes_left)[1])
    {
      if(outcomes_left[j,1] == 0)
      {
      count_zero_left = count_zero_left + 1
      }else
      {
        count_one_left = count_one_left + 1
      }
    }
    outcomes_right <- groups[[2]][2,]
    outcomes_right <- as.matrix(outcomes_right)
    outcomes_right <- t(outcomes_right)
    count_zero_right = 0
    count_one_right = 0
    for (j in 2:dim(outcomes_right)[1])
    {
      if(outcomes_right[j,1] == 0)
      {
        count_zero_right = count_zero_right + 1
      }else
      {
        count_one_right = count_one_right + 1
      }
    }
    temp1 <- is(count_zero_left>count_one_left)
    temp2 <- is(count_zero_right>count_one_right)
    if (temp1 == 1 && temp2 == 1)
    {
      return(c(0,0))
    }else if (temp1 == 0 && temp2 == 1)
    {
      return(c(1,0))
    }else if (temp1 == 1 && temp2 == 0)
    {
      return(c(0,1))
    } else
    {
      return(c(1,1))
    }

}
##For testing the function to terminal
groups <- m1
temp <- to_terminal(m1)
###__----------------------------------------

Split <- function(root,max_depth,min_size,n_features,depth){
  left <- root[[3]][1]
  right <- root[[3]][2]
  rm(root[[3]])
  # temp1 <- dim(as.matrix(left))
  # temp2 <- dim(as.matrix(right))

  if (left == 0 || right == 0)
  {
    val = to_terminal(left+right)
    left = val[1]
    right = val[2]
  }
   if (depth >= max_depth)
   {
     val2= to_terminal(left)
     left = val2[1]
     val3= to_terminal(right)
     right = val3[2]
   }
  
  # temp <- as.matrix(root[[3]])
  # temp1 <- temp[1]
  # temp2 <- temp[2]
  len_left <- dim(left[[1]])[2]
  len_right <- dim(right[[1]])[2]
  ##process left child
  if (len_left <= min_size){
    
    val4 <- to_terminal(left)
    left <- val4[1]
  }else {
    val4 <- get_split(left,n_features)
    left <- val4[[3]][1]
    split(left,max_depth,nmin_size,n_features,depth+1)
  }
  ##process right child
  if (right <= min_size){
    val5 <- to_terminal(right)
    right <- val5[2]
  }else {
    val5 <- get_split(right,n_features)
    right <- val5[[3]][2]
    split(right,max_depth,nmin_size,n_features,depth+1)
  }
  
} 




##build a decision tree

build_tree <- function(train,max_depth,min_size,n_features){
  
  root <- get_split(train, n_features)
  split(root, max_depth, min_size, n_features, 1)
  return(root)
}

## Make a prediction with a decision tree
library(XRPython)
predict <- function(node,row){
  if (row[node[index]] < node[value]){
    if (isinstance(node[left], dict_Python)){
      return( predict(node[left], row))
    }else{
      return(node[left])
    }
    }else{
      if (isinstance(node[right],dict_Python)){
        return(predict(node[right], row))
      }else{
        return(node[right])
      }
      }
}



# Create a random subsample from the dataset with replacement
subsample <- function(dataset, ratio)
{
  Sample = list()
  n_sample = round(length(dataset)*ratio)
  while (length(Sample) < n_sample){
    index <- runif(length(dataset))
    append(Sample,dataset[index])
  }
  return(sample)
  
  }
