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




##Terminal Node
to_terminal <- function(group){
  for (row in 1:length(group))
  {
    outcomes <- row[-1]##Check this again
  }
  
  return(list(max(unique(outcomes), key <- count(outcomes))))
}

Split <- function(node,max_depth,min_size,n_features,depth){
  c(left, right) <- node[groups] 
  del(node[groups])
  if (left == 0 | right==0)
  {
    node[left]= node[right]=to_terminal(left+right)
  }
   if (depth >= max_depth)
   {
     node[left]= to_terminal(left)
     node[right]= to_terminal(right)
   }
  
  ##process left child
  if (length(left) <= min_size){
    node[left] <- to_terminal(left)
  }else {
    node[left] <- get_split(left,n_features)
    split(node[left],max_depth,nmin_size,n_features,depth+1)
  }
  ##process right child
  if (length(right) <= min_size){
    node[right] <- to_terminal(right)
  }else {
    node[right] <- get_split(right,n_features)
    split(node[right],max_depth,nmin_size,n_features,depth+1)
  }
  
} 




##build a decision tree

build_tree <- function(train,max_depth,min_size,n_features){
  
  root <- get_split(train, n_features)
  split(node[right], max_depth, min_size, n_features, 1)
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
