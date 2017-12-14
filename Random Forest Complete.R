mydata1 <- read.csv("~/Desktop/train.csv")

mydata1
#mydata1<-as.matrix(mydata1)
library(ggplot2) # visualization
#library('tidyverse')
library(magrittr)
dim(mydata1)
library(gdata)
#str(mydata1) 
#summary(mydata1)
#sum(mydata1$Cabin=="")

#people with same ticket are from same group
#mydata1<-mutate(mydata1, grp=rep(0,nrow(mydata1)))#grp number, 0 =>single
# uniq_ticket<-table(mydata1$Ticket)
# not_single<-uniq_ticket[uniq_ticket>1]
# for(ii in 1:length(not_single)){
#   this_ticket<-names(not_single)[ii]
#   tickt_index<-which(mydata1$Ticket==this_ticket)
#   mydata1$grp[tickt_index]<-ii
# }
# mydata1 %>% group_by(grp) %>% summarise(sur=mean(Survived))->grp_stats

#extract title 
mydata1$Title <- gsub('(.*, )|(\\..*)', '', mydata1$Name)
table(mydata1$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

mydata1$Title[mydata1$Title == 'Mlle']        <- 'Miss' 
mydata1$Title[mydata1$Title == 'Ms']          <- 'Miss'
mydata1$Title[mydata1$Title == 'Mme']         <- 'Mrs' 
mydata1$Title[mydata1$Title %in% rare_title]  <- 'Rare Title'
Title_Miss<-which(mydata1$Title=='Miss')
Title_Mr<-which(mydata1$Title=='Mr')
Title_Mrs<-which(mydata1$Title=='Mrs')
Title_Master<-which(mydata1$Title=='Master')
Title_Rare<-which(mydata1$Title=='Rare Title')


#Code for filling the missing ages starts here. The missing ages are filled based on the avaerage of the title means.  

Age_miss<-mydata1$Age[Title_Miss]
mean_Age_miss<-mean(Age_miss,  na.rm = TRUE)

Age_mr<-mydata1$Age[Title_Mr]
mean_Age_mr<-mean(Age_mr,  na.rm = TRUE)

Age_mrs<-mydata1$Age[Title_Mrs]
mean_Age_mrs<-mean(Age_mrs,  na.rm = TRUE)

Age_master<-mydata1$Age[Title_Master]
mean_Age_master<-mean(Age_master,  na.rm = TRUE)

Age_rare<-mydata1$Age[Title_Rare]
mean_Age_rare<-mean(Age_rare,  na.rm = TRUE)


Age_missing<-which(is.na(mydata1$Age))
mydata1$Title[Age_missing[1]]


for(i in 1:length(Age_missing)){
  Title1<-mydata1$Title[Age_missing[i]]
  if(Title1=='Mr'){
    mydata1$Age[Age_missing[i]]<-mean_Age_mr
  }
  if(Title1=='Miss'){
    mydata1$Age[Age_missing[i]]<-mean_Age_miss
  }
  if(Title1=='Mrs'){
    mydata1$Age[Age_missing[i]]<-mean_Age_mrs
  }
  if(Title1=='Rare Title'){
    mydata1$Age[Age_missing[i]]<-mean_Age_rare
  }
  if(Title1=='Master'){
    mydata1$Age[Age_missing[i]]<-mean_Age_master
  }
  
  
} 

mydata <-read.csv("~/Desktop/train1.csv")

#mydata
print("No NA's in the age field: can se seen below") 
mydata$Age<-mydata1$Age
dim(mydata)
datasets<-mydata
nrow(datasets)
ncol(datasets)

#Random Forest algorithm starts here:
n_folds=3

cross_validation_split<-function(datasets,n_folds){
  fold_size<-nrow(datasets)/n_folds
  dataset_split_1<-matrix(nrow=fold_size , ncol=8)
  dataset_split_2<-matrix(nrow = fold_size,ncol = 8)
  datasets<-as.matrix(datasets)
  # Creating an array of matrices
  random_numbers<-c()
  a<-array(NA,c(fold_size,8,n_folds))
  for(ii in 1:(n_folds)){
    # dataset_split_list<-rep(NA,8)
    #randomly selecting samples for k fold cross validation
    random_numbers<-sample(1:nrow(datasets))
    randnew<-random_numbers[1:fold_size]
    for(j in 1:fold_size){
      dataset_split_1[j,]<-datasets[randnew[j],]
      
    }
    
    a[,,ii]<- dataset_split_1
  }
  return(a)
}


#We have now obtained the splitted datasets
b<-cross_validation_split(datasets,3)


#left_and_right<-array(NA,c(fold_size,8,n_folds))
datasets1<-matrix(ncol=8)
test_split<-function(index,value,datasets){
  #concat_data <- cbindX(concat_data, as.data.frame(mydata[5,]),as.data.frame(mydata[6,]))
  concat_data_left<-data.frame(ncol=8)
  concat_data_right<-data.frame(ncol=8)


  left<-c()
  right<-c()
  kim<-do.call(rbind,as.list(datasets))
  kim<-t(kim)
  datasets1<-matrix(kim,ncol=8)
  #print(dim(datasets1))
  print("In function test_split")
  print(dim(datasets1))
  for(i in 1:nrow(datasets1)){
    if(!is.na(datasets[i,index]< value)){
      concat_data_left <- cbindX(concat_data_left, as.data.frame(datasets1[i,]))
    }
    else{
      concat_data_right <- cbindX(concat_data_right, as.data.frame(datasets1[i,]))
    }
  }
  
  return(list(concat_data_left,concat_data_right))
  
}

#concat_data_left<-data.frame(ncol=8)
#concat_data_right<-data.frame(ncol=8)
#m1<-test_split(3,10,datasets)


evaluate_algorithm<-function(datasets,random_forest,n_folds,max_depth,min_size,sample_size,n_trees,n_features){
  folds<-cross_validation_split(datasets,n_folds)
  fold_size<-nrow(datasets)/n_folds
  scores<-c()
  actual<-c()
  # accuracy_list<-c()
  for(i in 1:n_folds){
    train_set<-folds
    #harsha<-harsha[ harsha[,,-1]]
    #harsha<-array(harsha,dim=c(n,m,k-1))
    train_set<-train_set[,,-i]
    train_set<-array(train_set,dim=c(fold_size,8,2))
    
    test_set<-folds[,,i]
    set_2<-folds[,,i]
    #Test data does not have the predicted class,  thus  NA'ing the predicted class value from the dataset
    #list_information<-c()
    for(i in 1:fold_size){
      test_set[i,2]<-NA
    }
    #predicted labels
    predicted<-random_forest(train_set,test_set,max_depth,min_size,sample_size,n_features)
    #Actual labels
    for(i in 1:fold_size){
      Actual_value<-set_2[i,8]
      actual<-c(actual,Actual_value)
      
    }
    accuracy<-accuracy_metric(actual,predicted)
    scores<-c(scores,accuracy)
  }
  
  return(scores)
  
}


matrix3<-matrix()

#Evaluating the ginin score for finding the best feature and the best split value
gini_index<-function(groups,class_values){
  gini<-0.0
  print("Hello in gini function")
  print(groups[[1]])
  dimention<-dim(groups)
  length3<-dimention[2]
  list_for_groups<-c()
  size_2<-c()
  gg<-matrix()
  for(i in 1:2){
    count_of_value <-0
    for(j in 1:2){
      
      #group<-groups[j]
      matrix3<-as.matrix(groups[[j]])
      matrix3<-t(matrix3)
      size_2<-dim(matrix3)
      size1<-size_2[1]
      # size1<-length(j)
      if(size1==0){
        next
      }
      
      print("IN gini index")
      #groups<-list(concat_data_left,concat_data_right)
      gg<-as.matrix(groups[[j]])
      #print(gg)
      gg<-t(gg)
      #list_for_groups <-gg[,2])
      # list_for_groups<-c("0","0","0","0","0","0","1","1","1","1","1","0" )
      
      #list_for_groups<-c(list_for_groups,Actual_value)
      print(dim(gg))
      list_temp<-dim(gg)
      #  }
      if((i==1)&& list_temp[1]!=1 ){
        count_of_value <-length(which(gg[,2]==0))
      }
      if((i==2)&& list_temp[1]!=1 ){
        count_of_value <-length(which(gg[,2]==1))
      }
      #print(count_of_value)
      proportion<-count_of_value/size1
      gini<-gini+(proportion*(1-proportion))
      print(gini)
    }
  }
  return(gini)
  
}


get_split<-function(datasets, n_features){
  class_values<-c("0","1")
  b_index<-999
  b_value<-999
  b_score<-999
  b_groups<-list()
  features<-c()
  index<-sample(1:7)
  features<-index[1:n_features]
  kim<-do.call(rbind,as.list(datasets))
  print("In function getsplit")
  #print(dim(kim))
  kim<-t(kim)
  datasets2<-matrix(kim,ncol=8)
  print(dim(datasets2))
  # print(dim(datasets2))
  
  groups<-list()
  for(i in 1:4){
    for(j in 1:20){
      groups<-test_split(i,datasets2[j,i],datasets2)
      #print(dim(groups))
      #print(groups[[1]])
      gini<-gini_index(groups,class_values)
      print(gini)
      if(gini<b_score){
        b_index<-i
        b_value<-datasets2[j,i]
        b_score<-gini
        b_groups<-groups
        print(j)
        print("Harsha")
        #  print(i)
        
      }
    }
  }
  
  return(list(index=b_index,value=b_value, groups=b_groups))
  
}



library(plyr)
Split <- function(root,max_depth,min_size,n_features,depth){
  print("I am in SPLIT")
  left <- root[[3]][1]
  right <- root[[3]][2]
  #rm(root[[3]])
  # temp1 <- dim(as.matrix(left))
  # temp2 <- dim(as.matrix(right))
  
  if (length(left[[1]]) == 1 || length(right[[1]]) == 1)
  {
    if(length(left[[1]]) == 1)
    {
      val = to_terminal(left)
      left = val
    }else
    {
      val = to_terminal(right)
      right = val
    }
    
    
    
  }
  
  if (depth >= max_depth)
  {
    val2= to_terminal(left)
    left = val2
    val3= to_terminal(right)
    right = val3
  }
  
  # temp <- as.matrix(root[[3]])
  # temp1 <- temp[1]
  # temp2 <- temp[2]
  len_left <- dim(left[[1]])[2]
  len_right <- dim(right[[1]])[2]
  ##process left child
  if (len_left <= min_size){
    
    val4 <- to_terminal(left)
    left <- val4
  }else {
    val4 <- get_split(left,n_features)
    left <- val4
    Split(left,max_depth,min_size,n_features,depth+1)
  }
  ##process right child
  if (len_right <= min_size){
    val5 <- to_terminal(right)
    right <- val5
  }else {
    val5 <- get_split(right,n_features)
    right <- val5
    Split(right,max_depth,min_size,n_features,depth+1)
  }
  print("I am exiting SPLIT")
} 


##Terminal Node
to_terminal <- function(LR){
  print("I am in TO_TERMINAL")
  outcomes <- LR[[1]]
  ##Check this again
  outcomes <- as.matrix(outcomes)
  outcomes <- t(outcomes)
  count_zero = 0
  count_one = 0
  temp <- dim(outcomes)
  for (j in 1:temp[2])
  {
    if(!is.na(outcomes[8,j] == 0))
    {
      count_zero = count_zero + 1
    }else
    {
      count_one = count_one + 1
    }
  }
  
  if(count_one>count_zero)
  {
    return(1)
  }else
  {
    return(0)
  }
  print("I am Exiting TO_TERMINAL")
}

##build a decision tree

build_tree <- function(train,max_depth,min_size,n_features){
  
  root <- get_split(train, n_features)
  print("Hello I am done finding the root!!!!")
  Split(root, max_depth, min_size, n_features, 1)
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





# root<-get_split(train_set,8)
# root$groups[[1]]

root <- build_tree(mydata,5,1,3)

