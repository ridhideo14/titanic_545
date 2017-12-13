mydata1 <- read.csv("C:/FALL 2017/STAT 545/STAT 545 Project/train.csv")
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







mydata <- read.csv("C:/FALL 2017/STAT 545/STAT 545 Project/train1.csv")
mydata
#mydata<-as.matrix(mydata)
library(ggplot2) # visualization
#library('tidyverse')
library(magrittr)
dim(mydata)
library(gdata)
#str(mydata) 
#summary(mydata)
#sum(mydata$Cabin=="")

#people with same ticket are from same group
#mydata<-mutate(mydata, grp=rep(0,nrow(mydata)))#grp number, 0 =>single
# uniq_ticket<-table(mydata$Ticket)
# not_single<-uniq_ticket[uniq_ticket>1]
# for(ii in 1:length(not_single)){
#   this_ticket<-names(not_single)[ii]
#   tickt_index<-which(mydata$Ticket==this_ticket)
#   mydata$grp[tickt_index]<-ii
# }
# mydata %>% group_by(grp) %>% summarise(sur=mean(Survived))->grp_stats

#extract title 
# mydata$Title <- gsub('(.*, )|(\\..*)', '', mydata$Name)
# table(mydata$Title)
# rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
#                 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
# 
# mydata$Title[mydata$Title == 'Mlle']        <- 'Miss' 
# mydata$Title[mydata$Title == 'Ms']          <- 'Miss'
# mydata$Title[mydata$Title == 'Mme']         <- 'Mrs' 
# mydata$Title[mydata$Title %in% rare_title]  <- 'Rare Title'
# Title_Miss<-which(mydata$Title=='Miss')
# Title_Mr<-which(mydata$Title=='Mr')
# Title_Mrs<-which(mydata$Title=='Mrs')
# Title_Master<-which(mydata$Title=='Master')
# Title_Rare<-which(mydata$Title=='Rare Title')
# 
# 
# #Code for filling the missing ages starts here. The missing ages are filled based on the avaerage of the title means.  
# 
# Age_miss<-mydata$Age[Title_Miss]
# mean_Age_miss<-mean(Age_miss,  na.rm = TRUE)
# 
# Age_mr<-mydata$Age[Title_Mr]
# mean_Age_mr<-mean(Age_mr,  na.rm = TRUE)
# 
# Age_mrs<-mydata$Age[Title_Mrs]
# mean_Age_mrs<-mean(Age_mrs,  na.rm = TRUE)
# 
# Age_master<-mydata$Age[Title_Master]
# mean_Age_master<-mean(Age_master,  na.rm = TRUE)
# 
# Age_rare<-mydata$Age[Title_Rare]
# mean_Age_rare<-mean(Age_rare,  na.rm = TRUE)
# 
# 
# Age_missing<-which(is.na(mydata$Age))
# mydata$Title[Age_missing[1]]
# 
# 
# for(i in 1:length(Age_missing)){
#   Title1<-mydata$Title[Age_missing[i]]
#   if(Title1=='Mr'){
#     mydata$Age[Age_missing[i]]<-mean_Age_mr
#   }
#   if(Title1=='Miss'){
#     mydata$Age[Age_missing[i]]<-mean_Age_miss
#   }
#   if(Title1=='Mrs'){
#     mydata$Age[Age_missing[i]]<-mean_Age_mrs
#   }
#   if(Title1=='Rare Title'){
#     mydata$Age[Age_missing[i]]<-mean_Age_rare
#   }
#   if(Title1=='Master'){
#     mydata$Age[Age_missing[i]]<-mean_Age_master
#   }
#   
#   
# } 

#mydata
print("No NA's in the age field: can se seen below") 
mydata$Age<-mydata1$Age
dim(mydata)
datasets<-mydata
nrow(datasets)
ncol(datasets)
folds<-array(NA,c(fold_size,8,n_folds))
#Random Forest algorithm starts here:
n_folds=3

cross_validation_split<-function(datasets,n_folds){
  fold_size<-nrow(datasets)/n_folds
  dataset_split_1<-matrix(nrow=fold_size , ncol=8)
  dataset_split_2<-matrix(nrow = fold_size,ncol = 8)
  datasets<-as.matrix(datasets)
  #dataset_split_1<-as.matrix(dataset_split_1) 
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
    #generating the training samples in the form of matrices
    #dataset_split_1<-datasets[,]

    #Place this sub sampled matrix in th array of matrices
    a[,,ii]<- dataset_split_1
    #dataset_split<-c(dataset_split,dataset_split_1)
  }
  return(a)
}


#We have now obtained the splitted datasets
b<-cross_validation_split(datasets,3)


#left_and_right<-array(NA,c(fold_size,8,n_folds))
datasets1<-matrix(ncol=8)
test_split<-function(index,value,datasets){
  #concat_data <- cbindX(concat_data, as.data.frame(mydata[5,]),as.data.frame(mydata[6,]))
 # rm(concat_data_left)
#  rm(concat_data_right)
  concat_data_left<-data.frame(ncol=8)
  concat_data_right<-data.frame(ncol=8)
  # concat_data_left<- data.frame(
  #   PassengerId     =integer(),
  #   Survived      =integer()  ,
  #   Pclass           =integer(),
  #   Name      =character(),
  #   Sex                   =character(),
  #   Age        =double(),
  #   SibSp        =integer(),
  #   Parch                  =integer(),
  #   Ticket       =character(),
  #   Fare                     =double(),  
  #   Cabin                     =character(),                                                  
  #   Embarked                  =character(),   
  #   Title  =character() )
  # concat_data_right<- data.frame(
  #   PassengerId     =integer(),
  #   Survived      =integer()  ,
  #   Pclass           =integer(),
  #   Name      =character(),
  #   Sex                   =character(),
  #   Age        =double(),
  #   SibSp        =integer(),
  #   Parch                  =integer(),
  #   Ticket       =character(),
  #   Fare                     =double(),  
  #   Cabin                     =character(),                                                 
  #   Embarked                  =character(),   
  #   Title  =character() )
  # 
  left<-c()
  right<-c()
  kim<-do.call(rbind,as.list(datasets))
  kim<-t(kim)
  datasets1<-matrix(kim,ncol=8)
  #print(dim(datasets1))
  print("In function test_split")
  print(dim(datasets1))
  for(i in 1:nrow(datasets1)){
    if(datasets1[i,index]< value){
      concat_data_left <- cbindX(concat_data_left, as.data.frame(datasets1[i,]))
    }
    else{
      concat_data_right <- cbindX(concat_data_right, as.data.frame(datasets1[i,]))
    }
  }
 # return(list(y1<-concat_data_left ,y2<-concat_data_right))
  #print(dim(concat_data_left))
  #print(dim(concat_data_right))
  groups<-list(concat_data_left,concat_data_right)
  print("Harsha")
#  print(groups[[1]])
  return(groups)
  
}
concat_data_left<-data.frame(ncol=8)
concat_data_right<-data.frame(ncol=8)
m1<-list(concat_data_left, concat_data_right)
#We obtain the splitted datasets based on values of the feature elements
#groups<-test_split(3,2,datasets)

#b[,,1]

#train_set<-array(NA,dim=c(fold_size,8,n_folds))
#train_set<-array()
#test_set<-array(NA,c(fold_size,8,1))
#set_2<-array(NA,c(fold_size,8,1))

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
      Actual_value<-set_2[i,2]
      actual<-c(actual,Actual_value)

    }
    accuracy<-accuracy_metric(actual,predicted)
    scores<-c(scores,accuracy)
  }

  return(scores)

}

#Calculating the Accuracy of the predictions

# accuracy_metric<-function(actual,predicted){
#   correct<-0
#   accuracy_percentage<-1
#   for(i in 1:length(actual)){
#     if(actaul[i]==predicted[i]){
#       correct<-correct+1
#     }
#   }
#   accuracy_percentage<-(correct/length(actual)*100)
#   return(accuracy_percentage)
#   
# }

matrix3<-matrix()

#Evaluating the ginin score for finding the best feature and the best split value
gini_index<-function(groups,class_values){
  gini<-0.0
  print("Hello in gini function")
  print(groups[[1]])
  dimention<-dim(groups)
  length3<-dimention[2]
  #groups<-list()
  list_for_groups<-c()
  size_2<-c()
  gg<-matrix()
  #count_of_class_values<-length(class_values)
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
      # for(i in 1:size1){
      #   gg<-as.matrix(group[[j]])
      #   list_for_groups <-gg[2,]
      #   
      #   #list_for_groups<-c(list_for_groups,Actual_value)
      #   
      # }
      
      #for(i in 1:size1){
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
  #kim<-do.call(rbind,as.list(train_set))
  index<-sample(1:7)
  features<-index[1:n_features]
  # while(length(features)< n_features){
  #  
  #   # if(floor(any(features)==index)){
  #   #   
  #   # }
  #  
  # }
  kim<-do.call(rbind,as.list(datasets))
  print("In function getsplit")
  #print(dim(kim))
  kim<-t(kim)
  datasets2<-matrix(kim,ncol=8)
  print(dim(datasets2))
 # print(dim(datasets2))
  
  groups<-list()
  for(i in 1:7){
    for(j in 1:nrow(datasets2)){
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

#install.packages("plyr")
#library(plyr)
# to_terminal<-function(group){
#   list_for_outcomes<-c()
#   list99<-c()
#   group<-concat_data_left
#   dimention<-dim(group)
#   length3<-dimention[2]
#   gg3<-as.matrix(group)
#   list_for_outcomes <-gg3[2,]
#   list99<-names(sort(summary(as.factor(list_for_outcomes)), decreasing=T)[1])
#   #list99[1]
#   
#   as.integer(list99)
#   return(as.integer(list99))
#   #return(which.max(table(list_for_outcomes)))
#  # return(max(unique(list_for_outcomes)))
#   
# }
# 
# to_terminal(concat_data_left)
# node<-list(index=2,value=3, groups=groups)
# left<-list()
# right<-list()

# split<-function(node,max_depth,min_size,n_features,depth){
#   left<-node$groups[1]
#   right<-node$groups[2]
#   
#   
# }

#root<-list()
root<-get_split(train_set,8)
root$groups[[1]]

#split()
#groups<-list(concat_data_left,concat_data_right)
