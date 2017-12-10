mydata <- read.csv("C:/FALL 2017/STAT 545/STAT 545 Project/train.csv")
mydata
mydata<-as.matrix(mydata)
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
mydata$Title <- gsub('(.*, )|(\\..*)', '', mydata$Name)
table(mydata$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

mydata$Title[mydata$Title == 'Mlle']        <- 'Miss' 
mydata$Title[mydata$Title == 'Ms']          <- 'Miss'
mydata$Title[mydata$Title == 'Mme']         <- 'Mrs' 
mydata$Title[mydata$Title %in% rare_title]  <- 'Rare Title'
Title_Miss<-which(mydata$Title=='Miss')
Title_Mr<-which(mydata$Title=='Mr')
Title_Mrs<-which(mydata$Title=='Mrs')
Title_Master<-which(mydata$Title=='Master')
Title_Rare<-which(mydata$Title=='Rare Title')


#Code for filling the missing ages starts here. The missing ages are filled based on the avaerage of the title means.  

Age_miss<-mydata$Age[Title_Miss]
mean_Age_miss<-mean(Age_miss,  na.rm = TRUE)

Age_mr<-mydata$Age[Title_Mr]
mean_Age_mr<-mean(Age_mr,  na.rm = TRUE)

Age_mrs<-mydata$Age[Title_Mrs]
mean_Age_mrs<-mean(Age_mrs,  na.rm = TRUE)

Age_master<-mydata$Age[Title_Master]
mean_Age_master<-mean(Age_master,  na.rm = TRUE)

Age_rare<-mydata$Age[Title_Rare]
mean_Age_rare<-mean(Age_rare,  na.rm = TRUE)


Age_missing<-which(is.na(mydata$Age))
mydata$Title[Age_missing[1]]


for(i in 1:length(Age_missing)){
  Title1<-mydata$Title[Age_missing[i]]
  if(Title1=='Mr'){
    mydata$Age[Age_missing[i]]<-mean_Age_mr
  }
  if(Title1=='Miss'){
    mydata$Age[Age_missing[i]]<-mean_Age_miss
  }
  if(Title1=='Mrs'){
    mydata$Age[Age_missing[i]]<-mean_Age_mrs
  }
  if(Title1=='Rare Title'){
    mydata$Age[Age_missing[i]]<-mean_Age_rare
  }
  if(Title1=='Master'){
    mydata$Age[Age_missing[i]]<-mean_Age_master
  }
  
  
} 

#mydata
print("No NA's in the age field: can se seen below") 
mydata$Age
dim(mydata)
datasets<-mydata
nrow(datasets)

folds<-array(NA,c(fold_size,13,n_folds))
#Random Forest algorithm starts here:
n_folds=3

cross_validation_split<-function(datasets,n_folds){
  fold_size<-nrow(datasets)/n_folds
  dataset_split_1<-matrix(nrow=fold_size , ncol=13)
  dataset_split_2<-matrix(nrow = fold_size,ncol = 13)
  
  
  # Creating an array of matrices
  random_numbers<-c()
  a<-array(NA,c(fold_size,13,n_folds))
  for(ii in 1:(n_folds)){
    dataset_split_list<-rep(NA,13)
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


#left_and_right<-array(NA,c(fold_size,13,n_folds))

test_split<-function(index,value,datasets){
  #concat_data <- cbindX(concat_data, as.data.frame(mydata[5,]),as.data.frame(mydata[6,]))
  concat_data_left<-data.frame(ncol=13)
  concat_data_right<-data.frame(ncol=13)
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
  for(i in 1:nrow(datasets)){
    if(datasets[i,index]<value){
      concat_data_left <- cbindX(concat_data_left, as.data.frame(datasets[1,]))
    }
    else{
      concat_data_right <- cbindX(concat_data_right, as.data.frame(datasets[2,]))
    }
  }
  
  return(list(concat_data_left,concat_data_right))
  
}

#We obtain the splitted datasets based on values of the feature elements
m1<-test_split(3,10,datasets)

#b[,,1]

train_set<-array(NA,c(fold_size,13,n_folds))
test_set<-array(NA,c(n,m,1))
set_2<-array(NA,c(n,m,1))

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
    train_set<-train_set[train_set[,,-i]]
    train_set<-array(train_set,dim=c(n,m,(n_folds-1)))
    
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

accuracy_metric<-function(actual,predicted){
  correct<-0
  accuracy_percentage<-1
  for(i in 1:length(actual)){
    if(actaul[i]==predicted[i]){
      correct<-correct+1
    }
  }
  accuracy_percentage<-(correct/length(actual)*100)
  return(accuracy_percentage)
  
}


#Evaluating the ginin score for finding the best feature and the best split value
gini_index<-function(groups,class_values){
  gini<-0.0
  #groups<-list()
  list_for_groups<-c()
  size_2<-c()
  gg<-matrix()
  #count_of_class_values<-length(class_values)
  for(i in 1:length(class_values)){
    count_of_value <-0
    for(j in 1:length(groups)){
      size1<-length(j)
      if(size1==0){
        next
      }
      group<-groups[j]
      size_2<-dim(as.matrix(groups[[j]]))
      size1<-size_2[2]
      # for(i in 1:size1){
      #   gg<-as.matrix(group[[j]])
      #   list_for_groups <-gg[2,]
      #   
      #   #list_for_groups<-c(list_for_groups,Actual_value)
      #   
      # }
      
      #for(i in 1:size1){
         gg<-as.matrix(groups[[j]])
           list_for_groups <-gg[2,]
           
           #list_for_groups<-c(list_for_groups,Actual_value)
           
       #  }
      if(i==1){
       count_of_value <-length(which(list_for_groups == "0"))
      }
      if(i==2){
        count_of_value <-length(which(list_for_groups == "1"))
      }
      
      proportion<-count_of_value/size1
      gini<-gini+(proportion*(1-proportion))
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
  # while(length(features)< n_features){
  #  
  #   # if(floor(any(features)==index)){
  #   #   
  #   # }
  #  
  # }
  groups<-list()
  for(i in 1:n_features){
    for(j in 1:nrow(datasets)){
      groups<-test_split(i,datasets [i,j],datasets)
      gini<-gini_index(groups,class_values)
      if(gini<b_score){
        b_index<-i
        b_value<-datasets[j,i]
        b_score<-gini
        b_groups<-groups
        
      }
    }
  }
  
  return(list(index=b_index,value=b_value, groups=b_groups))
  
}

