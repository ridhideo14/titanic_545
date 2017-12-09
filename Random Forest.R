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
