mydata = read.csv("train.csv")
mydata
names<-mydata[,4]
#names_characterized<-as.character(names[800])
#splitted_names<-strsplit(names_characterized,",")
splitted_names<-c()

for(i in 1:length(names)){
  names_characterized<-as.character(names[i])
  #print(names_characterized)
  splitted_name<-strsplit(names_characterized,",")
  print(splitted_name)
  list5<-splitted_name[[1]]
  list6<-list5[1]
  splitted_names[i]<-list6
  print(list6)
  splitted_names[i]
}

names_characterized<-as.character(names[40])
splitted_name<-strsplit(names_characterized,",")
list5<-splitted_name[[1]]
list6<-list5[1]
splitted_names[40]<-list6
splitted_names[40]

str(mydata)
library('ggplot2') # visualization
library('tidyverse')
dim(mydata)
str(mydata) 
summary(mydata)
sum(mydata$Cabin=="")

#people with same ticket are from same group
mydata<-mutate(mydata, grp=rep(0,nrow(mydata)))#grp number, 0 =>single
uniq_ticket<-table(mydata$Ticket)
not_single<-uniq_ticket[uniq_ticket>1]
for(ii in 1:length(not_single)){
  this_ticket<-names(not_single)[ii]
  tickt_index<-which(mydata$Ticket==this_ticket)
  mydata$grp[tickt_index]<-ii
}
mydata %>% group_by(grp) %>% summarise(sur=mean(Survived))->grp_stats

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

library(ggplot2)
#plot(mydata$Age, mydata$Survived, data = mydata)
ggplot(mydata, aes(Sex,fill = factor(Survived))) +
  geom_histogram(stat = "count")
ggplot(mydata, aes(Pclass,fill = factor(Survived))) +
  geom_histogram(stat = "count")

