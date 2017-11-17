mydata = read.csv("mydata.csv")
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

#extract surname
mydata$Surname <- sapply(mydata$Name, function(x) strsplit(as.character(x), split = '[,.]')[[1]][1])

#check same surname within grp0
tmp<-mydata[mydata$grp==0,]
tmp_name<-table(tmp$Surname)
tmp_name<-tmp_name[tmp_name>1]
tmp[tmp$Surname==names(tmp_name)[2],]
tmp2<-mydata[mydata$Surname==names(tmp_name)[2],]#interesting case to tell one entry might be entered wrong
