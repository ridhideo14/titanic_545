mydata = read.csv("C:/FALL 2017/STAT 545/STAT 545 Project/train.csv")
mydata
names<-mydata[,4]
#names_characterized<-as.character(names[800])
#splitted_names<-strsplit(names_characterized,",")
splitted_names<-c()

for(i in 1:length(names)){
  names_characterized<-as.character(names[i])
  splitted_name<-strsplit(names_characterized,",")
  list5<-splitted_name[[1]]
  list6<-list5[1]
  splitted_names[i]<-list6
  splitted_names[i]
}

names_characterized<-as.character(names[40])
splitted_name<-strsplit(names_characterized,",")
list5<-splitted_name[[1]]
list6<-list5[1]
splitted_names[40]<-list6
splitted_names[40]