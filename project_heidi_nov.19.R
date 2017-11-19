#Heidi
mydata <- mydata %>% mutate(
  Survived = factor(Survived),
  Embarked = factor(Embarked)
)

#embark
p_emb <- ggplot(mydata, aes(Embarked, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Embarked')
p_emb
#sib
p_sib <- ggplot(mydata, aes(SibSp, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = '# of siblings')
p_sib
#title
p_title <- ggplot(mydata, aes(Title, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'titles')
p_title
#traveling along or not
mydata["Along"] <- NA
mydata$Along[mydata$SibSp == 0 & mydata$Parch == 0 ] <- T
mydata$Along[mydata$SibSp != 0 & mydata$Parch != 0] <- F

p_along <- ggplot(mydata, aes(Along, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'along')
p_along
