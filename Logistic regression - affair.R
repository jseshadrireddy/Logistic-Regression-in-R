affair <- read.csv(file.choose())
View(affair)
library(psych)
summary(affair)
describe(affair)
head(affair)
colnames(affair)
sum(is.na(affair))
boxplot(affair)
str(affair)
###
library(ggplot2)
ggplot(data = affair,aes(x=age))+geom_histogram()
ggplot(data = affair,aes(x=age))+geom_histogram(bins = 50)
# Plot and ggplot 
ggplot(data = affair,aes(x=affair$affairs,y=affair$age,fill=affair$affairs))+
  geom_boxplot()+ggtitle('Box Plot')
plot(affair$affairs,affair$gender)
plot(affair$age,affair$gender)
plot(affair$age,affair$gender)
plot(affair$yearsmarried,affair$gender)
plot(affair$children,affair$gender)
plot(affair$religiousness,affair$gender)
plot(affair$education,affair$gender)
plot(affair$occupation,affair$gender)
plot(affair$rating,affair$gender)
###
ggplot(data = affair,aes(x=affair$affairs,y=affair$affairs,fill=affair$affairs))+
  geom_boxplot()+ggtitle('Box Plot')
##
ggplot(data = affair,aes(x=affair$age,y=affair$affairs,fill=affair$age))+
  geom_boxplot()+ggtitle('Box Plot')
##
ggplot(data = affair,aes(x=affair$children,y=affair$affairs,fill=affair$children))+
  geom_boxplot()+ggtitle('Box Plot')
###
ggplot(data = affair,aes(x=affair$yearsmarried,y=affair$affairs,fill=affair$yearsmarried))+
  geom_boxplot()+ggtitle('Box Plot')
##
ggplot(data = affair,aes(x=affair$religiousness,y=affair$affairs,fill=affair$religiousness))+
  geom_boxplot()+ggtitle('Box Plot')
##
ggplot(data = affair,aes(x=affair$rating,y=affair$affairs,fill=affair$rating))+
  geom_boxplot()+ggtitle('Box Plot')
##
ggplot(data = affair,aes(x=affair$occupation,y=affair$affairs,fill=affair$occupation))+
  geom_boxplot()+ggtitle('Box Plot')
###dencity plot
ggplot(data=affair,aes(x = affair$gender, fill = affair$affairs)) +
  geom_density(alpha = 0.9, color = 'black')

ggtitle("gender - Density Plot")
##2
ggplot(data=affair,aes(x =affair$religiousness, fill = affair$affairs)) +
  geom_density(alpha = 0.9, color = 'blue')+
  
  ggtitle("religiousness - Density Plot")
##3
ggplot(data=affair,aes(x =affair$affairs, fill = affair$affairs)) +
  geom_density(alpha = 0.9, color = 'black')

ggtitle("affairs - Density Plot")
##4
ggplot(data=affair,aes(x =affair$occupation, fill = affair$affairs)) +
  geom_density(alpha = 0.9, color = 'blue')

ggtitle("occupation - Density Plot")
##5
ggplot(data=affair,aes(x =affair$age, fill = affair$affairs)) +
  geom_density(alpha = 0.9, color = 'blue')

ggtitle("age - Density Plot")
##6
ggplot(data=affair,aes(x =affair$yearsmarried, fill = affair$affairsr)) +
  geom_density(alpha = 0.9, color = 'blue')

ggtitle("yearsmarried - Density Plot")
##7
ggplot(data=affair,aes(x =affair$education, fill = affair$affairs)) +
  geom_density(alpha = 0.9, color = 'blue')

ggtitle("education - Density Plot")
##8
ggplot(data=affair,aes(x =affair$rating, fill = affair$affairs)) +
  geom_density(alpha = 0.9, color = 'blue')

ggtitle("rating - Density Plot")
##9
ggplot(data=affair,aes(x =affair$children, fill = affair$affairs)) +
  geom_density(alpha = 0.9, color = 'blue')

ggtitle("children - Density Plot")
##
str(affair)
####bar plot
ggplot(data = affair,aes(x=affair$children,fill=affair$affairs))+
  geom_bar(position = 'fill')
##
ggplot(data = affair,aes(x=affair$age,fill=affair$affairs))+
  geom_bar(position = 'fill')
##
ggplot(data = affair,aes(x=affair$affairs,fill=affair$affairs))+
  geom_bar(position = 'fill')
##
ggplot(data = affair,aes(x=affair$gender,fill=affair$affairs))+
  geom_bar(position = 'fill')
##
ggplot(data = affair,aes(x=affair$yearsmarried,fill=affair$affairs))+
  geom_bar(position = 'fill')
##
ggplot(data = affair,aes(x=affair$religiousness,fill=affair$affairs))+
  geom_bar(position = 'fill')
##
ggplot(data = affair,aes(x=affair$occupation,fill=affair$affairs))+
  geom_bar(position = 'fill')
##
ggplot(data = affair,aes(x=affair$rating,fill=affair$affairs))+
  geom_bar(position = 'fill')
##
ggplot(data = affair,aes(x=affair$education,fill=affair$affairs))+
  geom_bar(position = 'fill')

###preparing a model
affair =na.omit(affair)
affair$affairs=as.factor(affair$affairs)
model = glm(affairs~.,data = affair,family = "binomial")
prob = predict(model,type=c("response"),affair)
prob
confusion = table(prob>0.5,affair$affairs)
confusion
accuracy = sum(diag(confusion)/sum(confusion))
accuracy
####
library(ROCR)
data("ROCR.simple")
rocrpred = prediction(ROCR.simple$predictions,ROCR.simple$labels)
rocrperf = performance(rocrpred,"tpr","fpr")
str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
##
library(ROCR)
rocrpred=prediction(prob,affair$affairs)
