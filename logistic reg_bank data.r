bank=read.csv(file.choose())
str(bank)
View(bank)
library(psych)
summary(bank)
describe(bank)
head(bank)
colnames(bank)
sum(is.na(bank))
boxplot(bank)
#######
library(ggplot2)
###density plot
ggplot(data=bank,aes(x =bank$age, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = "darkred"))+
  labs(title = 'bank data for age and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))

ggtitle("bank$age - Density Plot")
##2
ggplot(data=bank,aes(x =bank$job, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = "darkblue"))+
  labs(title = 'bank data for job and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))

ggtitle("bank$job - Density Plot")
##3
ggplot(data=bank,aes(x =bank$marital, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = "#999999"))+
  labs(title = 'bank data for marital and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##4
ggplot(data=bank,aes(x =bank$education, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = "#E69F00"))+
  labs(title = 'bank data for education and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##5
ggplot(data=bank,aes(x =bank$default, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for default and  y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##6
ggplot(data=bank,aes(x =bank$balance, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for balence and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##7
ggplot(data=bank,aes(x =bank$housing, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for housing and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##8
ggplot(data=bank,aes(x =bank$loan, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for loan and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##9
ggplot(data=bank,aes(x =bank$contact, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for contact and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##10
ggplot(data=bank,aes(x =bank$day, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for day and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##11
ggplot(data=bank,aes(x =bank$month, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for month and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##12
ggplot(data=bank,aes(x =bank$duration, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for duration and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))

##13
ggplot(data=bank,aes(x =bank$campaign, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for campaign and  y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##14
ggplot(data=bank,aes(x =bank$pdays, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for pdays and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##15
ggplot(data=bank,aes(x =bank$previous, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for previous and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##16
ggplot(data=bank,aes(x =bank$poutcome, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+ 
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for poutcome and y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##17
ggplot(data=bank,aes(x =bank$y, fill = bank$y)) +
  geom_density(alpha = 0.9, color = 'black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'bank data for y varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
########priparing a model
bank = na.omit(bank)
bank
model = glm(y~.,data = bank,family = "binomial")
model
model1 = glm(y~.-age,data = bank,family = 'binomial')
model1
prob = predict(model,type=c("response"),bank)
prob
confusion = table(prob>0.5,bank$y)
confusion
accuracy = sum(diag(confusion)/sum(confusion))
accuracy
library(ROCR)
rocrpred = prediction(prob,bank$y)
rocrperf = performance(rocrpred,"tpr","fpr")
str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

