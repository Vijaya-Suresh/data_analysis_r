#Import all the required libraries
install.packages("readr")
install.packages("dplyr")
library(readr)
library(dplyr)

#import the data
data <- read_csv("Mall_Customers.csv")
View(data)

#BASIC_INSIGHTS
glimpse(data)                      #DATA_TYPE
summary(data)                      #STATISTICAL_SUMMARY

#MISSING_VALUE
sum(is.na(data))                  #SUM_OF_MISSING_VALUE

#DISTINCT_DATA   
distinct(data)                     #DISTINCT_DATA

#RENAMING_COLUMN
colnames(data)[4]= "Income"
colnames(data)[5]="Score"
head(data)

#EXPLORATORY DATA ANALYSIS
#BAR_PLOT
score = pull(data,Score)
score_1=cut(score,breaks=seq(1,101,by=10),right=FALSE)
table(score_1)
barplot(table(score_1),col=c('red','pink'))

table(score)
barplot(table(score),col=c('red','yellow'))

#BOXPLOT
boxplot(score)

#SUBSETTING INTERQUARTILE DATA OF SCORE
df =filter(data,Score>=35 & Score<=73)
df
glimpse(df)
summary(df)
View(df)

#BAR CHART OF TWO ATTRIBUTES
install.packages("ggplot2")
library(ggplot2)
ggplot(df, aes(x =factor(Age), y = Score,fill=factor(Age))) + 
  geom_bar(stat = "identity")
  

ggplot(df, aes(x =factor(Genre), y = Score,fill=factor(Genre))) + 
  geom_bar(stat = "identity")

#BARCHART
age = pull(df,Age)
barplot(table(age),col=c('pink','yellow'))

#SCATTER_PLOT
plot(x=df$Age,y=df$Income)

#HEAT_MAP
ggplot(df,aes(x=Age,y=Income,fill =Score))+
  geom_tile(color='yellow',linewidth=0.2)

#BOXPLOT
boxplot(df$Income)

#subsetting  female customers
female = filter(df,Genre=='Female')
View(female)
#basic insight
summary(female)

#Correlation
install.packages("ggpubr")
library(ggpubr)

cor(female$Income,female$Score)
cor.test(female$Income,female$Score)

#BAR CHART OF TWO ATTRIBUTES
female_age = pull(female,Age)
female_age=cut(female_age,breaks=seq(18,80,by=10),right=FALSE)
table(female_age)

ggplot(female, aes(x =factor(female_age), y = Score)) + 
  geom_bar(stat = "identity")

#subsetting female customers based on age group of highest score
female_filtered=filter(female,Age>=28 & Age<38)
View(female_filtered)
summary(female_filtered)

#BAR CHART OF TWO ATTRIBUTES
ggplot(female_filtered, aes(x =factor(Age), y = Score)) + 
  geom_bar(stat = "identity")

#boxplot
boxplot(female_filtered$Income)

#Correlation
cor(female_filtered$Income,female_filtered$Score)
cor.test(female_filtered$Income,female_filtered$Score)

#subsetting female customers based on age of highest score
female_filtered_score = filter(female_filtered,Age>=30 & Age<=32)
View(female_filtered_score)

#BAR CHART OF TWO ATTRIBUTES
ggplot(female_filtered_score, aes(x =factor(Age), y = Income)) + 
  geom_bar(stat = "identity")

#group_by age and summarise
y=female_filtered_score%>%group_by(Age)%>%
  summarise(income=mean(Income))
z=female_filtered_score%>%group_by(Age)%>%
  summarise(score=mean(Score))
average_1=merge(y,z)

#barplot
barplot(as.matrix(average_1,col=c("orange","white","green")))

#Correlation btw average income and average score
cor(y,z)

#heatmap
heatmap(as.matrix(average_1),Rowv = NA, Colv = NA)

#subsetting male customers
male =filter(df,Genre=='Male')
#BASIC INSIGHTS
View(male)
summary(male)

#BAR CHART OF TWO ATTRIBUTES
male_age = pull(male,Age)
male_age=cut(male_age,breaks=seq(18,80,by=10),right=FALSE)
table(male_age)

ggplot(male, aes(x =factor(male_age), y = Score,fill=factor(male_age))) + 
  geom_bar(stat = "identity")

#subsetting male customers based on age group highest score
male_filtered=filter(male,Age>=18 & Age<28)
View(male_filtered)
summary(male_filtered)

#BAR CHART OF TWO ATTRIBUTES
ggplot(male_filtered, aes(x =factor(Age), y = Income,fill =factor(Age) )) + 
  geom_bar(stat = "identity")

#boxplot - Income
boxplot(male_filtered$Income)

#correlation
cor(male_filtered$Income,male_filtered$Score)
cor.test(male_filtered$Income,male_filtered$Score)

#subsetting male customers based on age of highest score
male_filtered_score = filter(male_filtered,Age==19 | Age==26 |Age ==27)
View(male_filtered_score)

#group_by age and summarise
a=male_filtered_score%>%group_by(Age)%>%
  summarise(income=mean(Income))

b=male_filtered_score%>%group_by(Age)%>%
  summarise(score=mean(Score))
average =merge(a,b)
average

#barplot
barplot(as.matrix(average),col=c("coral","tan","azure"))

#Correlation btw average income and average score
cor(a,b)

#Heatmap
heatmap(as.matrix(average),Rowv = NA, Colv = NA)

