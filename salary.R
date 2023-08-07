
## week 6 assignment - by Anirudh Vunnam


library(tidyverse)
library(ggplot2)
library(broom)
library(dplyr)
library(tidyr)

salarynew<-read.csv("salary_survey.csv")
attach(salarynew)
head(salarynew)
str(salarynew)
summary(salarynew)

#regression of log salary using qualification as a dependent variable

salary2<-lm(log(salary)~qualification)
summary(salary2)

#creating dummy variables

salarynew$white<-ifelse(salarynew$job_type=="white",1,0)

salarynew$blue<-ifelse(salarynew$job_type=="blue",1,0)

head(salarynew)
attach(salarynew)

#regression graph by the use of dummy-variable 

dummyvar<-lm(log(salary)~qualification+white)
summary(dummyvar)

#Two regression lines in the scatter plot

ggplot(data=salarynew, aes(y=log(salarynew$salary), x=salarynew$qualification ,color=salarynew$job_type))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(title = "Graph :Scatter Plot of salary log for Male and Female",
       x="Qualification",y="ln(salary)")

#constructing the subsets for the data 

salarynew_1<-salarynew %>%filter(white=="1")
str(salarynew_1)

salarynew_2<-salarynew %>% filter(blue=="1")
str(salarynew_2)

#following is the regression line for white job-type

whitelog<-lm(log(salarynew_1$salary)~salarynew_1$qualification)
summary(whitelog)

#following is the regression line for job-type

bluelog<-lm(log(salarynew_2$salary)~salarynew_2$qualification)
summary(bluelog)

#original regressions tables are following

salarylog<-tidy(lm(log(salary)~qualification))
dummyvar<-tidy(lm(log(salary)~qualification+white))

model_new<-bind_rows(
  salarylog %>% mutate(model=1),
  dummyvar %>% mutate(model=2))
model_new

combined<-combined_model %>%
  select(-statistic, -p.value) %>%
  mutate_each(funs(round(.,2)),-term) %>%
  gather(key,value,estimate:std.error) %>% 
  spread(model,value)
combined



