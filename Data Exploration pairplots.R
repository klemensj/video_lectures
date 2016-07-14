## this code creates a set of pairplots for all of the response variables for faculty and students
## it also runs a simple factor factor interaction ANOVA for each response variable 
## result is that responses are highly correlated, more for faculty than for students
## faculty or student and video type always highly significant
## interaction significant for response variables engage, enjoy, maintain, 
## interaction not significant for extract, retain, or context


library("psych", lib.loc="~/Library/R/3.2/library")

## Read in dataset that is all of the numerical data from the initial (fall) survey

Numerical_Fall <- read.csv("data/Numerical_Fall.csv")

## Separate into faculty and student

Faculty_Numerical_Fall <- subset(Numerical_Fall, Group == "Faculty")
Student_Numerical_Fall <- subset(Numerical_Fall, Group == "Student")

## Generate pairplots to look at correlation structure across all data

pairs.panels(Faculty_Numerical_Fall[,2:7], main = "Faculty All Responses")
pairs.panels(Student_Numerical_Fall[,2:7], main = "Students All Responses")

##  ANOVA of everything together for each response variable

engage <- as.numeric(Numerical_Fall$Engage)
video_ANOVA<- lm(engage ~ Numerical_Fall$Video*Numerical_Fall$Group)
anova(video_ANOVA)

extract <- as.numeric(Numerical_Fall$Extract)
video_ANOVA <- lm(extract ~ Numerical_Fall$Video*Numerical_Fall$Group)
anova(video_ANOVA)

enjoy <- as.numeric(Numerical_Fall$Enjoy)
video_ANOVA <- lm(enjoy ~ Numerical_Fall$Video*Numerical_Fall$Group)
anova(video_ANOVA)

retain <- as.numeric(Numerical_Fall$Retain)
video_ANOVA <- lm(retain ~ Numerical_Fall$Video*Numerical_Fall$Group)
anova(video_ANOVA)

context <- as.numeric(Numerical_Fall$Context)
video_ANOVA <- lm(context ~ Numerical_Fall$Video*Numerical_Fall$Group)
anova(video_ANOVA)

maintain <- as.numeric(Numerical_Fall$Maintain)
video_ANOVA <- lm(maintain ~ Numerical_Fall$Video*Numerical_Fall$Group)
anova(video_ANOVA)

## From here on idea is to do ANOVA and create comparisons of 
## Longform with regular student data just take AllStudentData , 
## create a bunch of new columns based on numbers

Numerical_Spring<-subset

## t test of the test scores

quiz_results<-read.csv("data/quiz_results.csv", header = TRUE, na.strings=".", check.names=FALSE)
t.test(quiz_results$animated, quiz_results$powerpoint)
