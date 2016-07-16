## this code creates a set of pairplots for all of the response variables for faculty and students
## it also runs a simple factor factor interaction ANOVA for each response variable 
## result is that responses are highly correlated, more for faculty than for students
## faculty or student and video type always highly significant
## interaction significant for response variables engage, enjoy, maintain, 
## interaction not significant for extract, retain, or context


library("psych", lib.loc="~/Library/R/3.2/library")

## Read in dataset that is all of the numerical data from the initial (fall) survey

Numerical_Fall <- read.csv("data/Numerical_Fall.csv", header = TRUE, na.strings=".", check.names=FALSE)

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

## Created a new dataset in excel of Longform data in numerical form Numerical_Spring.csv

Numerical_Spring<-read.csv("data/Numerical_Spring.csv", header = TRUE, na.strings=".", check.names=FALSE)

## Bind together with fall student data

SpringFallStudent<-rbind(Student_Numerical_Fall,Numerical_Spring)

##ANOVAS comparing fall and spring videos

engage <- as.numeric(SpringFallStudent$Engage)
video_ANOVA<- lm(engage ~ SpringFallStudent$Video*SpringFallStudent$Group)
anova(video_ANOVA)

extract <- as.numeric(SpringFallStudent$Extract)
video_ANOVA <- lm(extract ~ SpringFallStudent$Video*SpringFallStudent$Group)
anova(video_ANOVA)

enjoy <- as.numeric(SpringFallStudent$Enjoy)
video_ANOVA <- lm(enjoy ~ SpringFallStudent$Video*SpringFallStudent$Group)
anova(video_ANOVA)

retain <- as.numeric(SpringFallStudent$Retain)
video_ANOVA <- lm(retain ~ SpringFallStudent$Video*SpringFallStudent$Group)
anova(video_ANOVA)

context <- as.numeric(SpringFallStudent$Context)
video_ANOVA <- lm(context ~ SpringFallStudent$Video*SpringFallStudent$Group)
anova(video_ANOVA)

maintain <- as.numeric(SpringFallStudent$Maintain)
video_ANOVA <- lm(maintain ~ SpringFallStudent$Video*SpringFallStudent$Group)
anova(video_ANOVA)

## Means for use in generating interaction plots

fallavePP<-apply(subset(Student_Numerical_Fall[,2:7],Student_Numerical_Fall$Video == "PP"),2,mean, na.rm=TRUE)
fallaveAN<-apply(subset(Student_Numerical_Fall[,2:7],Student_Numerical_Fall$Video == "AN"),2,mean, na.rm=TRUE)
springavePP<-apply(subset(Numerical_Spring[,2:7],Numerical_Spring$Video == "PP"),2,mean, na.rm=TRUE)
springaveAN<-apply(subset(Numerical_Spring[,2:7],Numerical_Spring$Video == "AN"),2,mean, na.rm=TRUE)

fallavePP
fallaveAN
springavePP
springaveAN

## Create line graph for interaction plot 

################Moved this to Interaction_plots.R


## t test of the test scores

quiz_results<-read.csv("data/quiz_results.csv", header = TRUE, na.strings=".", check.names=FALSE)
t.test(quiz_results$animated, quiz_results$powerpoint)
apply(quiz_results,2,mean, na.rm=TRUE)
apply(quiz_results,2,sd, na.rm=TRUE)

## Boxplot of the t test