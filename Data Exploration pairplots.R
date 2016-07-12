library("psych", lib.loc="~/Library/R/3.2/library")

## Read in dataset that is all of the numberical data from the initial (fall) survey

Numerical_Fall <- read.csv("~/Code/R/video_lectures/Numerical_Fall.csv")

## Separate into faculty and student

Faculty_Numerical_Fall <- subset(Numerical_Fall, Group == "Faculty")
Student_Numerical_Fall <- subset(Numerical_Fall, Group == "Student")

## Generate pairplots to look at correlation structure across all data

pairs.panels(Faculty_Numerical_Fall[,2:7], main = "Faculty All Responses")
pairs.panels(Student_Numerical_Fall[,2:7], main = "Students All Responses")

 #  ANOVA of everything together

engage <- as.numeric(Numerical_Fall$Engage)
video_ANOVA<- lm(engage ~ Numerical_Fall$Video*Numerical_Fall$Group)
anova(video_ANOVA_engage)

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