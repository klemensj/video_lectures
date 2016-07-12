library("psych", lib.loc="~/Library/R/3.2/library")

## Read in dataset that is all of the numberical data from the initial (fall) survey

Numerical_Fall <- read.csv("~/Code/R/video_lectures/Numerical_Fall.csv")

## Separate into faculty and student

Faculty_Numerical_Fall <- subset(Numerical_Fall, Group == "Faculty")
Student_Numerical_Fall <- subset(Numerical_Fall, Group == "Student")

## Generate pairplots to look at correlation structure across all data

pairs.panels(Faculty_Numerical_Fall[,2:7], main = "Faculty All Responses")
pairs.panels(Student_Numerical_Fall[,2:7], main = "Students All Responses")

