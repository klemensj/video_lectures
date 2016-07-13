# documentation for package here https://github.com/jbryer/likert
# DEMO is very useful here  https://github.com/jbryer/likert/blob/master/demo/likert.R


require(likert)
# require(reshape)

# The dataset to import must consist of two elements. 
# The first column is called “Group” and contains group membership data
# Subsequent columns are headed with the question text for each question and contain the 


## create vector of video types to make this task easier

Types <-c("AN", "DI","NC", "PP", "WB")

## suck in all the data which was initially grouped by Video type
## Assign command uses the value in the vector as a variable, paste allows file names

n<-1
for(n in 1:5)
{
  assign(Types[n],read.csv(paste("data/",Types[n],".csv",sep=""), header = TRUE, na.strings=".", check.names=FALSE))
}

## this  adds a column to each that will be the grouping column
## in this case the video type name

AN$Video <- "AN"
DI$Video <- "DI"
NC$Video <- "NC"
PP$Video <- "PP"
WB$Video <- "WB"

#create one big data frame

AllData<-rbind(AN,DI,NC,PP,WB)

#reorder the columns to get the final presentation I want
## with extract contex retain grouped and attention, engagement,enjoyment grouped

AllData<-AllData[c(1,2,4,5,3,6,7,8)]

#create one data frame per likert graph

FacultyData <- subset(AllData, Group == "Faculty")
StudentData <- subset(AllData, Group == "Student")


#Create a data frame of just the responses (strip group data from the dataset)
### in preparation for creating the likert item


FacultyDF<-as.data.frame(FacultyData[ ,2:7])
StudentDF<-as.data.frame(StudentData[ ,2:7])


# Recode the ordering of the levels for each dataset 
# CHANGE LEVELS AND QUESTION NUMBER TO USE THIS CODE WITH DIFFERENT DATASETS

question_number<-6

n<-1
for(n in 1:question_number)
{
FacultyDF[, n]<-factor(FacultyDF[, n], levels = c('Strongly impaired', 'Slightly impaired', 'About the Same', 'Slightly improved', 'Strongly improved'))
}

n<-1
for(n in 1:question_number)
{
  StudentDF[, n]<-factor(StudentDF[, n], levels = c('Strongly impaired', 'Slightly impaired', 'About the Same', 'Slightly improved', 'Strongly improved'))
}


# Create the vector of group names and set the order of the levels so that 
###the graph stacks nicely
# CHANGE LEVELS IF NECESSARY

FacultyGroup<-FacultyData$Video
FacultyGroup<-factor(FacultyGroup, levels = c('PP','NC','DI','WB','AN'))

StudentGroup<-StudentData$Video
StudentGroup<-factor(StudentGroup, levels = c('PP','NC','DI','WB','AN'))

#create the likert item and print the summary

FacultyLIK<-likert(FacultyDF, grouping = FacultyGroup)
summary(FacultyLIK)

StudentLIK<-likert(StudentDF, grouping = StudentGroup)
summary(StudentLIK)

#Plot the likert item with a title
#For some reason the title only works when the histogram of responses is 
###turned to FALSE
# # CHANGE TITLE

title<-"FACULTY\nCompared to a typical in-person lecture the ______ video lecture\n affects my students':\n "
plot(FacultyLIK, include.histogram = F) + ggtitle(title)

title<-"STUDENTS\nCompared to a typical in-person lecture the ______ video lecture\n affects my:\n "
plot(StudentLIK, include.histogram = F) + ggtitle(title)