# documentation for package here https://github.com/jbryer/likert
# DEMO is very useful here  https://github.com/jbryer/likert/blob/master/demo/likert.R


require(likert)
# require(reshape)

# The dataset to import must consist of two elements. 
# The first column is called “Group” and contains group membership data
# Subsequent columns are headed with the question text for each question and contain the 
### responses that make up the data set CHANGE FILENAME IN THIS COMMAND

Survey<-read.csv("data/PP.csv", header = TRUE, na.strings=".", check.names=FALSE)


#Create the data frame of responses (strip group data from the dataset)
### in preparation for creating the likert item

Size<-ncol(Survey)
SurveyDF<-as.data.frame(Survey[ ,2:Size])
question_number<-ncol(SurveyDF)

# Recode the ordering of the levels for each question 
# CHANGE LEVELS IF NECESSARY

n<-1
for(n in 1:question_number)
{
SurveyDF[, n]<-factor(SurveyDF[, n], levels = c('Strongly impaired', 'Slightly impaired', 'About the Same', 'Slightly improved', 'Strongly improved'))
print(n)
}

# Create the vector of group names and set the order of the levels so that ###the graph stacks nicely
# CHANGE LEVELS IF NECESSARY

SurveyG<-Survey$Group
SurveyG<-factor(SurveyG, levels = c('Longform', 'Faculty','Student'))

#create the likert item and print the summary

SurveyLIK<-likert(SurveyDF, grouping = SurveyG)
summary(SurveyLIK)

#Plot the likert item with a title
#For some reason the title only works when the histogram of responses is ###turned to FALSE
# # CHANGE TITLE

title<-"Compared to a typical in-person lecture the POWERPOINT video lecture\n affects my (my students) learning experience:\n "
plot(SurveyLIK, include.histogram = F) + ggtitle(title)