## These are the line graph plots for the poster given at temple teaching conference
## weakness is don't show variation , based off data in Test results plots 

PlotData<-read.csv("data/Interaction_plots.csv", header = TRUE, na.strings=".", check.names=FALSE)

#subset student and faculty data
Plot1 <- subset(PlotData, Group=="Faculty")
Plot2 <- subset(PlotData, Group=="Student")
PlotL <- subset(PlotData, Group=="Longform")

## Create graphs of faculty and student interaction for Engagement and Extraction

plot.new()
xrange<-range(Plot1$X_coord)
yrange<-range(Plot1$Engage)
x<-c(1:5)
videolabels<-c("Whiteboard","Powerpoint","Newscaster","Diagrams","Animation")
plot(xrange, yrange, type="n", xaxt="n", xlab="Increasing Difficulty of Preparation",
     ylab="Engagement Score", cex.lab=1.5)
axis(1,at=x, labels=videolabels, cex.axis=1.5)
#faculty data
lines(Plot1$X_coord, Plot1$Engage,type="b", pch="F", cex=1.5)
#student data
lines(Plot2$X_coord, Plot2$Engage,type="b", pch="S", cex=1.5)

##text(1.9,3.1, "* p<0.05", pos=4)

#reset range for second graph
yrange<-range(Plot1$Extract)
## plot second graph for extract data
plot(xrange, yrange, type="n", xaxt="n", xlab="Increasing Difficulty of Preparation",
     ylab="Information Extraction Score", cex.lab=1.5)
axis(1,at=x, labels=videolabels, cex.axis=1.5)
#faculty data
lines(Plot1$X_coord, Plot1$Extract,type="b", pch="F", cex=1.5)
#student data
lines(Plot2$X_coord, Plot2$Extract,type="b", pch="S", cex= 1.5)


## Create plots for comparison of student fall survey S and spring longform L

xrange<-range(Plot2$X_coord)
yrange<-range(Plot2$Engage)
x<-c(1:5)
videolabels<-c("Whiteboard","Powerpoint","Newscaster","Diagrams","Animation")
plot(xrange, yrange, ylim = c(2.8,3.5),type="n", xaxt="n", xlab="Increasing Difficulty of Preparation",
     ylab="Engagement Score", cex.lab=1.5)
axis(1,at=x, labels=videolabels, cex.axis=1.5)
#faculty data
lines(Plot2$X_coord, Plot2$Engage,type="b", pch="S", cex=1.5)
#student data
lines(PlotL$X_coord, PlotL$Engage,type="b", pch="L", cex=1.5)

#reset range for second graph
yrange<-range(Plot2$Extract)
## plot second graph for extract data
plot(xrange, yrange, type="n", xaxt="n", ylim = c(3.1,3.6), xlab="Increasing Difficulty of Preparation",
     ylab="Information Extraction Score" , cex.lab=1.5)
axis(1,at=x, labels=videolabels, cex.axis=1.5)
#faculty data
lines(Plot2$X_coord, Plot2$Extract,type="b", pch="S", cex=1.5)
#student data
lines(PlotL$X_coord, PlotL$Extract,type="b", pch="L", cex=1.5)