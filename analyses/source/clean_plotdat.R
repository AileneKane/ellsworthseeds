colnames(plotdat)[1]<-"plot"

#clean some plot names so that they merge properly
# plotdat$plot[which(plotdat$plot=="C1-10V")]<-"C1-10VB"
# plotdat$plot[which(plotdat$plot=="C1-13V")]<-"C1-13VB"
# plotdat$plot[which(plotdat$plot=="C1-15V")]<-"C1-15VB"
# plotdat$plot[which(plotdat$plot=="C1-8V")]<-"C1-8VB"
# plotdat$plot[which(plotdat$plot=="C1-9V")]<-"C1-9VB"
# plotdat$plot[which(plotdat$plot=="C1-19V")]<-"C1-19VB"
# plotdat$plot[which(plotdat$plot=="N2-18V")]<-"N2-18VB"
# plotdat$plot[which(plotdat$plot=="N2-1V")]<-"N2-1VB"
# plotdat$plot[which(plotdat$plot=="N2-4V")]<-"N2-4VB"
# plotdat$plot[which(plotdat$plot=="N2-7V")]<-"N2-7VB"
# plotdat$plot[which(plotdat$plot=="N2-2V")]<-"N2-2VB"
# plotdat$plot[which(plotdat$plot=="N2-17V")]<-"N2-17VB"
# plotdat$plot[which(plotdat$plot=="N2-24V")]<-"N2-24VB"


#Instead,create a new column for merging that removes B and replaces with V in all cases

plotdat$plot2<-gsub("VB","V",plotdat$plot)
