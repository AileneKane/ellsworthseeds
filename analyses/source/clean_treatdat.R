#create a new column for merging that removes B and replaces with V in all cases
treatdat$plot2<-gsub("VB","V",treatdat$plot)
#treatdat$plot[which(treatdat$plot=="C1-10VB")]<-"C1-10V"
#treatdat$plot[which(treatdat$plot=="C1-13VB")]<-"C1-13V"
#treatdat$plot[which(treatdat$plot=="C1-15VB")]<-"C1-15V"
#treatdat$plot[which(treatdat$plot=="C1-19VB")]<-"C1-19V"
#treatdat$plot[which(treatdat$plot=="C1-8VB")]<-"C1-8V"
#treatdat$plot[which(treatdat$plot=="C1-9VB")]<-"C1-9V"
# treatdat$plot[which(treatdat$plot=="C1-12VB")]<-"C1-12V"
# treatdat$plot[which(treatdat$plot=="C2-12VB")]<-"C2-12V"
# treatdat$plot[which(treatdat$plot=="C2-14VB")]<-"C2-14V"
# treatdat$plot[which(treatdat$plot=="C2-15VB")]<-"C2-15V"
# treatdat$plot[which(treatdat$plot=="C2-17VB")]<-"C2-17V"
# treatdat$plot[which(treatdat$plot=="C2-20VB")]<-"C2-20V"
# treatdat$plot[which(treatdat$plot=="C2-23VB")]<-"C2-23V"
# treatdat$plot[which(treatdat$plot=="N1-11VB")]<-"N1-11V"
# treatdat$plot[which(treatdat$plot=="N1-14VB")]<-"N1-14V"
# treatdat$plot[which(treatdat$plot=="N1-15VB")]<-"N1-15V"
# treatdat$plot[which(treatdat$plot=="N1-17VB")]<-"N1-17V"
# treatdat$plot[which(treatdat$plot=="N1-24VB")]<-"N1-24V"
# treatdat$plot[which(treatdat$plot=="N1-25VB")]<-"N1-25V"
# treatdat$plot[which(treatdat$plot=="N1-4VB")]<-"N1-11V"
# treatdat$plot[which(treatdat$plot=="N1-8VB")]<-"N1-14V"
#treatdat$plot[which(treatdat$plot=="N2-17VB")]<-"N2-17V"
#treatdat$plot[which(treatdat$plot=="N2-18VB")]<-"N2-18V"
#treatdat$plot[which(treatdat$plot=="N2-1VB")]<-"N2-1V"
#treatdat$plot[which(treatdat$plot=="N2-24VB")]<-"N2-24V"
#treatdat$plot[which(treatdat$plot=="N2-2VB")]<-"N2-2V"
#treatdat$plot[which(treatdat$plot=="N2-4VB")]<-"N2-4V"
#treatdat$plot[which(treatdat$plot=="N2-7VB")]<-"N2-7V"


