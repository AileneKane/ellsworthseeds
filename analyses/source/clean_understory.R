#file that cleans the understory ellsworth data from 2007 and 2020
d2007<-d2007[1:9008,1:9]
d2020<-d2020[1:1394,1:9]

#replace "T" with 0 for cover and make it numeric

d2007$COVER[which(d2007$COVER=="T")]<-0
d2007$COVER[which(d2007$COVER=="   T")]<-0
d2007$COVER<-as.numeric(d2007$COVER)

d2020$COVER[which(d2020$COVER=="T")]<-0
d2020$COVER[which(d2020$COVER=="T ")]<-0
d2020$COVER[which(d2020$COVER=="t")]<-0
d2020$COVER<-as.numeric(d2020$COVER)

#convert height in 2020 to m (from cm), to make comparable to 2007
d2020$HT<-d2020$HT..CM./100
