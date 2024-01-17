#Code to create categorical value for young and old forests for all datafiles:

#seedling data
allsdt$OLD.CAT<-0#young forests =0
allsdt$OLD.CAT[allsdt$AGE>40]<-1
allsdt$OLD.CAT<-as.factor(allsdt$OLD.CAT)
allsdt$alltreat<-paste(allsdt$OLD.CAT,allsdt$TREAT,sep="_")

allsdtwide$OLD.CAT<-0#young forests =0
allsdtwide$OLD.CAT[allsdtwide$AGE>40]<-1
allsdtwide$OLD.CAT<-as.factor(allsdtwide$OLD.CAT)
allsdtwide$alltreat<-paste(allsdtwide$OLD.CAT,allsdtwide$TREAT,sep="_")

#sapling data
allspt$OLD.CAT<-0#young forests =0
allspt$OLD.CAT[allspt$AGE>40]<-1
allspt$OLD.CAT<-as.factor(allspt$OLD.CAT)
allspt$alltreat<-paste(allspt$OLD.CAT,allspt$TREAT,sep="_")

allsptwide$OLD.CAT<-0#young forests =0
allsptwide$OLD.CAT[allsptwide$AGE>40]<-1
allsptwide$OLD.CAT<-as.factor(allsptwide$OLD.CAT)
allsptwide$alltreat<-paste(allsptwide$OLD.CAT,allsptwide$TREAT,sep="_")

#adult tree data
dalltrt$OLD.CAT<-0#young forests =0
dalltrt$OLD.CAT[dalltrt$AGE>40]<-1
dalltrt$OLD.CAT<-as.factor(dalltrt$OLD.CAT)
dalltrt$alltreat<-paste(dalltrt$OLD.CAT,dalltrt$TREAT,sep="_")

# mortdat$OLD.CAT<-0#young forests =0
# mortdat$OLD.CAT[mortdat$AGE>40]<-1
# mortdat$OLD.CAT<-as.factor(mortdat$OLD.CAT)
# mortdat$alltreat<-paste(mortdat$OLD.CAT,mortdat$TREAT,sep="_")
# 
# #mistletoe data
# misttrt$OLD.CAT<-0#young forests =0
# misttrt$OLD.CAT[misttrt$AGE>40]<-1
# misttrt$OLD.CAT<-as.factor(misttrt$OLD.CAT)
# misttrt$alltreat<-paste(misttrt$OLD.CAT,misttrt$TREAT,sep="_")
# mistalltrtwide$OLD.CAT<-0
# mistalltrtwide$OLD.CAT[mistalltrtwide$AGE>40]<-1
# mistalltrtwide$OLD.CAT<-as.factor(mistalltrtwide$OLD.CAT)
# mistalltrtwide$alltreat<-paste(mistalltrtwide$OLD.CAT,mistalltrtwide$TREAT,sep="_")

#lwd data
lwdalltrt$OLD.CAT<-0#young forests =0
lwdalltrt$OLD.CAT[lwdalltrt$AGE>40]<-1
lwdalltrt$OLD.CAT<-as.factor(lwdalltrt$OLD.CAT)
lwdalltrt$alltreat<-paste(lwdalltrt$OLD.CAT,lwdalltrt$TREAT,sep="_")
