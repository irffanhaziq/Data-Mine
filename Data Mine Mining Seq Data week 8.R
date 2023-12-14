
#Mining Sequence Data
install.packages("TraMineR")

library(TraMineR)
data(mvad)
mvad
head(mvad)
dim(mvad)

summary(mvad)

class(mvad)

#define a sequnce data 
#Define a lables and codes for each state
mvad.labels = c("emplotment","further education","higher education",
                "joblessness","school","training")
mvad.scode = c("EM","FE","HE","JL","SC","TR")
mvad.seq = seqdef(mvad, 17:86, state = mvad.scode,
                  labels = mvad.labels)
mvad.seq
class(mvad.seq)


#statstical summry indicator
#mean time spent in each state
#overall data set
seqmeant(mvad.seq)

#mean time spenmt in each state by group

#group :fathers employment status
by(mvad.seq,mvad$funemp,seqmeant)

#group :gender
by(mvad.seq,mvad$male,seqmeant)

#number of transiston 
nT =seqtransn(mvad.seq)
hist(seqtransn(mvad.seq))

#trnistion rates
#overall data set
mvad.trate =seqtrate(mvad.seq)


#### dalam phone gmb
mvad.trate2 = seqtrate(mvad.seq,time.varying = T)


## Visualizing Sequence Data ##
#Sequence index plot
seqiplot(mvad.seq,border=NA,
         main ="Sequence index plot")

seqiplot(mvad.seq, group = cl.4fac ,border=NA,
         main ="Sequence index plot")

#Sequence freq plot
seqfplot(mvad.seq,border=NA,
         main ="Sequence Frequency plot")

seqfplot(mvad.seq,group = cl.4fac,border=NA,
         main ="Sequence Frequency plot")

#State Distribution plot

seqdplot(mvad.seq,border=NA,
         main ="State Disrtibution plot")

seqdplot(mvad.seq, group = cl.4fac,border=NA,
         main ="State Disrtibution plot")

#Modal State plot
seqmsplot(mvad.seq,border=NA,
         main ="Modal State plot")


#sequence Characvterists By Enreopy Index

seqHtplot(mvad.seq,border=NA,
          main ="tranversal plot")
#event Sequence Analysis

#define event ; sequnxce of transitiom
mvad.seqe= seqecreate(mvad.seq)


#look for dreq event subsequence 
fsubseq = seqefsub(mvad.seqe,pmin.support=0.05)


#plot 15 most frq event
plot(fsubseq[1:15],col ='blue')



##categoring Patterns
library(cluster)
#compute optimal matching distance based on
#transition rate
submat<- seqsubm (mvad.seq, method="TRATE")
dist.om<- seqdist (mvad.seq, method="OM", sm=submat)

#build a hierarchical clusterinhg based on optimal matching distance
clusterward<- agnes (dist.om, diss=T, method="ward") 
plot (clusterward)

#retrieve each sequence data for some cluster
#example: k=4 cluster

cl.4<- cutree (clusterward, k=4)

cl.4fac =factor(cl.4,labels = paste("Type"),1:4)



###_
## Determine the most discriminating transitions
## between cluster and plot the frequencice by clusrter
dicsr = seqecmpgroup(fsubseq,group=cl.4fac)
plot(dicsr[1:6])

 

