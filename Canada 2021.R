#Canada 2021#

results<-read.csv(file.choose(), header=FALSE) #pick csv file called results
adjustments<-read.csv(file.choose(), header=FALSE) #pick adjustment file. Can be because a long-term incumbent retired or if the demographics changed
proj_numbers<-read.csv(file.choose(), header=FALSE) #pick the file containing the most recent projection numbers


n=400 # sample size, corresponding to MoE of 4.8% for a party at 40%. Consistent with historical polling accuracy
N=5000 #number of simulations. My computer has no issue with 2000 but takes about 1 minute for 5,000.
nr=338 # number of ridings
pr=c(as.numeric(proj_numbers[7,])) #current voting intentions, Canada-wide. CPC, LPC, NDP, Green, Bloc and PPC. No need for 'others'
sumpr=sum(pr)
pr2=c(pr,100-sumpr) # so that it adds to 100% with others


prp=matrix(unlist(proj_numbers),7,6)
sumprp=rowSums(prp)
prp2=cbind(prp, c(100-sumprp))

# Randomize at the federal level
rcan=100*t(rmultinom(N, n, prob = pr2))/n

#Next step is to randomize at the provincial level but with a correlation (1st step takes care of that)

rcoef=t(t(rcan) / pr2) #This is to crate correlations across provinces


pratlantic=t(t(rcoef)*prp2[1,])
ratlantic=matrix(0, nrow=N, ncol=7)

for (i in 1:N) {
  ratlantic[i,]=100*t(rmultinom(1, n, prob = pratlantic[i,]))/n #we still actually draw randomly per province, so correlation isn't 1:1
}

prqc=t(t(rcoef)*prp2[2,])
rqc=matrix(0, nrow=N, ncol=7)

for (i in 1:N) {
  rqc[i,]=100*t(rmultinom(1, n, prob = prqc[i,]))/n
}

pront=t(t(rcoef)*prp2[3,])
ront=matrix(0, nrow=N, ncol=7)

for (i in 1:N) {
  ront[i,]=100*t(rmultinom(1, n, prob = pront[i,]))/n
}

prpr=t(t(rcoef)*prp2[4,])
rpr=matrix(0, nrow=N, ncol=7)

for (i in 1:N) {
  rpr[i,]=100*t(rmultinom(1, n, prob = prpr[i,]))/n
}

pralb=t(t(rcoef)*prp2[5,])
ralb=matrix(0, nrow=N, ncol=7)

for (i in 1:N) {
  ralb[i,]=100*t(rmultinom(1, n, prob = pralb[i,]))/n
}

prbc=t(t(rcoef)*prp2[6,])
rbc=matrix(0, nrow=N, ncol=7)

for (i in 1:N) {
  rbc[i,]=100*t(rmultinom(1, n, prob = prbc[i,]))/n
}



s=t(matrix(c(28.6, 40.9, 15.9, 12.3, 0, 1.2,
             16, 34.3, 10.8, 4.5, 32.4, 1.5,
             33.1, 41.6, 16.8, 6.2, 0, 1.6,
             54.5, 19.1, 20.2, 3.9, 0, 1.7,
             69, 13.8, 11.6, 2.8, 0, 2.2,
             34, 26.2, 24.4, 12.5, 0, 1.7,
             34.3, 33.1, 16, 6.5, 7.65, 1.6),
             nrow=6,ncol=7)) #results % by provinces. From ATL to BC # past election results at the provincial level



swing1=rbind(rep(1,32)%*%matrix(t(ratlantic[,1]-s[1,1]),1,N),
             rep(1,78)%*%matrix(t(rqc[,1]-s[2,1]),1,N),
             rep(1,121)%*%matrix(t(ront[,1]-s[3,1]),1,N),
             rep(1,28)%*%matrix(t(rpr[,1]-s[4,1]),1,N),
             rep(1,34)%*%matrix(t(ralb[,1]-s[5,1]),1,N),
             rep(1,42)%*%matrix(t(rbc[,1]-s[6,1]),1,N),
             rep(1,3)%*%matrix(t(rcan[,1]-s[7,1]),1,N))

swing2=rbind(rep(1,32)%*%matrix(t(ratlantic[,2]-s[1,2]),1,N),
             rep(1,78)%*%matrix(t(rqc[,2]-s[2,2]),1,N),
             rep(1,121)%*%matrix(t(ront[,2]-s[3,2]),1,N),
             rep(1,28)%*%matrix(t(rpr[,2]-s[4,2]),1,N),
             rep(1,34)%*%matrix(t(ralb[,2]-s[5,2]),1,N),
             rep(1,42)%*%matrix(t(rbc[,2]-s[6,2]),1,N),
             rep(1,3)%*%matrix(t(rcan[,2]-s[7,2]),1,N))

swing3=rbind(rep(1,32)%*%matrix(t(ratlantic[,3]-s[1,3]),1,N),
             rep(1,78)%*%matrix(t(rqc[,3]-s[2,3]),1,N),
             rep(1,121)%*%matrix(t(ront[,3]-s[3,3]),1,N),
             rep(1,28)%*%matrix(t(rpr[,3]-s[4,3]),1,N),
             rep(1,34)%*%matrix(t(ralb[,3]-s[5,3]),1,N),
             rep(1,42)%*%matrix(t(rbc[,3]-s[6,3]),1,N),
             rep(1,3)%*%matrix(t(rcan[,3]-s[7,3]),1,N))

swing4=rbind(rep(1,32)%*%matrix(t(ratlantic[,4]-s[1,4]),1,N),
             rep(1,78)%*%matrix(t(rqc[,4]-s[2,4]),1,N),
             rep(1,121)%*%matrix(t(ront[,4]-s[3,4]),1,N),
             rep(1,28)%*%matrix(t(rpr[,4]-s[4,4]),1,N),
             rep(1,34)%*%matrix(t(ralb[,4]-s[5,4]),1,N),
             rep(1,42)%*%matrix(t(rbc[,4]-s[6,4]),1,N),
             rep(1,3)%*%matrix(t(rcan[,4]-s[7,4]),1,N))

swing5=rbind(rep(1,32)%*%matrix(t(ratlantic[,5]-s[1,5]),1,N),
             rep(1,78)%*%matrix(t(rqc[,5]-s[2,5]),1,N),
             rep(1,121)%*%matrix(t(ront[,5]-s[3,5]),1,N),
             rep(1,28)%*%matrix(t(rpr[,5]-s[4,5]),1,N),
             rep(1,34)%*%matrix(t(ralb[,5]-s[5,5]),1,N),
             rep(1,42)%*%matrix(t(rbc[,5]-s[6,5]),1,N),
             rep(1,3)%*%matrix(t(rcan[,5]-s[7,5]),1,N))

swing5[336:338,]=0

swing6=rbind(rep(1,32)%*%matrix(t(ratlantic[,6]-s[1,6]),1,N),
             rep(1,78)%*%matrix(t(rqc[,6]-s[2,6]),1,N),
             rep(1,121)%*%matrix(t(ront[,6]-s[3,6]),1,N),
             rep(1,28)%*%matrix(t(rpr[,6]-s[4,6]),1,N),
             rep(1,34)%*%matrix(t(ralb[,6]-s[5,6]),1,N),
             rep(1,42)%*%matrix(t(rbc[,6]-s[6,6]),1,N),
             rep(1,3)%*%matrix(t(rcan[,6]-s[7,6]),1,N))

proj1=matrix(0, nrow=nr, ncol=N)
proj2=matrix(0, nrow=nr, ncol=N)
proj3=matrix(0, nrow=nr, ncol=N)
proj4=matrix(0, nrow=nr, ncol=N)
proj5=matrix(0, nrow=nr, ncol=N)
proj6=matrix(0, nrow=nr, ncol=N)
proj7=matrix(0, nrow=nr, ncol=N) #independents, Maverick, True North


for (i in 1:N) {
  proj1[,i]=results[,1]+swing1[,i]+adjustments[,1]
}
proj1[proj1<0]=0
proj1[proj1>100]=100

for (i in 1:N) {
  proj2[,i]=results[,2]+swing2[,i]+adjustments[,2]
}
proj2[proj2<0]=0
proj2[proj2>100]=100

for (i in 1:N) {
  proj3[,i]=results[,3]+swing3[,i]+adjustments[,3]
}
proj3[proj3<0]=0
proj3[proj3>100]=100

for (i in 1:N) {
  proj4[,i]=results[,4]+swing4[,i]+adjustments[,4]
}
proj4[proj4<0]=0
proj4[proj4>100]=100

for (i in 1:N) {
  proj5[,i]=results[,5]+swing5[,i]+adjustments[,5]
}
proj5[proj5<0]=0
proj5[proj5>100]=100

for (i in 1:N) {
  proj6[,i]=results[,6]+swing6[,i]+adjustments[,6]
}
proj6[proj6<0]=0
proj6[proj6>100]=100

for (i in 1:N) {
  proj7[,i]=results[,7]+adjustments[,7]
}



#Adjustments to the simple linear swing in case a party is experiencing a big swing
#For instance, if the CPC were to reach 50% in Ontario, they would start increasing even in DT Toronto.
#It's a lot of code for fringe results, but it's useful in case of a wave

r1=rbind(rep(1,32)%*%matrix(t(ratlantic[,1]),1,N),
             rep(1,78)%*%matrix(t(rqc[,1]),1,N),
             rep(1,121)%*%matrix(t(ront[,1]),1,N),
             rep(1,28)%*%matrix(t(rpr[,1]),1,N),
             rep(1,34)%*%matrix(t(ralb[,1]),1,N),
             rep(1,42)%*%matrix(t(rbc[,1]),1,N),
             rep(1,3)%*%matrix(t(rcan[,1]),1,N))

above1<-matrix(as.numeric(proj1>=r1),nr,N)

r2=rbind(rep(1,32)%*%matrix(t(ratlantic[,2]),1,N),
         rep(1,78)%*%matrix(t(rqc[,2]),1,N),
         rep(1,121)%*%matrix(t(ront[,2]),1,N),
         rep(1,28)%*%matrix(t(rpr[,2]),1,N),
         rep(1,34)%*%matrix(t(ralb[,2]),1,N),
         rep(1,42)%*%matrix(t(rbc[,2]),1,N),
         rep(1,3)%*%matrix(t(rcan[,2]),1,N))

above2<-matrix(as.numeric(proj2>=r2),nr,N)

r3=rbind(rep(1,32)%*%matrix(t(ratlantic[,3]),1,N),
         rep(1,78)%*%matrix(t(rqc[,3]),1,N),
         rep(1,121)%*%matrix(t(ront[,3]),1,N),
         rep(1,28)%*%matrix(t(rpr[,3]),1,N),
         rep(1,34)%*%matrix(t(ralb[,3]),1,N),
         rep(1,42)%*%matrix(t(rbc[,3]),1,N),
         rep(1,3)%*%matrix(t(rcan[,3]),1,N))

above3<-matrix(as.numeric(proj3>=r3),nr,N)

r4=rbind(rep(1,32)%*%matrix(t(ratlantic[,4]),1,N),
         rep(1,78)%*%matrix(t(rqc[,4]),1,N),
         rep(1,121)%*%matrix(t(ront[,4]),1,N),
         rep(1,28)%*%matrix(t(rpr[,4]),1,N),
         rep(1,34)%*%matrix(t(ralb[,4]),1,N),
         rep(1,42)%*%matrix(t(rbc[,4]),1,N),
         rep(1,3)%*%matrix(t(rcan[,4]),1,N))

above4<-matrix(as.numeric(proj4>=r4),nr,N)

r5=rbind(rep(1,32)%*%matrix(t(ratlantic[,5]),1,N),
         rep(1,78)%*%matrix(t(rqc[,5]),1,N),
         rep(1,121)%*%matrix(t(ront[,5]),1,N),
         rep(1,28)%*%matrix(t(rpr[,5]),1,N),
         rep(1,34)%*%matrix(t(ralb[,5]),1,N),
         rep(1,42)%*%matrix(t(rbc[,5]),1,N),
         rep(1,3)%*%matrix(t(rcan[,5]),1,N))

above5<-matrix(as.numeric(proj5>=r5),nr,N)

r6=rbind(rep(1,32)%*%matrix(t(ratlantic[,6]),1,N),
         rep(1,78)%*%matrix(t(rqc[,6]),1,N),
         rep(1,121)%*%matrix(t(ront[,6]),1,N),
         rep(1,28)%*%matrix(t(rpr[,6]),1,N),
         rep(1,34)%*%matrix(t(ralb[,6]),1,N),
         rep(1,42)%*%matrix(t(rbc[,6]),1,N),
         rep(1,3)%*%matrix(t(rcan[,6]),1,N))

above6<-matrix(as.numeric(proj6>=r6),nr,N)


#Fixing thresholds for a party to be considered 'high' or 'low' in a province

rthresholdlow=t(matrix(c(23, 35, 10, 7, 0, 0.5,
                         10, 25, 5, 2, 25, 0.5,
                         27, 35, 12, 2, 0, 0.5,
                         45, 15, 15, 1.5, 0, 0.5,
                         55, 8, 6, 1.5, 0, 0.5,
                         28, 20, 20, 5, 0, 0.5,
                         29, 28, 11, 2, 0, 0.5),
                         6, 7))

rthresholdhigh=t(matrix(c(34, 46, 21, 15, 0, 5,
                          22, 40, 16, 8, 37, 5,
                          38, 47, 23, 10, 0, 5,
                          65, 25, 25, 8, 0, 5,
                          70, 20, 18, 6, 0, 5,
                          40, 31, 31, 18, 0, 5,
                          39, 38, 21, 10, 0, 5),
                          6, 7))


r1thresholdlow=(c(rep(rthresholdlow[1,1], 32), rep(rthresholdlow[2,1],78),
                 rep(rthresholdlow[3,1], 121), rep(rthresholdlow[4,1], 28),
                 rep(rthresholdlow[5,1], 34), rep(rthresholdlow[6,1], 42),
                 rep(rthresholdlow[7,1], 3)))%*%t(rep(1,N))

r1thresholdhigh=(c(rep(rthresholdhigh[1,1], 32), rep(rthresholdhigh[2,1],78),
                             rep(rthresholdhigh[3,1], 121), rep(rthresholdhigh[4,1], 28),
                             rep(rthresholdhigh[5,1], 34), rep(rthresholdhigh[6,1], 42),
                             rep(rthresholdhigh[7,1], 3)))%*%t(rep(1,N))


r2thresholdlow=(c(rep(rthresholdlow[1,2], 32), rep(rthresholdlow[2,2],78),
                             rep(rthresholdlow[3,2], 121), rep(rthresholdlow[4,2], 28),
                             rep(rthresholdlow[5,2], 34), rep(rthresholdlow[6,2], 42),
                             rep(rthresholdlow[7,2], 3)))%*%t(rep(1,N))

r2thresholdhigh=(c(rep(rthresholdhigh[1,2], 32), rep(rthresholdhigh[2,2],78),
                              rep(rthresholdhigh[3,2], 121), rep(rthresholdhigh[4,2], 28),
                              rep(rthresholdhigh[5,2], 34), rep(rthresholdhigh[6,2], 42),
                              rep(rthresholdhigh[7,2], 3)))%*%t(rep(1,N))


r3thresholdlow=(c(rep(rthresholdlow[1,3], 32), rep(rthresholdlow[2,3],78),
                             rep(rthresholdlow[3,3], 121), rep(rthresholdlow[4,3], 28),
                             rep(rthresholdlow[5,3], 34), rep(rthresholdlow[6,3], 42),
                             rep(rthresholdlow[7,3], 3)))%*%t(rep(1,N))

r3thresholdhigh=(c(rep(rthresholdhigh[1,3], 32), rep(rthresholdhigh[2,3],78),
                              rep(rthresholdhigh[3,3], 121), rep(rthresholdhigh[4,3], 28),
                              rep(rthresholdhigh[5,3], 34), rep(rthresholdhigh[6,3], 42),
                              rep(rthresholdhigh[7,3], 3)))%*%t(rep(1,N))

r4thresholdlow=(c(rep(rthresholdlow[1,4], 32), rep(rthresholdlow[2,4],78),
                             rep(rthresholdlow[3,4], 121), rep(rthresholdlow[4,4], 28),
                             rep(rthresholdlow[5,4], 34), rep(rthresholdlow[6,4], 42),
                             rep(rthresholdlow[7,4], 3)))%*%t(rep(1,N))

r4thresholdhigh=(c(rep(rthresholdhigh[1,4], 32), rep(rthresholdhigh[2,4],78),
                              rep(rthresholdhigh[3,4], 121), rep(rthresholdhigh[4,4], 28),
                              rep(rthresholdhigh[5,4], 34), rep(rthresholdhigh[6,4], 42),
                              rep(rthresholdhigh[7,4], 3)))%*%t(rep(1,N))

r5thresholdlow=(c(rep(rthresholdlow[1,5], 32), rep(rthresholdlow[2,5],78),
                             rep(rthresholdlow[3,5], 121), rep(rthresholdlow[4,5], 28),
                             rep(rthresholdlow[5,5], 34), rep(rthresholdlow[6,5], 42),
                             rep(rthresholdlow[7,5], 3)))%*%t(rep(1,N))

r5thresholdhigh=(c(rep(rthresholdhigh[1,5], 32), rep(rthresholdhigh[2,5],78),
                              rep(rthresholdhigh[3,5], 121), rep(rthresholdhigh[4,5], 28),
                              rep(rthresholdhigh[5,5], 34), rep(rthresholdhigh[6,5], 42),
                              rep(rthresholdhigh[7,5], 3)))%*%t(rep(1,N))

r6thresholdlow=(c(rep(rthresholdlow[1,6], 32), rep(rthresholdlow[2,6],78),
                             rep(rthresholdlow[3,6], 121), rep(rthresholdlow[4,6], 28),
                             rep(rthresholdlow[5,6], 34), rep(rthresholdlow[6,6], 42),
                             rep(rthresholdlow[7,6], 3)))%*%t(rep(1,N))

r6thresholdhigh=(c(rep(rthresholdhigh[1,6], 32), rep(rthresholdhigh[2,6],78),
                              rep(rthresholdhigh[3,6], 121), rep(rthresholdhigh[4,6], 28),
                              rep(rthresholdhigh[5,6], 34), rep(rthresholdhigh[6,6], 42),
                              rep(rthresholdhigh[7,6], 3)))%*%t(rep(1,N))

                     
aboveprov1<-matrix(as.numeric(r1>=r1thresholdhigh),nr,N)
belowprov1<-matrix(as.numeric(r1<=r1thresholdlow),nr,N)

aboveprov2<-matrix(as.numeric(r2>=r2thresholdhigh),nr,N)
belowprov2<-matrix(as.numeric(r2<=r2thresholdlow),nr,N)

aboveprov3<-matrix(as.numeric(r3>=r3thresholdhigh),nr,N)
belowprov3<-matrix(as.numeric(r3<=r3thresholdlow),nr,N)

aboveprov4<-matrix(as.numeric(r4>=r4thresholdhigh),nr,N)
belowprov4<-matrix(as.numeric(r4<=r4thresholdlow),nr,N)

aboveprov5<-matrix(as.numeric(r5>=r5thresholdhigh),nr,N)
belowprov5<-matrix(as.numeric(r5<=r5thresholdlow),nr,N)

aboveprov6<-matrix(as.numeric(r6>=r6thresholdhigh),nr,N)
belowprov6<-matrix(as.numeric(r6<=r6thresholdlow),nr,N)



proj1=(1-above1*belowprov1-(1-above1)*aboveprov1)*proj1+ above1*belowprov1*(r1+r1/r1thresholdlow*(proj1-r1))+
  (1-above1)*aboveprov1*(proj1+(r1-r1thresholdhigh)/(100-r1thresholdhigh)*(r1-proj1))

proj2=(1-above2*belowprov2-(1-above2)*aboveprov2)*proj2+above2*belowprov2*(r2+r2/r2thresholdlow*(proj2-r2))+
  (1-above2)*aboveprov2*(proj2+(r2-r2thresholdhigh)/(100-r2thresholdhigh)*(r2-proj2))

proj3=(1-above3*belowprov3-(1-above3)*aboveprov3)*proj3+above3*belowprov3*(r3+r3/r3thresholdlow*(proj3-r3))+
  (1-above3)*aboveprov3*(proj3+(r3-r3thresholdhigh)/(100-r3thresholdhigh)*(r3-proj3))

proj4=(1-above4*belowprov4-(1-above4)*aboveprov4)*proj4+above4*belowprov4*(r4+r4/r4thresholdlow*(proj4-r4))+
  (1-above4)*aboveprov4*(proj4+(r4-r4thresholdhigh)/(100-r4thresholdhigh)*(r4-proj4))

proj5=(1-above5*belowprov5-(1-above5)*aboveprov5)*proj5+above5*belowprov5*(r5+r5/r5thresholdlow*(proj5-r5))+
  (1-above5)*aboveprov5*(proj5+(r5-r5thresholdhigh)/(100-r5thresholdhigh)*(r5-proj5))

proj5[1:32,]=0
proj5[111:338,]=0

proj6=(1-above6*belowprov6-(1-above6)*aboveprov6)*proj6+above6*belowprov6*(r6+r6/r6thresholdlow*(proj6-r6))+
  (1-above6)*aboveprov6*(proj6+(r6/r6thresholdhigh)/(100-r6thresholdhigh)*(r6-proj6))




# 2nd randomization, at the riding level (because having correct province-wide % isn't everything)

proj1_2=matrix(0, nrow=nr, ncol=N)
proj2_2=matrix(0, nrow=nr, ncol=N)
proj3_2=matrix(0, nrow=nr, ncol=N)
proj4_2=matrix(0, nrow=nr, ncol=N)
proj5_2=matrix(0, nrow=nr, ncol=N)
proj6_2=matrix(0, nrow=nr, ncol=N)
proj7_2=matrix(0, nrow=nr, ncol=N)

n2=200 # sample size for the randomization at the riding level. Yes it's low because a lot of uncertainty exists empirically


#This is the computationally intensive part. This is what takes time really.

for (i in 1:N) {
  for (j in 1:nr) {
    sumproj=proj1[j,i]+proj2[j,i]+proj3[j,i]+proj4[j,i]+proj5[j,i]+proj6[j,i]+proj7[j,i]
    pr3=c(proj1[j,i]/sumproj, proj2[j,i]/sumproj, proj3[j,i]/sumproj, proj4[j,i]/sumproj, proj5[j,i]/sumproj,
          proj6[j,i]/sumproj, proj7[j,i]/sumproj)
    rr2=100*t(rmultinom(1, n2, prob = pr3))/n2
    proj1_2[j,i]=rr2[1]+0.00001*runif(1) #small random number added to avoid ties
    proj2_2[j,i]=rr2[2]+0.00001*runif(1)
    proj3_2[j,i]=rr2[3]+0.00001*runif(1)
    proj4_2[j,i]=rr2[4]+0.00001*runif(1)
    proj5_2[j,i]=rr2[5]+0.00001*runif(1)
    proj6_2[j,i]=rr2[6]+0.00001*runif(1)
    proj7_2[j,i]=rr2[7]+0.00001*runif(1)
  }
}


#wins

projmax=pmax(proj1_2, proj2_2, proj3_2, proj4_2, proj5_2, proj6_2, proj7_2)

win1<-matrix(as.numeric(proj1_2==projmax),nr,N)
win2<-matrix(as.numeric(proj2_2==projmax),nr,N)
win3<-matrix(as.numeric(proj3_2==projmax),nr,N)
win4<-matrix(as.numeric(proj4_2==projmax),nr,N)
win5<-matrix(as.numeric(proj5_2==projmax),nr,N)
win6<-matrix(as.numeric(proj6_2==projmax),nr,N)
win7<-matrix(as.numeric(proj7_2==projmax),nr,N)



#Sum of wins

sumwin1=rowSums(t(win1))
sumwin2=rowSums(t(win2))
sumwin3=rowSums(t(win3))
sumwin4=rowSums(t(win4))
sumwin5=rowSums(t(win5))
sumwin6=rowSums(t(win7))
sumwin7=rowSums(t(win7))


#Outcomes

outcome=matrix(0,9,1)
outcome[1]=sum(sumwin1>=169) # CPC majority
outcome[2]=sum(sumwin1<169 & sumwin1>sumwin2 & sumwin1>sumwin3) # CPC plurality
outcome[3]=sum(sumwin2>=169) # LPC majority
outcome[4]=sum(sumwin2<169 & sumwin2>sumwin1 & sumwin2>sumwin3) # LPC plurality
outcome[5]=sum(sumwin3>=169) # NDP majority
outcome[6]=sum(sumwin3<169 & sumwin3>sumwin1 & sumwin3>sumwin2) # NDP plurality
outcome[7]=sum(sumwin1==sumwin2) # Tie CPC-LPC
outcome[8]=sum(sumwin1==sumwin3) # Tie CPC-NDP
outcome[9]=sum(sumwin2==sumwin3) # Tie LPC-NDP

outcome


#Probabilities to win each riding

sumwin1riding=rowSums(win1)
sumwin2riding=rowSums(win2)
sumwin3riding=rowSums(win3)
sumwin4riding=rowSums(win4)
sumwin5riding=rowSums(win5)
sumwin6riding=rowSums(win6)
sumwin7riding=rowSums(win7)

probwinriding=cbind(sumwin1riding, sumwin2riding, sumwin3riding, sumwin4riding, sumwin5riding, 
                    sumwin6riding, sumwin7riding)*1/N # chances of winning each riding


