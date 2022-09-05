#STEP 1: CREATE PAPER PAIRS FROM RAW DATA

setwd("ORGSCI_papers")

#combine data files
files <- list.files(pattern="csv")
mat <- matrix(0,ncol=7,nrow=1)
for(f in files){
  a <- read.csv(f,sep=',',head=F)
  mat <- rbind(mat,a)
}
mat <- mat[-1,]


allPapers <- mat[,-4]
allPapers <- allPapers[,-c(5)]

colnames(allPapers) <- c("PaperID","Reference","JournalID","Title","Year")

#create all reference pairs
papers <- unique(allPapers[,1])
pairs <- matrix(0,ncol=4,nrow=1)

for(p in papers){
  refs <- allPapers[which(allPapers[,1]==p),2]
  if(length(refs) > 1){
    prs <- t(combn(refs,2))
    prs <- cbind(prs,rep(allPapers[which(allPapers[,1]==p),4][1],nrow(prs)),rep(allPapers[which(allPapers[,1]==p),1][1],nrow(prs)))
    pairs<- rbind(pairs, prs)
  }
}
pairs <- pairs[-1,]
pairs <- pairs[,c(1,2,4,3)]


#2. ADD JOURNAL INFO FOR REFERENCED PAPERS

setwd("ORGSCI_JournalInfo/")
files <- list.files(pattern="csv")
mat <- matrix(0,ncol=4,nrow=1)
colnames(mat) <- c("na","Ref","Journal","Year")
for(f in files){
  a <- read.csv(f,sep=',',head=F)
  a <- a[,c(1,2,5,6)]
  colnames(a) <- c("na","Ref","Journal","Year")
  a[which(is.na(a[,3])),3] <- 0
  unname(a)
  unname(mat)
  mat <- rbind(mat,a)
}
mat <- mat[-1,]

pairyears <- pairs
pairs <- pairs[,-c(3,4)]
pairs2 <- as.matrix(cbind(pairs,pairs,pairs))
for(i in 1:nrow(pairs)){
  
  paperPairs <- pairs[i,]
  a <- mat[which(mat$Ref == paperPairs[1]),c(3,4)]
  b <- mat[which(mat$Ref == paperPairs[2]),c(3,4)]
  
  pairs2[i,c(3,4)] <- as.numeric(a[1,])
  pairs2[i,c(5,6)] <- as.numeric(b[1,])
  
}
journalPairs <- pairs2[,c(1,3,5)]
journalPairs <- cbind(journalPairs,pairyears[,3])
journalPairs <- journalPairs[,-1]
journalPairs <- cbind(journalPairs,pairyears[,4])
#remove non-journal entries
journalPairs <- journalPairs[-which(journalPairs[,1]==0),]
journalPairs <- journalPairs[-which(journalPairs[,2]==0),]


#count pair frequencies
comb <- matrix(0,ncol=1,nrow=nrow(journalPairs))
for(i in 1:nrow(journalPairs)){
  comb[i,] <- paste0(journalPairs[i,1],'_',journalPairs[i,2])
}


comb <- cbind(comb,journalPairs[,3],journalPairs[,4])
comb <- as.data.frame(comb)

empirical_frequencies <- comb

save(empirical_frequencies,file="empfreq_orgsci.Rdata")
#null model frequencies



#3. CREATE NULLMODELS


setwd("ORGSCI_papers")

#combine data files
files <- list.files(pattern="csv")
mat <- matrix(0,ncol=7,nrow=1)
for(f in files){
  a <- read.csv(f,sep=',',head=F)
  mat <- rbind(mat,a)
}
mat <- mat[-1,]

allPapers <- mat[,-4]
colnames(allPapers) <- c("PaperID","Reference","JournalID","Title","Citation Count","PaperYear")
allPapers <- allPapers[,c(1,2,6)]

setwd("ORGSCI_JournalInfo")
files <- list.files(pattern="csv")
mat <- matrix(0,ncol=4,nrow=1)
colnames(mat) <- c("na","Ref","Journal","Year")
for(f in files){
  a <- read.csv(f,sep=',',head=F)
  a <- a[,c(1,2,5,6)]
  colnames(a) <- c("na","Ref","Journal","Year")
  a[which(is.na(a[,3])),3] <- 0
  unname(a)
  unname(mat)
  mat <- rbind(mat,a)
}
mat <- mat[-1,]

#add year to references
referencesYear <- as.matrix(cbind(allPapers,rep(0,nrow(allPapers))))
for(i in 1:nrow(allPapers)){
  
  a <- mat[which(mat$Ref == allPapers$Reference[i]),4]
  referencesYear[i,4] <- a[1]
}

colnames(referencesYear) <- c("PaperId","References","PaperYear","ReferenceYear")

save(referencesYear,file="Data_orgsci.Rdata")



load("Data_orgsci.Rdata")
years <- sort(unique(referencesYear[,3]))
q <- as.integer( commandArgs(TRUE)[1])

#Q*E rewirings 157980 * 100
instances <- list()
#for(i in 1:10){
#print(i)
for(i in 1:15798000){ #100 more
  
  y = sample(years,1)
  
  #select papers from particular year
  a <- referencesYear[ which(referencesYear[,3] == y),]
  
  others <- 0
  while(length(others) <= 1){
    choose <- sample(1:nrow(a),1) #choose a random paper
    pap1 <- a[choose,]
    
    others <- which(a[,4] == pap1[4])
    others <- others[others!=choose]
    
  }
  pap2 <- a[sample(others,1),]
  
  
  
  referencesYear[intersect(which(referencesYear[,1] == pap1[1]),  which(referencesYear[,2] == pap1[2])),2] <- pap2[2] 
  referencesYear[intersect(which(referencesYear[,1] == pap2[1]),  which(referencesYear[,2] == pap2[2])),2] <- pap1[2] 
  
  
  
  
}

instances[[1]] <- referencesYear
#}
name<-paste0("Null_",q,".Rdata")
save(instances,file=name)



# step 4. calculate z-scores ----------------------------------------------

rm(list=ls())
library(tidyverse)
load("empfreq_orgsci.Rdata")
empfreq <- empirical_frequencies 
setwd("nullModels/")
files <- list.files()
nulls <- list()
i=0
for(f in files){
  i=i+1
  load(f)
  nulls[[i]] <- comb
}


years <- unique(empfreq$V3)
i=0
e <- list()
for(y in years){
  i=i+1
  
  empYear = empfreq %>% group_by(V1,V2,V3) %>% filter(V3 == y) %>% summarise(n=n())
  null1 = nulls[[1]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
  null2 = nulls[[2]]%>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
  null3 = nulls[[3]]%>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
  null4 = nulls[[4]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
  null5 = nulls[[5]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
  null6 = nulls[[6]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
  null7 = nulls[[7]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
  null8 = nulls[[8]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
  null9 = nulls[[9]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
  null10 = nulls[[10]] %>% group_by(V1) %>% filter(V2 == y) %>% summarise(n=n())
  
  
  df2 = empYear %>% full_join(null1,by="V1") %>%
    full_join(null2,by="V1") %>%
    full_join(null3,by="V1") %>%
    full_join(null4,by="V1") %>%
    full_join(null5,by="V1") %>%
    full_join(null6,by="V1") %>%
    full_join(null7,by="V1") %>%
    full_join(null8,by="V1") %>%
    full_join(null9,by="V1") %>%
    full_join(null10,by="V1") 
  
  df2[which(is.na(df2[,4])),4] <- 0
  df2[which(is.na(df2[,5])),5] <- 0
  df2[which(is.na(df2[,6])),6] <- 0
  df2[which(is.na(df2[,7])),7] <- 0
  df2[which(is.na(df2[,8])),8] <- 0
  df2[which(is.na(df2[,9])),9] <- 0
  df2[which(is.na(df2[,10])),10] <- 0
  df2[which(is.na(df2[,11])),11] <- 0
  df2[which(is.na(df2[,12])),12] <- 0
  df2[which(is.na(df2[,13])),13] <- 0
  df2[which(is.na(df2[,14])),14] <- 0
  colnames(df2) <- c("V1","P1","Y1","N1","N2","N3","N4","N5","N6","N7","N8","N9","N10","N11")
  df2 <- df2 %>% group_by(V1)
  
  df3 <- df2 %>% select(V1,P1,Y1, N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11) %>%
    mutate(z = (N1 - mean(c(N2, N3, N4, N5, N6, N7, N8, N9, N10, N11)) / sd(c(N2, N3, N4, N5, N6, N7, N8, N9, N10, N11))) )
  
  e[[i]]=empYear %>%  left_join(df3,by="V1") %>% select(V1,V2,V3,z)
} 


combine <- purrr::reduce(e, dplyr::full_join)

save(combine,file="final_OS_zcores.Rdata")
#remove NaN
combine2 <- combine %>% na.omit()

#calculate median z-score
z_median <- combine2 %>% group_by(V2) %>% mutate(zmed = median(z))

#calculate 10th percentile z-score
z_10th <- z_median %>% group_by(V2) %>% mutate(z10 = quantile(z,0.1))

z_scores <- z_median %>% group_by(V2) #%>% slice(1)
plot(ecdf((z_scores$zmed)),main="median z-score",xlab="median z-score",ylab="cumulative distribution",xlim=c(-5,70))
abline(v=0)



thor2 <- allPapers#[which(allPapers[,5] == "2020"),]
words <- list()
for(i in 1:nrow(thor2)){
  words[[i]] <- word(thor2$Title[i],1)
}
thor2[which(words %in% "Ensuring"),]
allPapers[33317,]
which(z_scores$V2 == "2100593292") 



#z scores
z_scores2 <- z_median %>% group_by(V2) %>% slice(1)
plot(ecdf((z_scores$zmed)),main="median z-score",xlab="median z-score",ylab="cumulative distribution",xlim=c(-10,200))
abline(v=0)


z_scores <- as.data.frame(z_scores)



e = ecdf(z_scores$zmed)

ggplot(z_scores2, aes(zmed)) + stat_ecdf(geom = "step")+
  labs(title="",
       y = "Cumulative distribution", x="Median z-score")+
  theme_classic()+
  geom_label(aes(x = z_scores[which(z_scores[,2] == "2140448817"),]$zmed[1], y = e(z_scores[which(z_scores[,2] == "2140448817"),]$zmed[1])), 
             label = "Jacob 2006")+
  geom_label(aes(x = z_scores[which(z_scores[,2] == "2163657674"),]$zmed[1], y = e(z_scores[which(z_scores[,2] == "2163657674"),]$zmed[1])), 
             label = "Bill 2014")+
  geom_label(aes(x = z_scores[which(z_scores[,2] == "2107017285"),]$zmed[1], y = e(z_scores[which(z_scores[,2] == "2107017285"),]$zmed[1])), 
             label = "Dobra 2015")+
  geom_label(aes(x = z_scores[which(z_scores[,2] == "2223634589"),]$zmed[1], y = e(z_scores[which(z_scores[,2] == "2223634589"),]$zmed[1])), 
             label = "Sang 2016")+
  geom_label(aes(x = z_scores[which(z_scores[,2] == "3210169015"),]$zmed[1], y = e(z_scores[which(z_scores[,2] == "3210169015"),]$zmed[1])), 
             label = "Chris 2021")+
  geom_label(aes(x = z_scores[which(z_scores[,2] == "2100593292"),]$zmed[1], y = e(z_scores[which(z_scores[,2] == "2100593292"),]$zmed[1])), 
             label = "Baum 2013")



#geom_point(data = z_scores[which(z_scores[,2] == "3034164684"),]$z, aes(x=x, y=y))


z_scores[which(z_scores[,2] == "3034164684"),]$z
z_scores[which(z_scores[,2] == "1962401551"),]$z
z_scores[which(z_scores[,2] == "3046382452"),]$z
z_scores[which(z_scores[,2] == "2334007494"),]$z
z_scores[which(z_scores[,2] == "1942971405"),]$z
z_scores[which(z_scores[,2] == "2783149505"),]$z
