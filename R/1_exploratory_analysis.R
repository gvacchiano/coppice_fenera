df <- read.csv("../output/polloni.csv", sep=";")
tot <- aggregate(df,by=list(df$Ceppaia),function(x){length(x)})[,1:2]
names(tot) <- c("ceppaia","tot")

v<-df[df$Vitalità=="v",]
m<-df[df$Vitalità=="m",]
t<-df[df$Vitalità=="t",]

vtot <- aggregate(v,by=list(v$Ceppaia),function(x){length(x)})[,1:2]
mtot <- aggregate(m,by=list(m$Ceppaia),function(x){length(x)})[,1:2]
ttot <- aggregate(t,by=list(t$Ceppaia),function(x){length(x)})[,1:2]

names(vtot) <- c("ceppaia","tot")
names(mtot) <- c("ceppaia","tot")
names(ttot) <- c("ceppaia","tot")

vx <- (1:150) [!c(1:150) %in% vtot$ceppaia]; vtot <- rbind (vtot,data.frame("ceppaia"=vx,"tot"=0))
mx <- (1:150) [!c(1:150) %in% mtot$ceppaia]; mtot <- rbind (mtot,data.frame("ceppaia"=mx,"tot"=0))
tx <- (1:150) [!c(1:150) %in% ttot$ceppaia]; ttot <- rbind (ttot,data.frame("ceppaia"=tx,"tot"=0))

vtot <- vtot[order(vtot$ceppaia),]
mtot <- mtot[order(mtot$ceppaia),]
ttot <- ttot[order(ttot$ceppaia),]

tot$vive=vtot$tot
tot$morte=mtot$tot
tot$tagliate=ttot$tot
hist(tot$morte/tot$tot, breaks=6)

diam <- df[,c("Ceppaia","Diametro")]
dtot <- aggregate(diam$Diametro,by=list(df$Ceppaia),mean)
hist(dtot$x[dtot$x<60], breaks=8)

tot$dm=dtot$x
par(mfcol=c(1,2))

hist(tot$dm[tot$dm<60 & tot$morte/tot$tot<0.5], breaks=8, main="vive >50%", xlab="dm (cm)")
hist(tot$dm[tot$dm<60 & tot$morte/tot$tot>=0.5], breaks=8, main="vive <50%", xlab="dm (cm)", xlim=c(0,50))
