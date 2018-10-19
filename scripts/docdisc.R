library(plyr)
library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(FField)
#library(Hmisc)

## read 2015-1
d=read.csv2("data1/AvalDocPeloDisc 2015-1.v1.csv")
colnames(d)=c("disc","turma","curso","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","avg")
values=4:14
year="2015-1"

## read 2015-2
## Note: empty fields and NAs have been mapped to NA
d=read.csv2("data2/ADoc Disc - Quant.csv")
colnames(d)=c("unid","dept","curso","disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","avg","grp1","grp2","grp3","grp4","grp5","tot")
d$grp1=NULL; d$grp2=NULL; d$grp3=NULL; d$grp4=NULL; d$grp5=NULL; d$tot=NULL
values=6:16
year="2015-2"

## validation of means
d$m=rowMeans(d[,values],na.rm=T)
## number of student feedback per course
ddply(d,.(curso),summarize,N=length(turma))

## clean up data: remove all courses with less than 10 responses
fewR=subset(ddply(d,.(curso),summarize,N=length(turma)),N<10)$curso

e=subset(d,!(curso %in% fewR))

## aggregate by courses
f=ddply(e,.(disc),summarize,N=length(q1),q1=mean(q1,na.rm=T),q2=mean(q2,na.rm=T),q3=mean(q3,na.rm=T),q4=mean(q4,na.rm=T),q5=mean(q5,na.rm=T),q6=mean(q6,na.rm=T),q7=mean(q7,na.rm=T),q8=mean(q8,na.rm=T),q9=mean(q9,na.rm=T),q10=mean(q10,na.rm=T),q11=mean(q11,na.rm=T))
f$m=rowMeans(f[,3:13],na.rm=T)
f=f[order(f$m),]
f=subset(f,N>4)

r1=data.frame(disc=f$disc,rank=1:nrow(f),m=f$m)

pdf(paste0("geral-",year,".pdf"),11,7)
op <- par(mar = c(5,14,4,2) + 0.1)
with(f,barplot(rbind(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11),names=paste(disc," (",N,")",sep=""),horiz=T,las=1,cex.names=0.5,col=brewer.pal(11,"Blues"),border=NA,xlab="Average number of points (number of samples in parentheses)",main=paste0("Evaluation of courses in ",year,"\n(Only courses with 5 or more samples)"),legend.text=c("1","2","3","4","5","6","7","8","9","10","11"),args.legend = list(x = "bottomright", title="Questão", ncol = 2)))
grid()
dev.off()
par(op)

## check if there are significant discrepancies in classes
bycd=ddply(e,.(disc,turma),summarize,N=length(q1),q1=mean(q1,na.rm=T),q2=mean(q2,na.rm=T),q3=mean(q3,na.rm=T),q4=mean(q4,na.rm=T),q5=mean(q5,na.rm=T),q6=mean(q6,na.rm=T),q7=mean(q7,na.rm=T),q8=mean(q8,na.rm=T),q9=mean(q9,na.rm=T),q10=mean(q10,na.rm=T),q11=mean(q11,na.rm=T))
bycd$m=rowMeans(bycd[,4:14],na.rm=T)
bycd=subset(bycd,N>4)
bycd=merge(bycd,ddply(bycd,.(disc),summarize,Nt=length(N)),by="disc")
bycd.2=subset(bycd,Nt==2)


bycd.2a=ddply(bycd.2,.(disc),summarize,mA=m[1],mB=m[2])
x.fact <- 100 / max(bycd.2a$mA)
y.fact <- 100 / max(bycd.2a$mB)
coords <- with(bycd.2a,FFieldPtRep(coords = cbind(mA*x.fact,  mB*y.fact), rep.fact = 2, iter.max=100))

pdf(paste0("correl-",year,".pdf"),10,5)

with(bycd.2a,plot(mA,mB,xlim=c(2.5,5),ylim=c(3.75,5),xlab="Média turma A",ylab="Média turma B",main=paste0("Comparação das disciplinas em ",year," com duas turmas\n(somente turmas com mais que 4 respostas)")))
abline(0,1)
disc.lb=lapply(bycd.2a$disc,function(x) { do.call(paste,c(as.list(strwrap(x,15)),sep="\n")) })
with(bycd.2a,text(coords$x/x.fact,coords$y/y.fact,disc.lb,cex=0.2,adj=c(0.5,0)))
##with(bycd.2a,text(mA,mB,disc.lb,cex=0.3,adj=c(0.5,0)))
grid()
abline(0.5,1)
abline(-0.5,1)

dev.off()

## ideas
## 1) overall correlation of all eleven questions (plot a matrix)
## 2) Need: additional data per course (semester)
## 3) seperate aspects of professors and courses
## 4) correlation of "turmas"


## for two "ranks" data frames
pdf("comp-15-averages.pdf",10,5)
ranks=merge(r1,r2,by="disc",all=T)
ranks=subset(ranks,!is.na(m.x)&!is.na(m.y))
x.fact <- 100 / max(ranks$m.x)
y.fact <- 100 / max(ranks$m.y)
coords <- with(ranks,FFieldPtRep(coords = cbind(m.x*x.fact,  m.y*y.fact), rep.fact = 2, iter.max=100))
with(ranks,plot(m.x,m.y,xlab="Média em 2015-1",ylab="Média em 2015-2"))
with(ranks,text(coords$x/x.fact,coords$y/y.fact,disc,cex=0.5,adj=c(0,0)))
with(ranks,text(m.x,m.y,disc,cex=0.5,adj=c(0,0)))
abline(0,1)
dev.off()
