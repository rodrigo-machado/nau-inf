######################################################################
## Avalição do docente pelos discentes
######################################################################
library(plyr)
library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(FField)
##library(Hmisc)

# wrapping function
wrap.it <- function(x, len) { 
  sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE)
}

## evaluate the courses of a given semester
## input: a data file `f`, the name of the year, the format
evaluateSemester = function(f,year,format="v1") {
    d=read.csv2(f)
    switch(format) 
    colnames(d)=switch(format,
                v1=c("disc","turma","curso","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","avg"),
                v2=c("unid","dept","curso","disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","avg","grp1","grp2","grp3","grp4","grp5","tot"))

    ## (1) clean up data: remove all programs with less than 10 responses
    fewR=subset(ddply(d,.(curso),summarize,N=length(turma)),N<10)$curso
    e=subset(d,!(curso %in% fewR))

    ## (2) aggregate by courses
    f=ddply(e,.(disc),summarize,N=length(q1),q1=mean(q1,na.rm=T),q2=mean(q2,na.rm=T),q3=mean(q3,na.rm=T),q4=mean(q4,na.rm=T),q5=mean(q5,na.rm=T),q6=mean(q6,na.rm=T),q7=mean(q7,na.rm=T),q8=mean(q8,na.rm=T),q9=mean(q9,na.rm=T),q10=mean(q10,na.rm=T),q11=mean(q11,na.rm=T))
    f$m=rowMeans(f[,3:13],na.rm=T)
    f=f[order(f$m),]
    f=subset(f,N>4) ## remove all courses with 4 evaluations or less

    ## (3) produce a overview plot
    ##pdf(paste0("overall-",year,".pdf"),11,7)
    op <- par(mar = c(5,14,4,2) + 0.1)
    with(f,barplot(rbind(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11),names=paste(disc," (",N,")",sep=""),horiz=T,las=1,cex.names=0.5,col=brewer.pal(11,"Blues"),border=NA,xlab="Average number of points (number of samples in parentheses)",main=paste0("Evaluação geral disciplinas em ",year,"\n(Somente disciplinas com 5 ou mais avaliações)"),legend.text=c("1","2","3","4","5","6","7","8","9","10","11"),args.legend = list(x = "bottomright", title="Questão", ncol = 2)))
    grid()
    ##dev.off()
    par(op)
    list(d,e,f,year)
}

## compare semesters `s1` and `s2`
compareSemesters = function(s1,s2) {
    ## (1) produce two "rank" data frames, merge them
    r1=data.frame(disc=s1[[3]]$disc,rank=1:nrow(s1[[3]]),m=s1[[3]]$m)
    r2=data.frame(disc=s2[[3]]$disc,rank=1:nrow(s2[[3]]),m=s2[[3]]$m)
    ranks=merge(r1,r2,by="disc",all=T)
    ranks=subset(ranks,!is.na(m.x)&!is.na(m.y))

    ## (2) place the labels
    x.fact <- 100 / max(ranks$m.x)
    y.fact <- 100 / max(ranks$m.y)
    coords <- with(ranks,FFieldPtRep(coords = cbind(m.x*x.fact,  m.y*y.fact), rep.fact = 2, iter.max=100))
    ranks$x=coords$x/x.fact
    ranks$y=coords$y/y.fact
    ranks$label=wrap.it(ranks$disc,20)
    
    ## (3) create a plot
    ##pdf("comp-15-averages.pdf",10,5)
    ggplot(data=ranks)+geom_abline(intercept=0,slope=1,color="white")+geom_segment(data=ranks,aes(x=x,y=y,xend=m.x,yend=m.y),color="gray")+geom_point(aes(x=m.x,y=m.y),color="red")+labs(x=paste0("Média em ",s1[[4]]),y=paste0("Média em ",s2[[4]]),title=paste0("Comparação dos semestres ",s1[[4]]," e ",s2[[4]]))+geom_text(aes(x=x,y=y,label=label),size=2,hjust=0)
    ## +xlim(c(2,5))+ylim(c(2,5))
    ##dev.off()
}

## compare classes for semester `s`
## goal: check if there are significant discrepancies in classes
compareClasses = function(s) {
    ## (1) aggregate data, count number of classes
    bycd=ddply(s[[2]],.(disc,turma),summarize,N=length(q1),q1=mean(q1,na.rm=T),q2=mean(q2,na.rm=T),q3=mean(q3,na.rm=T),q4=mean(q4,na.rm=T),q5=mean(q5,na.rm=T),q6=mean(q6,na.rm=T),q7=mean(q7,na.rm=T),q8=mean(q8,na.rm=T),q9=mean(q9,na.rm=T),q10=mean(q10,na.rm=T),q11=mean(q11,na.rm=T))
    bycd$m=rowMeans(bycd[,c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11")],na.rm=T)
    bycd=subset(bycd,N>4)
    bycd=merge(bycd,ddply(bycd,.(disc),summarize,Nt=length(N)),by="disc")
    bycd.2=subset(bycd,Nt==2)

    ## (2) extract coordinates, layout them
    bycd.2a=ddply(bycd.2,.(disc),summarize,m.x=m[1],m.y=m[2])
    x.fact <- 100 / max(bycd.2a$m.x)
    y.fact <- 100 / max(bycd.2a$m.y)
    coords <- with(bycd.2a,FFieldPtRep(coords = cbind(m.x*x.fact,  m.y*y.fact), rep.fact = 2, iter.max=100))
    bycd.2a$x=coords$x/x.fact
    bycd.2a$y=coords$y/y.fact
    bycd.2a$label=wrap.it(bycd.2a$disc,20)
    
    ## (3) plot it
    ##pdf(paste0("correl-",year,".pdf"),10,5)
    ggplot(data=bycd.2a)+geom_abline(intercept=0,slope=1,color="white")+geom_abline(intercept=-0.5,slope=1,color="white")+geom_abline(intercept=0.5,slope=1,color="white")+geom_segment(aes(x=x,y=y,xend=m.x,yend=m.y),color="gray")+geom_point(aes(x=m.x,y=m.y),color="red")+labs(x="Média turma A",y="Média turma B",title=paste0("Comparação das disciplinas em ",s[[4]]," com duas turmas\n(somente turmas com mais que 4 respostas)"))+geom_text(aes(x=x,y=y,label=label),size=2,hjust=0)
    ##dev.off()
}

######################################################################
## evaluations
######################################################################

## (1) evaluation of complete semesters
y15s1=evaluateSemester("data1/AvalDocPeloDisc 2015-1.v1.csv","2015-1",format="v1")
## 2015-2: Note: empty fields and NAs have been mapped to NA
y15s2=evaluateSemester("data2/ADoc Disc - Quant.csv","2015-2",format="v2")

## (2) compare evalutions of two semesters
compareSemesters(y15s1,y15s2)

## (3) compare classes in a semester
compareClasses(y15s1)
compareClasses(y15s2)


#################### leftovers

## validation of means
rowMeans(y15s1[[1]][,c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11")],na.rm=T)

## number of student feedback per course
fb=ddply(y15s1[[1]],.(curso),summarize,N=length(turma))
fb=ddply(y15s2[[1]],.(curso),summarize,N=length(turma))

fb[order(-fb$N),]

## ideas
## 1) overall correlation of all eleven questions (plot a matrix)
## 2) Need: additional data per course (semester)
## 3) seperate aspects of professors and courses
## 4) correlation of "turmas"


