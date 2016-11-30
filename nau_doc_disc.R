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

## the questions and their weights
questions=c(
"analisou com os discentes os resultados das avaliações.",
"realizou avaliações compatíveis com o que foi trabalhado na atividade de ensino.",
"teve postura adequada diante da diversidade sociocultural.",
"utilizou recursos e procedimentos didáticos adequados.",
"foi assíduo e pontual.",
"cumpriu o plano de ensino.",
"contextualizou os conhecimentos desenvolvidos.",
"manteve atitudes de respeito e cortesia.",
"trabalhou com clareza e objetividade.",
"disponibilizou tempo para atender os discentes fora da sala de aula, pessoalmente e/ou à distância.",
"demonstrou domínio dos conteúdos.")
w=c(0.08,0.1,0.07,0.1,0.05,0.1,0.08,0.08,0.13,0.05,0.16)
qcol=c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11")

## evaluate the courses of a given semester
## input: a data file `f`, the name of the year, the format
evaluateSemester = function(f,year,format="v1") {
    ## (0) read data, apply weights
    d=read.csv2(f)
    switch(format) 
    colnames(d)=switch(format,
                v1=c("disc","turma","curso","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","avg"), ## 2014-1, 2015-1
                v2=c("unid","dept","curso","disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","avg","grp1","grp2","grp3","grp4","grp5","tot"), ## 2015-2, mine
                v3=c("unid","dept","curso","disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11"), ## 2015-2
                v4=c("curso","disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","avg") ## 2016-1
                       )
    ## (0.1) apply the weights
    d$tw=apply(d[,qcol],1,function(x) { weighted.mean(!is.na(x),w,na.rm=T) })
    for (i in 1:length(qcol)) {
        d[,qcol[i]]=10*w[i]*d[,qcol[i]]/d$tw
    }
    ## (0.2) for verification: compute the means
    d$nm=apply(d[,qcol],1,function(x) { ifelse(sum(is.na(x))==length(qcol),NA,sum(x,na.rm=T)) })
    
    ## (1) clean up data: remove all programs with less than 10 responses
    fewR=subset(ddply(d,.(curso),summarize,N=length(turma)),N<10)$curso
    e=subset(d,!(curso %in% fewR))

    ## (2) aggregate by courses
    f=ddply(e,.(disc),summarize,N=length(q1),q1=mean(q1,na.rm=T),q2=mean(q2,na.rm=T),q3=mean(q3,na.rm=T),q4=mean(q4,na.rm=T),q5=mean(q5,na.rm=T),q6=mean(q6,na.rm=T),q7=mean(q7,na.rm=T),q8=mean(q8,na.rm=T),q9=mean(q9,na.rm=T),q10=mean(q10,na.rm=T),q11=mean(q11,na.rm=T))
    f$m=rowMeans(f[,3:13],na.rm=T)
    f=f[order(f$m),]
    f=subset(f,N>4) ## remove all courses with 4 evaluations or less

    ## (3) produce a overview plot
    op <- par(mar = c(5,14,4,2) + 0.1)
    with(f,barplot(rbind(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11),names=paste(disc," (",N,")",sep=""),horiz=T,las=1,cex.names=0.5,col=brewer.pal(11,"Oranges"),border=NA,xlab="Average number of points (number of samples in parentheses)",main=paste0("Evaluação geral disciplinas em ",year,"\n(somente disciplinas com 5 ou mais avaliações)"),legend.text=c("1","2","3","4","5","6","7","8","9","10","11"),args.legend = list(x = "bottomright", title="Questão", ncol = 2)))
    grid()
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
    ggplot(data=ranks)+geom_abline(intercept=0,slope=1,color="white")+geom_abline(intercept=-0.5,slope=1,color="white")+geom_abline(intercept=0.5,slope=1,color="white")+geom_segment(data=ranks,aes(x=x,y=y,xend=m.x,yend=m.y),color="gray")+geom_point(aes(x=m.x,y=m.y),color="red")+labs(x=paste0("Média em ",s1[[4]]),y=paste0("Média em ",s2[[4]]),title=paste0("Comparação dos semestres ",s1[[4]]," e ",s2[[4]]))+geom_text(aes(x=x,y=y,label=label),size=2,hjust=0)
    ## +xlim(c(2,5))+ylim(c(2,5))
}

## compare classes for semester `s`
## goal: check if there are significant discrepancies in classes
compareClasses = function(s) {
    ## (1) aggregate data, count number of classes
    bycd=ddply(s[[2]],.(disc,turma),summarize,N=length(q1),q1=mean(q1,na.rm=T),q2=mean(q2,na.rm=T),q3=mean(q3,na.rm=T),q4=mean(q4,na.rm=T),q5=mean(q5,na.rm=T),q6=mean(q6,na.rm=T),q7=mean(q7,na.rm=T),q8=mean(q8,na.rm=T),q9=mean(q9,na.rm=T),q10=mean(q10,na.rm=T),q11=mean(q11,na.rm=T))
    bycd$m=rowMeans(bycd[,qcol],na.rm=T)
    bycd=subset(bycd,N>4)
    bycd=merge(bycd,ddply(bycd,.(disc),summarize,Nt=length(N)),by="disc")
    bycd.2=subset(bycd,Nt==2)
    bycd.3=subset(bycd,Nt>2)

    ## (2) extract coordinates, layout them
    bycd.2a=ddply(bycd.2,.(disc),summarize,m.x=m[1],m.y=m[2])
    x.fact <- 100 / max(bycd.2a$m.x)
    y.fact <- 100 / max(bycd.2a$m.y)
    coords <- with(bycd.2a,FFieldPtRep(coords = cbind(m.x*x.fact,  m.y*y.fact), rep.fact = 2, iter.max=100))
    bycd.2a$x=coords$x/x.fact
    bycd.2a$y=coords$y/y.fact
    bycd.2a$label=wrap.it(bycd.2a$disc,20)
    
    ## (3) plot it
    print(ggplot(data=bycd.2a)+geom_abline(intercept=0,slope=1,color="white")+geom_abline(intercept=-0.5,slope=1,color="white")+geom_abline(intercept=0.5,slope=1,color="white")+geom_segment(aes(x=x,y=y,xend=m.x,yend=m.y),color="gray")+geom_point(aes(x=m.x,y=m.y),color="red")+labs(x="Nota média turma A",y="Nota média turma B",title=paste0("Comparação das disciplinas em ",s[[4]]," com duas turmas\n(somente turmas com mais que 4 respostas)"))+geom_text(aes(x=x,y=y,label=label),size=2,hjust=0))
    list(bycd.3,s[[4]])
}


evaluateQuestions = function(s) {
    ## (1) melt it, define a couple of auxiliary factors, cut it
    ms=subset(melt(s[[1]]),grepl("q",variable))
    ms$variable=factor(ms$variable,ordered=T,levels=qcol)
    ms$variable20=factor(ms$variable,ordered=T,levels=rev(qcol),labels=rev(wrap.it(questions,20)))
    ms$variable55=factor(ms$variable,ordered=T,levels=rev(qcol),labels=rev(wrap.it(questions,55)))
    ms$dvalue=cut(ms$value,breaks=c(1:5),include.lowest=T)

    ## (2) plot it
    ## (2.1) boxplots per question
    ##ggplot(data=ms,aes(x=variable20,y=value))+geom_boxplot(color="red")+coord_flip()+labs(title=paste0("Distribuição das repostas em ",s[[4]],". O professor..."),y="Nota",x="Questão")
    ## (2.2) histograms per question
    ##ggplot(data=ms,aes(x=value))+geom_bar(stat="bin",fill="red")+facet_wrap(~variable55)+labs(title=paste0("Distribuição das repostas em ",s[[4]],". O professor..."),x="Nota",y="Número de respostas")#+scale_y_log10()
    ## (2.3) percentages/counts per interval
    ggplot(data=ms,aes(x=variable20,fill=dvalue))+geom_bar(aes(y=..count../1611))+scale_y_continuous(labels=scales::percent)+coord_flip()+labs(title=paste0("Distribuição das repostas em ",s[[4]],". O professor..."),y="Percentagens",x="Questão")+scale_fill_brewer(name="Nota",palette="Oranges")#values=c("red","black","white","gray"))
}

## track a list of semesters `sl` (with names `sn`) over time
trackSemesters = function(sl,sn,rank=F) {
    ## (1) merge semester, compute rank if requested, melt
    ma=sl[[1]][[3]][,c("disc","m")]
    for (s in sl[-1]) {
        ma=merge(ma,s[[3]][,c("disc","m")],by="disc")
    }
    colnames(ma)=c("disc",sn)
    if (rank) {
        for (cn in sn) {
            ma[[cn]]=nrow(ma)+1-rank(ma[[cn]],na.last="keep")
        }
    }
    mma=melt(ma)

    ## (2) plot it
    g=ggplot(data=mma,aes(x=variable,y=value,color=disc,group=disc))+geom_point()+geom_line()+theme(legend.position="none")
    if (rank) {
        g=g+geom_text(data=ma,x=1,y=-ma[,sn[1]],label=wrap.it(ma$disc,40),hjust=1.1,size=2)+geom_text(data=ma,x=length(sn),y=-ma[,sn[length(sn)]],label=wrap.it(ma$disc,40),hjust=-0.1,size=2)+scale_y_reverse()+labs(title=paste("Posições na avaliação ",sn[1],"-",sn[length(sn)],sep=""),x="Semestre",y="Posição")
    } else {
        g=g+geom_text(data=ma,x=1,y= ma[,sn[1]],label=wrap.it(ma$disc,40),hjust=1.1,size=2)+geom_text(data=ma,x=length(sn),y= ma[,sn[length(sn)]],label=wrap.it(ma$disc,40),hjust=-0.1,size=2)+labs(title=paste("Avaliação ",sn[1],"-",sn[length(sn)],sep=""),x="Semestre",y="Nota")
    }
    g
}



######################################################################
## evaluations
######################################################################
pdf(onefile=F,width=20,height=11)

## (1) evaluation of complete semesters
## NOTE: empty fields and NAs have been mapped to NA
y14s2=evaluateSemester("data/sai/2014-2/AvalDocPeloDisc 2014-2.csv","2014-2",format="v1")
y15s1=evaluateSemester("data/sai/2015-1/AvalDocPeloDisc 2015-1.v1.csv","2015-1",format="v1")
y15s2=evaluateSemester("data/sai/2015-2/ADoc Disc - Quant.csv","2015-2",format="v2")
y16s1=evaluateSemester("data/sai/2016-1/AvalDocPeloDisc 2016-1.csv","2016-1",format="v4")

## (2) compare evalutions of two semesters
compareSemesters(y14s2,y15s1)
compareSemesters(y14s2,y15s2)
compareSemesters(y14s2,y16s1)
compareSemesters(y15s1,y15s2)
compareSemesters(y15s1,y16s1)
compareSemesters(y15s2,y16s1)

## (3) compare classes in a semester; and compare multiple classes
t3=compareClasses(y14s2)
ggplot(t3[[1]],aes(wrap.it(paste0(disc," (",Nt,")"),20),m))+geom_boxplot(color="red")+coord_flip()+labs(title=paste0("Disciplinas com 3 ou mais turmas em ",t3[[2]]),y="Nota média da turma",x="Disciplina")
t3=compareClasses(y15s1)
ggplot(t3[[1]],aes(wrap.it(paste0(disc," (",Nt,")"),20),m))+geom_boxplot(color="red")+coord_flip()+labs(title=paste0("Disciplinas com 3 ou mais turmas em ",t3[[2]]),y="Nota média da turma",x="Disciplina")
t3=compareClasses(y15s2)
ggplot(t3[[1]],aes(wrap.it(paste0(disc," (",Nt,")"),20),m))+geom_boxplot(color="red")+coord_flip()+labs(title=paste0("Disciplinas com 3 ou mais turmas em ",t3[[2]]),y="Nota média da turma",x="Disciplina")
t3=compareClasses(y16s1)
ggplot(t3[[1]],aes(wrap.it(paste0(disc," (",Nt,")"),20),m))+geom_boxplot(color="red")+coord_flip()+labs(title=paste0("Disciplinas com 3 ou mais turmas em ",t3[[2]]),y="Nota média da turma",x="Disciplina")

## (4) compares multiple classes (data from 3)
##ggplot(t3[[1]],aes(wrap.it(paste0(disc," (",Nt,")"),20),m))+geom_boxplot(color="red")+coord_flip()+labs(title=paste0("Disciplinas com 3 ou mais turmas em ",t3[[2]]),y="Nota média da turma",x="Disciplina")

## (5) plots per question
evaluateQuestions(y14s2)
evaluateQuestions(y15s1)
evaluateQuestions(y15s2)
evaluateQuestions(y16s1)

## (6) track ranks over semesters
trackSemesters(list(y14s2,y15s1,y15s2),c("2014-2","2015-1","2015-2"))
trackSemesters(list(y14s2,y15s1,y15s2),c("2014-2","2015-1","2015-2"),rank=T)
trackSemesters(list(y14s2,y15s1,y15s2,y16s1),c("2014-2","2015-1","2015-2","2016-1"))
trackSemesters(list(y14s2,y15s1,y15s2,y16s1),c("2014-2","2015-1","2015-2","2016-1"),rank=T)

dev.off()

#################### leftovers

## validation of means
rowMeans(y15s1[[1]][,qcol],na.rm=T)

## number of student feedback per course
fb=ddply(y15s1[[1]],.(curso),summarize,N=length(turma))
fb=ddply(y15s2[[1]],.(curso),summarize,N=length(turma))

fb[order(-fb$N),]

## Open points/Ideas:
## 1) Overall correlation of all eleven questions (plot a matrix)?
## 2) Seperate aspects of professors and courses?a
## 3) Join by "question groups"?
