######################################################################
## Avalição do docente pelos discentes
######################################################################
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(tibble)

# general information about lectures
dinfo=read.table("data/other/disciplinas.dat",h=T)

# wrapping function
wrap.it <- function(x, len) { 
  sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE)
}

######################################################################################################################################################
## The questions and their weights.
##
##   The latest decision with respect to the questions is
##     Dec. 03/2014, http://www.ufrgs.br/cpa/pessoal/decisoes/Decisao%20CPA%20No%2003-2014.pdf
##
##   Note that the order of the question in the data files and in this script is different from the order in the decision. We follow the order of the
##   data files, which has been the same for 2014/2, 2015/1, 2015/2, 2016/1, 2016/2, and 2017/1.
##
##   The weights are the weights of "Bloco I", multiplied by their respective group weights. In 2017/1, for the first time, we received also the
##   responses to questions 12, 13, and 14. To maintain comparability, we continue the evaluation on questions 1-11 only, but compare for 2017/1 the
##   values of 1-11 only to the whole set of questions, to assess their correlation. In future semesters we may switch to the whole set of questions.
######################################################################################################################################################
questions=c(
"analisou com os discentes os resultados das avaliações.", # 1
"realizou avaliações compatíveis com o que foi trabalhado na atividade de ensino.", # 2
"teve postura adequada diante da diversidade sociocultural.", # 3
"utilizou recursos e procedimentos didáticos adequados.", # 4
"foi assíduo e pontual.", # 5
"cumpriu o plano de ensino.", # 6
"contextualizou os conhecimentos desenvolvidos.", # 7
"manteve atitudes de respeito e cortesia.", # 8
"trabalhou com clareza e objetividade.", # 9
"disponibilizou tempo para atender os discentes fora da sala de aula, pessoalmente e/ou à distância.", # 10
"demonstrou domínio dos conteúdos.") # 11
## weights according to block I only (see above)
w=c(0.08,0.1,0.07,0.1,0.05,0.1,0.08,0.08,0.13,0.05,0.16)
qcol=c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11")

fix_dots <- function(y) {
  return(as.numeric(gsub(",",".",as.character(y))))
}

readRawData = function(file,year,format) {
    d=read.csv2(file)
    colnames(d)=switch(format,
                       v1=c("disc","turma","curso","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","avg"), ## 2014-2, 2015-1
                       v2=c("unid","dept","curso","disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","avg","grp1","grp2","grp3","grp4","grp5","tot"), ## 2015-2, mine
                       v3=c("unid","dept","curso","disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11"), ## 2015-2
                       v4=c("curso","disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","avg"), ## 2016-1
                       v5=c("dept","sigla","disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11"), # 2016-2
                       v6=c("dept","sigla","disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14"), # 2017-1
                       v7=c("disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","tam") # 2017-2
                       )
    
    ## (0.0) fix format: there's no "curso" information from 2016-2 on
    if (format=="v5" || format=="v6" || format=="v7") {
        d$curso="CC"
    }
    d
}

readCookedData = function(file,year,format) {
    file = paste("data/sai/", year, "/AvalDiscente_", year, ".csv", sep="")
    ## (0) read data, apply weights
    d=read.csv2(file)
    colnames(d)=c("disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11")
    d
}

## evaluate the courses of a given semester
## input: a data file `f`, the name of the year, the format
evaluateSemester = function(file,year,format="v1",minResponsesProgram=10,minResponsesCourse=5) {
    ## (0) read data, apply weights
    d=readRawData(file,year,format)
    #d=readCookedData(file,year,format)

    ## (0.0) fix commas to dots
    #d[,qcol] <- lapply(d[,qcol], function (x) sapply(x, fix_dots ))
    
    ## (0.1) sum the weights
    d$tw=apply(d[,qcol],1,function(x) { weighted.mean(!is.na(x),w,na.rm=T) })
    ## (0.2) for verification: compute the means
    d$nm=apply(d[,qcol],1,function(x) { sum(x*w,na.rm=T) })
    d$nm=d$nm/d$tw
    d$nm[d$tw==0]=NA
    
    ## (1) clean up data: remove all programs with less than 10 responses
    fewR=(d %>% group_by(curso) %>% summarize(N=length(turma)) %>% filter(N<minResponsesProgram))$curso
    e=subset(d,!(curso %in% fewR))
    
    ## (2) aggregate by courses
    f = e %>% group_by(disc) %>% summarize(N=length(q1),q1=mean(q1,na.rm=T),q2=mean(q2,na.rm=T),q3=mean(q3,na.rm=T),q4=mean(q4,na.rm=T),q5=mean(q5,na.rm=T),q6=mean(q6,na.rm=T),q7=mean(q7,na.rm=T),q8=mean(q8,na.rm=T),q9=mean(q9,na.rm=T),q10=mean(q10,na.rm=T),q11=mean(q11,na.rm=T))
    f$m=rowMeans(f[,qcol],na.rm=T)
    f=f[order(f$m),]
    f=subset(f,N>=minResponsesCourse) ## remove all courses with 4 evaluations or less

    #add column qNA (sum of the medium for each NA)
    qNA <- apply(f[,qcol], 1, function(x) sum(is.na(x)))
    f = add_column(f, qNA, .before = "q1")
    f$qNA <- apply(f[,c("qNA","m")], 1, function(x) prod(x))
    
    ## (3) produce an overview plot
    ##op <- par(mar = c(5,14,4,2) + 0.1)
    ##with(f,barplot(rbind(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11),names=paste0(disc," (",N,")"),horiz=T,las=1,cex.names=0.5,col=brewer.pal(11,"Oranges"),border=NA,xlab="Nota média  (número de avaliações)",legend.text=c("1","2","3","4","5","6","7","8","9","10","11"),args.legend = list(x = "bottomright", title="Questão", ncol = 2)))
    main.title=paste0("Avaliação geral disciplinas em ",year)
    ##abline(v=44,col="red",lwd=2)
    ##grid()
    ##par(op)

    f$xnames=paste0(f$disc," (",f$N,")")
    f$xnames=factor(f$xnames,levels=f$xnames)
    mf=subset(melt(f),grepl("q",variable))
    print(ggplot(data=mf,aes(x=xnames,y=value,fill=variable))+geom_bar(stat="identity")+coord_flip()+labs(title="",x="",y="Nota média (número de avaliações)")+scale_fill_brewer(name="Questão",palette="Paired", direction = -1)+geom_hline(yintercept=44,color="red")+theme(axis.text.y=element_text(size=5))+theme(legend.position="bottom", legend.key.width = unit(0.5, "line"), legend.key.height = unit(0.5, "line"),legend.text=element_text(size=10))+guides(fill=guide_legend(nrow=1, reverse = TRUE))+labs(title=main.title))#+theme(legend.position=c(0.95,0.15))

    ## (4) return raw data, filtered 
    list(d,e,f,year)
}

## compare semesters `s1` and `s2`
compareSemesters = function(s1,s2) {
    ## (1) produce two "rank" data frames, merge them
    r1=data.frame(disc=s1[[3]]$disc,rank=1:nrow(s1[[3]]),m=s1[[3]]$m)
    r2=data.frame(disc=s2[[3]]$disc,rank=1:nrow(s2[[3]]),m=s2[[3]]$m)
    ranks=merge(r1,r2,by="disc",all=T)
    ranks=subset(ranks,!is.na(m.x)&!is.na(m.y))
    ranks$label=wrap.it(ranks$disc,20)

    ## (2) create a plot
    ggplot(data=ranks)+
        geom_abline(intercept=0,slope=1,color="lightpink1",size=0.2)+
        geom_abline(intercept=-0.5,slope=1,color="lightpink2",size=0.2)+
        geom_abline(intercept=0.5,slope=1,color="lightpink2",size=0.2)+
        geom_point(aes(x=m.x,y=m.y),color="red")+
        labs(x=paste0("Média em ",s1[[4]]),y=paste0("Média em ",s2[[4]]),title=paste0("Comparação dos semestres ",s1[[4]]," e ",s2[[4]]))+
        geom_text_repel(aes(x = m.x, y = m.y, label = label), color = "purple4",
                        size=2, box.padding = unit(0.06, "lines"),
                        arrow = arrow(length = unit(0.01, 'npc'), ends = "first"),
                        force=1, segment.color = "grey40", segment.alpha = 0.70, min.segment.length = 0)
}

## compare classes for semester `s`
## goal: check if there are significant discrepancies in classes
compareClasses = function(s,minResponsesCourse=5) {
    ## (1) aggregate data, count number of classes
    bycd = s[[2]] %>% group_by(disc,turma) %>% summarize(N=length(q1),q1=mean(q1,na.rm=T),q2=mean(q2,na.rm=T),q3=mean(q3,na.rm=T),q4=mean(q4,na.rm=T),q5=mean(q5,na.rm=T),q6=mean(q6,na.rm=T),q7=mean(q7,na.rm=T),q8=mean(q8,na.rm=T),q9=mean(q9,na.rm=T),q10=mean(q10,na.rm=T),q11=mean(q11,na.rm=T))
    bycd$m=rowMeans(bycd[,qcol],na.rm=T)
    bycd=subset(bycd,N>=minResponsesCourse)
    bycd=merge(bycd,bycd %>% group_by(disc) %>% summarize(Nt=length(N)),by="disc")
    bycd.2=subset(bycd,Nt==2)
    bycd.3=subset(bycd,Nt>2)

    ## (2) extract coordinates, layout them
    bycd.2a=bycd.2 %>% group_by(disc) %>% summarize(m.x=m[1],m.y=m[2])
    bycd.2a$label=wrap.it(bycd.2a$disc,20)
    
    ## (3) plot it
    print(ggplot(data=bycd.2a)+
          geom_abline(intercept=0,slope=1,color="lightpink1",size=0.2)+
          geom_abline(intercept=-0.5,slope=1,color="lightpink2",size=0.2)+
          geom_abline(intercept=0.5,slope=1,color="lightpink2",size=0.2)+
          geom_point(aes(x=m.x,y=m.y),color="red")+
          labs(x="Nota média turma A",y="Nota média turma B",title=paste0("Comparação das disciplinas em ",s[[4]]," com duas turmas"))+
          geom_text_repel(aes(x = m.x, y = m.y, label = label), color = "purple4",
                          size=2, box.padding = unit(0.06, "lines"),
                          arrow = arrow(length = unit(0.01, 'npc'), ends = "first"),
                          force=1, segment.color = "grey40", segment.alpha = 0.70, min.segment.length = 0))
    
    list(bycd.3,s[[4]])
}

evaluateQuestions = function(s) {
    ## (1) melt it, define a couple of auxiliary factors, cut it
    ms=subset(melt(s[[1]]),grepl("q",variable))
    ms$variable=factor(ms$variable,ordered=T,levels=qcol)
    ms$variable20=factor(ms$variable,ordered=T,levels=rev(qcol),labels=rev(wrap.it(questions,20)))
    ms$variable30=factor(ms$variable,ordered=T,levels=rev(qcol),labels=rev(wrap.it(questions,30)))
    ms$variable55=factor(ms$variable,ordered=T,levels=rev(qcol),labels=rev(wrap.it(questions,55)))
    ms$dvalue=cut(ms$value,breaks=c(1:5),include.lowest=T)
    #print(paste("Value range",fivenum(ms$value)))

    ## (2) plot it
    ## (2.1) boxplots per question
    ##ggplot(data=ms,aes(x=variable20,y=value))+geom_boxplot(color="red")+coord_flip()+labs(title=paste0("Distribuição das repostas em ",s[[4]],". O professor..."),y="Nota",x="Questão")
    ## (2.2) histograms per question
    ##ggplot(data=ms,aes(x=value))+geom_bar(stat="bin",fill="red")+facet_wrap(~variable55)+labs(title=paste0("Distribuição das repostas em ",s[[4]],". O professor..."),x="Nota",y="Número de respostas")#+scale_y_log10()
    ## (2.3) percentages/counts per interval
    eval(substitute(expr = {
                        ggplot(data=ms,aes(x=variable30,fill=dvalue))+geom_bar(aes(y=..count../nr))+scale_y_continuous(labels=scales::percent)+coord_flip()+labs(title=paste0("Distribuição das repostas em ",s[[4]],". O professor..."),y="Percentagens",x="Questão")+scale_fill_brewer(name="Nota",palette="Oranges")#values=c("red","black","white","gray"))
                    },env = list(nr=nrow(s[[1]]))))
}

## track a list of semesters `sl` over time
## show only courses with at least one evaluation below `onlybelow`
trackSemesters = function(sl,rank=F,comment="",onlybelow=4,onlyMandatory=F) {
    restrictions=list()
    ## (1) merge semester, compute rank if requested
    ma=sl[[1]][[3]][,c("disc","m")]
    colnames(ma)[colnames(ma) == 'm'] <- sl[[1]][[4]] ##rename col to year
    for (s in sl[-1]) {
        ma=merge(ma,s[[3]][,c("disc","m")],by="disc")
        colnames(ma)[colnames(ma) == 'm'] <- s[[4]] ##rename col to year
    }
    sn = colnames(ma[-1]) ##Semester's year's name
    ma = ma %>% merge(dinfo %>% select(-theory,-semester))

    ## (2) filter for mandatory, low evaluations, apply rank
    if (onlyMandatory) {
        ma = ma %>% filter(mandatory==T)
        restrictions=c(restrictions,"obrigatórias")
    }
    selectedCourses=apply(ma[,sn],1,function(x) { min(x)<onlybelow })
    if (onlybelow<5) {
        restrictions=c(restrictions,paste("com pelo menos uma nota média menor que ",onlybelow,sep=""))
    }    
    if (rank) {
        for (cn in sn) {
            ma[[cn]]=nrow(ma)+1-rank(ma[[cn]],na.last="keep")
        }
    }
    ma = ma[selectedCourses,]
    mma=melt(ma, id.vars = c("disc","mandatory"))
    ma = ma %>% filter(disc %in% mma$disc)
    
    ## (3) plot it
    g=ggplot(data=mma,aes(x=variable,y=value,color=disc,group=disc))+geom_point()+geom_line()+theme(legend.position="none")
    if (onlyMandatory) {
        labels=wrap.it(ma$disc,30)
    } else {
        labels=wrap.it(paste(ma$disc,ifelse(ma$mandatory,"","(E)")),30)
    }
    if (length(restrictions)>0) {
        comment=paste(comment," (Somente disciplinas ",paste(restrictions,collapse=" "),")",sep="")
    }
    if (rank) {
        g=g+geom_text_repel(data=ma,x=1,y=-ma[,sn[1]],label=labels,hjust=1.1,size=2)+
            geom_text_repel(data=ma,x=length(sn),y=-ma[,sn[length(sn)]],label=labels,hjust=-0.1,size=2)+
            scale_y_reverse()+
            labs(title=paste("Posições na avaliação ",sn[1],"-",sn[length(sn)],comment,sep=""),x="Semestre",y="Posição")
    } else {
        g=g+geom_text_repel(data=ma,x=1,y= ma[,sn[1]],label=labels,hjust=1.1,size=2)+
            geom_text_repel(data=ma,x=length(sn),y= ma[,sn[length(sn)]],label=labels,hjust=-0.1,size=2)+
            labs(title=paste("Avaliação ",sn[1],"-",sn[length(sn)],comment,sep=""),x="Semestre",y="Nota")
    }
    g
}

######################################################################
## evaluations
######################################################################
pdf(onefile=F,width=9,height=6)

## (1) evaluation of complete semesters
## NOTE: empty fields and NAs have been mapped to NA
y14s2=evaluateSemester("data/sai/2014-2/AvalDocPeloDisc 2014-2.csv","2014-2",format="v1")
y15s1=evaluateSemester("data/sai/2015-1/AvalDocPeloDisc 2015-1.v1.csv","2015-1",format="v1")
y15s2=evaluateSemester("data/sai/2015-2/ADoc Disc - Quant.csv","2015-2",format="v2")
y16s1=evaluateSemester("data/sai/2016-1/AvalDocPeloDisc 2016-1.csv","2016-1",format="v4")
y16s2=evaluateSemester("data/sai/2016-2/Bloco do Professor.csv","2016-2",format="v5")
y17s1=evaluateSemester("data/sai/2017-1/DocenteDisc-2017-1.csv","2017-1",format="v6")
y17s2=evaluateSemester("data/sai/2017-2/AvalDiscente_2017-2.csv","2017-2",format="v7")

allsemesters=list(y14s2,y15s1,y15s2,y16s1,y17s1,y17s2)

## (2) compare evaluations of two semesters
for (s1 in 1:(length(allsemesters)-1)) {
    for (s2 in (s1+1):length(allsemesters)) {
        print(compareSemesters(allsemesters[[s1]],allsemesters[[s2]]))
        readline(prompt="Press [enter] to continue")
    }
}

## (3) compare classes in a semester; and compare multiple classes
for (s in 1:length(allsemesters)) {
    t3=compareClasses(allsemesters[[s]])
    print(ggplot(t3[[1]],aes(wrap.it(paste0(disc," (",Nt,")"),20),m))+geom_boxplot(color="red")+coord_flip()+labs(title=paste0("Disciplinas com 3 ou mais turmas em ",t3[[2]]),y="Nota média da turma",x="Disciplina"))
    readline(prompt="Press [enter] to continue")
}

## (4) plots per question
for (s in 1:length(allsemesters)) {
    print(evaluateQuestions(allsemesters[[s]]))
    readline(prompt="Press [enter] to continue")
}

## (5) track ranks over semesters
evensemesters=list(y14s2,y15s2,y16s2,y17s2)
oddsemesters=list(y15s1,y16s1,y17s1)
for (limit in c(4,5)) {
    trackSemesters(evensemesters,onlybelow=limit)
    trackSemesters(evensemesters,rank=T,onlybelow=limit)
    
    trackSemesters(oddsemesters,onlybelow=limit)
    trackSemesters(oddsemesters,rank=T,onlybelow=limit)
    
    trackSemesters(allsemesters,onlybelow=limit)
    trackSemesters(allsemesters,rank=T,onlybelow=limit)
}

dev.off()

#################### leftovers

########## (1) evaluation by program over the semesters (less useful, since from 2016/1 on, we don't have any information about the other programs)

## program table
program=read.table("data/cursos.dat",h=T)

## number of student feedback per course
allfb=data.frame()
## first semester is different
fb=y14s2[[1]] %>% group_by(curso) %>% summarize(N=length(turma))
fb$cnum=c("",13703,1259276,13717,45005,20979,110275,45020,13705,1140229,13708,45026)
fb$sem=y14s2[[4]]
allfb=rbind(allfb,fb)
## process remaining semesters
for (s in 2:(length(allsemesters))) {
    fb=(allsemesters[[s]])[[1]] %>% group_by(curso) %>% summarize(N=length(turma))
    fb$cnum=sub(".+\\((.+)\\)","\\1",fb$curso,perl=T)
    fb$sem=(allsemesters[[s]])[[4]]
    allfb=rbind(allfb,fb)
}
##
allfb=merge(allfb,program)#,all.x=T)
allfb=arrange(allfb,desc(N))
allfb$fcname=paste0(allfb$cname,"(",sapply(allfb$ctype,FUN=function(x) { substr(x,1,1) }),")")
##
fcnameorder=arrange(data.frame(allfb %>% group_by(cnum,fcname) %>% summarize(N=sum(N))),N)$fcname
allfb$fcname=factor(allfb$fcname,levels=fcnameorder)

pdf("respostas.pdf",9,6)
ggplot(data=subset(allfb,N>=10),aes(x=fcname,y=N,fill=sem))+geom_bar(stat="identity")+coord_flip()+labs(title="",x="")+scale_fill_discrete(name="Sem.")+theme(legend.position=c(0.9,0.13))+scale_fill_brewer(name="Nota",palette="Oranges")#+scale_y_log10()##
dev.off()

print(xtable(fb),file="cursos-2015-1.tex")
fb[order(-fb$N),]

## Open points/Ideas:
## 1) Overall correlation of all eleven questions (plot a matrix)?
## 2) Seperate aspects of professors and courses?
## 3) Join by "question groups"?
