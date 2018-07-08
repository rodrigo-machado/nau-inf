######################################################################
## Avaliação do docente pelos discentes
######################################################################
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(tibble)
library(stringr)
library(GGally)

# general information about lectures
dinfo=read.table("data/other/disciplinas.dat",h=T)

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

readRawData = function(file,year,format,percFile=F) {
    ## (0) read data, fix column names
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
    #remove extra spaces
    d$turma <- unlist(lapply(d$turma, function (x) gsub(' ', '', x)))
    if(format=="v6"){
      d = d[c("dept","sigla","disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11")]
    }
    ## (0.1) add participation percentange
    if (percFile){
      file2 = paste("data/sai/", year, "/AtividadesDeEnsino-", year, ".csv", sep="")  
      d_perc = read.csv2(file2)
      colnames(d_perc) = c("tipoAval","semestre","unid","dept","sigla","disc","turma","avg","desvio","percResp","percNA")
      d_perc = d_perc[c("disc", "turma", "percResp", "percNA")]
      
      #remove extra spaces(2017-2 problem)
      d_perc$turma <- unlist(lapply(d_perc$turma, function (x) gsub(' ', '', x)))
      
      
      d=merge(d,d_perc, by=c("disc", "turma"), all.x=T, sort=FALSE)
    }
    
    ## (0.2) fix format: there's no "curso" information from 2016-2 on
    if (format=="v5" || format=="v6" || format=="v7")
      d$curso="CC"
    d
}

readCookedData = function(file,year,format) {
    file = paste("data/sai/", year, "/AvalDiscente_", year, ".csv", sep="")
    ## (0) read data, set column names
    d=read.csv2(file)
    colnames(d)=c("disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11")
    d
}

## evaluate the courses of a given semester
## input: a data file `f`, the name of the year, the format
evaluateSemester = function(file,year,format="v1",percFile=F,minResponsesProgram=10,minResponsesCourse=5) {
    ## (0) read data, apply weights
    d=readRawData(file,year,format,percFile)
    #d=readCookedData(file,year,format)
    ## (0.0) fix commas to dots
    #d[,qcol] <- lapply(d[,qcol], function (x) sapply(x, fix_dots ))
    ## (0.1) sum the weights
    d$tw=apply(d[,qcol],1,function(x) { weighted.mean(!is.na(x),w,na.rm=T) })
    ## (0.2) for verification: compute the means
    d$nm=apply(d[,qcol],1,function(x) { sum(x*w,na.rm=T) })
    d$nm=d$nm/d$tw
    d$nm[d$tw==0]=NA
    
    ## (1) clean up data: remove all programs with less than `minResponses` responses
    fewR=(d %>% group_by(curso) %>% summarize(N=length(turma)) %>% filter(N<minResponsesProgram))$curso
    e=subset(d,!(curso %in% fewR))
    
    ## (2) aggregate by courses
    if (percFile){ #include percentage of participation data
      g = e %>% group_by(disc,turma,percResp,percNA) %>% summarize_at(vars(q1:q11),funs(mean(.,na.rm=T))) %>% full_join(e %>% group_by(disc,turma,percResp,percNA) %>% summarize(N=length(q1)))
      g$tam=round(100*g$N/g$percResp)
      f = g %>% group_by(disc) %>% summarize_at(vars(q1:q11),funs(mean(.,na.rm=T))) %>% full_join(g %>% group_by(disc) %>% summarize(tam=sum(tam),N=sum(N)))
    } else {
      f = e %>% group_by(disc) %>% summarize_at(vars(q1:q11),funs(mean(.,na.rm=T))) %>% full_join(e %>% group_by(disc) %>% summarize(N=length(q1)))
    }
    f$m=rowMeans(f[,qcol],na.rm=T)
    f=f[order(f$m),]
    f=subset(f,N>=minResponsesCourse) ## remove all courses with 4 evaluations or less

    #add column qNA (sum of the medium for each NA)
    qNA <- apply(f[,qcol], 1, function(x) sum(is.na(x)))
    f = add_column(f, qNA, .before = "q1")
    f$qNA <- apply(f[,c("qNA","m")], 1, function(x) prod(x))
    
    ## (3) produce an overview plot
    main.title=paste0("Avaliação geral disciplinas em ",year)
    if (percFile){ #include classes size
      f$xnames=paste0(f$disc," [",f$N,"/",f$tam," - ",round(100*f$N/f$tam),"%]")
    }else {
      f$xnames=paste0(f$disc," (",f$N,")")
    }
    
    f$xnames=factor(f$xnames,levels=f$xnames)
    mf=subset(melt(f),grepl("q",variable))
    print(ggplot(data=mf,aes(x=xnames,y=value,fill=variable))+
          geom_bar(stat="identity")+
          geom_hline(yintercept=44,color="red")+
          coord_flip()+
          guides(fill=guide_legend(nrow=1, reverse = TRUE))+
          scale_fill_brewer(name="Questão",palette="Paired", direction = -1)+
          theme(legend.position="bottom", legend.key.width = unit(0.5, "line"), legend.key.height = unit(0.5, "line"),legend.text=element_text(size=10),axis.text.y=element_text(size=5))+
          labs(title=main.title,x="",y="Nota média (número de avaliações)"))
    f$qNA <- NULL
    ## (4) return (raw data,filtered data,data aggregated by course,year)
    list(d,e,f,year)
}

## compare semesters `s1` and `s2`
compareSemesters = function(s1,s2) {
    ## (1) produce two "rank" data frames, merge them
    r1=data.frame(disc=s1[[3]]$disc,rank=1:nrow(s1[[3]]),m=s1[[3]]$m)
    r2=data.frame(disc=s2[[3]]$disc,rank=1:nrow(s2[[3]]),m=s2[[3]]$m)
    ranks=merge(r1,r2,by="disc",all=T)
    ranks=subset(ranks,!is.na(m.x)&!is.na(m.y))
    ranks$label=str_wrap(ranks$disc,20)

    ## (2) create a plot
    ggplot(data=ranks)+
        geom_abline(intercept=0,slope=1,color="lightpink1",size=0.2)+
        geom_abline(intercept=-0.5,slope=1,color="lightpink2",size=0.2)+
        geom_abline(intercept=0.5,slope=1,color="lightpink2",size=0.2)+
        geom_point(aes(x=m.x,y=m.y),color="red")+
        labs(x=paste0("Média em ",s1[[4]]),y=paste0("Média em ",s2[[4]]),title=paste0("Comparação dos semestres ",s1[[4]]," e ",s2[[4]]))+
        geom_text_repel(aes(x = m.x, y = m.y, label = label), color = "purple4", size=2, box.padding = unit(0.06, "lines"),
                        arrow = arrow(length = unit(0.01, 'npc'), ends = "first"), force=1,
                        segment.color = "grey40", segment.alpha = 0.70, min.segment.length = 0)
}

## compare classes for semester `s`
## goal: check if there are significant discrepancies in classes
compareClasses = function(s,minResponsesCourse=5) {
    ## (1) aggregate data by course and class, count number of classes Nt
    bycd = s[[2]] %>% group_by(disc,turma) %>% summarize_at(vars(q1:q11),funs(mean(.,na.rm=T))) %>% full_join(s[[2]] %>% group_by(disc,turma) %>% summarize(N=length(q1)))
    bycd$m=rowMeans(bycd[,qcol],na.rm=T)
    bycd=subset(bycd,N>=minResponsesCourse)
    bycd=merge(bycd,bycd %>% group_by(disc) %>% summarize(Nt=length(N)),by="disc")
    bycd.2=subset(bycd,Nt==2) ## two classes
    bycd.3=subset(bycd,Nt>2)  ## three or more classes

    ## (2) extract coordinates for courses with two classes
    bycd.2a=bycd.2 %>% group_by(disc) %>% summarize(m.x=m[1],m.y=m[2])
    bycd.2a$label=str_wrap(bycd.2a$disc,20)
    
    ## (3) plot courses with two classes
    print(ggplot(data=bycd.2a)+
          geom_abline(intercept=0,slope=1,color="lightpink1",size=0.2)+
          geom_abline(intercept=-0.5,slope=1,color="lightpink2",size=0.2)+
          geom_abline(intercept=0.5,slope=1,color="lightpink2",size=0.2)+
          geom_point(aes(x=m.x,y=m.y),color="red")+
          labs(x="Nota média turma A",y="Nota média turma B",title=paste0("Comparação das disciplinas em ",s[[4]]," com duas turmas"))+
          geom_text_repel(aes(x = m.x, y = m.y, label = label), color = "purple4",  size=2, box.padding = unit(0.06, "lines"),
                          arrow = arrow(length = unit(0.01, 'npc'), ends = "first"), force=1,
                          segment.color = "grey40", segment.alpha = 0.70, min.segment.length = 0))
    ## (4) return courses with three or more classes, year
    list(bycd.3,s[[4]])
}

evaluateQuestions = function(s) {
    ## (1) melt answer table, define a couple of auxiliary factors, cut it
    ms=subset(melt(s[[1]]),grepl("q",variable))
    ms$variable=factor(ms$variable,ordered=T,levels=qcol)
    ms$variable20=factor(ms$variable,ordered=T,levels=rev(qcol),labels=rev(str_wrap(questions,20)))
    ms$variable30=factor(ms$variable,ordered=T,levels=rev(qcol),labels=rev(str_wrap(questions,30)))
    ms$variable55=factor(ms$variable,ordered=T,levels=rev(qcol),labels=rev(str_wrap(questions,55)))
    ms$dvalue=cut(ms$value,breaks=c(1:5),include.lowest=T)

    ## (2) plot it
    ## (2.1) boxplots per question
    ##ggplot(data=ms,aes(x=variable20,y=value))+geom_boxplot(color="red")+coord_flip()+labs(title=paste0("Distribuição das repostas em ",s[[4]],". O professor..."),y="Nota",x="Questão")
    ## (2.2) histograms per question
    ##ggplot(data=ms,aes(x=value))+geom_bar(stat="bin",fill="red")+facet_wrap(~variable55)+labs(title=paste0("Distribuição das repostas em ",s[[4]],". O professor..."),x="Nota",y="Número de respostas")#+scale_y_log10()
    ## (2.3) percentages/counts per interval
    eval(substitute(expr = {
        ggplot(data=ms,aes(x=variable30,fill=dvalue))+
            geom_bar(aes(y=..count../nr))+
            scale_y_continuous(labels=scales::percent)+
            coord_flip()+
            labs(title=paste0("Distribuição das repostas em ",s[[4]],". O professor..."),y="Percentagens",x="Questão")+
            scale_fill_brewer(name="Nota",palette="Oranges")#values=c("red","black","white","gray"))
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
        label_function = geom_text_repel
    } else
        label_function = geom_text

    if (rank)
        for (cn in sn)
            ma[[cn]]=nrow(ma)+1-rank(ma[[cn]],na.last="keep")
    
    ma = ma[selectedCourses,]
    mma=melt(ma, id.vars = c("disc","mandatory"))
    ma = ma %>% filter(disc %in% mma$disc)

    mma$variable=factor(mma$variable,levels=c("llp",levels(mma$variable),"rlp"))
    mma=rbind(mma,data.frame(disc=" ",mandatory=T,variable="llp",value=NA))
    mma=rbind(mma,data.frame(disc=" ",mandatory=T,variable="rlp",value=NA))
    
    ## (3) plot it
    g=ggplot(data=mma,aes(x=variable,y=value,color=disc,group=disc))+geom_point()+geom_line()+theme(legend.position="none")+scale_x_discrete(breaks=sn,labels=sn)
    if (onlyMandatory)
        labels=str_wrap(ma$disc,80)
    else
        labels=str_wrap(paste(ma$disc,ifelse(ma$mandatory,"","(E)")),80)

    if (length(restrictions)>0)
        comment=paste(comment," (Somente disciplinas ",paste(restrictions,collapse=" "),")",sep="")

    if (rank)
        g=g+label_function(data=ma,x=2,y=-ma[,sn[1]],label=labels,hjust=1.1,size=2)+
            label_function(data=ma,x=length(sn)+1,y=-ma[,sn[length(sn)]],label=labels,hjust=-0.1,size=2)+
            scale_y_reverse()+
            labs(title=paste("Posições na avaliação ",sn[1],"-",sn[length(sn)],comment,sep=""),x="Semestre",y="Posição")
    else
        g=g+label_function(data=ma,x=2,y= ma[,sn[1]],label=labels,hjust=1.1,size=2)+
            label_function(data=ma,x=length(sn)+1,y= ma[,sn[length(sn)]],label=labels,hjust=-0.1,size=2)+
            labs(title=paste("Avaliação ",sn[1],"-",sn[length(sn)],comment,sep=""),x="Semestre",y="Nota")
    g
}

######################################################################
## evaluations
######################################################################
pdf(onefile=T,width=9,height=6)

## (1) evaluation of complete semesters
## NOTE: empty fields and NAs have been mapped to NA
y14s2=evaluateSemester("data/sai/2014-2/AvalDocPeloDisc 2014-2.csv","2014-2",format="v1")
y15s1=evaluateSemester("data/sai/2015-1/AvalDocPeloDisc 2015-1.v1.csv","2015-1",format="v1")
y15s2=evaluateSemester("data/sai/2015-2/ADoc Disc - Quant.csv","2015-2",format="v2")
y16s1=evaluateSemester("data/sai/2016-1/AvalDocPeloDisc 2016-1.csv","2016-1",format="v4")
y16s2=evaluateSemester("data/sai/2016-2/Bloco do Professor.csv","2016-2",format="v5",percFile=T)
y17s1=evaluateSemester("data/sai/2017-1/DocenteDisc-2017-1.csv","2017-1",format="v6",percFile=T)
y17s2=evaluateSemester("data/sai/2017-2/AvalDiscente_2017-2.csv","2017-2",format="v7",percFile=T)

allsemesters=list(y14s2,y15s1,y15s2,y16s1,y16s2,y17s1,y17s2)

## (2) compare evaluations of two semesters
for (s1 in 1:(length(allsemesters)-1)) {
    for (s2 in (s1+1):length(allsemesters)) {
        print(compareSemesters(allsemesters[[s1]],allsemesters[[s2]]))
        ##readline(prompt="Press [enter] to continue")
    }
}

## (3) compare classes in a semester; and compare multiple classes
for (s in 1:length(allsemesters)) {
    t3=compareClasses(allsemesters[[s]])
    print(ggplot(t3[[1]],aes(str_wrap(paste0(disc," (",Nt,")"),20),m))+geom_boxplot(color="red")+geom_point(colour = "darkred", size = 1)+coord_flip()+labs(title=paste0("Disciplinas com 3 ou mais turmas em ",t3[[2]]),y="Nota média da turma",x="Disciplina"))
    ##readline(prompt="Press [enter] to continue")
}

## (4) plots per question
for (s in 1:length(allsemesters)) {
    print(evaluateQuestions(allsemesters[[s]]))
    ##readline(prompt="Press [enter] to continue")
}

## (5) track ranks over semesters
evensemesters=list(y14s2,y15s2,y16s2,y17s2)
oddsemesters=list(y15s1,y16s1,y17s1)
for (limit in c(4,5)) {
    print(trackSemesters(evensemesters,onlybelow=limit))
    print(trackSemesters(evensemesters,rank=T,onlybelow=limit))
    
    print(trackSemesters(oddsemesters,onlybelow=limit))
    print(trackSemesters(oddsemesters,rank=T,onlybelow=limit))
    
    print(trackSemesters(allsemesters,onlybelow=limit))
    print(trackSemesters(allsemesters,rank=T,onlybelow=limit))
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
ggplot(data=subset(allfb,N>=10),aes(x=fcname,y=N,fill=sem))+
    geom_bar(stat="identity")+
    coord_flip()+
    labs(title="",x="")+
    scale_fill_discrete(name="Sem.")+
    theme(legend.position=c(0.9,0.13))+
    scale_fill_brewer(name="Nota",palette="Oranges")#+scale_y_log10()##
dev.off()

print(xtable(fb),file="cursos-2015-1.tex")
fb[order(-fb$N),]

## Open points/Ideas:
## 1) Overall correlation of all eleven questions (plot a matrix)?
ggpairs((y17s2[[3]] %>% select(-N,-qNA,-m,-xnames,-disc)))
ggplot(data=y17s2[[3]])+geom_point(aes(x=q3,y=q5))+geom_abline()+labs(x=questions[3],y=questions[5])
## 2) Seperate aspects of professors and courses?
## 3) Join by "question groups"?
