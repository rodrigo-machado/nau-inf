##########################################################################################
# Script que processa informaçoes do NAU-INF
# AUTOAVALIAÇÃO DOCENTE (2015-2)
# Autor: Rodrigo Machado (rma@inf.ufrgs.br)
##########################################################################################


##########################################################################################
# Bibliotecas necessárias: GGPlot2, RColorBrewer                                         #
##########################################################################################
library(ggplot2)
library(RColorBrewer)
##########################################################################################


########################################################################################## 
#  Instruções para importar dados a partir das planilhas da SAI                          #
#  - Libreoffice: mudar separador decimal para ponto (virgula eh o padrao no Brasil)     #
#  - Eliminar as duas primeiras colunas da planilha                                      #
#  - Exportar a planilha de notas em formato CSV com nome <semestre>.csv                 #
#       Exemplo: 2015-2.csv                                                              #
##########################################################################################


##########################################################################################
#  Leitura dos dados de 2015-2                                                           #
##########################################################################################
dados <- read.csv("2015-2.csv")


##########################################################################################
#  AUTOAVALIAÇÃO DOCENTE: QUESTÕES EM 2015-2                                             #
##########################################################################################
p      <- list()
p[[1]]  = ''
p[[2]]  = ''
p[[3]]  = '1.Mantive atualizados os conteúdos e conhecimentos \nrelacionados com a atividade de ensino'
p[[4]]  = '2.Os resultados de minhas pesquisas enriqueceram a \natividade de ensino.'
p[[5]]  = '3.Os alunos possuíam os conhecimentos prévios \nnecessários para o acompanhamento da atividade de ensino.'
p[[6]]  = '4.Os alunos mostraram interesse e dedicação \ndurante as aulas e nas demais atividades solicitadas para \na atividade de ensino.'
p[[7]]  = '5.Disponibilizei tempo para atender aos alunos \nfora da sala de aula, pessoalmente e/ou à distância.'
p[[8]]  = '6.Desenvolvi a atividade de ensino utilizando \nrecursos e procedimentos adequados, de modo a contribuir \npara a reflexão, participação e a formação integral dos alunos.'
p[[9]]  = '7.A atividade de ensino alocada para mim pelo \nDepartamento é compatível com minha área de\n formação e/ou atuação.'
p[[10]] = '8.Cumpri o plano da atividade de ensino,\n disponibilizado no site da UFRGS.'
p[[11]] = '9.Não tive dificuldades em estabelecer relações \nentre os conteúdos da atividade de ensino\n e o currículo do curso.'
p[[12]] = '10.No desenvolvimento da atividade de ensino, a \ndiversidade sociocultural dos alunos foi contemplada.'
p[[13]] = '11.Utilizei atividades de avaliação compatíveis com \nos conhecimentos, habilidades e atitudes requeridos \nna atividade de ensino.'
p[[14]] = '12.As atividades de extensão por mim desenvolvidas\n enriqueceram a minha atividade de ensino.'
p[[15]] = '13.Minha atividade de ensino inclui a atuação dos\n alunos em atividades de extensão junto à comunidade.'
p[[16]] = '14.Os resultados das avaliações da atividade\n de ensino foram analisados com a turma.'
p[[17]] = '15.Foi possível manter sempre atitudes de\n respeito no trato com os alunos.'
p[[18]] = '16.Estabeleci relações entre os conteúdos da atividade\n de ensino e os campos de trabalho da profissão,\n contextualizando com as demandas da realidade do país.'

##########################################################################################
#                             PREPARAÇÃO DOS DADOS                                       #
##########################################################################################

# Remove valores não-numéricos, preenchendo todos com NA. 
d = dados
for (i in 3:18) { 
  temp       <- as.numeric(as.character(d[[i]]))
  d[[i]] <- temp
}

# DADOS
# p = data.frame (3 a 18) de perguntas (string)
# d = data.frame (3 a 18) de respostas (lista de (números ou NA))


##########################################################################################
#                             PERGUNTAS                                                  #
##########################################################################################
### Há XX entradas na base de dados, cada entrada correspondendo a uma turma
### Para algumas questões, há espaços em branco ou texto indicando (não se aplica)
###  PRIMEIRA PERGUNTA:  da representatividade da amostra.
###  Quantas turmas foram oferecidas ao total?
###  Quantos professores não responderam?
###    (estabeler o tamanho da amostra em relação à população)
##########################################################################################



##########################################################################################
#                             ANALISE DAS AMOSTRAS                                       #
##########################################################################################

################################################# 
# Função que gera o histograma para cada questão, 
# armazenando em arquivo pdf, de nome plotN.pdf
# - arg1 : número da questão 
# usa: global array: p (perguntas)
#      global array: d (respostas)
################################################# 
qHist = function(i){
  svg(paste('plots/plot',i-2,'.svg'))
  hist(d[[i]],
     include.lowest = TRUE,
     right = FALSE,
     breaks= seq(1,5,by=0.25),
     xlim=c(1,5),
     #ylim=c(0,100),
     main=p[[i]],
     xlab='Autoavaliação',
     ylab=paste('Respostas (',length(na.omit(d[[i]])),'/',length(dados$X1),')'),
     col='steelblue',
     border="blue",
     na.rm=TRUE,
     fg="blue")
  dev.off()
}
############################################## 


##########################################################################################
#                     GERA HISTOGRAMA PARA CADA QUESTÃO                                  #
##########################################################################################
for (i in 3:18) qHist(i)
##########################################################################################


##########################################################################################
#                     GERA BLOXPLOT COM TODAS AS QUESTÕES                                #
##########################################################################################
#svg(paste('plots/boxplot.svg'))
#boxplot(d[3:18],
#  col=rainbow(14),
#  names=c(1:16),
#  horizontal=FALSE)
#dev.off()
##########################################################################################


##########################################################################################
#                    GERA STACKED BARPLOT COM TODAS AS QUESTÕES                          #
##########################################################################################


#### countInRange (i,j,dat) retorna a contagem de quantos
#### números em dat estão no intervalo (i,j]
#### valores não-numéricos são ignorados
countInRange = function (i,j,dat) {
  t <- na.omit(dat)
  cnt <- t[ i<t & t<=j ]
  return (length(cnt))
}  


#### countNA retorna o número de questões respondidas "na" (não se aplica)
countNA = function (dat) {
  t   <- as.character(dat)
  cnt <- length (t [ t=="Não se Aplica" ])
  return (cnt)
}


#### toLevels (dat) retorna um vetor de seis posições
#### contendo a contagem de elementos nos intervalos de 
#### (0,1], (1,2], (2,3], (3,4], (4,5], na
toLevels = function (dat,dat2) {
  return (c(countInRange(0,1,dat),
            countInRange(1,2,dat),
            countInRange(2,3,dat),
            countInRange(3,4,dat),
            countInRange(4,5,dat),
            countNA(dat2)))
}



#### l é o dataframe necessário para criação do stacked bar plot
l <- data.frame(Q1 =toLevels(d$X1,dados$X1),
                Q2 =toLevels(d$X2,dados$X2),
                Q3 =toLevels(d$X3,dados$X3),
                Q4 =toLevels(d$X4,dados$X4),
                Q5 =toLevels(d$X5,dados$X5),
                Q6 =toLevels(d$X6,dados$X6),
                Q7 =toLevels(d$X7,dados$X7),
                Q8 =toLevels(d$X8,dados$X8),
                Q9 =toLevels(d$X9,dados$X9),
                Q10=toLevels(d$X10,dados$X10),
                Q11=toLevels(d$X11,dados$X11),
                Q12=toLevels(d$X12,dados$X12),
                Q13=toLevels(d$X13,dados$X13),
                Q14=toLevels(d$X14,dados$X14),
                Q15=toLevels(d$X15,dados$X15),
                Q16=toLevels(d$X16,dados$X16)
                )


### Escolha de sequência de cores
colseq <- c(brewer.pal(5,"RdBu"),"orange")


### Gera o barplot
svg('plots/stackedbarplot.svg')

barplot(as.matrix(l),
        col=colseq,
        cex.names=0.5,
        ylim=c(0,100),
        xlim=c(0,23),
        width=1)

legend("bottomright", 
       legend=c("na","(4,5]","(3,4]","(2,3]","(1,2]","[1,1]"),
       fill=colseq[6:1])

dev.off()




##########################################################################################
#       Gera estatísticas sobre questões, direcionando para um arquivo CSV               #
##########################################################################################


#### toLevelsAndStats (dat) retorna um vetor de oito posições
#### contendo a contagem de elementos nos intervalos de 
#### [1,1], (1,2], (2,3], (3,4], (4,5], na, média e desvio padrão
toLevelsAndStats = function (dat,dat2) {
  return (c(countInRange(0,1,dat),
            countInRange(1,2,dat),
            countInRange(2,3,dat),
            countInRange(3,4,dat),
            countInRange(4,5,dat),
            countNA(dat2),
            mean(dat,na.rm=TRUE),
            sd(dat,na.rm=TRUE)))
}


#### l é o dataframe necessário para criação do arquivo de estatísticas
lstats <- data.frame(
  Q1 =toLevelsAndStats(d$X1,dados$X1),
  Q2 =toLevelsAndStats(d$X2,dados$X2),
  Q3 =toLevelsAndStats(d$X3,dados$X3),
  Q4 =toLevelsAndStats(d$X4,dados$X4),
  Q5 =toLevelsAndStats(d$X5,dados$X5),
  Q6 =toLevelsAndStats(d$X6,dados$X6),
  Q7 =toLevelsAndStats(d$X7,dados$X7),
  Q8 =toLevelsAndStats(d$X8,dados$X8),
  Q9 =toLevelsAndStats(d$X9,dados$X9),
  Q10=toLevelsAndStats(d$X10,dados$X10),
  Q11=toLevelsAndStats(d$X11,dados$X11),
  Q12=toLevelsAndStats(d$X12,dados$X12),
  Q13=toLevelsAndStats(d$X13,dados$X13),
  Q14=toLevelsAndStats(d$X14,dados$X14),
  Q15=toLevelsAndStats(d$X15,dados$X15),
  Q16=toLevelsAndStats(d$X16,dados$X16)
  )

options(digits=5)
write.csv(lstats,"plots/stats.csv")


