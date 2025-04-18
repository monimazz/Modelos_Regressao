############ AULA 2: REGRESS�O SIMPLES E M�LTIPLA #############

# Importando dados em Excel

install.packages("readxl") # Instala o pacote para ler arquivos de Excel
library(readxl) # seleciona o pacote na aba Packages (canto inferior direito)
salario <- read_excel("C:/Users/vmotta/Desktop/Aula Econometria/Dados/salario.xlsx") 
View(salario) # visualiza o arquivo de Excel no RStudio


# SOBRE OS DADOS
# O arquivo salario.xlsx cont�m um subconjunto de vari�veis da Pesquisa
# Nacional por Amostra de Domic�lios (PNAD) de 2014.

#################### REGRESS�O LINEAR SIMPLES ###########################

# Qual � o impacto da escolaridade no sal�rio?
# Para responder a pergunta, estimamos um modelo de regress�o do sal�rio (y) na
# escolaridade (x)

# O comando lm (y ~ x, data=df) roda um modelo linear (regress�o) da
# vari�vel dependente (y) na(s) vari�vel(eis) independente(s)

lm(salariom ~ educ, data = salario, na.action=na.exclude)

# Para obter mais informa��o sobre a regress�o, podemos colocar o comando lm 
# em algum objeto e usar o comando summary da seguinte forma

mqo<-lm(salariom ~ educ, data = salario, na.action=na.exclude)

# Resumo da informa��o no objeto mqo
summary(mqo)


# Como obtemos os valores estimados do sal�rio (yhat) e os res�duos (uhat)?
# Criamos duas vari�veis: yhat e uhat

salario$yhat<-fitted(mqo) # vari�vel yhat
salario$uhat<-resid(mqo) # vari�vel uhat

# Gostar�amos de estimar o sal�rio mensal de ind�viduos com 5 anos de escolaridade
valpred<-data.frame(educ=5)
predict(mqo, valpred)

# E se eu quisesse saber o sal�rio mensal de indiv�duos com 0, 5, 10 e 15 anos
# de escolaridade?
valpred<-data.frame(educ=c(0,5,10,15))
predict(mqo, valpred)

############### R-QUADRADO ######################################

# Uma maneira simples de calcular o R-Quadrado � o quadrado do coeficiente de
# correla��o entre o salario observado (y) e o sal�rio estimado pelo modelo (yhat)
cor(salario$salariom,salario$yhat, use="complete.obs")^2 


############# REGRESS�O LINEAR M�LTIPLA (ESTIMA��O) #######################

# SOBRE OS DADOS
# O arquivo filhos.xls cont�m um subconjunto de vari�veis da Pesquisa
# Nacional por Amostra de Domic�lios (PNAD) de 2014, contendo informa��es
# de mulheres entre 18 e 45 anos.


filhos <- read_excel("C:/Users/vmotta/Desktop/Aula Econometria/Dados/filhos.xls") 
View(filhos) # visualiza o arquivo de Excel no RStudio


# Regress�o de sal�rio-hora (y) em escolaridade (x1) e idade (x2)
mqo<-lm(salario_h ~ educ+idade, data=filhos, na.action=na.exclude)
summary(mqo)

# Regress�o de sal�rio-hora (y) em escolaridade (x1), idade (x2) e n�mero de filhos (x3)
mqo<-lm(salario_h ~ educ+idade+filhos, data=filhos, na.action=na.exclude)
summary(mqo)



