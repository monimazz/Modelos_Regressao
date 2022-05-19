############ AULA 2: REGRESSÃO SIMPLES E MÚLTIPLA #############

# Importando dados em Excel

install.packages("readxl") # Instala o pacote para ler arquivos de Excel
library(readxl) # seleciona o pacote na aba Packages (canto inferior direito)
salario <- read_excel("C:/Users/vmotta/Desktop/Aula Econometria/Dados/salario.xlsx") 
View(salario) # visualiza o arquivo de Excel no RStudio


# SOBRE OS DADOS
# O arquivo salario.xlsx contém um subconjunto de variáveis da Pesquisa
# Nacional por Amostra de Domicílios (PNAD) de 2014.

#################### REGRESSÃO LINEAR SIMPLES ###########################

# Qual é o impacto da escolaridade no salário?
# Para responder a pergunta, estimamos um modelo de regressão do salário (y) na
# escolaridade (x)

# O comando lm (y ~ x, data=df) roda um modelo linear (regressão) da
# variável dependente (y) na(s) variável(eis) independente(s)

lm(salariom ~ educ, data = salario, na.action=na.exclude)

# Para obter mais informação sobre a regressão, podemos colocar o comando lm 
# em algum objeto e usar o comando summary da seguinte forma

mqo<-lm(salariom ~ educ, data = salario, na.action=na.exclude)

# Resumo da informação no objeto mqo
summary(mqo)


# Como obtemos os valores estimados do salário (yhat) e os resíduos (uhat)?
# Criamos duas variáveis: yhat e uhat

salario$yhat<-fitted(mqo) # variável yhat
salario$uhat<-resid(mqo) # variável uhat

# Gostaríamos de estimar o salário mensal de indíviduos com 5 anos de escolaridade
valpred<-data.frame(educ=5)
predict(mqo, valpred)

# E se eu quisesse saber o salário mensal de indivíduos com 0, 5, 10 e 15 anos
# de escolaridade?
valpred<-data.frame(educ=c(0,5,10,15))
predict(mqo, valpred)

############### R-QUADRADO ######################################

# Uma maneira simples de calcular o R-Quadrado é o quadrado do coeficiente de
# correlação entre o salario observado (y) e o salário estimado pelo modelo (yhat)
cor(salario$salariom,salario$yhat, use="complete.obs")^2 


############# REGRESSÃO LINEAR MÚLTIPLA (ESTIMAÇÂO) #######################

# SOBRE OS DADOS
# O arquivo filhos.xls contém um subconjunto de variáveis da Pesquisa
# Nacional por Amostra de Domicílios (PNAD) de 2014, contendo informações
# de mulheres entre 18 e 45 anos.


filhos <- read_excel("C:/Users/vmotta/Desktop/Aula Econometria/Dados/filhos.xls") 
View(filhos) # visualiza o arquivo de Excel no RStudio


# Regressão de salário-hora (y) em escolaridade (x1) e idade (x2)
mqo<-lm(salario_h ~ educ+idade, data=filhos, na.action=na.exclude)
summary(mqo)

# Regressão de salário-hora (y) em escolaridade (x1), idade (x2) e número de filhos (x3)
mqo<-lm(salario_h ~ educ+idade+filhos, data=filhos, na.action=na.exclude)
summary(mqo)



