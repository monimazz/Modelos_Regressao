# AULA 2: TRANSFORMA��O LOGARITMICA, PREVIS�O E FORMA FUNCIONAL QUADR�TICA

# diret�rio da base de dados
setwd("C:/Users/vmotta/Documents/Dados")

# Importando dados em Excel
library(readxl) # seleciona o pacote na aba Packages (canto inferior direito)

trabalho <- read_excel("trabalho.xlsx") 
salario <- read_excel("salario.xlsx") 
imoveis <- read_excel("imoveis.xls") 
filhos<-read_excel("filhos.xls")

View(trabalho) # visualiza o arquivo de Excel no RStudio
View(salario) 
View(imoveis)
View(filhos)

################## TRANSFORMA��O LOGAR�TMICA ###################

# Exemplo modelo log-linear

# log sal�rio-hora
filhos$logsalario_h<-log(filhos$salario_h) # cria vari�vel com o log do sal�rio-hora

summary(filhos$logsalario_h) # estat�stica descritiva do log de sal�rio-hora

is.na(filhos) <- sapply(filhos, is.infinite) # transformando todos os - Inf 
# em missing data (NA)

# regress�o de log do sal�rio-hora no n�mero de filhos, escolaridade e idade
loglin<-lm(logsalario_h ~ filhos+educ+idade, data=filhos, na.action=na.exclude)
summary(loglin)

# Exemplo modelo log-log com os dados de im�veis

# regress�o de log do pre�o no log da dist�ncia do metr� mais pr�ximo e outras covariadas
loglog<-lm(ln_preco ~ ln_metro, data=imoveis, na.action=na.exclude)
summary(loglog)

################## PREVIS�O ########################

# obtendo a previs�o de y quando log(y) � a vari�vel de resultado (ver slide 9)

# Regress�o do log(pre�o) sobre as vari�veis explicativas
mqo<-lm(ln_preco ~ ln_metro+dorm+banho+garag+elev, data=imoveis, na.action=na.exclude)

summary(mqo)

# 1) Extrair o valor ajustado de log(preco) e o valor residual
imoveis$logyhat<-fitted(mqo)
imoveis$uhat<-resid(mqo)

# 2) Obter alpha_0
alpha0_hat<- sum(exp(imoveis$uhat))/nobs(mqo)

# 3) predizer log sal�rio para um apartamento m�dio

#m�dias amostrais
mmetro<-mean(imoveis$ln_metro)
mdorm<-mean(imoveis$dorm)
mbanho<-mean(imoveis$banho)
mgarag<-mean(imoveis$garag)
melev<-mean(imoveis$elev)

valpred<-data.frame(ln_metro=mmetro, dorm=mdorm, banho=mbanho, garag=mgarag, elev=melev)

pred<-predict(mqo, valpred)

# 4) estabelecer yhat da equa��o do item 4, slide 9
alpha0_hat*exp(pred)

############### FORMA FUNCIONAL QUADR�TICA #####################

# Efeito n�o linear da idade no sal�rio
mqo<-lm(salario ~ idade+I(idade^2)+mulher, data=trabalho) # banco de dados da PNAD Cont�nua 2016 (trabalho.xlsx)
summary(mqo)

# Gr�fico do impacto da idade no sal�rio (Ver slide 28)

idade <- seq(14, 95, len = 100)
beta0 <- -74.76326
beta1 <-  74.37019
beta2 <- -0.69620
salario_previsto = beta0 + beta1 * idade + beta2 * idade^2
plot (idade,salario_previsto, type="l", xlab="Idade", 
      ylab = "Salario")

