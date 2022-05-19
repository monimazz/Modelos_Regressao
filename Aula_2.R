# AULA 2: TRANSFORMAÇÃO LOGARITMICA, PREVISÃO E FORMA FUNCIONAL QUADRÁTICA

# diretório da base de dados
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

################## TRANSFORMAÇÃO LOGARÍTMICA ###################

# Exemplo modelo log-linear

# log salário-hora
filhos$logsalario_h<-log(filhos$salario_h) # cria variável com o log do salário-hora

summary(filhos$logsalario_h) # estatística descritiva do log de salário-hora

is.na(filhos) <- sapply(filhos, is.infinite) # transformando todos os - Inf 
# em missing data (NA)

# regressão de log do salário-hora no número de filhos, escolaridade e idade
loglin<-lm(logsalario_h ~ filhos+educ+idade, data=filhos, na.action=na.exclude)
summary(loglin)

# Exemplo modelo log-log com os dados de imóveis

# regressão de log do preço no log da distância do metrô mais próximo e outras covariadas
loglog<-lm(ln_preco ~ ln_metro, data=imoveis, na.action=na.exclude)
summary(loglog)

################## PREVISÃO ########################

# obtendo a previsão de y quando log(y) é a variável de resultado (ver slide 9)

# Regressão do log(preço) sobre as variáveis explicativas
mqo<-lm(ln_preco ~ ln_metro+dorm+banho+garag+elev, data=imoveis, na.action=na.exclude)

summary(mqo)

# 1) Extrair o valor ajustado de log(preco) e o valor residual
imoveis$logyhat<-fitted(mqo)
imoveis$uhat<-resid(mqo)

# 2) Obter alpha_0
alpha0_hat<- sum(exp(imoveis$uhat))/nobs(mqo)

# 3) predizer log salário para um apartamento médio

#médias amostrais
mmetro<-mean(imoveis$ln_metro)
mdorm<-mean(imoveis$dorm)
mbanho<-mean(imoveis$banho)
mgarag<-mean(imoveis$garag)
melev<-mean(imoveis$elev)

valpred<-data.frame(ln_metro=mmetro, dorm=mdorm, banho=mbanho, garag=mgarag, elev=melev)

pred<-predict(mqo, valpred)

# 4) estabelecer yhat da equação do item 4, slide 9
alpha0_hat*exp(pred)

############### FORMA FUNCIONAL QUADRÁTICA #####################

# Efeito não linear da idade no salário
mqo<-lm(salario ~ idade+I(idade^2)+mulher, data=trabalho) # banco de dados da PNAD Contínua 2016 (trabalho.xlsx)
summary(mqo)

# Gráfico do impacto da idade no salário (Ver slide 28)

idade <- seq(14, 95, len = 100)
beta0 <- -74.76326
beta1 <-  74.37019
beta2 <- -0.69620
salario_previsto = beta0 + beta1 * idade + beta2 * idade^2
plot (idade,salario_previsto, type="l", xlab="Idade", 
      ylab = "Salario")

