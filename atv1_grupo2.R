install.packages("readxl") # Instala o pacote para ler arquivos de Excel
library(readxl) # seleciona o pacote na aba Packages (canto inferior direito)
filhos_base <- read_excel("filhos.XLS") 

filhos_base$salariom <- filhos_base$salario_h*filhos_base$horas_m

#PERGUNTA 1

mqo <- lm(salariom ~ filhos, data = filhos_base, na.action=na.exclude)

summary(mqo)


#PERGUNTA 3

mqo2 <- lm(salariom ~ filhos+educ, data = filhos_base, na.action=na.exclude)
summary(mqo2)


#PERGUNTA 4
mqo3 <- lm(salariom ~ filhos+educ+idade, data = filhos_base, na.action=na.exclude)
summary(mqo3)
valpred<-data.frame(educ=15, filhos =1, idade=38)
predict(mqo3, valpred)


#PERGUNTA 5
plot(mqo3)


