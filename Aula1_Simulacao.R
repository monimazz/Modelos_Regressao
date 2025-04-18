##################### SIMULA��O ##########################



# Vari�vel hipot�tica que simula a m�dia e o desvio padr�o de educ (Aula 1), usando uma distribui��o normal
# e 200 indiv�duos 
x<-rnorm(n=200, mean=8.609, sd=4.36)


# vari�vel hipot�tica do sal�rio encontrada atrav�s da regress�o de sal�rio (y) em educ (x)
y<-0.83+180.67*x

# gr�fico de dispers�o
plot(x,y)

# Reta da regress�o no gr�fico
reg<-lm(y~x)
abline(reg, col="red", lwd=2)

#correla��o de x e y
cor(x,y)

# Os resultados da simula��o mostram que existe uma correla��o perfeita entre x e y, e todos os
# valores observados dos sal�rios s�o iguais aos valores estimados (todos os pontos est�o na reta)

# Agora vamos dar algum ru�do (termo de erro aleat�rio u) em y. O termo de erro possui distribui��o normal,
# m�dia zero e desvio-padr�o desconhecido. Quando maior o desvio-padr�o, maior a variabilidade dos dados.

# Experimentem alterar o desvio-padr�o (sd), plotar o novo gr�fico de dispers�o e ver como a correla��o
# diminui.

# A dist�ncia entre o y observado de cada indiv�duo e o y estimado pela reta de regress�o (yhat) � o 
# valor residual


y<-0.83+180.67*x+rnorm(n=200, mean=0, sd=750) # colocando um termo de erro aleat�rio em y

# gr�fico de dispers�o
plot(x,y)

# Reta da regress�o no gr�fico
reg<-lm(y~x)
abline(reg, col="red", lwd=2)

#correla��o de x e y
cor(x,y)

