##################### SIMULAÇÂO ##########################



# Variável hipotética que simula a média e o desvio padrão de educ (Aula 1), usando uma distribuição normal
# e 200 indivíduos 
x<-rnorm(n=200, mean=8.609, sd=4.36)


# variável hipotética do salário encontrada através da regressão de salário (y) em educ (x)
y<-0.83+180.67*x

# gráfico de dispersão
plot(x,y)

# Reta da regressão no gráfico
reg<-lm(y~x)
abline(reg, col="red", lwd=2)

#correlação de x e y
cor(x,y)

# Os resultados da simulação mostram que existe uma correlação perfeita entre x e y, e todos os
# valores observados dos salários são iguais aos valores estimados (todos os pontos estão na reta)

# Agora vamos dar algum ruído (termo de erro aleatório u) em y. O termo de erro possui distribuição normal,
# média zero e desvio-padrão desconhecido. Quando maior o desvio-padrão, maior a variabilidade dos dados.

# Experimentem alterar o desvio-padrão (sd), plotar o novo gráfico de dispersão e ver como a correlação
# diminui.

# A distância entre o y observado de cada indivíduo e o y estimado pela reta de regressão (yhat) é o 
# valor residual


y<-0.83+180.67*x+rnorm(n=200, mean=0, sd=750) # colocando um termo de erro aleatório em y

# gráfico de dispersão
plot(x,y)

# Reta da regressão no gráfico
reg<-lm(y~x)
abline(reg, col="red", lwd=2)

#correlação de x e y
cor(x,y)

