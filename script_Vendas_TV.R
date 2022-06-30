dados<-read.csv2("advertising.csv",sep=";",dec=".")
dados$vendas
dados$TV
pairs(dados)
modelo<-lm(dados$vendas~dados$TV+dados$radio+dados$jornal)
summary(modelo)
anova(modelo)
cor(dados)

modeloTV<-lm(dados$vendas~dados$TV)
modelojornal<-lm(dados$vendas~dados$jornal)
modeloradio<-lm(dados$vendas~dados$radio)
anova(modeloTV)
anova(modeloradio)
anova(modelojornal)

dados$TV_pad<-(dados$TV-mean(dados$TV))/sqrt(var(dados$TV))

dados$radio_pad<-(dados$radio-mean(dados$radio))/sqrt(var(dados$radio))

dados$jornal_pad<-(dados$jornal-mean(dados$jornal))/sqrt(var(dados$jornal))

dados$vendas_pad<-(dados$vendas-mean(dados$vendas))/sqrt(var(dados$vendas))

modelo_pad<-lm(dados$vendas_pad~dados$TV_pad+dados$radio_pad+dados$jornal_pad-1)

summary(modelo_pad)

anova(modelo_pad)


##############################################
#Avaliando fatores de inflação de incerteza
##############################################

#No modelo completo (exemplo de vendas e orçamentos em diferentes mídias)
modelo<-lm(dados$vendas~dados$TV+dados$radio+dados$jornal)
library(car)
vif(modelo)

#Criando, artificialmente, uma variável que tenha associação linear com o orçamento em TV

#Quando a variável criada tem associação muito forte com orçamento em TV:
tv_art<-0.5*dados$TV+rnorm(200,0,1)
plot(tv_art,dados$TV)
modelo_art<-lm(dados$vendas~dados$TV+tv_art+dados$jornal+dados$radio)
vif(modelo_art)
summary(modelo_art)
summary(modelo)

#Quando a variável criada tem associação linear com orçamento em TV, mas não tão forte:
tv_art<-0.5*dados$TV+rnorm(200,0,10)
plot(tv_art,dados$TV)
modelo_art<-lm(dados$vendas~dados$TV+tv_art+dados$jornal+dados$radio)
vif(modelo_art)


###################
#matriz hat
###################
X<-cbind(rep(1,length(dados$TV)),dados$TV,dados$jornal,dados$radio)
H<-X%*%solve(t(X)%*%X)%*%t(X)
dim(H)
H[1:5,1:30]
Z<-H>0.1

############################
# Análise Residual
############################
modelo<-lm(dados$venda~dados$TV+dados$radio)
h_ii<-hatvalues(modelo)
index<-seq(1,length(dados$TV))
plot(index,h_ii)
y_i<-dados$vendas
y_hat_i<-modelo$fitted.values
plot(y_i,y_hat_i)

#Resíduos ordinários
######################
e_i<-modelo$residuals
plot(y_hat_i,e_i)
plot(dados$TV,e_i)
plot(dados$radio,e_i)
plot(dados$jornal,e_i)


#Resíduos padronizados
######################
MQ_Res<-sum(e_i^2)/modelo$df.residual
sigma.hat<-sqrt(MQ_Res)
d_i<-e_i/sigma.hat
plot(y_hat_i,d_i)

#Resíduo Studentizado
######################
r_i<-rstandard(modelo)
plot(y_hat_i,r_i)
#cálculo detalhado de r_i, sem usar a fç interna do R
r_i_calc<-e_i/(sigma.hat*sqrt(1-h_ii))
plot(r_i,r_i_calc)

#Resíduo PRESS
#######################
press_i<-e_i/(1-h_ii)
plot(y_hat_i,press_i)

#Resíduo studentizado externamente
###################################
t_i<-rstudent(modelo)
plot(y_hat_i,t_i)
#cálculo detalhado de s_i, sem usar a fç interna do R
s2_i<-(df.residual(modelo)*MQ_Res-e_i^2/(1-h_ii))/(df.residual(modelo)-1)
t_i_calc<-e_i/sqrt(s2_i*(1-h_ii))
plot(t_i,t_i_calc)

library(MASS)
? boxcox
boxcox(modelo)

v<-exp(dados$vendas)
modelov<-lm(v~dados$TV+dados$radio+dados$jornal)
boxcox(modelov)
v<-(dados$vendas)^2
modelov<-lm(v~dados$TV+dados$radio+dados$jornal)
boxcox(modelov)


