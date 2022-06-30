dados = read.csv("carprice.csv")
dados
pairs(preco~., data=dados)
modelo = lm(preco~.,data=dados)
modelo
as.numeric(dados)
cor(dados)
modelo2 = lm(preco~comprimento+largura+cavalos, data=dados)
modelo2
anova(modelo)
anova(modelo2)


ajuste123=lm(y~x1+x2+x3,data=dados)
ajuste1=lm(y~x1,data=dados)
ajuste2=lm(y~x2,data=dados)
ajuste3=lm(y~x3,data=dados)
ajuste23=lm(y~x2+x3,data=dados)
ajuste13=lm(y~x1+x3,data=dados)
ajuste12=lm(y~x1+x2,data=dados)
aov(ajuste123) # anova do modelo 123
352.2698+33.1689+11.5459 #calcula SQreg do modelo 123
aov(ajuste23) # anova do modelo 23
381.9658+2.3139 #calcula SQreg do modelo 23
396.9846-384.2797 #calcula SQ Extra de beta1 dado que beta2 e beta3 já estão no MRL
aov(ajuste13) #anova do modelo 13
352.3+37.2 #calcula SQreg do modelo 13
396.9846-389.5 #calcula SQ Extra de beta2 dado que beta 1 e beta 3 já estão no modelo
aov(ajuste12) # anova do modelo 12
352.2698+33.1689 #calcula SQReg do modelo 12
396.9846-385.4387 # calcula SQ Extra de beta3 dado que beta1 e beta2 já estão no modelo
#calcula estatísticas dos testes F parciais de cada coeficiente 
12.7049/6.2
7.4846/6.2
11.5459/6.2
qf(.95,1,16) # limiar da região crítica dos testes F parciais
# calcula os p-valores dos testes F parciais
1-pf(2.05,1,16)
1-pf(1.21,1,16)
1-pf(1.86,1,16)
X=matrix(1,20,4) # matriz do modelo completo 123
X[,2]=dados$x1
X[,3]=dados$x2
X[,4]=dados$x3
# calcula matriz X'X
XTX=t(X)%*%X
XTX
# calcula o determinante da matriz X'X
det(XTX)
# calcula a inversa de X'X
iXTX=solve(XTX)
iXTX
# calcula o determinante da inversa de X'X
det(iXTX)
# calcula a matriz de correlação das variáveis explicativas e arredonda para 4 casas decimais
round(cor(dados[,1:3]),digits=4)
#constrói gráficos da análise de resíduos do modelo 13
residuosp=ajuste13$residuals/2.496
plot(ajuste13$fitted,residuosp,main="Residuos padronizados versus valores ajustados")
abline(h=0,lty=2)
plot(ajuste13$fitted,residuosp,main="Residuos padronizados versus valores ajustados",ylim=c(-2,2))
abline(h=0,lty=2)
abline(h=2,lty=2,col="red")
abline(h=-2,lty=2,col="red")
par(mfrow=c(1,2))
plot(dados$x1,residuosp,main="Residuos padronizados versus X1",ylim=c(-2,2))
abline(h=0,lty=2)
abline(h=2,lty=2,col="red")
abline(h=-2,lty=2,col="red")
plot(dados$x3,residuosp,main="Residuos padronizados versus X3",ylim=c(-2,2))
abline(h=0,lty=2)
abline(h=-2,lty=2,col="red")
abline(h=2,lty=2,col="red")
par(mfrow=c(2,1))
boxplot(residuosp,main="Boxplot dos resíduos padronizados")
qqnorm(residuosp,main="Normal plot dos resíduos padronizados")
abline(0,1,lty=2)
win.graph()
par(mfrow=c(1,2))
boxplot(residuosp,main="Boxplot dos resíduos padronizados")
qqnorm(residuosp,main="Normal plot dos resíduos padronizados")
abline(0,1,lty=2)
# realiza teste de Kolmogorov-Smirnoff para aderência à suposição de normalidade
ks.test(residuosp,"pnorm")
# realiza teste de Shapiro - Wilk para aderência à suposição de normalidade
shapiro.test(residuosp)