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
