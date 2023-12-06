#atribuicao a bd os microdados
bd <- microdados_enem_2022_ok

#CRIACAO TABELAS E GRAFICOS

# instalção pacote gtsummary
install.packages("gtsummary")

require(gtsummary)

#cricação tabela 1 (escola X idade X notas)

tabela <- bd %>% select(TP_FAIXA_ETARIA, TP_COR_RACA, TP_ESCOLA, NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO)
tabela %>% tbl_summary()
tabela %>% tbl_summary(by=TP_ESCOLA)

#criação tabela 2 (nota CN X serie pai X serie mãe X renda mes X computador na residencia X internet na residencia)
#prova CN - média das notas mais baixas, provavelmente a mais dificil comparada as demais provas

tabela <- bd %>% select(NU_NOTA_CN, SERIE_PAI, SERIE_MAE, RENDA_MES, COMPUT_RESID, INTERNET_RESID)
tabela %>% tbl_summary()
tabela %>% tbl_summary(by=COMPUT_RESID)

#grafico (uf X notas)

install.packages("gridExtra")
library(gridExtra)

install.packages("ggplot2")
library(ggplot2)


#calculo da media

bd$media <- (bd$NU_NOTA_CN+bd$NU_NOTA_CH+bd$NU_NOTA_LC+
               bd$NU_NOTA_MT+ bd$NU_NOTA_REDACAO)/5

#boxplot com media de todas as notas comparando a cor/raça
cores <- brewer.pal(8, "Blues")
boxplot(bd$media~bd$TP_COR_RACA, col=(cores), main="Média das notas por cor/raça", xlab = "Tipo cor/raça", ylab = "Media das notas")


#boxplot media da nota por renda
cores <- brewer.pal(8, "Blues")
boxplot(bd$media~bd$SG_UF_PROVA, col=(cores), main="Média das notas por sigla UF prova", xlab = "Sigla UF prova", ylab = "Media das notas")


#boxplot de cada nota com tipo de escola

cores <- brewer.pal(3, "Blues")
boxplot(bd$NU_NOTA_CH~bd$TP_ESCOLA, col=(cores), main="Notas por tipo de escola", xlab = "Tipo de escola", ylab = "Nota em CH")

cores <- brewer.pal(3, "Greens")
boxplot(bd$NU_NOTA_LC~bd$TP_ESCOLA, col=(cores), main="Notas por tipo de escola", xlab = "Tipo de escola", ylab = "Nota em LC")

cores <- brewer.pal(3, "Oranges")
boxplot(bd$NU_NOTA_CN~bd$TP_ESCOLA, col=(cores), main="Notas por tipo de escola", xlab = "Tipo de escola", ylab = "Nota em CN")

cores <- brewer.pal(3, "Purples")
boxplot(bd$NU_NOTA_MT~bd$TP_ESCOLA, col=(cores), main="Notas por tipo de escola", xlab = "Tipo de escola", ylab = "Nota em MT")

cores <- brewer.pal(3, "Reds")
boxplot(bd$NU_NOTA_REDACAO~bd$TP_ESCOLA, col=(cores), main="Notas por tipo de escola", xlab = "Tipo de escola", ylab = "Nota em Redação")

#dispercao
library(ggplot2)
ggplot(bd, aes(x = bd$media, y = bd$RENDA_MES, color = bd$TP_ESCOLA)) + 
  geom_point(size = 2, shape = 16) + # Ajustar tamanho e forma dos pontos
  scale_color_brewer(palette = "Pastel1") + # Escolher uma paleta de cores
  labs(title = "Renda x Média das notas x Tipo escola", x = "Média das notas", y = "Renda mensal", color = "Tipo escola") + # Adicionar títulos
  theme_minimal() # Estilo do gráfico


#CRIAÇÃO TESTES, APRESENTAÇÃO DADOS

#normalidade

#teste para cada prova

ks.test(bd$NU_NOTA_CH,"pnorm",mean(bd$NU_NOTA_CH),sd(bd$NU_NOTA_CH))

ks.test(bd$NU_NOTA_REDACAO,"pnorm",mean(bd$NU_NOTA_REDACAO),sd(bd$NU_NOTA_REDACAO))

ks.test(bd$NU_NOTA_LC,"pnorm",mean(bd$NU_NOTA_LC),sd(bd$NU_NOTA_LC))

ks.test(bd$NU_NOTA_CN,"pnorm",mean(bd$NU_NOTA_CN),sd(bd$NU_NOTA_CN))

ks.test(bd$NU_NOTA_MT,"pnorm",mean(bd$NU_NOTA_MT),sd(bd$NU_NOTA_MT))

#modelo de regressão linear
modelo <- lm(bd$NU_NOTA_MT ~ bd$TP_ESCOLA + bd$TP_COR_RACA, data=bd)

#resumo do modelo
summary(modelo)

#apresentacao estatitica de dados

tapply(bd$media,bd$TP_COR_RACA,summary)

tapply(bd$media,bd$SG_UF_PROVA,summary)

tapply(bd$NU_NOTA_CH,bd$TP_ESCOLA,summary)

tapply(bd$NU_NOTA_REDACAO,bd$TP_ESCOLA,summary)

tapply(bd$NU_NOTA_MT,bd$TP_ESCOLA,summary)

tapply(bd$NU_NOTA_CN,bd$TP_ESCOLA,summary)

tapply(bd$NU_NOTA_LC,bd$TP_ESCOLA,summary)

summary(bd$NU_NOTA_REDACAO)
