#instalar as bibliotecas que vamos utilizar:
install.packages(c("lmtest", "sandwich", "broom", "stargazer", "readr", "dplyr","data.table", "tidyr", "corrplot"))

library(readr)
library(dplyr)
library(lmtest)
library(corrplot)
library(data.table)
library(tidyr)
library(sandwich)
library(broom)
library(stargazer)
library(corrplot)

#importar a base de dados previamente
df <- read.csv("C:\\Users\\juanc\\OneDrive\\Documentos\\Base_econometria_1_funcional.csv")

# Converter a coluna de UFs para fator
df$UF <- as.factor(df$UF)
# Converter os níveis do fator em números
df$UF_num <- as.numeric(df$UF)

# Selecionar apenas as colunas numéricas do dataframe
df_numeric <- select_if(df, is.numeric)
#criar variavel de PIB percapta para nossa base:
df_numeric$PIB_per_capt <- df_numeric$PIB_pm/df_numeric$pop_res
#criar var de despesa por função saude e saneamento em milhoes de reais (dividir pro 1000)
df_numeric$desp_func_ss <- df_numeric$desp_func_ss/1000
df_numeric <- df_numeric %>%
  select(UF_num, tm_0a5, desp_func_ss, tf, razao_pop_coleta_de_lixo, pop_dom_aguaencanada, p_dom_esg,urb_pct, PIB_per_capt)
df_numeric_estat <- df_numeric %>%
  select(tm_0a5, desp_func_ss, tf, razao_pop_coleta_de_lixo, pop_dom_aguaencanada, p_dom_esg, urb_pct, PIB_per_capt)

#calculando as estatísticas descritivas
# Calcular estatísticas descritivas usando a função summary de dplyr
stargazer(df_numeric_estat, type = "text", title = 'Estat Desc', digits = 2, out = "estatisticas_descritivas.txt")

#calcular a correlação entre as variáveis
cor_matrix <- cor(df_numeric_estat)
# Plota a matriz de correlação triangular
corrplot(cor_matrix, method = "square", type = "lower",tl.cex=0.8, tl.col="black", number.cex=0.8, addCoef.col="black", order = "hclust")
#modelos de teste 
model_sem_controle <- lm(tm_0a5 ~ desp_func_ss + razao_pop_coleta_de_lixo + pop_dom_aguaencanada + p_dom_esg, data = df_numeric)
summary(model_sem_controle)

#modelo com controles
model_fit <- lm(log(tm_0a5+1) ~ log(PIB_per_capt+1) + desp_func_ss + p_dom_esg + pop_dom_aguaencanada + razao_pop_coleta_de_lixo + urb_pct + tf + as.factor(UF_num), data = df_numeric)
summary(model_fit)

#verificar homocedasticidade atraves do teste de Breusch-Pagan
bptest(model_sem_controle)
bptest(model_fit)

# Calcular a matriz de covariância robusta (White)
vcov_white <- vcovHC(model_sem_controle, type = "HC1")
vcov_white_3 <- vcovHC(model_fit, type = "HC1")

# Teste de hipóteses com ajuste de White
coeftest(model_sem_controle, vcov = vcov_white)
coeftest(model_fit, vcov = vcov_white_3)







