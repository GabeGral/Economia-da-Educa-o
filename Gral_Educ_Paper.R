## Economia da Educação - Paper Final
## Gabriel Gral

# Libraries ---------------------------------------------------------------

library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(plyr)
library(stargazer)
library(xtable)
library(lmtest)
library(sandwich)

# Base de Dados -----------------------------------------------------------
## Nessa seção, importamos, limpamos e manipulamos a base de dados do INEP

ENEM_2019 <- data.table::fread(input='microdados_ENEM_2019.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)

## Selecionar as variÃ¡veis que serão úteis

View(as.data.frame(colnames(ENEM_2019)))

ENEM_2019 <- ENEM_2019[,-c(2:5, 12:14, 20:24, 29:41, 44:78, 80:82, 96:99, 101:104, 106:110)]

## Vamos ver o formato das nossas variáveis

str(ENEM_2019)

## Selecionar aquelas que queremos transformar em factors

View(as.data.frame(colnames(ENEM_2019)))
vec_factors <- c(1:length(colnames(ENEM_2019)))
vec_factors <- vec_factors[-c(1,3,29,30,31,32,35)]

#Criar nova base com as variáveis transformadas

ENEM_2019 <- as.data.frame(ENEM_2019)
ENEM <- ENEM_2019

for (i in vec_factors){
  ENEM[,i] <- as.factor(ENEM_2019[,i])
}

## Checar se todas as variáveis estão no formato desejado

str(ENEM)

## Criar idade ao quadrado 

ENEM$NU_IDADE2 <- (ENEM$NU_IDADE)^2

## Criar dummy de raça (1 indica não branco)

ENEM_B <- ENEM %>% filter(TP_COR_RACA == 1)
ENEM_B$RACE <- c(rep(0, length(ENEM_B$TP_SEXO)))
ENEM_P <- ENEM %>% filter(TP_COR_RACA != 1)
ENEM_P$RACE <- c(rep(1, length(ENEM_P$TP_SEXO)))
ENEM <- rbind(ENEM_B, ENEM_P)

## Criar dummy de sexo (1 indica mulher)

ENEM_F <- ENEM %>% filter(TP_SEXO == "F")
ENEM_F$SEX<- c(rep(1, length(ENEM_F$TP_SEXO)))
ENEM_M <- ENEM %>% filter(TP_SEXO == "M")
ENEM_M$SEX<- c(rep(0, length(ENEM_M$TP_SEXO)))
ENEM <- rbind(ENEM_F, ENEM_M)

## Criar interação entre raça e sexo

ENEM$RACE_SEX <- ENEM$RACE*ENEM$SEX

#Nova variavel com nota media

ENEM$NOTA_MEDIA <- rowMeans(select(ENEM, starts_with("NU_NOTA")), na.rm = FALSE)

## Criar dummy de administração da escola

ENEM_DM <- ENEM %>% filter(TP_DEPENDENCIA_ADM_ESC == 3)
ENEM_DM$MUNICIPAL <- c(rep(1, length(ENEM_DM$TP_SEXO)))
ENEM_DM$ESTADUAL <- c(rep(0, length(ENEM_DM$TP_SEXO)))
ENEM_DM$FEDERAL <- c(rep(0, length(ENEM_DM$TP_SEXO)))
ENEM_DM$PRIVADA <- c(rep(0, length(ENEM_DM$TP_SEXO)))

ENEM_DE <- ENEM %>% filter(TP_DEPENDENCIA_ADM_ESC == 2)
ENEM_DE$MUNICIPAL <- c(rep(0, length(ENEM_DE$TP_SEXO)))
ENEM_DE$ESTADUAL <- c(rep(1, length(ENEM_DE$TP_SEXO)))
ENEM_DE$FEDERAL <- c(rep(0, length(ENEM_DE$TP_SEXO)))
ENEM_DE$PRIVADA <- c(rep(0, length(ENEM_DE$TP_SEXO)))

ENEM_DF <- ENEM %>% filter(TP_DEPENDENCIA_ADM_ESC == 1)
ENEM_DF$MUNICIPAL <- c(rep(0, length(ENEM_DF$TP_SEXO)))
ENEM_DF$ESTADUAL <- c(rep(0, length(ENEM_DF$TP_SEXO)))
ENEM_DF$FEDERAL <- c(rep(1, length(ENEM_DF$TP_SEXO)))
ENEM_DF$PRIVADA <- c(rep(0, length(ENEM_DF$TP_SEXO)))

ENEM_DP <- ENEM %>% filter(TP_DEPENDENCIA_ADM_ESC == 4)
ENEM_DP$MUNICIPAL <- c(rep(0, length(ENEM_DP$TP_SEXO)))
ENEM_DP$ESTADUAL <- c(rep(0, length(ENEM_DP$TP_SEXO)))
ENEM_DP$FEDERAL <- c(rep(0, length(ENEM_DP$TP_SEXO)))
ENEM_DP$PRIVADA <- c(rep(1, length(ENEM_DP$TP_SEXO)))

ENEM <- rbind(ENEM_DM, ENEM_DE, ENEM_DF, ENEM_DP)

## Limpar o Environment 

rm(ENEM_F) + rm(ENEM_M) + rm(ENEM_B) + rm(ENEM_P) + rm(ENEM_2019) 
rm(ENEM_DE) + rm(ENEM_DP) + rm(ENEM_DM) + rm(ENEM_DF)

# Estatísticas Descritivas ------------------------------------------------
## Nessa seção, fazemos os gráficos e tabelas de estatísticas descritivas

#Histogramas de nota por sexo

mu <- ddply(ENEM, "TP_SEXO", summarise, grp.mean=mean(NOTA_MEDIA, na.rm = T))
head(mu)

ggplot(ENEM, aes(x=NOTA_MEDIA))+
  geom_histogram(aes(y = ..density..), color="black", fill="grey", alpha = 0.7, bins = 40)+
  facet_grid(TP_SEXO ~ .) +
  labs(title = "Histograma de Notas Médias", x = "Notas", y = "Densidade") + 
  theme_calc(base_size = 14, base_family = "serif") + 
  geom_vline(data=mu, aes(xintercept=grp.mean, color="red"), linetype="dashed", size = 1.5) +
  theme(legend.position="none")

#Tabela de estatisticas descritivas

setDT(ENEM)
t <- rbind(
  cbind(c("Nota Média", "Nota Média"), ENEM[, as.list(summary(NOTA_MEDIA)), by = TP_SEXO]),
  cbind(c("Nota CH", "CH"), ENEM[, as.list(summary(NU_NOTA_CH)), by = TP_SEXO]),
  cbind(c("Nota CN", "Nota CN"), ENEM[, as.list(summary(NU_NOTA_CN)), by = TP_SEXO]),
  cbind(c("Nota LC", "Nota LC"), ENEM[, as.list(summary(NU_NOTA_LC)), by = TP_SEXO]),
  cbind(c("Nota MT", "Nota MT"), ENEM[, as.list(summary(NU_NOTA_MT)), by = TP_SEXO]),
  cbind(c("Nota Redação", "Nota Redação"), ENEM[, as.list(summary(NU_NOTA_REDACAO)), by = TP_SEXO]))
xtable(t)

# Modelos -----------------------------------------------------------------

## Modelos para Nota Média
  ## Modelo simples, apenas com sexo e raça
  
  model0_MEDIA <- lm(data = ENEM,
                     NOTA_MEDIA ~ SEX + RACE + RACE_SEX)
  
  ## Modelo incluindo idade e administração da escola
  
  model1_MEDIA <- lm(data = ENEM,
                     NOTA_MEDIA ~ SEX + RACE + RACE_SEX + NU_IDADE + NU_IDADE2 + 
                       MUNICIPAL + FEDERAL + PRIVADA )
    
  ## Modelo incluindo geografia e perguntas demográficas, que visam controlar renda e background familiar
  
  model2_MEDIA <- lm(data = ENEM,
                  NOTA_MEDIA ~ SEX + RACE + RACE_SEX + NU_IDADE + NU_IDADE2 + 
                    MUNICIPAL + FEDERAL + PRIVADA + 
                    Q001 + Q002 + Q003 + Q004 + Q005 + Q006 + Q025 + 
                    SG_UF_RESIDENCIA)
  
  ## Calcular Robust Standard Errors
  cov0 <- vcovHC(model0_MEDIA, type = "HC0")
  robust_se0   <- sqrt(diag(cov0))
  cov1 <- vcovHC(model1_MEDIA, type = "HC0")
  robust_se1   <- sqrt(diag(cov1))
  cov2 <- vcovHC(model2_MEDIA, type = "HC0")
  robust_se2   <- sqrt(diag(cov2))

## Modelos para Matemática
    
  ## Modelo simples, apenas com sexo e raça
  ENEM$NU_NOT
  model0_MT <- lm(data = ENEM,
                     NU_NOTA_MT ~ SEX + RACE + RACE_SEX)
  
  ## Modelo incluindo idade e administração da escola
  
  model1_MT <- lm(data = ENEM,
                  NU_NOTA_MT ~ SEX + RACE + RACE_SEX + NU_IDADE + NU_IDADE2 + 
                       MUNICIPAL + FEDERAL + PRIVADA )
  
  ## Modelo incluindo geografia e perguntas demográficas, que visam controlar renda e background familiar
  
  model2_MT <- lm(data = ENEM,
                  NU_NOTA_MT ~ SEX + RACE + RACE_SEX + NU_IDADE + NU_IDADE2 + 
                       MUNICIPAL + FEDERAL + PRIVADA + 
                       Q001 + Q002 + Q003 + Q004 + Q005 + Q006 + Q025 + 
                       SG_UF_RESIDENCIA)
  
  ## Calcular Robust Standard Errors
  cov0_MT <- vcovHC(model0_MT, type = "HC0")
  robust_se0_MT   <- sqrt(diag(cov0_MT))
  cov1_MT <- vcovHC(model1_MT, type = "HC0")
  robust_se1_MT   <- sqrt(diag(cov1_MT))
  cov2_MT <- vcovHC(model2_MT, type = "HC0")
  robust_se2_MT   <- sqrt(diag(cov2_MT))

## Modelo para Redação
  
  model2_RE <-lm(data = ENEM,
               NU_NOTA_REDACAO ~ SEX + RACE + RACE_SEX + NU_IDADE + NU_IDADE2 + 
                 MUNICIPAL + FEDERAL + PRIVADA + 
                 Q001 + Q002 + Q003 + Q004 + Q005 + Q006 + Q025 + 
                 SG_UF_RESIDENCIA)
  
  cov2_RE <- vcovHC(model2_RE, type = "HC0")
  robust_se2_RE   <- sqrt(diag(cov2_RE))
  
## Montar tabela
stargazer(model0_MEDIA, model1_MEDIA, model2_MEDIA, model0_MT, model1_MT, model2_MT, model2_RE, se = list(robust_se0, robust_se1, robust_se2, robust_se0_MT, robust_se1_MT, robust_se2_MT, robust_se2_RE))
