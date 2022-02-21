#Questão 3
library(readxl)
library(dplyr)
install.packages("dgof")
require(dgof)
dados_enem <- read_excel("Github/probabilidade-e-estatistica/Atividade 2/dados_enem.xlsx")
municipios <- unique(dados_enem$NO_MUNICIPIO_PROVA)

dados_RL    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[1])
dados_MCZ   <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[2])
dados_PC    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[3])
dados_SJT   <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[4])
dados_SMC   <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[5])
dados_TV    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[6])
dados_SI    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[7])
dados_CA    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[8])
dados_PI    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[9])
dados_ARA   <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[10])
dados_TRA   <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[11])
dados_AB    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[12])
dados_IGA   <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[13])
dados_BTT   <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[14])
dados_VIS   <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[15])
dados_OLHO  <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[16])
dados_SLQ   <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[17])
dados_UP    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[18])
dados_ATA   <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[19])
dados_DEL   <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[20])
dados_PE    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[21])
dados_PIL   <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[22])
dados_BM    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[23])
dados_MD    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[24])
dados_GP    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[25])
dados_CO    <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[26])


dados_sertao  <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[4] |
                                       NO_MUNICIPIO_PROVA == municipios[7] |
                                       NO_MUNICIPIO_PROVA == municipios[12] |
                                       NO_MUNICIPIO_PROVA == municipios[14] |
                                       NO_MUNICIPIO_PROVA == municipios[16] |
                                       NO_MUNICIPIO_PROVA == municipios[20])


dados_agreste <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[9] |
                                       NO_MUNICIPIO_PROVA == municipios[10] |
                                       NO_MUNICIPIO_PROVA == municipios[11] |
                                       NO_MUNICIPIO_PROVA == municipios[13] |
                                       NO_MUNICIPIO_PROVA == municipios[25])


dados_leste  <- dados_enem %>% filter(NO_MUNICIPIO_PROVA == municipios[1] |
                                      NO_MUNICIPIO_PROVA == municipios[2] |
                                      NO_MUNICIPIO_PROVA == municipios[3] |
                                      NO_MUNICIPIO_PROVA == municipios[5] |
                                      NO_MUNICIPIO_PROVA == municipios[6] |
                                      NO_MUNICIPIO_PROVA == municipios[8] |
                                      NO_MUNICIPIO_PROVA == municipios[15] |
                                      NO_MUNICIPIO_PROVA == municipios[17] |
                                      NO_MUNICIPIO_PROVA == municipios[18] |
                                      NO_MUNICIPIO_PROVA == municipios[19] |
                                      NO_MUNICIPIO_PROVA == municipios[21] |
                                      NO_MUNICIPIO_PROVA == municipios[22] |
                                      NO_MUNICIPIO_PROVA == municipios[23] |
                                      NO_MUNICIPIO_PROVA == municipios[24] |
                                      NO_MUNICIPIO_PROVA == municipios[26])




dados_leste.notas.media <- mean(dados_leste$NOTA_ENEN)
dados_agreste.notas.media <- mean(dados_agreste$NOTA_ENEN)
dados_sertao.notas.media <- mean(dados_sertao$NOTA_ENEN)
dados_enem.notas.media <- mean(dados_enem$NOTA_ENEN)



#-----------------------------------------#
dados_enem <- dados_enem %>% mutate(NO_REGIAO_PROVA = case_when(NO_MUNICIPIO_PROVA == municipios[1] |
                                                                  NO_MUNICIPIO_PROVA == municipios[2] |
                                                                  NO_MUNICIPIO_PROVA == municipios[3] |
                                                                  NO_MUNICIPIO_PROVA == municipios[5] |
                                                                  NO_MUNICIPIO_PROVA == municipios[6] |
                                                                  NO_MUNICIPIO_PROVA == municipios[8] |
                                                                  NO_MUNICIPIO_PROVA == municipios[15] |
                                                                  NO_MUNICIPIO_PROVA == municipios[17] |
                                                                  NO_MUNICIPIO_PROVA == municipios[18] |
                                                                  NO_MUNICIPIO_PROVA == municipios[19] |
                                                                  NO_MUNICIPIO_PROVA == municipios[21] |
                                                                  NO_MUNICIPIO_PROVA == municipios[22] |
                                                                  NO_MUNICIPIO_PROVA == municipios[23] |
                                                                  NO_MUNICIPIO_PROVA == municipios[24] |
                                                                  NO_MUNICIPIO_PROVA == municipios[26] ~ "Leste",
                                                                NO_MUNICIPIO_PROVA == municipios[9] |
                                                                  NO_MUNICIPIO_PROVA == municipios[10] |
                                                                  NO_MUNICIPIO_PROVA == municipios[11] |
                                                                  NO_MUNICIPIO_PROVA == municipios[13] |
                                                                  NO_MUNICIPIO_PROVA == municipios[25] ~ "Agreste",
                                                                NO_MUNICIPIO_PROVA == municipios[4] |
                                                                  NO_MUNICIPIO_PROVA == municipios[7] |
                                                                  NO_MUNICIPIO_PROVA == municipios[12] |
                                                                  NO_MUNICIPIO_PROVA == municipios[14] |
                                                                  NO_MUNICIPIO_PROVA == municipios[16] |
                                                                  NO_MUNICIPIO_PROVA == municipios[20] ~ "Sertão"))


aov_regiao <- aov(dados_enem$NOTA_ENEN ~ factor(dados_enem$NO_REGIAO_PROVA))
anova(aov_regiao)
plot(TukeyHSD(aov_regiao))








#teste de normalidade Kolmogorov-Smirnov

ks.test(dados_enem$NOTA_ENEN,"pnorm",mean(dados_enem$NOTA_ENEN),sd(dados_enem$NOTA_ENEN))


municipios.media <- c(mean(dados_RL$NOTA_ENEN),
                      mean(dados_MCZ$NOTA_ENEN),
                      mean(dados_PC$NOTA_ENEN),
                      mean(dados_SJT$NOTA_ENEN),
                      mean(dados_SMC$NOTA_ENEN),
                      mean(dados_TV$NOTA_ENEN),
                      mean(dados_SI$NOTA_ENEN),
                      mean(dados_CA$NOTA_ENEN),
                      mean(dados_PI$NOTA_ENEN),
                      mean(dados_ARA$NOTA_ENEN),
                      mean(dados_TRA$NOTA_ENEN),
                      mean(dados_AB$NOTA_ENEN),
                      mean(dados_IGA$NOTA_ENEN),
                      mean(dados_BTT$NOTA_ENEN),
                      mean(dados_VIS$NOTA_ENEN),
                      mean(dados_OLHO$NOTA_ENEN),
                      mean(dados_SLQ$NOTA_ENEN),
                      mean(dados_UP$NOTA_ENEN),
                      mean(dados_ATA$NOTA_ENEN),
                      mean(dados_DEL$NOTA_ENEN),
                      mean(dados_PE$NOTA_ENEN),
                      mean(dados_PIL$NOTA_ENEN),
                      mean(dados_BM$NOTA_ENEN),
                      mean(dados_MD$NOTA_ENEN),
                      mean(dados_GP$NOTA_ENEN),
                      mean(dados_CO$NOTA_ENEN)
                      )



dados_regiao <- data.frame(Municipio = municipios, Média = municipios.media)


dados_regiao <- dados_regiao %>% mutate(Região = case_when(Municipio == municipios[1] |
                                                                  Municipio == municipios[2] |
                                                                  Municipio == municipios[3] |
                                                                  Municipio == municipios[5] |
                                                                  Municipio == municipios[6] |
                                                                  Municipio == municipios[8] |
                                                                  Municipio == municipios[15] |
                                                                  Municipio == municipios[17] |
                                                                  Municipio == municipios[18] |
                                                                  Municipio == municipios[19] |
                                                                  Municipio == municipios[21] |
                                                                  Municipio == municipios[22] |
                                                                  Municipio == municipios[23] |
                                                                  Municipio == municipios[24] |
                                                                  Municipio == municipios[26] ~ "Leste",
                                                                Municipio == municipios[9] |
                                                                  Municipio == municipios[10] |
                                                                  Municipio == municipios[11] |
                                                                  Municipio == municipios[13] |
                                                                  Municipio == municipios[25] ~ "Agreste",
                                                                Municipio == municipios[4] |
                                                                  Municipio == municipios[7] |
                                                                  Municipio == municipios[12] |
                                                                  Municipio == municipios[14] |
                                                                  Municipio == municipios[16] |
                                                                  Municipio == municipios[20] ~ "Sertão"))



#Teste de normalidade
shapiro.test(dados_regiao$Média)
dados_regiao$Média = sqrt(dados_regiao$Média)

aov_regiao <- aov(dados_regiao$Média ~ factor(dados_regiao$Região))
anova(aov_regiao)


bartlett.test(dados_regiao$Média, dados_regiao$Região)


boxplot(dados_regiao$Média ~ dados_regiao$Região, 
              data = dados_regiao,
                ylim=c(470,530),
                #ylab="Notas da redação",
                #xlab="Sexo",
                main="Média do enem x Mesorregião")


dados_regiao.regiao <- data.frame(Região = c("Sertão","Agreste", "Leste"),
                                  Média = c(dados_sertao.notas.media, dados_agreste.notas.media, dados_leste.notas.media))





barplot(dados_regiao.regiao$Média, ylab = "Média", xlab = "Mesorregião", main = "Média por Mesorregião",
        names.arg = dados_regiao.regiao$Região, col = c("darkgoldenrod1", "chartreuse2", "cadetblue2"))











#--------------------------
#Questão 4
dados_regiao$Média_red <- c(mean(dados_RL$NU_NOTA_REDACAO),
                           mean(dados_MCZ$NU_NOTA_REDACAO),
                           mean(dados_PC$NU_NOTA_REDACAO),
                           mean(dados_SJT$NU_NOTA_REDACAO),
                           mean(dados_SMC$NU_NOTA_REDACAO),
                           mean(dados_TV$NU_NOTA_REDACAO),
                           mean(dados_SI$NU_NOTA_REDACAO),
                           mean(dados_CA$NU_NOTA_REDACAO),
                           mean(dados_PI$NU_NOTA_REDACAO),
                           mean(dados_ARA$NU_NOTA_REDACAO),
                           mean(dados_TRA$NU_NOTA_REDACAO),
                           mean(dados_AB$NU_NOTA_REDACAO),
                           mean(dados_IGA$NU_NOTA_REDACAO),
                           mean(dados_BTT$NU_NOTA_REDACAO),
                           mean(dados_VIS$NU_NOTA_REDACAO),
                           mean(dados_OLHO$NU_NOTA_REDACAO),
                           mean(dados_SLQ$NU_NOTA_REDACAO),
                           mean(dados_UP$NU_NOTA_REDACAO),
                           mean(dados_ATA$NU_NOTA_REDACAO),
                           mean(dados_DEL$NU_NOTA_REDACAO),
                           mean(dados_PE$NU_NOTA_REDACAO),
                           mean(dados_PIL$NU_NOTA_REDACAO),
                           mean(dados_BM$NU_NOTA_REDACAO),
                           mean(dados_MD$NU_NOTA_REDACAO),
                           mean(dados_GP$NU_NOTA_REDACAO),
                           mean(dados_CO$NU_NOTA_REDACAO)
)

dados_regiao$Média_mt <- c(mean(dados_RL$NU_NOTA_MT),
                            mean(dados_MCZ$NU_NOTA_MT),
                            mean(dados_PC$NU_NOTA_MT),
                            mean(dados_SJT$NU_NOTA_MT),
                            mean(dados_SMC$NU_NOTA_MT),
                            mean(dados_TV$NU_NOTA_MT),
                            mean(dados_SI$NU_NOTA_MT),
                            mean(dados_CA$NU_NOTA_MT),
                            mean(dados_PI$NU_NOTA_MT),
                            mean(dados_ARA$NU_NOTA_MT),
                            mean(dados_TRA$NU_NOTA_MT),
                            mean(dados_AB$NU_NOTA_MT),
                            mean(dados_IGA$NU_NOTA_MT),
                            mean(dados_BTT$NU_NOTA_MT),
                            mean(dados_VIS$NU_NOTA_MT),
                            mean(dados_OLHO$NU_NOTA_MT),
                            mean(dados_SLQ$NU_NOTA_MT),
                            mean(dados_UP$NU_NOTA_MT),
                            mean(dados_ATA$NU_NOTA_MT),
                            mean(dados_DEL$NU_NOTA_MT),
                            mean(dados_PE$NU_NOTA_MT),
                            mean(dados_PIL$NU_NOTA_MT),
                            mean(dados_BM$NU_NOTA_MT),
                            mean(dados_MD$NU_NOTA_MT),
                            mean(dados_GP$NU_NOTA_MT),
                            mean(dados_CO$NU_NOTA_MT)
)

dados_regiao$Média_lc <- c(mean(dados_RL$NU_NOTA_LC),
                           mean(dados_MCZ$NU_NOTA_LC),
                           mean(dados_PC$NU_NOTA_LC),
                           mean(dados_SJT$NU_NOTA_LC),
                           mean(dados_SMC$NU_NOTA_LC),
                           mean(dados_TV$NU_NOTA_LC),
                           mean(dados_SI$NU_NOTA_LC),
                           mean(dados_CA$NU_NOTA_LC),
                           mean(dados_PI$NU_NOTA_LC),
                           mean(dados_ARA$NU_NOTA_LC),
                           mean(dados_TRA$NU_NOTA_LC),
                           mean(dados_AB$NU_NOTA_LC),
                           mean(dados_IGA$NU_NOTA_LC),
                           mean(dados_BTT$NU_NOTA_LC),
                           mean(dados_VIS$NU_NOTA_LC),
                           mean(dados_OLHO$NU_NOTA_LC),
                           mean(dados_SLQ$NU_NOTA_LC),
                           mean(dados_UP$NU_NOTA_LC),
                           mean(dados_ATA$NU_NOTA_LC),
                           mean(dados_DEL$NU_NOTA_LC),
                           mean(dados_PE$NU_NOTA_LC),
                           mean(dados_PIL$NU_NOTA_LC),
                           mean(dados_BM$NU_NOTA_LC),
                           mean(dados_MD$NU_NOTA_LC),
                           mean(dados_GP$NU_NOTA_LC),
                           mean(dados_CO$NU_NOTA_LC)
)

dados_regiao$Média_cn <- c(mean(dados_RL$NU_NOTA_CN),
                           mean(dados_MCZ$NU_NOTA_CN),
                           mean(dados_PC$NU_NOTA_CN),
                           mean(dados_SJT$NU_NOTA_CN),
                           mean(dados_SMC$NU_NOTA_CN),
                           mean(dados_TV$NU_NOTA_CN),
                           mean(dados_SI$NU_NOTA_CN),
                           mean(dados_CA$NU_NOTA_CN),
                           mean(dados_PI$NU_NOTA_CN),
                           mean(dados_ARA$NU_NOTA_CN),
                           mean(dados_TRA$NU_NOTA_CN),
                           mean(dados_AB$NU_NOTA_CN),
                           mean(dados_IGA$NU_NOTA_CN),
                           mean(dados_BTT$NU_NOTA_CN),
                           mean(dados_VIS$NU_NOTA_CN),
                           mean(dados_OLHO$NU_NOTA_CN),
                           mean(dados_SLQ$NU_NOTA_CN),
                           mean(dados_UP$NU_NOTA_CN),
                           mean(dados_ATA$NU_NOTA_CN),
                           mean(dados_DEL$NU_NOTA_CN),
                           mean(dados_PE$NU_NOTA_CN),
                           mean(dados_PIL$NU_NOTA_CN),
                           mean(dados_BM$NU_NOTA_CN),
                           mean(dados_MD$NU_NOTA_CN),
                           mean(dados_GP$NU_NOTA_CN),
                           mean(dados_CO$NU_NOTA_CN)
)

dados_regiao$Média_ch <- c(mean(dados_RL$NU_NOTA_CH),
                           mean(dados_MCZ$NU_NOTA_CH),
                           mean(dados_PC$NU_NOTA_CH),
                           mean(dados_SJT$NU_NOTA_CH),
                           mean(dados_SMC$NU_NOTA_CH),
                           mean(dados_TV$NU_NOTA_CH),
                           mean(dados_SI$NU_NOTA_CH),
                           mean(dados_CA$NU_NOTA_CH),
                           mean(dados_PI$NU_NOTA_CH),
                           mean(dados_ARA$NU_NOTA_CH),
                           mean(dados_TRA$NU_NOTA_CH),
                           mean(dados_AB$NU_NOTA_CH),
                           mean(dados_IGA$NU_NOTA_CH),
                           mean(dados_BTT$NU_NOTA_CH),
                           mean(dados_VIS$NU_NOTA_CH),
                           mean(dados_OLHO$NU_NOTA_CH),
                           mean(dados_SLQ$NU_NOTA_CH),
                           mean(dados_UP$NU_NOTA_CH),
                           mean(dados_ATA$NU_NOTA_CH),
                           mean(dados_DEL$NU_NOTA_CH),
                           mean(dados_PE$NU_NOTA_CH),
                           mean(dados_PIL$NU_NOTA_CH),
                           mean(dados_BM$NU_NOTA_CH),
                           mean(dados_MD$NU_NOTA_CH),
                           mean(dados_GP$NU_NOTA_CH),
                           mean(dados_CO$NU_NOTA_CH)
)

cor(dados_enem.cornotas)


shapiro.test(dados_regiao$Média_red)
shapiro.test(dados_regiao$Média_mt)
shapiro.test(dados_regiao$Média_lc)
shapiro.test(dados_regiao$Média_cn)
shapiro.test(dados_regiao$Média_ch)









