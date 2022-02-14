library(readxl)
ENEM_AL_EXCEL_AJUS_OKSNZ <- read_excel("ENEM_AL_EXCEL_AJUS_OKSNZ.xlsx")

notas <- ENEM_AL_EXCEL_AJUS_OKSNZ$NOTA_ENEN

notas.media <- mean(notas)
notas.var <- var(notas)
notas.sd <- sd(notas)
n <- length(notas)

#5% de significancia
alfa = 0.05  
zc = qnorm(1- alfa/2, 0, 1)
zc = round(zc,2)

erro = zc * notas.sd
erro = round (erro,5) / sqrt(n)
cat ("[", notas.media - erro, ",", notas.media + erro, "]")

#1% de significancia
alfa = 0.01
zc = qnorm(1- alfa/2, 0, 1)
zc = round(zc,2)
erro = zc * notas.sd
erro = round (erro,5) / sqrt(n)
cat ("[", notas.media - erro, ",", notas.media + erro, "]")

t.test(notas, conf.level= 0.95)
shapiro.test(notas)


colegio <- ENEM_AL_EXCEL_AJUS_OKSNZ %>% filter(CO_ESCOLA == "27354024")

colegio.notas <- colegio$NOTA_ENEN

colegio.notas.MT <- colegio$NU_NOTA_MT
colegio.notas.LC <- colegio$NU_NOTA_LC
colegio.notas.CN <- colegio$NU_NOTA_CN
colegio.notas.CH <- colegio$NU_NOTA_CH
colegio.notas.RD <- colegio$NU_NOTA_REDACAO

shapiro.test(colegio.notas.MT)
shapiro.test(colegio.notas.LC)
shapiro.test(colegio.notas.CN)
shapiro.test(colegio.notas.CH)
shapiro.test(colegio.notas.RD)

colegio <- colegio %>% mutate(NU_NOTA_MT_ZSCORE = (NU_NOTA_MT - colegio.notas.MT.media)/colegio.notas.MT.sd)
colegio <- colegio %>% mutate(NU_NOTA_MT_TRANSFORMED = NU_NOTA_MT_ZSCORE*sd(NU_NOTA_MT) + mean(NU_NOTA_MT))


colegio.notas.MT.media <- mean(colegio.notas.MT)
colegio.notas.MT.sd <- sd(colegio.notas.MT)
colegio.notas.MT.transformed <- colegio$NU_NOTA_MT_TRANSFORMED
shapiro.test(colegio.notas.MT.zscore)


#letra c
colegio.M <- colegio %>% filter(TP_SEXO == "Feminino")
colegio.H <- colegio %>% filter(TP_SEXO == "Masculino")

#nota de linguagens
colegio.M.notas.LC <- colegio.M$NU_NOTA_LC
colegio.M.notas.LC.media <- mean(colegio.M.notas.LC)
colegio.M.notas.LC.media

colegio.H.notas.LC <- colegio.H$NU_NOTA_LC
colegio.H.notas.LC.media <- mean(colegio.H.notas.LC)
colegio.H.notas.LC.media

sd.HM.LC <- sqrt((var(colegio.H.notas.LC)/length(colegio.H))+(var(colegio.M.notas.LC)/length(colegio.M)))
ztest.HM.LC <- (colegio.H.notas.LC.media - colegio.M.notas.LC.media) / sd.HM.LC
ztest.HM.LC




#nota de ciencias humanas
colegio.M.notas.CH <- colegio.M$NU_NOTA_CH
colegio.M.notas.CH.media <- mean(colegio.M.notas.CH)
colegio.M.notas.CH.media

colegio.H.notas.CH <- colegio.H$NU_NOTA_CH
colegio.H.notas.CH.media <- mean(colegio.H.notas.CH)
colegio.H.notas.CH.media

sd.HM.CH <- sqrt((var(colegio.H.notas.CH)/length(colegio.H))+(var(colegio.M.notas.CH)/length(colegio.M)))
ztest.HM.CH <- (colegio.H.notas.CH.media - colegio.M.notas.CH.media) / sd.HM.CH
ztest.HM.CH



#nota de ciencias naturais
colegio.M.notas.CN <- colegio.M$NU_NOTA_CN
colegio.M.notas.CN.media <- mean(colegio.M.notas.CN)
colegio.M.notas.CN.media

colegio.H.notas.CN <- colegio.H$NU_NOTA_CN
colegio.H.notas.CN.media <- mean(colegio.H.notas.CN)
colegio.H.notas.CN.media

sd.HM.CN <- sqrt((var(colegio.H.notas.CN)/length(colegio.H))+(var(colegio.M.notas.CN)/length(colegio.M)))
ztest.HM.CN <- (colegio.H.notas.CN.media - colegio.M.notas.CN.media) / sd.HM.CN
ztest.HM.CN



#nota de redação
colegio.M.notas.RD <- colegio.M$NU_NOTA_REDACAO
colegio.M.notas.RD.media <- mean(colegio.M.notas.RD)
colegio.M.notas.RD.media

colegio.H.notas.RD <- colegio.H$NU_NOTA_REDACAO
colegio.H.notas.RD.media <- mean(colegio.H.notas.RD)
colegio.H.notas.RD.media

sd.HM.RD <- sqrt((var(colegio.H.notas.RD)/length(colegio.H))+(var(colegio.M.notas.RD)/length(colegio.M)))
ztest.HM.RD <- (colegio.H.notas.RD.media - colegio.M.notas.RD.media) / sd.HM.RD
ztest.HM.RD

#letra d

boxplot(colegio$NU_NOTA_LC ~ colegio$TP_SEXO, 
        data = colegio, 
        #ylim = c(0,1000), 
        ylab="Notas de Linguagens",
        xlab="Sexo",
        col=c("palegoldenrod", "mediumorchid4")
        )

boxplot(colegio$NU_NOTA_CH ~ colegio$TP_SEXO, 
        data = colegio, 
        #ylim = c(0,1000), 
        ylab="Notas de Ciências Humanas",
        xlab="Sexo",
        col=c("palegoldenrod", "mediumorchid4")
)

boxplot(colegio$NU_NOTA_CN ~ colegio$TP_SEXO, 
        data = colegio, 
        #ylim = c(0,1000), 
        ylab="Notas de Ciências Naturais",
        xlab="Sexo",
        col=c("palegoldenrod", "mediumorchid4")
)

boxplot(colegio$NU_NOTA_REDACAO ~ colegio$TP_SEXO, 
        data = colegio, 
        #ylim = c(0,1000), 
        ylab="Notas de Redação",
        xlab="Sexo",
        col=c("palegoldenrod", "mediumorchid4")
)
