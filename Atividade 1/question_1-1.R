df <- as.data.frame(dados_enem)

notas.tb <- (df$NOTA_ENEN)

#cálculo dos quartiles
quantile(df$NOTA_ENEN, na.rm=T)
#divisão das classes
classes <- c("300.02-450.06", "450.06-493.96", "493.96-550.28", "550.28-796.14")
notas.cut = table(cut(df$NOTA_ENEN, breaks=quantile(df$NOTA_ENEN, na.rm=T), include.lowest = TRUE, labels=classes))

#----Boxplot
boxplot(df$NOTA_ENEN, col = "light blue", main="Boxplot - Notas ENEM", ylab="Notas",
        ylim=c(0,1000))

#Frequência relativa
#relfreq <- notas.tb/sum(notas.cut)
relfreq <- prop.table(notas.cut)

#Frequência absoluta
absfreq <- table(notas.tb)

#Frequência relativa acumulada
relfreq_a <- cumsum(relfreq)

#Frequência absoluta acumulada
absfreq_a <- cumsum(notas.cut)

#----Tabela das frequências
frequencies = cbind(notas.cut, absfreq_a, 
                    relfreq = round(relfreq*1, digits=2),
                    relfreq_a=round(relfreq_a*1,digits = 2))
colnames(frequencies) <- c("Frequência absoluta", "Frequência absoluta acumulada", "Frequência relativa", "Frequência relativa acumulada")

View(frequencies)
