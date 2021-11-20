new_df <- as.data.frame(dados_enem)

notas.tb <- table(new_df$NOTA_ENEN)

View(notas.tb)

#----Boxplot
boxplot(new_df$NOTA_ENEN, col = "light blue", main="Boxplot - Notas", ylab="Notas")

#Frequência relativa
relfreq <- notas.tb/sum(notas.tb)
View(relfreq)

#Frequência absoluta
absfreq <- table(new_df$NOTA_ENEN)
View(absfreq)

#Frequência relativa acumulada
relfreq_a <- cumsum(relfreq)

#Frequência absoluta acumulada
absfreq_a <- cumsum(absfreq)

#----Tabela das frequências
frequencies = cbind(notas.tb, absfreq, absfreq_a,
                       relfreq=round(relfreq*100,digits = 2),
                       relfreq_a=round(relfreq_a*100,digits = 2))
colnames(frequencies) <- c("Notas", "Frequência absoluta", "Frequência absoluta acumulada", "Frequência relativa", "Frequência relativa acumulada")
View(frequencies)

#----Histograma
hist(notas.tb, main="Histograma - Notas", ylab="Frequência", xlab = "Notas")


