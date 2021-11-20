df <- as.data.frame(dados_enem)

notas.tb <- (df$NOTA_ENEN)

hist(notas.tb, col="spring green", ylab="Frequência", xlab="Notas", main="Histograma - Notas", ylim=c(0,8000))
