notas.quartil <- cut(df$NOTA_ENEN, breaks=quantile(df$NOTA_ENEN, na.rm=T))
notas.sexo <- table(dados_enem$TP_SEXO, notas.quartil)

barplot(notas.sexo, 
        col=c("thistle1", "pale turquoise1"),
        main="Notas x Sexo",
        xlab="Quantiles",
        ylim=c(0,15000),
        ylab="Frequência notas",
        legend=T,
        beside = T
        )


