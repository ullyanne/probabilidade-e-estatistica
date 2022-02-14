notas.quartil <- cut(df$NOTA_ENEN, breaks=quantile(df$NOTA_ENEN, na.rm=T))
notas.is_treineiro <- table(dados_enem$IN_TREINEIRO, notas.quartil)

is_treineiro.tab <- with(dados_enem, table(dados_enem$IN_TREINEIRO, notas.quartil))
notas.is_treineiro <- (prop.table(is_treineiro.tab, margin = 1)*100)


barplot(notas.is_treineiro, 
        col=c("mediumslateblue", "mediumspringgreen"),
        main="Relação de notas de alunos treineiros e não treineiros",
        xlab="Quantiles",
        ylim=c(0,35),
        ylab="Porcentagem de alunos",
        legend.text=T,
        args.legend = list(x = "top"),
        beside = T
)


