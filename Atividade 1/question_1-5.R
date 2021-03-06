tab <- with(dados_enem, table(dados_enem$TP_COR_RACA, dados_enem$TP_LINGUA))
ra�a.idioma <- t(prop.table(tab, margin = 1)*100)

colnames(ra�a.idioma)[colnames(ra�a.idioma) == "Indigina"] = "Ind�gena"
colnames(ra�a.idioma)[colnames(ra�a.idioma) == "Nao declardo"] = "N�o declarado"

barplot(ra�a.idioma,
        col=c("skyblue1", "brown1"),
        main="Idioma escolhido x Ra�a",
        xlab="Ra�as",
        ylab="Porcentagem",
        legend=T,
        space=0.5,
        beside = F
)
