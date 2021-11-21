tab <- with(dados_enem, table(dados_enem$TP_COR_RACA, dados_enem$TP_LINGUA))
raça.idioma <- t(prop.table(tab, margin = 1)*100)

colnames(raça.idioma)[colnames(raça.idioma) == "Indigina"] = "Indígena"
colnames(raça.idioma)[colnames(raça.idioma) == "Nao declardo"] = "Não declarado"

barplot(raça.idioma,
        col=c("skyblue1", "brown1"),
        main="Idioma escolhido x Raça",
        xlab="Raças",
        ylab="Porcentagem",
        legend=T,
        space=0.5,
        beside = F
)
