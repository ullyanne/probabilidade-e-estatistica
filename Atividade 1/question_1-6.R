boxplot(dados_enem$NU_NOTA_REDACAO ~ dados_enem$TP_SEXO, 
        data = dados_enem,
        ylim=c(0,1100),
        ylab="Notas da reda��o",
        xlab="Sexo",
        main="Notas da reda��o x Sexo",
        col=c("palegoldenrod", "mediumorchid4")
        ) 
