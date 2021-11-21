boxplot(dados_enem$NU_NOTA_REDACAO ~ dados_enem$TP_SEXO, 
        data = dados_enem,
        ylim=c(0,1100),
        ylab="Notas da redação",
        xlab="Sexo",
        main="Notas da redação x Sexo",
        col=c("palegoldenrod", "mediumorchid4")
        ) 
