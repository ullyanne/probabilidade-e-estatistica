idade.cut <- cut(dados_enem$NU_IDADE, breaks = quantile(dados_enem$NU_IDADE),include.lowest = TRUE)

notas_mat.cut <- cut(dados_enem$NU_NOTA_MT, breaks = quantile(dados_enem$NU_NOTA_MT),
                   include.lowest = TRUE)

idade.notas_mat.tb <- table(idade.cut, notas_mat.cut)

idade.notas_mat.tb
