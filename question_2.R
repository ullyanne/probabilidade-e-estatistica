library(magrittr)
library(dplyr)

all_data <- dados_enem
murici_data <- all_data %>% filter(NO_MUNICIPIO_RESIDENCIA == "Murici")  

etnia_preta <- murici_data %>% filter(TP_COR_RACA == "Preta")
etnia_branca <- murici_data %>% filter(TP_COR_RACA == "Branca")
etnia_parda <- murici_data %>% filter(TP_COR_RACA == "Pardo")
etnia_amarela <- murici_data %>% filter(TP_COR_RACA == "Amarelo")
etnia_indigena <- murici_data %>% filter(TP_COR_RACA == "Indigina")
etnia_nao_declarada <- murici_data %>% filter(TP_COR_RACA == "Nao declardo")



media_preta <- mean(etnia_preta$NOTA_ENEN)
media_branca <- mean(etnia_branca$NOTA_ENEN)
media_parda <- mean(etnia_parda$NOTA_ENEN)
media_amarela <- mean(etnia_amarela$NOTA_ENEN)
media_indigena <- mean(etnia_indigena$NOTA_ENEN)
media_nao_declarado <- mean(etnia_nao_declarada$NOTA_ENEN)



etnias <- c('Preta','Branca','Pardo', 'Amarelo', 'Indígena', "N/D")
medias <- c(media_preta, media_branca, media_parda,media_amarela, media_indigena, media_nao_declarado)

chart_data <- data.frame(etnias,medias)
 
barplot(height=chart_data$medias,
        names=chart_data$etnias,
        main = "Média das notas por Etnia - Murici",
        col=c("antiquewhite4", "azure2", "lightgreen", "lightsalmon","mediumpurple1", "lightpink"),
        ylim=c(0,600),
        width=c(0.1,0.1,0.1,0.1, 0.1 ), las=1 )
 