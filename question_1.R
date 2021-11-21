new_df <- as.data.frame(dados_enem)

notas <- table(new_df$NOTA_ENEN)

View(notas)

#plot(notas, col =  "pink", type = "h", lwd = 2, cex.lab=1.2, main = "Frequência Absoluta", xlab= "Nota", ylab= "Frquência")

#plot(notas, type = "S",col = "orange",main = "Frequência relativa",lwd = 2 )

#prop.table(new_df$NOTA_ENEN)


 
#BOXPLOT (Q1)
boxplot(new_df$NOTA_ENEN, col = "orange", main="Boxplot - Notas", ylab="Notas")

#Histograma
hist(notas, main="HIstograma - Notas", ylab="Frequência", xlab = "Notas")



