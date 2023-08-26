##diferencias de Medias de utilidad manufactura ENTRE 2017 Y 2018
xbarramuestra1=mean(MFinal$GANANCIA..PERDIDA..2017[MFinal$MACROSECTOR == "MANUFACTURA"], na.rm = TRUE)
xbarramuestra1
xbarramuestra2=mean(MFinal$GANANCIA..PERDIDA..2018[MFinal2$MACROSECTOR == "MANUFACTURA"], na.rm = TRUE)
xbarramuestra2
varianzaPoblacional2=var(database$GANANCIA..PERDIDA..2018[database$MACROSECTOR == "MANUFACTURA"],na.rm = TRUE)
varianzaPoblacional1=var(database$GANANCIA..PERDIDA..2017[database$MACROSECTOR == "MANUFACTURA"], na.rm = TRUE)
n1=100
n2=100

#95
vc = qnorm(0.05/2, lower.tail = F)
Li= (xbarramuestra1-xbarramuestra2)-(vc*(sqrt((varianzaPoblacional1/n1)+(varianzaPoblacional2/n2))))
Ls= (xbarramuestra1-xbarramuestra2)+(vc*(sqrt((varianzaPoblacional1/n1)+(varianzaPoblacional2/n2))))


datos2017 = MFinal$GANANCIA..PERDIDA..2017[MFinal$MACROSECTOR == "MANUFACTURA"]
datos2018 = MFinal$GANANCIA..PERDIDA..2018[MFinal$MACROSECTOR == "MANUFACTURA"]
media2017 = mean(datos2017, na.rm = TRUE)
media2018 = mean(datos2018, na.rm = TRUE)
mediana2017 = median(datos2017, na.rm = TRUE)
mediana2018 =median(datos2018, na.rm = TRUE)
error_estandar= sqrt((varianzaPoblacional1/n1) + (varianzaPoblacional2/n2))
intervalo = vc * error_estandar

# Gráfico de intervalo de confianza
bar_height = c(xbarramuestra1 - xbarramuestra2)
bar_labels = sprintf("Diferencia de Medias: %.2f", bar_height)

bar_data = data.frame(Grupo = "Diferencia de Medias", Altura = bar_height, Error = intervalo)

library(ggplot2)

ggplot(bar_data, aes(x = Grupo, y = Altura, fill = Grupo)) +
  geom_bar(stat = "identity", color = "black", position = "dodge") +
  geom_errorbar(aes(ymin = Altura - Error, ymax = Altura + Error), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Intervalo de Confianza para Diferencia de Medias (2017 vs 2018)",
       y = "Diferencia de Medias",
       fill = "MANUFACTURA") +
  theme_minimal()
