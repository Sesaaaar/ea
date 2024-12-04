#1) Leer datos----
  library(readxl)
  datos <- read_excel("EA/Ejercicio1.xlsx")
  View(datos)


#2) Convertir a factor----
  datos$Agencia = factor(datos$Agencia)
  plot(datos$Agencia, datos$Valor) #(x,y)diagrama de cajas y bigotes
  
  # Por medio del gráfico de cajas podemos observar el comportamiento de los datos y valorizaciones para
  #cada una de las agencias , donde a simple vista las agencias (o niveles) C y D se desmarcan de las demás,
  #lo que preeliminarmente da un indicio sobre la diferencia de las medias existente entre estas agencias.

#3) ANOVA----
  model <- lm(Valor ~ Agencia, data=datos)
  summary(model)
  ANOVA <- aov(model)
  summary(ANOVA)
  #Deseamos probar que la media de cada Agencia es igual para todas, o bien, si el efecto del
  #tipo de agencia es nulo sobre la verdadera media poblacional,
  #por lo tanto, a través del criterio del valor  $p = 2.21, al ser este menor al nivel de
  #significancia dado alpha=5%, se rechaza la hipótesis nula planteada, es decir, por lo menos una
  #agencia difiere de las demás en cuanto a su media.
  
#4) Análisis de diferencias entre tratatmientos mediante Tukey()----
  Tukey <- TukeyHSD(ANOVA,conf.level = .95)
  print(Tukey)
  plot(Tukey,las=1,col="brown")
  #Mediante el reporte, se puede observar que los tratamientos A y B no muestran diferencias significativas entre sus medias, esto se puede determinar:
  #Primero, observando el intervalo de confianza para esta diferencia, el cual
  #pasa efectivamente por el 0, abriendo la posibilidad de que   estas mecias sean iguales.
  #Segundo, bajo la hipótesis de que ambas medias sean iguales , mediante el criterio del valor p de esta           diferencia (0,6) no se rechaza la hipótesis nula, por lo que no       existe evidencia para señalar que ambas medias (A y B) difieren       entre sí.

#5) Análisis de diferencias entre trt mediante LSD----
  #install.packages("agricolae")
  library(agricolae)
  grupos <- LSD.test(y = ANOVA, trt = "Agencia"
                     , group = T, console = T)
  #A través del método LSD, se llegan a los mismos resultados, ya que los niveles con la misma letra no muestran diferencia significativa en sus medias.
  
#6) Gráfico para validación de supuestos----
  
  #Normalidad
  par(mfrow=c(2, 2))
  plot(model, las = 1, col = 'deepskyblue', which = 1:3)
  hist(model$residuals, main = "Histrograma de los residuos", xlab="Residuos",ylab="Frecuencia") #Histograma
  #Graficamente en el histograma y QQ-Plot se aprecia que aparentemente los residuos siguen la distribución normal, puesto que son pocos los datos atípicos y la curva pareciera ajustarse a una campana.
  
  # Homocedasticidad
  plot(model$fitted.values, model$residuals, pch = 19, col = 'orange', xlab = "Respuesta Media", ylab = "ei's", main = "Residuos v/s Respuesta Media")
  #Graficamente pareciera ser que no se cumple el supuesto de homocedasticidad ( varianzas iguales), ya que cada factor muestra un comportamiento distinto. Existe heterocedasticidad

  # Independencia
  plot(residuals(model), pch=19, col="tomato", xlab = "Observacion", ylab = "Residuo", type ="l"); abline(h=0,lty=2,col="blue") #Residuos v/s Observaciones
  #Gráficamente pareciera ser que se cumple con el supuesto de independencia, ya que no se muestra ningún tipo de patrón.


