# -*- coding: utf-8 -*-

# -- Sheet --

# # Tarea 4 (Tema 6): Contrastes de hipótesis


# Vamos a trabajar con la tabla de datos
# airquality
# ya utilizada en la Tarea 1 para realizar varios contrastes de hipótesis. Cargar en primer lugar dichos datos y
# pasar las temperaturas a Kelvin.


#Pasamos de grados Farenheit a Kelvin:
airqualityKelvin <- data.frame(airquality[-(4)])   #Quita la cuarta columna (Temp)
TempKelvin <- (airquality[4]-32)*(5/9)+273.15           #Se pasa a Kelvin
airqualityKelvin <- data.frame(airqualityKelvin[1:3],Temp=TempKelvin,airqualityKelvin[4:5]) #data frame con Temp en Kelvin                         

# ### 1) 
# ¿Cómo podríamos obtener, con una única instrucción en R, una tabla que nos diese latemperatura media en cada uno de los 5 meses incluídos en los datos? Escribir dicha instrucción(sin usar ";").


mediames <- tapply(airqualityKelvin$Temp,airqualityKelvin$Month,mean,na.rm=TRUE,simplify=TRUE) #na.rm ignoraría los NA
#aplica media sobre el conjunto de datos que lleven asociado el mismo valor en la categoría Month
cat('Las medias mensuales son:\n')
mediames

# ### 2)
# Hacer y subir al campus virtual (en formato pdf) un diagrama de tipo box-and-whisker endonde se comparen las distribuciones de temperatura en función del mes.


vec5 <- airqualityKelvin$Temp[airquality$Month==5] #Vectores con las temperaturas para cada mes
vec6 <- airqualityKelvin$Temp[airquality$Month==6]
vec7 <- airqualityKelvin$Temp[airquality$Month==7]
vec8 <- airqualityKelvin$Temp[airquality$Month==8]
vec9 <- airqualityKelvin$Temp[airquality$Month==9]
#Hacemos el gráfico tipo box-and-whisker
boxplot(vec5,vec6,vec7,vec8,vec9,col=c('#CD0BBC','#2297E6','green3','yellow','#DF536B'),names=c('5','6','7','8','9'),horizontal=TRUE,ylab='Mes',xlab='Temperatura (K)')

# ### 3)
# Seleccionar las temperaturas para los días de los tres meses más cálidos
# (junio, julio yagosto)
# y guardar los datos en una nueva variable llamada
# T.verano
# . Suponiendo que las medidas de
# T.verano
# siguen una distribución normal:


sortmediames<- sort(mediames,decreasing=TRUE) # Ordena los meses según su temperatura media de manera decreciente
sortmediames

nummes <- as.numeric(names(sortmediames))     # Crea un vector con los números de mes de los meses ordenados

T.verano <- as.numeric()
for (i in nummes[1:3]){                       # Sólo se toman los 3 meses más cálidos
    T.verano <- c(T.verano,airqualityKelvin$Temp[airqualityKelvin$Month==i])
}                                             #Vector con las temperaturas de esos 3 meses

# ### 3A) 
# ¿Es el valor medio de T.verano significativamente diferente de 300.5 K? Usar un nivel designificación de 0.05.


cat('Hacemos un contraste bilateral con H_0: mu=300.5=mu0 Y h_1: mu distinto de mu0=300.5.\n')
mu <- 300.5
xbar <- mean(T.verano)
cat('La media de los datos es:',xbar,'. En principio, no parece significativamente diferente.','\n')
cat('Como no conocemos la varianza poblacional, necesitamos calcular la desviación típica, pero usaremos el estadístico z, 
al tener una muestra mucho mayor de 30.','\n')
z <- (xbar-mu)/(sd(T.verano/sqrt(length(T.verano))))   #Se calcula el estadístico z
alpha <- 0.05
z.alpha <- qnorm(1-alpha/2)                              #Valor crítico de z
cat('El valor crítico de z es',z.alpha,', mientras que nuestro estadístio vale',z,'por lo que se acepta la hipótesis nula 
y suponemos que el valor medio de T.verano no es significativamente diferente de ',mu,'.\n')

# ### 3B)
# ¿Para qué nivel de significación mínimo (valor de
# p) es la media de
# T.verano
# significativamente diferente del valor de 300.5 K?


p <- 2*pnorm(z, lower.tail=FALSE)
cat('El nivel de significación con el que se rechazaría la hipótesis nula es',p,'.\n')

# # 3C)
# ¿Es el valor medio de
# T.verano
# significativamente mayor que 300.5 K? Usar un nivel de significación de 0.05.


cat('La hipótesis nula sería en este caso H_0 menor o igual que 300.5K y el contraste sería unilateral.','\n')
mu <- 300.5
xbar <- mean(T.verano)
cat('De nuevo, usamos el estadístico z, al ser la muestra mayor a 30.','\n')
z <- (xbar-mu)/(sd(T.verano)/sqrt(length(T.verano))) #Se calcula el estadístico z
alpha <- 0.05
z.alpha <- qnorm(1-alpha)   #Valor crítico de z
cat('Los valores de z y de z crítico serían',z,'y',z.alpha,', respectivamente. Como el valor de z está en la región
crítica, se niega la hipótesis nula, con lo que se considera que el valor medio poblacional en verano es significativamente
mayor que 300.5 K.','\n')

# ### 3D)
# ¿Para qué nivel de significación mínimo (valor de
# p) es significativamente mayor que 300.5K?


p <- pnorm(z,lower.tail=FALSE) #Nivel de significación
cat('El nivel de significación mínimo debe ser inferior a',p,', ya que con este valor se aceptaría la hipótesis nula.
Nótese que a menor nivel de significación, más amplia será la región de aceptación.','\n')

# ### 4)
# Siguiendo con la temperatura durante los meses de verano (T.verano), y suponiendo normalidad:


# ### 4A)
# ¿Puede ser la desviación típica de la población igual a un valor de 3 K? Usar un nivel designificación de 0.05.


cat('La desviación típica muestral es:',sd(T.verano),'K','\n')
cat('Haciendo un contraste bilateral con H_0: varpob(varianza poblacional)=3^2 K: \n')

varpob <- (length(T.verano)-1)*var(T.verano)/3^2                    #Estadístico chi^2
alpha <- 0.05
inter1 <- qchisq(alpha/2,df=length(T.verano)-1)                     #Valor crítico de chi^2 por la izquierda
inter2 <- qchisq(alpha/2,df=length(T.verano)-1,lower.tail=FALSE)    #Valor crítico de chi^2 por la derecha

cat('Para aceptar la hipótesis nula se debe cumplir que ( inter1 < varpob < inter2 ), donde inter1 e inter2 son los valores críticos
del estadístico.\n') 
cat('Los valores resultantes para estas variables son:', '\n')
cat('varpob=',varpob,'\n')
cat('inter1=',inter1,'\n')
cat('inter2=',inter2,'\n')
cat('El valor del estadístico chi^2 es superior al valor máximo de aceptación, por lo que se rechaza la hipótesis nula y
suponemos que la desviación típica de la población debe ser distinta a 3K.\n')

# ### 4B)
# ¿Para qué nivel de significación mínimo (valor de
# p) es la desviación típica significativamente diferente de un valor de 3 K?


p <- 2*pchisq(varpob,length(T.verano)-1,lower.tail=FALSE)
cat('El nivel de significación mínimo para considerar la desviación típica significativamente diferente de 3K es',p,'.\n')

# De otro modo:


install.packages("EnvStats")
library("EnvStats")
varTest(T.verano, conf.level=0.95, alternative = "two.sided",sigma.squared=3^2)

# ### 5A)
# ¿Dirías que la temperatura media del mes de julio es significativamente mayor que la del mes de junio? En caso afirmativo, ¿para qué nivel de significación?


MediaJulio <- mediames[names='7']  #Recuperamos las medias para esos dos meses calculadas anteriormente
MediaJunio <- mediames[names='6']
cat('La diferencia entre las medias es de',MediaJulio-MediaJunio,'K. \n')

var7 <- var(vec7)                  #Varianzas de los vectores de temperatura para cada mes
var6 <- var(vec6)

cat('Haremos un contraste unilateral de hipótesis con H0: mu7 menor o igual que mu6, donde mu7 y mu6 son las medias de 
Julio y Junio, respectivamente y con un nivel de significación de 0.05. \n\n')

z <- (MediaJulio-MediaJunio)/sqrt(var7/length(vec7)+var6/length(vec6))   #Estadístico z (para N=N1+N2>30)

alpha <- 0.05                                                            #Nivel de significación
z.alpha <- qnorm(1-alpha)                                                #Valor crítico de aceptación

cat('Los valores de z y de z crítico son',z,'y',z.alpha,'. Así que se rechaza la hipótesis nula y se puede suponer que la
temperatura de Julio es significativamente mayor a la de Junio para el nivel 0.05 de significación. \n')

#Cálculo del nivel de significación mínimo para considerar la temperatura de Julio significativamente mayor:
p <- pnorm(z,lower.tail=FALSE)
cat('Se puede considerar la temperatura de Julio significativamente mayor que la de Junio para un nivel de significación mínimo de',p,'.\n')

# ### 5B)
# ¿Dirías que la temperatura media del mes de agosto es significativamente diferente de ladel mes de julio? En caso afirmativo, ¿para qué nivel de significación?


MediaAgosto <- mediames[names='8']  #Recuperamos las medias para esos dos meses calculadas anteriormente
MediaJulio <- mediames[names='7']
cat('La diferencia entre las medias es de',MediaAgosto-MediaJulio,'K. \n')

var8 <- var(vec7)                  #Varianzas de los vectores de temperatura para cada mes
var7 <- var(vec6)

cat('Haremos un contraste bilateral de hipótesis con H0: mu8=mu7, donde mu8 y mu7 son las medias de Agosto y Julio, respectivamente
y con un nivel de significación de 0.05. \n\n')

z <- (MediaAgosto-MediaJulio)/sqrt(var8/length(vec8)+var7/length(vec7))  #Estadístico z (para N=N1+N2>30)

alpha <- 0.05                                                            #Nivel de significación
z.alphamed <- qnorm(1-alpha/2)                                           #Valor crítico de aceptación

cat('Los valores de z y de z crítico son',z,'y',z.alphamed,'. Así que se acecpta la hipótesis nula y se puede suponer que la
temperatura de Agosto no es significativamente distinta a la de Julio para el nivel 0.05 de significación. \n')

#Cálculo del nivel de significación mínimo para considerar la temperatura de Agosto significativamente diferente a la de Julio:
p <- 2*pnorm(z,lower.tail=FALSE)
cat('Se puede considerar las temperaturas de Agosto y Julio significativamente diferentes para un nivel de significación mínimo de\n')
cat('p=',p,'.\n')

# ### 6)
# Realizar un test de bondad del ajuste para comprobar si los datos de temperatura en verano(
# T.verano
# ) pueden seguir una distribución normal. Según los resultados de este apartado,¿crees que podrían tener sentido los contrastes de hipótesis realizados en los apartados 3-5?


limites <- seq(from=290, to=310, by=2)
histog <- hist(T.verano,breaks=limites,freq=FALSE,ylim=c(0,0.15),main='Histograma T.verano y curva normal',
               xlab='Temperatura (K)',ylab='frecuencia relativa',xlim=c(285,315))
histog
curve(dnorm(x, mean=mean(T.verano), sd=sd(T.verano)), add=TRUE, type="l", col="red", lwd=3)

# calculamos las frecuencias esperadas
# número total de intervalos
ninterv <- length(limites)-1
# inicializamos vector de frecuencias relativas (probabilidades)
p <- numeric(ninterv)
# Rellenamos el vector para cada intervalo
for (i in 1:ninterv) {
    # último intervalo
    if (i == ninterv) {
        p[i] = pnorm(limites[i], mean=mean(T.verano), sd=sd(T.verano), lower.tail=FALSE) # cola derecha
    } else {
        # primer intervalo
        if (i == 1) {
            p[i] = pnorm(limites[i+1], mean=mean(T.verano), sd=sd(T.verano))
            # ni primer ni último intervalo
        } else {
            p[i] = pnorm(limites[i+1], mean=mean(T.verano), sd=sd(T.verano))-pnorm(limites[i], mean=mean(T.verano), sd=sd(T.verano))
        }
    }
}

#Informacion del histograma - tabla
inter <- histog$breaks[2]-histog$breaks[1]  #longitud del intervalo
tfreq <- data.frame(1:length(histog$mids),histog$breaks[1:10],histog$breaks[2:11],histog$mids,histog$counts)
names(tfreq) <- c("intervalo","a_i","a_i+1","c_i","n_i")
#las frecuencias absolutas en cada intervalo según nuestra distribución normal serán e_i=N*p_i=92*p_i
pabs <- length(T.verano)*p
#Agrupamos intervalos para que en cada uno haya al menos 5 elementos.
tfreq <- cbind(tfreq,pabs) #Le añadimos al data frame una columna con las frecuencias anteriores
names(tfreq)[6] <- "e_i"   #El nombre salía erroneamente
tfreq2 <- data.frame(tfreq[c(-1,-2,-3,-9,-10),],row.names=NULL)
tfreq2$intervalo <- c(2,3,4,5,6)
c1 <- c(1,'inf',296,(296+290)/2,sum(tfreq$n_i[1:3]),sum(tfreq$e_i[1:3]))
c7 <- c(7,306,'inf',(310+306)/2,sum(tfreq$n_i[9:10]),sum(tfreq$e_i[9:10]))
tfreq3 <- rbind(c1,tfreq2,c7)
names(tfreq3)[3] <- "a_i+1"
cat('La tabla con las frecuencias por intervalo, una vez aunados los de frecuencias menores a 5 es: \n')
tfreq3

#Necesitamos unir los tres primeros intervalos y los dos últimos:
p5<-c(p[1]+p[2]+p[3],p[4:8],p[9]+p[10])
#Número observado de medidas por intervalo:
o <- histog$counts
onew <- c(o[1]+o[2]+o[3],o[4:8],o[9]+o[10])
#Número esperado de medidas por intervalo:
enew <- p5*length(T.verano)

#El número de intervalos será entonces k=7. Y al haber deducido los parámetros poblacionales a partir de los muestrales, el 
#número final de grados de libertad será k-p-1=7-2=4

#La hipótesis nula consiste en suponer que la distribución de las temperaturas en verano siguen una distribución normal
#El estadístico de prueba será:
chi2 <- sum((onew-enew)^2/enew)
#Con alpha=0.05 y 4 grados de libertad, el valor crítico del estadístico es:
chi2.alpha <- qchisq(0.05,df=4,lower.tail=FALSE)

cat('Como el valor del estadístico es',chi2,'y el valor crítico de aceptación de la hipótesis',chi2.alpha,', para un nivel de 
confianza de 0.05, se acepta la hipótesis de que los datos de temperatura en verano siguen una distribución normal. Entonces 
los contrastes de hipótesis realizados en los apartados del 3 al 5 tendrían sentido.\n')

# ### 7)
# Volver a usar la temperatura considerando todos los meses para los que hay datos. Hacer un
# análisis de varianza para determinar si hay relación entre la temperatura media y el mes ¿Qué
# valores se obtienen para: A) el cuadrado medio de los tratamientos; B) el cuadrado medio del
# azar; C) el nivel de significación para el que se puede rechazar la hipotesis nula de igualdad de
# medias?


cat('La hipótesis nula es que los efectos producidos por los tratamientos no son significativamente diferentes. \n')
nt <- nrow(airqualityKelvin)
fmeses <- factor(airqualityKelvin$Month) #factor de los meses
p <- length(levels(fmeses))
C <- (sum(airqualityKelvin$Temp))^2/nt
VT <- sum(airqualityKelvin$Temp^2)-C     #Variación total
T <- tapply(airqualityKelvin$Temp, fmeses, sum)
n <- tapply(airqualityKelvin$Temp, fmeses, length)
VET <- sum(T^2/n)-C                      #Variación entre tratamientos
VDT <- VT - VET                          #Variación dentro de los tratamientos
MT <- VET/(p-1)                          #Cuadrado medio de los tratamientos
ME <- VDT/(nt-p)                         #Cuadrado medio del azar
cat('El cuadrado medio de los tratamientos es',MT,' y el cuadrado medio del azar',ME,', que es menor, por lo que
esperaríamos que la hipótesis no se cumpliera y la variación entre tratamientos no se debiera al azar. En ese caso
existiría una relación entre temperatura media y mes.\n')


#Hallamos el nivel de significación para el que se puede rechazar la hipótesis nula mediante la función aov
mianova <- aov(airqualityKelvin$Temp ~ fmeses)
sumario <- summary(mianova)
sumario
cat('Se aceptaría la hipótesis de igualdad de medias de temperatura entre los meses para un nivel de significación de 2e-16.\n')


cat('Si usaramos un nivel de significación de 0.05, podríamos hacer el \n')
F <- MT/ME                               #Estadístico de Fisher
alpha <- 0.05                            #nivel de significación
Fcrit <- qf(alpha, df1=p-1, df2=nt-p, lower.tail=FALSE)
cat('El valor del estadístico F y su valor crítico son',F,'y',Fcrit,', por lo que se debe rechazar la hipótesis nula
y considerar que sí existe una relación entre la temperatura media y el mes. \n')

