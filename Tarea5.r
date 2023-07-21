# -*- coding: utf-8 -*-

# -- Sheet --

tabla <- read.table('datosXY.txt')
set.seed(123)
indices <- sample(100,20,replace=FALSE)
xn <- x[]indices]
yn <- ...

sum(...)

plot(xn,yn,)
lm(yn ~ xn)
recta:

...

ajuste2 < lm(xn ~ yn)

...

plot(xn,yn,pch=16)

abline(a=ar, b=br,...)
 error de dos formas:

...

summary(ajuste)
error de a y de b y pvaule de ambas, para el contraste de hipoteiss
Error estandar de los residuos...
summary$coeff[1,2] es el error de a y [2,2] el de b? Creo que es eso(posiciones en el summary)
si pvaule es menor que alfa, se rechaza la hipoteiss nula (x=0) (Es el que salen en la fila de xn)

Correlacion lineal: cor()
Sale que hay correlacion porque se rechaza la hip nula de que x=0

cor.test(xn,yn,method="spearman")$p.value

tabla <- read.table('datosXY.txt')
head(tabla)
set.seed(123)
indices <- sample(length(tabla[,1]),20,replace=FALSE)

xn <- tabla$xl[indices]
yn <- tabla$yl[indices]

cat(sum(xn*yn),'Correcto .\n')

#pch=tipo de punto
plot(xn, yn, pch=21, bg="grey", cex=1.5, cex.lab=1.4, xlab="X", ylab="Y",main="Diagrama de dispersión")
text(x=3, y=4.5 , expression(ylx), cex=1.5, pos=4, col='red')
ajuste1 <- lm(yn ~ xn)                                                              #Ajuste por minimos cuadrados
cat('El valor de la pendiente del ajuste es',ajuste1$coefficients[2],'.\n')

abline(a=ajuste1$coefficients[1] , b=ajuste1$coefficients[2] , lw=3 , col="red")     #Dibujamos la regresión

#Ajuste de X sobre Y (minimiza las distancias horizontales):
a2 <- -ajuste2$coefficients[1]/ajuste2$coefficients[2] #Se transforma los coeficientes:
b2 <- 1/ajuste2$coefficients[2] ;
cat('La pendiente de la recta de ajuste de x sobre y es',b2,'.\n')
abline(a=a2 , b=b2 , lw=3 , col="blue")     #Dibujamos la regresión
text(x=3, y=1.5 , expression(xly), cex=1.5, pos=4, col='blue')

# ## Apartado D)
# Para el primer ajuste, obtener la desviación típica de la distribución muestral del coeficiente de regresión de la recta (pendiente de la recta).


#podemos calcular la desviación típica residual como:
sr <- sqrt(sum(residuals(ajuste1)^2)/(length(xn)-2))
cat('La desviación típica de la distribución muestral del coeficiente de regresión de la recta 1 es',sr,'.\n')

# ## Apartados E) y F)
# ¿Podemos concluir que la pendiente es significativamente distinta de cero?
# 
# ¿Con qué nivel de significación?


sumario1 <- summary(ajuste1)
sumario1
cat('Como el nivel de significación es muy pequeño,',sumario1$coefficients[8],', se rechaza la hipótesis nula y se considera que la
pendiente es significativamente distinta de 0.\n')

# ## G)
# Calcular el coeficiente de correlación lineal.


r <- cov(xn,yn)/(sd(xn)*sd(yn))
r
cat('O también:')
as.numeric(sqrt(ajuste1$coefficients[2]*ajuste2$coefficients[2]))
cat('ó\n')
cor(xn,yn)
cat('Sale que hay correlacion porque se rechaza la hip nula de que x=0. Si x = 0, pendiente 0, no hay relacion x-y \n')

# ## H)
# Indicar su nivel de significación


cor.test(xn,yn)$p.value

# ## I) y J) 
# Calcular el coeficiente de correlación de Spearman.
# Indicar su nivel de significación.


cat('Coeficiente')
cor.test(xn,yn,method="spearman")$estimate
cat('con nivel de significación \n')
cor.test(xn,yn,method="spearman")$p.value

# # Parte 2


# Como en la Tarea 3, utilizando como semilla (seed) el número
# 123
# , generar un vector que contenga unasecuencia de 100 números que sigan una distribución normal con media 5 y desviación típica 2.
# Comparar lasecuencia de 100 números generada (a la que llamaremos
# muestra1
# ) con los datos disponibles en el ficheromuestra2.txt (a los que llamaremos
# muestra2
# ) respondiendo a las siguentes preguntas
# **SIN HACER LASUPOSICIÓN de que ambas variables están distribuidas normalmente.**
# 
# **Nota: para comprobar que las muestras son las correctas, tener en cuenta que el productorio de sustérminos debe ser respectivamente 7.875e+67 y 2.809e+12.**


set.seed(123)
muestra1 <- rnorm(100,mean=5,sd=2)
muestra2 <- read.table('muestra2.txt')

cat('Comprobación')
prod(muestra1)
prod(muestra2)

# ## 1)
# Comparar gráficamente la distribución de ambos conjuntos de datos mediante una única instrucción en R.


boxplot(muestra1,muestra2$V1,col=c('cyan', 'red'))
text(x=1.5, y=13 , expression(ylx), cex=1.5, pos=4, col='green')

# ## 2A) 
# Mediante una única instrucción en R, llevar a cabo un contraste sobre la igualdad de medianas de laspoblaciones de partida.


# Test no paramétrico: Test U de Wilcoxon-Mann-Whitney para comparar "promedios" de dos muestras, pero usando la
# mediana como medida de centralización.
test <- wilcox.test(muestra1,muestra2$V1, alternative = "two.sided") # "less", "greater"
test
cat('Se acepta la hipótesis de igualdad. No tenemos pruebas de que las medianas sean significativamente distintas.\n')

# ## 2B)
# Indicar con qué nivel de significación podemos rechazar la hipótesis nula de igualdad de medianas.


cat('Se puede rechazar la hipótesis para un nivel de significación mayor que',test$p.value,'\n')

# ## 2C)
# ¿Es la diferencia significativa? (Sí/No)


cat('No \n')

# ## 3)
# Llevar a cabo un contraste sobre la igualdad de varianzas de las poblaciones de partida. Indicar el valor delnivel de significación a partir del cual podemos rechazar la hipótesis nula de igualdad de varianzas.
# Usar el valor de la semilla indicado
# .


moses.test <- function(x, y, k){
    # Tomar muestras aleatorias de las dos poblaciones
    dni <- 123
    set.seed(dni) ; x0 <- sample(x)
    set.seed(dni) ; y0 <- sample(y)
    m <- trunc(length(x0)/k) #Calcular el número de subgrupos en cada muestra
    n <- trunc(length(y0)/k)
    # Se crean vectores que contengan las desviaciones cuadráticas medias para los diferentes subgrupos, de forma separada
    # para cada muestra
    ci <- numeric(m) ; di <- numeric(n)
    
    for (i in 1:m){                       # Bucle para la primera muestra
        i1 <- (i-1)*k+1
        i2 <- i1+k-1
        xmed <- sum(x0[i1:i2])/k
        ci[i] <- sum((x0[i1:i2]-xmed)^2)
    }
    for (i in 1:n){                       # Bule para la segunda muestra
        i1 <- (i-1)*k+1
        i2 <- i1+k-1
        xmed <- sum(y0[i1:i2])/k
        di[i] <- sum((y0[i1:i2]-xmed)^2)
    }
    cat("Test de rangos de Moses:","\n")
    wilcox.test(ci, di)                   # Aplica el test de Wilcoxon-Mann-Whitney para los valores de ci y di
}

# En la funcion anterior el profe metió el set seed para que se ejecute antes de cada sample y asegurarse de que siempre
# se generan muestras de la misma forma  y por tanto de que el p-value del test se mantiene inalterado sea cual sea el orden
# en que se pasen las dos muestras.


#Muestra 1
test.moses <- moses.test(muestra1, muestra2$V1, 5)
test.moses
cat('Se rechaza la hipótesis, en general. Se rechaza a partir de un nivel',test.moses$p.value,'\n')
cat('La varianzas son significativamente desiguales \n\n')
cat('El resultado es el mismo que metiendo las muestras en orden inverso.\n')

# ## 4A)
# Mediante una única instrucción, llevar a cabo un test de Kolmogorov-Smirnov para comparar las distribuciones.


ks.test(muestra1, muestra2$V1)
cat('Se acepta la hipótesis nula: No hay diferencias significativas en las distribuciones \n')

# ## 4B)
# Indicar el valor del nivel de significación a partir del cual podemos rechazar la hipótesis nula de igualdad de distribuciones


0.06967

# ## 4C)
# Indicar y representar el valor de la diferencia máxima absoluta entre funciones relativas acumuladas.


0.31

tab1 <- table(muestra1)/length(muestra1)
tab2 <- table(muestra2)/length(muestra2$V1)

suma1 <- cumsum(tab1)
suma2 <- cumsum(tab2)

plot(suma1,type='s')
plot(suma2,add=TRUE,type='s')

plot(ecdf(muestra1), do.points=FALSE, lwd=1.5, verticals=TRUE, col="red",
xlab="X", ylab="e.d.f.", main="", xlim=c(0,20))
plot(ecdf(muestra2$V1), do.points=FALSE, lwd=1.5, verticals=TRUE, col="blue",
add=TRUE)
k <- seq(min(c(muestra1,muestra2$V1))-0.01, max(c(muestra1,muestra2$V1))+0.01,by=1)
x1 <- numeric(length(k)) ; x2 <- numeric(length(k))
for (i in 1:length(k)) {
    x1[i] <- length(muestra1[muestra1 <= k[i]])/length(muestra1)
    x2[i] <- length(muestra2$V1[muestra2$V1 <= k[i]])/length(muestra2$V1)
}

ks <- abs(x1-x2)
ksmax <- max(ks) ;                               # valor del estadístico D
cat('El valor del estadístico D es',ksmax,'\n')

kmax <- k[which(ks == ksmax)] ;             # valor de la variable para la que ocurre el máximo
cat('La variable donde se da el máximo es',kmax,'\n')
abline(v=kmax,lty=3,col='darkgreen')

estad <- 4*ksmax^2*length(muestra1)*length(muestra2$V1)/(length(muestra1)+length(muestra2$V1))
estad                                            # aproximación a una chi2

cat('Nivel de significación \n') 
2*pchisq(estad, df=2, lower.tail=FALSE)          # nivel de significación

help(abline)

