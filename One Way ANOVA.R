library(car)

# Parámetros
n <- 30 # Número de observaciones por tratamiento
media <- c(10, 20, 30) # Medias para los tres tratamientos
desviacion <- c(5, 5, 50) # Desviaciones estándar para los tres tratamientos

# Simulación de datos
grupo1 <- rnorm(n, mean=media[1], sd=desviacion[1])
grupo2 <- rnorm(n, mean=media[2], sd=desviacion[2])
grupo3 <- rnorm(n, mean=media[3], sd=desviacion[3])

# Combinación de datos en un marco de datos
datos <- data.frame(
  Valor = c(grupo1, grupo2, grupo3),
  Tratamiento = factor(rep(1:3, each=n))
)


boxplot(Valor ~ Tratamiento, data = datos,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))



resultado_anova <- aov(Valor ~ Tratamiento, data=datos)
summary(resultado_anova)

TukeyHSD(resultado_anova)

# Homogeneidad de varianza
plot(resultado_anova,1)

# Normalidad
plot(resultado_anova,2)

leveneTest(Valor ~ Tratamiento, data=datos)


