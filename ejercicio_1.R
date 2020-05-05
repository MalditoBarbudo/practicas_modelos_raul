library(readxl)
library(dplyr)

datos_censo <- read_excel("CensoDelta.xls")
names(datos_censo) <- c('Any', 'Parelles')
datos_censo$Any <- as.numeric(datos_censo$Any)

datos_censo

# ajustar el modelo:
modelo_exponencial <- lm(log(Parelles) ~ Any, data = datos_censo)
# ver los coeficientes
coef(modelo_exponencial)

summary(modelo_exponencial)

# graficarlo:
plot(datos_censo$Any, datos_censo$Parelles)
lines(
  datos_censo$Any, 1.419e-108 * exp(0.129*datos_censo$Any),
  col = 'red'
)


datos_censo_tasa_crecim <- datos_censo %>%
  mutate(
    lambda = Parelles/lag(Parelles),
    r = log(lambda)
  )

datos_censo_tasa_crecim

datos_censo_tasa_crecim %>%
  summarise(
    media = mean(r, na.rm = TRUE),
    desvest = sd(r, na.rm = TRUE)
  )
