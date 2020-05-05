library(ggplot2)
library(tidyr)
library(dplyr)

matriz_pob_struc <- matrix(c(
  0, 0.01, 0.09, 0,
  0.92, 0.86, 0, 0,
  0, 0.08, 0.8, 0.83,
  0, 0.02, 0.19, 0
), nrow = 4, byrow = TRUE)
matriz_pob_struc

matriz_inicial <- matrix(c(
  0,
  0,
  300,
  0
), nrow = 4, byrow = TRUE)
matriz_inicial

res_2001 <- matriz_pob_struc %*% matriz_inicial
res_2001
res_2002 <- matriz_pob_struc %*% res_2001
res_2002

matriz_final <- matriz_inicial
for (time_step in 1:50) {
  matriz_final <- cbind(
    matriz_final,
    round(matriz_pob_struc %*% matriz_final[,time_step], 0)
  )
}
matriz_final

datos_finales <- t(matriz_final) %>%
  as.data.frame()
names(datos_finales) <- c('Crias', 'Inmaduros', 'Adultos', 'Madres')
datos_finales <- datos_finales %>%
  mutate(
    Total = Crias + Inmaduros + Adultos + Madres,
    Año = 2000:2050
  ) %>%
  pivot_longer(cols = Crias:Total, names_to = 'Stage')
datos_finales

datos_finales %>%
  ggplot(
    aes(x = Año, y = value, color = Stage)
  ) +
  geom_point()


