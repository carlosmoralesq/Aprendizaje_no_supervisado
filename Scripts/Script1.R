# Preparamos el espacio de trabajo
# Cargamos los datos --------------------------------------------------------------------------

library(data.table
)

data <- fread(
  # Ubicación del archivo
  input = "data/base_indices_2005-2021.csv",
  colClasses = c("numeric","numeric")
)

lapply(data, class)

summary(data)

# Transformación / Manipulación ---------------------------------------------------------------

#data[j = `:=`(
#  Puntaje_de_corte = as.numeric(`Puntaje de corte`),
#  Valor_arancel = numeric(`Valor de arancel`)

#)][]


# Guardamos los datos procesados ------------------------------------------------

saveRDS(data, file = "data/data.RDS")


# Graficamos de forma exploratoria los datos ------------------------------

library(ggplot2)
ggplot(aes(x = `Puntaje de corte`, y = `Valor de arancel`), data = data) + geom_point( alpha = 0.5) + ggtitle('Conjunto de Datos')



# Utilizar el método del codo para determinar la cantidad de centr --------

set.seed(1234)
wcss <- vector()
for(i in 1:20){
  wcss[i] <- sum(kmeans(data, i)$withinss)
}


#Graficar
ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
  ggtitle("Método del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')


#Una vez seleccionada la cantidad de centroides, se aplica el algoritmo con esa cantidad----


set.seed(1234)
kmeans <- kmeans(data, 6, iter.max = 1000, nstart = 10)


#Ahora graficamos los clusters -----

data$cluster <- kmeans$cluster
ggplot() + geom_point(aes(x = `Puntaje de corte`, y = `Valor de arancel`, color = cluster), data = data, size = 1) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_point(aes(x = kmeans$centers[, 1], y = kmeans$centers[, 2]), color = 'black', size = 3) + 
  ggtitle('Clusters de Datos con k = 6 / K-Medios') + 
  xlab('Puntaje de corte') + ylab('Valor de arancel')
