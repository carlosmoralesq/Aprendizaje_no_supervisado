# Preparamos el espacio de trabajo----
#install.packages(data.table)
library(data.table)
#install.packages("gtsummary")
library(gtsummary)
# install.packages("knitr")
library(knitr)
# install.packages("kableExtra")
library(kableExtra)
# Cargamos los datos --------------------------------------------------------------------------



data <- fread(
  # Ubicación del archivo
  input = "data/base_indices_2005-2021.csv",
  colClasses = c("numeric","numeric")
)[`Valor de arancel`>1000,]

lapply(data, class)

summary(data)
tbl_summary(
  data,
            # Expresando por fila, debajo de cada variable
            type = all_continuous() ~ "continuous2",
            # Los siguientew estadísticos
            statistic = all_continuous() ~ c(
              "{mean} ± {sd}", "{median} ({p25}, {p75})", "[{min}, {max}]"
            ))

# Guardamos los datos procesados ------------------------------------------------

saveRDS(data, file = "data/data.RDS")


# Graficamos de forma exploratoria los datos ------------------------------

library(ggplot2)
ggplot(aes(x = `Puntaje de corte`, y = `Valor de arancel`), data = data) + geom_point( alpha = 0.5) + ggtitle('Conjunto de Datos')


# Agrupamiento K-means ----------------------------------------------------



# Utilizar el método del codo para determinar la cantidad de centroides

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


#Una vez seleccionada la cantidad de centroides, se aplica el algoritmo con esa cantidad


set.seed(1234)
kmeans <- kmeans(data, 4, iter.max = 1000, nstart = 10)


#Ahora graficamos los clusters

data$cluster_KM <- kmeans$cluster
ggplot() + geom_point(aes(x = `Puntaje de corte`, y = `Valor de arancel`, color = as.factor(cluster_KM)), data = data, size = 1) +
  geom_point(aes(x = kmeans$centers[, 1], y = kmeans$centers[, 2]), color = 'black', size = 3) +
  labs(x='Puntaje de corte', y='Valor de arancel', col='cluster') +
  ggtitle('Clusters de Datos con k = 4 / K-Medios') +
  theme_classic()

knitr::kable(data) %>%
  # Le asignamos un tema (propósitos estéticos)
  kableExtra::kable_styling(bootstrap_options = "condensed")


# Agrupamiento Jerárquico -------------------------------------------------

#Se debe instalar y cargar la librería para graficar dendrogramas
install.packages('ggdendro')
library(ggdendro)

#Se grafica el dendrograma
dendrogram <- hclust(dist(data, method = 'euclidean'), method = 'ward.D')
ggdendrogram(dendrogram, rotate = FALSE, labels = FALSE, theme_dendro = TRUE) + 
  labs(title = "Dendrograma")

#se realiza un corte del dendrograma para limitar los centroides utilizando método euclideano
agrupamientoJ <- hclust(dist(data, method = 'euclidean'), method = 'ward.D')
clases_aj <- cutree(agrupamientoJ, k = 4)
data$clusterAJ <- clases_aj

#Se grafica nuevamente para revisar agrupamiento
ggplot() + geom_point(aes(x = `Puntaje de corte`, y = `Valor de arancel`, color = as.factor(clusterAJ)), data = data, size = 1) +
  ggtitle('Clusters de Datos con k = 4 / Agrupamiento Jerárquico') + 
  labs(x='Puntaje de corte', y='Valor de arancel', col='cluster') +
  theme_classic()

#Para visualizar la información de ambos modelos, revisan los resultados del agrupamiento para cada modelo en distintas columnas agregadas a la tabla original----


knitr::kable(data) %>%
  # Le asignamos un tema (propósitos estéticos)
  kableExtra::kable_styling(bootstrap_options = "condensed")

