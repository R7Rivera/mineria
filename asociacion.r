

library(readr)

pedidos <- read.csv(file = "C:/Users/rosar/mineria/asociacion.csv", header = TRUE, sep = ";")
View(pedidos)


library(arules)

# Cargamos los datos a un objeto de tipo transaccion 
transacciones <- read.transactions(file = "C:/Users/rosar/mineria/asociacion.csv",
                                   header = TRUE,
                                   format = "single", 
                                   sep = ";",
                                   cols = c("TID", "items"),  
                                   rm.duplicates = TRUE)

# El objeto contiene transacciones en filas e items en columnas
rownames(transacciones)[1:7]
colnames(transacciones)[1:7]
transacciones


inspect(transacciones[1:7])


df_transacciones <- as(transacciones, Class = "data.frame")
# Para que el tamaño de la tabla se ajuste mejor, se convierte el dataframe 
as.tibble(df_transacciones) %>% view()



library(ggplot2)

# Tamaño de todas las transacciones
tamanhos_trans <- data.frame(tamanho = size(transacciones))
head(tamanhos_trans)


ggplot(tamanhos_trans, aes(x = tamanho)) +
  geom_density(fill = "orangered3") +
  labs(x = "Tamaño de las transacciones") +
  theme_bw()


summary(tamanhos_trans)


# Frecuencia de cada item
head(itemFrequency(transacciones))


# Mantenemos las transacciones con al menos dos items
transacciones <- transacciones[tamanhos_trans > 1]
dim(transacciones)



# Umbral de soporte
soporte <- 0.333/dim(transacciones)[1]
soporte


confianza <- 0.6
confianza




itemsets_frecuentes <- apriori(data = transacciones,
                               parameter = list(support = soporte,
                                                target = "frequent itemsets"),
                               control = list(verbose = FALSE))

summary(itemsets_frecuentes)


# Obtencion de reglas de asociacion
reglas <- apriori(data = transacciones, 
                  parameter = list(support = soporte,
                                   confidence = confianza,
                                   target = "rules"),
                  control = list(verbose = FALSE))

print(paste("Reglas generadas:", length(reglas)))

summary(reglas)




# Reglas obtenidas ordenadas por orden descendente de confianza
inspect(sort(reglas, decreasing = TRUE, by = "confidence"))



##Podemos obtener también las reglas maximales, que son aquellas que están generadas por itemsets maximales, con la función is.maximal():

reglas_maximales <- reglas[is.maximal(reglas)]
reglas_maximales



inspect(reglas_maximales)

##El paquete aruleViz nos ofrece distintas posibilidades para visualizar reglas de asociación, aquí se muestran solo algunas:

library(arulesViz)

# Grafico de dispersion coloreado en funcion del lift
plot(reglas, measure = c("support", "confidence"), shading = "lift")


# Grafico de dispersion coloreado en funcion del numero de items
plot(reglas, measure = c("support", "confidence"), shading = "order")


#### Filtrar reglas creadas
filtrado_reglas <- subset(x = reglas,
                          subset = lhs %ain% c("c","m"))
inspect(filtrado_reglas)



### flitar con Restringir las reglas que se crean


reglas_c<- apriori(data = transacciones,
                             parameter = list(support = soporte,
                                              confidence = confianza,
                                              # Se especifica que se creen reglas
                                              target = "rules"),
                             appearance = list(rhs = "c"))

summary(reglas_c)


inspect(reglas_c)
