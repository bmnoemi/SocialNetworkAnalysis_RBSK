

# ==== 1. INSTALACION PAQUETES ====

# ---- 1.1 igraph ----

install.packages("igraph")
library(igraph)
library(igraph, warn.conflicts = FALSE)


# ---- 2. DIRECTORIO DE TRABAJO ----

list.files()  #enlistar los archivos presentes en el directorio
list.files(pattern = "*.csv")



# ==== 3. IMPORTACION DE DATOS ====

#rm(list = ls())  #funciones rm() y ls()


#### Com_tipo_RBSK - Remocion de FUND y PH2-PH5) ####

df.tipo.RBSK <- read.csv("Com_tipo_RBSK.csv", header = TRUE, row.names = 1) #importar matriz de adyacencia
df.tipo.RBSK
View(df.tipo.RBSK)


#### Com_tipo_PA ####

df.tipo.PA <- read.csv("Com_tipo_PA.csv", header = TRUE, row.names = 1) #importar matriz de adyacencia
df.tipo.PA

#### Com_tipo_CZ ####

df.tipo.CZ <- read.csv("Com_tipo_CZ.csv", header = TRUE, row.names = 1) #importar matriz de adyacencia
df.tipo.CZ


# ==== 3. CREACION Y ANALISIS DE REDES ====

# ---- 3.1 Com_tipo_RBSK -----

class(df.tipo.RBSK)
m.tipo.RBSK <- as.matrix(df.tipo.RBSK) #para convertir el daframe en matriz
class(m.tipo.RBSK)
View(m.tipo.RBSK)

r.tipo.RBSK <- graph_from_adjacency_matrix(adjmatrix = m.tipo.RBSK, mode ="directed", diag = FALSE, add.rownames = "name", weighted = TRUE) #crear red
#weignhted = TRUE si la matriz contiene datos poderados, entonces se crea una red ponderada

class(r.tipo.RBSK)

r.tipo.RBSK
#IGRAPH 87bbe7d DNW- 55 175 -- 
#+ attr: name (v/c), weight (e/n)

plot(r.tipo.RBSK)


#### Dibujar la red ####

# Layouts (diseño de la red) Por default R encuentra el mejor arreglo de manera que los nodos no se sobrepongan. 

plot(r.tipo.RBSK, layout=layout.random, main="random")
plot(r.tipo.RBSK, edge.arrow.size = 0.2,layout = layout_with_graphopt)

#layout_with_mds(r.tipo.RBSK)

plot(r.tipo.RBSK, layout = coords, axes = TRUE)  # Se reescalan los valores entre -1 y 1 
plot(r.tipo.RBSK, layout = coords, axes = TRUE, rescale = FALSE)




V(r.tipo.RBSK)  #funcion para ver los vertices
E(r.tipo.RBSK) #funcion que muestra conexiones


# ---- 3.2 Com_tipo_PA -----

class(df.tipo.PA)
m.tipo.PA <- as.matrix(df.tipo.PA) #para convertir el daframe en matriz
class(m.tipo.PA)
View(m.tipo.PA)

r.tipo.PA <- graph_from_adjacency_matrix(adjmatrix = m.tipo.PA, mode ="directed", diag = FALSE, add.rownames = "name", weighted = TRUE) #crear red
#weignhted = TRUE si la matriz contiene datos poderados, entonces se crea una red ponderada

class(r.tipo.PA)

r.tipo.PA
#IGRAPH c57a0e6 DNW- 41 129 -- 
#+ attr: name (v/c), weight (e/n)

plot(r.tipo.PA)


# ---- 3.2 Com_tipo_PH -----

class(df.tipo.CZ)
m.tipo.CZ <- as.matrix(df.tipo.CZ) #para convertir el daframe en matriz
class(m.tipo.CZ)
View(m.tipo.CZ)

r.tipo.CZ <- graph_from_adjacency_matrix(adjmatrix = m.tipo.CZ, mode ="directed", diag = FALSE, add.rownames = "name", weighted = TRUE) #crear red
#weignhted = TRUE si la matriz contiene datos poderados, entonces se crea una red ponderada

class(r.tipo.CZ)

r.tipo.CZ
#IGRAPH c57a0e6 DNW- 41 129 -- 
#+ attr: name (v/c), weight (e/n)

plot(r.tipo.CZ)
#IGRAPH 0793b30 DNW- 38 72 -- 
#  + attr: name (v/c), weight (e/n)

