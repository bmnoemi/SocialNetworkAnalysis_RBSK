

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

<<<<<<< HEAD
# get vertex set with actor_type property
df.RBSK_vertices <- read.csv("RBSK_vertices.csv")

m.RBSK_vertices <- as.matrix(df.RBSK_vertices) #para convertir el daframe en matriz
class(m.RBSK_vertices)


#### Com_tipo_RBSK - Remocion de FUND y PH2-PH5) ####
#### Com_tipo_PA ####
#### Com_tipo_CZ ####

df.tipo.RBSK <- read.csv("Com_tipo_RBSK.csv", header = TRUE, row.names = 1) #importar matriz de adyacencia
df.tipo.PA <- read.csv("Com_tipo_PA.csv", header = TRUE, row.names = 1) #importar matriz de adyacencia
df.tipo.CZ <- read.csv("Com_tipo_CZ.csv", header = TRUE, row.names = 1) #importar matriz de adyacencia

df.tipo.PA
df.tipo.RBSK
df.tipo.CZ

class(df.tipo.RBSK)
View(df.tipo.RBSK)


=======

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


>>>>>>> 3833a2deb56172007287f2e4b2cccbe46abbfe01
# ==== 3. CREACION Y ANALISIS DE REDES ====

# ---- 3.1 Com_tipo_RBSK -----

<<<<<<< HEAD

#para convertir el dataframe en matriz y crear red

r.tipo.RBSK <- as.matrix(df.tipo.RBSK)%>%
  graph_from_adjacency_matrix(mode = "directed", diag = FALSE, add.rownames = "name", weighted = TRUE)

class(r.tipo.RBSK)
View(r.tipo.RBSK)
=======
class(df.tipo.RBSK)
m.tipo.RBSK <- as.matrix(df.tipo.RBSK) #para convertir el daframe en matriz
class(m.tipo.RBSK)
View(m.tipo.RBSK)

r.tipo.RBSK <- graph_from_adjacency_matrix(adjmatrix = m.tipo.RBSK, mode ="directed", diag = FALSE, add.rownames = "name", weighted = TRUE) #crear red
#weignhted = TRUE si la matriz contiene datos poderados, entonces se crea una red ponderada

class(r.tipo.RBSK)
>>>>>>> 3833a2deb56172007287f2e4b2cccbe46abbfe01

r.tipo.RBSK
#IGRAPH 87bbe7d DNW- 55 175 -- 
#+ attr: name (v/c), weight (e/n)

<<<<<<< HEAD
plot(r.tipo.RBSK, edge.arrow.size = 1)
=======
plot(r.tipo.RBSK)
>>>>>>> 3833a2deb56172007287f2e4b2cccbe46abbfe01


#### Dibujar la red ####

# Layouts (diseño de la red) Por default R encuentra el mejor arreglo de manera que los nodos no se sobrepongan. 

plot(r.tipo.RBSK, layout=layout.random, main="random")
<<<<<<< HEAD
plot(r.tipo.RBSK, layout=layout_with_fr, axes = TRUE)
plot(r.tipo.RBSK, layout=layout_in_circle, main="random")
plot(r.tipo.RBSK, layout=layout_in_circle,axes = TRUE)
plot(r.tipo.RBSK, edge.arrow.size = 0.2,layout = layout_with_graphopt)

=======
plot(r.tipo.RBSK, edge.arrow.size = 0.2,layout = layout_with_graphopt)

#layout_with_mds(r.tipo.RBSK)

>>>>>>> 3833a2deb56172007287f2e4b2cccbe46abbfe01
plot(r.tipo.RBSK, layout = coords, axes = TRUE)  # Se reescalan los valores entre -1 y 1 
plot(r.tipo.RBSK, layout = coords, axes = TRUE, rescale = FALSE)



<<<<<<< HEAD
install.packages("ggraph") #its an extension of ggplot2 aimed at supporting relational data structures such as networks, graphs, and trees. 
library(ggraph)

set.seed(123)
ggraph(r.tipo.RBSK, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.8) + 
  geom_node_point(color = "blue", size = 5) +
  theme_void() + 
  labs(title = "Social Analysis Network in RBSK")

# if we want to associate a property of the nodes or edges with a property of the plot, we can use aesthetic mappings.




# create management structure as dendrogram (tree)
set.seed(123)
ggraph(r.tipo.RBSK, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()



#layout_with_mds(r.tipo.RBSK)
=======
>>>>>>> 3833a2deb56172007287f2e4b2cccbe46abbfe01

V(r.tipo.RBSK)  #funcion para ver los vertices
E(r.tipo.RBSK) #funcion que muestra conexiones

<<<<<<< HEAD
#### Indicadores globales ####

vcount(r.tipo.RBSK) #numero de nodos
ecount(r.tipo.RBSK) #numeros de conexiones
edge_density(graph =r.tipo.RBSK, loops = FALSE) #Densidad de la red
ecount(r.tipo.RBSK) / (vcount(r.tipo.RBSK)*(vcount(r.tipo.RBSK)-1)) # num real de conex / max. teorico de conexiones

dist.r.tipo.RBSK <- distances(graph = r.tipo.RBSK) #distancias que hay entre todos los nodos
#distances() calculates the length of all the shortest paths from or to the vertices in the network.
dist.r.tipo.RBSK
mean_distance(graph = r.tipo.RBSK) #average Path Length
diameter(graph = r.tipo.RBSK)
max(dist.r.tipo.RBSK)

components(r.tipo.RBSK, mode = "strong") 
#weak, cuando no se considera la direccion de las flechas. 
#strong, cuando se considera la dirección se coloca "strong.
max(components(r.tipo.RBSK, mode = "strong")$csize)
max(components(r.tipo.RBSK, mode = "weak")$no)

transitivity(graph = r.tipo.RBSK) #coeficiente de agrupamiento

sort(degree(graph = r.tipo.RBSK)) #indice de grado o num de conexiones para c/u de los 35 nodos
grado.r.tipo.RBSK <- degree(r.tipo.RBSK)
sort(grado.r.tipo.RBSK)
mean(grado.r.tipo.RBSK)
hist(grado.r.tipo.RBSK) #distribución de grado #la mayor parte de los nodos tienen pocas conexiones




## Colocar en una matriz los valores de Grado, Eigencentralidad, Cercanía e Intermediación.

r.tipo.RBSK.centrality <- data.frame (Grado = degree(r.tipo.RBSK), Eigencentralidad = eigen_centrality(r.tipo.RBSK, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(r.tipo.RBSK), intermediacion = betweenness(r.tipo.RBSK))

write.csv(r.tipo.RBSK.centrality, "r.tipo.RBSK_centrality_values.csv")


=======
>>>>>>> 3833a2deb56172007287f2e4b2cccbe46abbfe01

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

<<<<<<< HEAD


=======
>>>>>>> 3833a2deb56172007287f2e4b2cccbe46abbfe01
