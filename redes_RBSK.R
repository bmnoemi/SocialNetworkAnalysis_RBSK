

############################################################################## #
###                       1. Install Packages                               ====
############################################################################## #

# Packages
install.packages("igraph")
install.packages("statnet")
install.packages("intergraph")
install.packages("ggraph") #its an extension of ggplot2 aimed at supporting relational data structures such as networks, graphs, and trees.

library(igraph, warn.conflicts = FALSE)
library(ggraph)
library(statnet)
library(intergraph)
library(ggplot2)



############################################################################## #
###                       2. Import data                                  ====
############################################################################## #

#rm(list = ls())  #funciones rm() y ls()
list.files()  #enlistar los archivos presentes en el directorio
list.files(pattern = "*.csv")


#### 2.1 Attribute node data  ####

attr_nodes <- read.csv("RBSK_vertices.csv", header = TRUE, row.names = 1)
attr_nodes #Nodelist



############################################################################## #
###                 3. Global & Centrality indexes                          ====
############################################################################## #


## Note that actors in the graph are in the same order as actors in the attribut data frame.
class(tipo_RBSK) # In igraph, networks are objects of class "igraph"
V(tipo_RBSK)
attr_nodes$type

V(tipo_RBSK)$type <- attr_nodes$type  #Asignar el atributo "tipo" de los nodos al objeto de red

# Definir los colores para cada tipo de nodo
colores <- c("OSC" = "blue", "CONANP" = "brown", "CONAPESCA" = "brown", "INAPESCA" = "brown", "SEMAR" = "brown", "CECIMS" = "darkgoldenrod1", "SCPP.CZ" = "bisque", "SCPP.JMA" = "black", "SCPP.VCH" = "purple", "PA" = "purple", "ME" = "bisque", "ACADEMY" = "cyan", "FUND" = "chartreuse4", "PH" = "coral" ) # Ajusta los colores según tus necesidades



#### ---- 3.1 RBSK   ####

## Import and read the adjacency matrix 
#Remocion de FUND y PH2-PH5 ##
tipo_RBSK_adj <- read.csv("Com_tipo_RBSK.csv", header = TRUE, row.names = 1) 

## Crear el objeto de red a partir de la matriz de adyacencia ##
tipo_RBSK <- graph_from_adjacency_matrix(as.matrix(tipo_RBSK_adj), mode = "directed",
                                         diag = FALSE, add.rownames = "name", weighted = TRUE)


E(tipo_RBSK) # view edges
V(tipo_RBSK) # view nodes
edge_attr(tipo_RBSK)

tipo_RBSK
#IGRAPH cb28c4d DNW- 55 175 -- 
# + attr: name (v/c), weight (e/n)

plot(tipo_RBSK, edge.arrow.size=0.5, vertex.label.cex=0.5)



###### ---- 3.1.2 Com_tipo_RBSK   ####


##### ---- Global indexes   ####

vcount(tipo_RBSK_n) #numero de nodos
ecount(tipo_RBSK_n) #numeros de conexiones
edge_density(graph = tipo_RBSK_n, loops = FALSE) #Densidad de la red
ecount(tipo_RBSK_n) / (vcount(tipo_RBSK_n)*(vcount(tipo_RBSK_n)-1)) # num real de conex / max. teorico de conexiones

dist_tipo_RBSK_n <- distances(graph = tipo_RBSK_n) #distancias que hay entre todos los nodos
#distances() calculates the length of all the shortest paths from or to the vertices in the network.
dist_tipo_RBSK_n
mean_distance(graph = tipo_RBSK_n) #average Path Length
diameter(graph = tipo_RBSK_n)
max(dist_tipo_RBSK_n)

transitivity(tipo_RBSK_n) #coeficiente de agrupamiento

sort(degree(tipo_RBSK_df)) #indice de grado o num de conexiones para c/u de los 35 nodos
grado_tipo_RBSK_n <- degree(tipo_RBSK_df)
View(grado_tipo_RBSK_n)
sort(grado_tipo_RBSK_n)
mean(grado_tipo_RBSK_n)
hist(grado_tipo_RBSK_n) #distribución de grado #la mayor parte de los nodos tienen pocas conexiones

modularity_matrix(graph= grado_tipo_RBSK_n, membership(wtc), weights = NULL,resolution = 1, directed = TRUE) #modularidad 

clique_num(tipo_RBSK_n)



###### ---- Centrality indexes   ####

## Colocar en una matriz los valores de Grado, Eigencentralidad, Cercanía e Intermediación.
tipo_RBSK_centrality <- data.frame(Grado = degree(tipo_RBSK_df), Eigencentralidad = eigen_centrality(tipo_RBSK_n, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(tipo_RBSK_df), Intermediacion = betweenness(tipo_RBSK_df))

write.csv(tipo_RBSK_centrality, "tipo.RBSK_centrality_values.csv")



###### ---- Plotting the network  ####

ggraph(tipo_RBSK, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in RBSK")

# Layouts (diseño de la red) Por default R encuentra el mejor arreglo de manera que los nodos no se sobrepongan. 

set.seed(123)
ggraph(tipo_RBSK_n, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.8) + 
  geom_node_point(color = "blue", size = 5) +
  theme_void() + 
  labs(title = "Social Analysis Network in RBSK")

# if we want to associate a property of the nodes or edges with a property of the plot, we can use aesthetic mappings.


# create management structure as dendrogram (tree)
set.seed(123)
ggraph(tipo_RBSK_n, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()

#layout_with_mds(r.tipo.RBSK)





#### ---- 3.2 PUNTA ALLEN  ####

###### ---- 3.2.1 Com_tipo_PA   ####

tipo_PA_df <- read.csv("Com_tipo_PA.csv", header = TRUE, row.names = 1) 
tipo_PA_df


tipo_PA_n <- graph_from_adjacency_matrix(as.matrix(tipo_PA_df), mode = "directed",
                                         diag = FALSE, add.rownames = "name", 
                                         weighted = TRUE)
class(tipo_PA_n)
tipo_PA_n
#IGRAPH 6115433 DNW- 41 129 -- 
#+ attr: name (v/c), weight (e/n)

plot(tipo_PA_n)

E(tipo_PA_n) # view edges
V(tipo_PA_n) # view nodes
edge_attr(tipo_PA_n)


###### ---- Global indexes   ####

vcount(tipo_PA_n) #numero de nodos
ecount(tipo_PA_n) #numeros de conexiones
edge_density(graph = tipo_PA_n, loops = FALSE) #Densidad de la red
ecount(tipo_PA_n) / (vcount(tipo_PA_n)*(vcount(tipo_PA_n)-1)) # num real de conex / max. teorico de conexiones

dist_tipo_PA_n <- distances(graph = tipo_PA_n)
dist_tipo_PA_n
mean_distance(graph = tipo_PA_n) #average Path Length
diameter(graph = tipo_PA_n)
max(dist_tipo_PA_n)

transitivity(tipo_PA_n) #coeficiente de agrupamiento

sort(degree(tipo_PA_df)) #indice de grado o num de conexiones para c/u de los 35 nodos
grado_tipo_PA_n <- degree(tipo_PA_df)
sort(grado_tipo_PA_n)
mean(grado_tipo_PA_n)
hist(grado_tipo_PA_n) #distribución de grado #la mayor parte de los nodos tienen pocas conexiones

modularity_matrix(graph= grado_tipo_PA_n, membership(wtc), weights = NULL,resolution = 1, directed = TRUE) #modularidad 

clique_num(tipo_PA_n)


###### ---- Centrality indexes   ####

tipo_PA_centrality <- data.frame(Grado = degree(tipo_PA_df), Eigencentralidad = eigen_centrality(tipo_PA_n, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(tipo_PA_df), Intermediacion = betweenness(tipo_PA_df))

write.csv(tipo_PA_centrality, "tipo.PA_centrality_values.csv")


###### ---- Plotting the network  ####

ggraph(tipo_PA_n, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in PA")



#### ---- 3.3 MARIA ELENA  ####

###### ---- 3.3.1 Com_tipo_CZ   ####

tipo_CZ_df <- read.csv("Com_tipo_CZ.csv", header = TRUE, row.names = 1) 
tipo_CZ_df

tipo_CZ_n <- graph_from_adjacency_matrix(as.matrix(tipo_CZ_df), mode = "directed",
                                         diag = FALSE, add.rownames = "name", 
                                         weighted = TRUE)
class(tipo_CZ_n)
tipo_CZ_n
#IGRAPH bd36c17 DNW- 38 72 -- 
#+ attr: name (v/c), weight (e/n)

plot(tipo_CZ_n)

E(tipo_CZ_n) # view edges
V(tipo_CZ_n) # view nodes
edge_attr(tipo_CZ_n)



###### ---- Global indexes   ####

vcount(tipo_CZ_n) #numero de nodos
ecount(tipo_CZ_n) #numeros de conexiones
edge_density(graph = tipo_CZ_n, loops = FALSE) #Densidad de la red
ecount(tipo_CZ_n) / (vcount(tipo_CZ_n)*(vcount(tipo_CZ_n)-1)) # num real de conex / max. teorico de conexiones

dist_tipo_CZ_n <- distances(graph = tipo_CZ_n)
dist_tipo_CZ_n
mean_distance(graph = tipo_CZ_n) #average Path Length
diameter(graph = tipo_CZ_n)
max(dist_tipo_CZ_n)

transitivity(tipo_CZ_n) #coeficiente de agrupamiento

sort(degree(tipo_CZ_df)) #indice de grado o num de conexiones para c/u de los 35 nodos
grado_tipo_CZ_n <- degree(tipo_CZ_df)
sort(grado_tipo_CZ_n)
mean(grado_tipo_CZ_n)
hist(grado_tipo_CZ_n) #distribución de grado #la mayor parte de los nodos tienen pocas conexiones

modularity_matrix(graph= grado_tipo_CZ_n, membership(wtc), weights = NULL,resolution = 1, directed = TRUE) #modularidad 

clique_num(tipo_CZ_n)


###### ---- Centrality indexes   ####

tipo_CZ_centrality <- data.frame(Grado = degree(tipo_CZ_df), Eigencentralidad = eigen_centrality(tipo_CZ_n, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(tipo_CZ_df), Intermediacion = betweenness(tipo_CZ_df))

write.csv(tipo_CZ_centrality, "tipo.CZ_centrality_values.csv")


###### ---- Plotting the network  ####

ggraph(tipo_CZ_n, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in CZ")


