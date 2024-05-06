

############################################################################## #
###                       1. INSTALL PACKAGES                             ====
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
###                       2. IMPORT DATA                                  ====
############################################################################## #

#rm(list = ls())  #funciones rm() y ls()
list.files()  #enlistar los archivos presentes en el directorio
list.files(pattern = "*.csv")


#### 2.1 Attribute node data  ####

attr_nodes_RBSK <- read.csv("./Data/nodos_RBSK.csv", header = TRUE, row.names = 1)
attr_nodes_RBSK #Nodelist

attr_nodes_PA <- read.csv("./Data/nodos_PA.csv", header = TRUE, row.names = 1)
attr_nodes_PA

attr_nodes_ME <- read.csv("./Data/nodos_ME.csv", header = TRUE, row.names = 1)
attr_nodes_ME



############################################################################## #
###                 3.   RBSK                                              ====
############################################################################## #

##### ---- Comunicacion_tipo   ######

## Import and read the adjacency matrix 
#Remocion de FUND y PH2-PH5 ##
tipo_RBSK_adj <- read.csv("./Data/Comunicacion_tipo_RBSK.csv", header = TRUE, row.names = 1) 
## Crear el objeto de red a partir de la matriz de adyacencia ##
tipo_RBSK <- graph_from_adjacency_matrix(as.matrix(tipo_RBSK_adj), mode = "directed",
                                         diag = FALSE, add.rownames = "name",       
                                         weighted = TRUE)

E(tipo_RBSK) # view edges
V(tipo_RBSK) # view nodes
edge_attr(tipo_RBSK)

tipo_RBSK
#IGRAPH cb28c4d DNW- 55 175 -- 
# + attr: name (v/c), weight (e/n)


## Note that actors in the graph are in the same order as actors in the attribut data frame.
class(tipo_RBSK) # In igraph, networks are objects of class "igraph"
V(tipo_RBSK)
attr_nodes$type


##### ---- Comunicacion_frecuencia   ######




##### ---- Colaboracion   ######





##### ---- Confianza   ######






# - - - - - - - - - - - - - - - - - - 
##### ---- 3.1 Comunicacion_tipo   #####
# - - - - - - - - - - - - - - - - - - 

###### ---- Global indexes   ######

vcount(tipo_RBSK) #numero de nodos
ecount(tipo_RBSK) #numeros de conexiones
edge_density(graph = tipo_RBSK, loops = FALSE) #Densidad de la red
ecount(tipo_RBSK) / (vcount(tipo_RBSK)*(vcount(tipo_RBSK)-1)) # num real de conex / max. teorico de conexiones

dist_tipo_RBSK <- distances(graph = tipo_RBSK) #distancias que hay entre todos los nodos
#distances() calculates the length of all the shortest paths from or to the vertices in the network.
dist_tipo_RBSK
mean_distance(graph = tipo_RBSK) #average Path Length
diameter(graph = tipo_RBSK)
max(dist_tipo_RBSK)

transitivity(tipo_RBSK) #coeficiente de agrupamiento

sort(degree(tipo_RBSK_adj)) #indice de grado o num de conexiones para c/u de los 35 nodos
grado_tipo_RBSK <- degree(tipo_RBSK_adj)
#View(grado_tipo_RBSK)
sort(grado_tipo_RBSK)
mean(grado_tipo_RBSK)
hist(grado_tipo_RBSK) #distribución de grado #la mayor parte de los nodos tienen pocas conexiones



###### ---- Centrality indexes   ####

## Colocar en una matriz los valores de Grado, Eigencentralidad, Cercanía e Intermediación.
tipo_RBSK_centrality <- data.frame(Grado = degree(tipo_RBSK_adj), Eigencentralidad = eigen_centrality(tipo_RBSK, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(tipo_RBSK_adj), Intermediacion = betweenness(tipo_RBSK_adj))

write.csv(tipo_RBSK_centrality, "tipo.RBSK_centrality_values.csv")



###### ---- Plotting the network  ####


V(tipo_RBSK)$type <- attr_nodes_RBSK$type  #Asignar el atributo "tipo" de los nodos al objeto de red

# Definir los colores para cada tipo de nodo
colores <- c("OSC" = "blue", "CONANP" = "brown", "CONAPESCA" = "brown", "INAPESCA" = "brown", "SEMAR" = "brown", "CECIMS" = "darkgoldenrod1", "SCPP.CZ" = "bisque", "SCPP.JMA" = "black", "SCPP.VCH" = "purple", "PA" = "purple", "ME" = "bisque", "ACADEMY" = "cyan", "FUND" = "chartreuse4", "PH" = "coral" ) # Ajusta los colores según tus necesidades


ggraph(tipo_RBSK, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in RBSK")

# Layouts (diseño de la red) Por default R encuentra el mejor arreglo de manera que los nodos no se sobrepongan. 

# if we want to associate a property of the nodes or edges with a property of the plot, we can use aesthetic mappings.

# create management structure as dendrogram (tree)
set.seed(123)
ggraph(tipo_RBSK, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()

#layout_with_mds(tipo_RBSK)




# - - - - - - - - - - - - - - - - - - 
##### ---- 3.2 Comunicacion_frecuencia   #####
# - - - - - - - - - - - - - - - - - - 










############################################################################## #
###                 4.   PUNTA ALLEN                                      ====
############################################################################## #


##### ---- Comunicacion_tipo   ######



##### ---- Comunicacion_frecuencia   ######



##### ---- Colaboracion   ######



##### ---- Confianza   ######




# - - - - - - - - - - - - - - - 
#### ---- 4.1  Comunicacion_tipo_PA ####
# - - - - - - - - - - - - - - - 

tipo_PA_adj <- read.csv("./Data/Comunicacion_tipo_PA.csv", header = TRUE, row.names = 1) 
tipo_PA_adj

tipo_PA <- graph_from_adjacency_matrix(as.matrix(tipo_PA_adj), mode = "directed",
                                         diag = FALSE, add.rownames = "name", 
                                         weighted = TRUE)
class(tipo_PA)
tipo_PA
#IGRAPH a6efc6f DNW- 41 129 -- 
#  + attr: name (v/c), weight (e/n)

plot(tipo_PA)

E(tipo_PA) # view edges
V(tipo_PA) # view nodes
edge_attr(tipo_PA)



###### ---- Global indexes   ######


vcount(tipo_PA) #numero de nodos
ecount(tipo_PA) #numeros de conexiones
edge_density(graph = tipo_PA, loops = FALSE) #Densidad de la red
ecount(tipo_PA) / (vcount(tipo_PA)*(vcount(tipo_PA)-1)) # num real de conex / max. teorico de conexiones

dist_tipo_PA <- distances(graph = tipo_PA) #distancias que hay entre todos los nodos
#distances() calculates the length of all the shortest paths from or to the vertices in the network.
dist_tipo_PA
mean_distance(graph = tipo_PA) #average Path Length
diameter(graph = tipo_PA)
max(dist_tipo_PA)

transitivity(tipo_PA) #coeficiente de agrupamiento

sort(degree(tipo_PA_adj)) #indice de grado o num de conexiones para c/u de los 35 nodos
grado_tipo_PA <- degree(tipo_PA_adj)
sort(grado_tipo_PA)
mean(grado_tipo_PA)
hist(grado_tipo_PA) #distribución de grado #la mayor parte de los nodos tienen pocas conexiones



###### ---- Centrality indexes   ####

## Colocar en una matriz los valores de Grado, Eigencentralidad, Cercanía e Intermediación.
tipo_PA_centrality <- data.frame(Grado = degree(tipo_PA_adj), Eigencentralidad = eigen_centrality(tipo_PA, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(tipo_PA_adj), Intermediacion = betweenness(tipo_PA_adj))

write.csv(tipo_PA_centrality, "tipo.PA_centrality_values.csv")



###### ---- Plotting the network  ####

dev.off()

V(tipo_PA)$type <- attr_nodes_PA$type

ggraph(tipo_PA, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in Punta Allen")


# create management structure as dendrogram (tree)
set.seed(123)
ggraph(tipo_PA, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()




# - - - - - - - - - - - - - - - - - - 
##### ---- 4.2 Comunicacion_frecuencia   #####
# - - - - - - - - - - - - - - - - - - 









############################################################################## #
###                 5.   MARIA ELENA                                      ====
############################################################################## #


# - - - - - - - - - - - - - - - 
#### ---- 5.1  Com_tipo_ME ####
# - - - - - - - - - - - - - - - 

tipo_ME_adj <- read.csv("./Data/Comunicacion_tipo_ME.csv", header = TRUE, row.names = 1) 
tipo_ME_adj


tipo_ME <- graph_from_adjacency_matrix(as.matrix(tipo_ME_adj), mode = "directed",
                                       diag = FALSE, add.rownames = "name", 
                                       weighted = TRUE)
class(tipo_ME)
tipo_ME
#IGRAPH a6efc6f DNW- 41 129 -- 
#  + attr: name (v/c), weight (e/n)

plot(tipo_ME)

E(tipo_ME) # view edges
V(tipo_ME) # view nodes
edge_attr(tipo_ME)



###### ---- Global indexes   ######


vcount(tipo_ME) #numero de nodos
ecount(tipo_ME) #numeros de conexiones
edge_density(graph = tipo_ME, loops = FALSE) #Densidad de la red
ecount(tipo_ME) / (vcount(tipo_ME)*(vcount(tipo_ME)-1)) # num real de conex / max. teorico de conexiones

dist_tipo_ME <- distances(graph = tipo_ME) #distancias que hay entre todos los nodos
#distances() calculates the length of all the shortest paths from or to the vertices in the network.
dist_tipo_ME
mean_distance(graph = tipo_ME) #average Path Length
diameter(graph = tipo_ME)
max(dist_tipo_ME)

transitivity(tipo_ME) #coeficiente de agrupamiento

sort(degree(tipo_ME_adj)) #indice de grado o num de conexiones para c/u de los 35 nodos
grado_tipo_ME <- degree(tipo_ME_adj)
sort(grado_tipo_ME)
mean(grado_tipo_ME)
hist(grado_tipo_ME) #distribución de grado #la mayor parte de los nodos tienen pocas conexiones



###### ---- Centrality indexes   ####

## Colocar en una matriz los valores de Grado, Eigencentralidad, Cercanía e Intermediación.
tipo_ME_centrality <- data.frame(Grado = degree(tipo_ME_adj), Eigencentralidad = eigen_centrality(tipo_ME, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(tipo_ME_adj), Intermediacion = betweenness(tipo_ME_adj))

write.csv(tipo_ME_centrality, "tipo.ME_centrality_values.csv")



###### ---- Plotting the network  ####

V(tipo_ME)$type <- attr_nodes_ME$type

ggraph(tipo_ME, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in Maria Elena")


# create management structure as dendrogram (tree)
set.seed(123)
ggraph(tipo_ME, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()



# - - - - - - - - - - - - - - - - - - 
##### ---- 5.2 Comunicacion_frecuencia   #####
# - - - - - - - - - - - - - - - - - - 


