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
attr_nodes_PA <- read.csv("./Data/nodos_PA.csv", header = TRUE, row.names = 1)
attr_nodes_ME <- read.csv("./Data/nodos_ME.csv", header = TRUE, row.names = 1)

attr_nodes_PA
attr_nodes_RBSK #Nodelist
attr_nodes_ME



############################################################################## #
###                 3.   RBSK                                           ====
############################################################################## #


##### ---- 3.1 Comunicacion_tipo   ######

## Import and read the adjacency matrix 
#Remocion de FUND y PH2-PH5 ##
tipo_RBSK_adj <- read.csv("./Data/Comunicacion_tipo_RBSK.csv", header = TRUE, row.names = 1) 
## Crear el objeto de red a partir de la matriz de adyacencia ##
tipo_RBSK <- graph_from_adjacency_matrix(as.matrix(tipo_RBSK_adj), mode = "directed",
                                         diag = FALSE, add.rownames = "name",       
                                         weighted = TRUE)



###### ---- Global indexes   ######

E(tipo_RBSK) # view edges
V(tipo_RBSK) # view nodes
edge_attr(tipo_RBSK)

tipo_RBSK
#IGRAPH cb28c4d DNW- 56 176 -- 
# + attr: name (v/c), weight (e/n)


## Note that actors in the graph are in the same order as actors in the attribut data frame.
class(tipo_RBSK) # In igraph, networks are objects of class "igraph"
V(tipo_RBSK)
attr_nodes_RBSK$type


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

write.csv(tipo_RBSK_centrality, "./Results/ComTipo.RBSK_centrality_values.csv")



###### ---- Plotting the network  ####


V(tipo_RBSK)$type <- attr_nodes_RBSK$type  #Asignar el atributo "tipo" de los nodos al objeto de red

# Definir los colores para cada tipo de nodo
colores <- c("OSC" = "blue", "CONANP" = "brown", "CONAPESCA" = "brown", "INAPESCA" = "brown", "SEMAR" = "brown", "CECIMS" = "darkgoldenrod1", "SCPP.CZ" = "bisque", "SCPP.JMA" = "black", "SCPP.VCH" = "purple", "PA" = "purple", "ME" = "bisque", "ACADEMY" = "cyan", "FUND" = "chartreuse4", "PH" = "coral" ) # Ajusta los colores según tus necesidades


com_tipo_RBSK_plot <- ggraph(tipo_RBSK, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1.2) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in RBSK - Type of communication")

com_tipo_RBSK_plot

# Layouts (diseño de la red) Por default R encuentra el mejor arreglo de manera que los nodos no se sobrepongan. 

# if we want to associate a property of the nodes or edges with a property of the plot, we can use aesthetic mappings.

# create management structure as dendrogram (tree)
set.seed(123)
ggraph(tipo_RBSK, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()

#layout_with_mds(tipo_RBSK)





##### ---- 3.2 Comunicacion_frecuencia   ######

frec_RBSK_adj <- read.csv("./Data/Comunicacion_frecuencia_RBSK.csv", header = TRUE, row.names = 1) 

frec_RBSK <- graph_from_adjacency_matrix(as.matrix(frec_RBSK_adj), mode = "directed",
                                         diag = FALSE, add.rownames = "name",       
                                         weighted = TRUE)



###### ---- Global indexes   ######


frec_RBSK
#IGRAPH cb28c4d DNW- 56 178 -- 
# + attr: name (v/c), weight (e/n)

attr_nodes_RBSK$type

vcount(frec_RBSK) 
ecount(frec_RBSK) 
edge_density(graph = frec_RBSK, loops = FALSE) 
ecount(frec_RBSK) / (vcount(frec_RBSK)*(vcount(frec_RBSK)-1)) 

dist_frec_RBSK <- distances(graph = frec_RBSK) 
dist_frec_RBSK
mean_distance(graph = frec_RBSK) 
diameter(graph = frec_RBSK)
max(dist_frec_RBSK)

transitivity(frec_RBSK) 

sort(degree(frec_RBSK_adj)) 

grado_frec_RBSK <- degree(frec_RBSK_adj)
sort(grado_frec_RBSK)
mean(grado_frec_RBSK)
hist(grado_frec_RBSK) 



###### ---- Centrality indexes   ####

## Colocar en una matriz los valores de Grado, Eigencentralidad, Cercanía e Intermediación.
frec_RBSK_centrality <- data.frame(Grado = degree(frec_RBSK_adj), Eigencentralidad = eigen_centrality(frec_RBSK, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(frec_RBSK_adj), Intermediacion = betweenness(frec_RBSK_adj))

write.csv(frec_RBSK_centrality, "./Results/ComFrec.RBSK_centrality_values.csv")



###### ---- Plotting the network  ####


V(frec_RBSK)$type <- attr_nodes_RBSK$type 

colores <- c("OSC" = "blue", "CONANP" = "brown", "CONAPESCA" = "brown", "INAPESCA" = "brown", "SEMAR" = "brown", "CECIMS" = "darkgoldenrod1", "SCPP.CZ" = "bisque", "SCPP.JMA" = "black", "SCPP.VCH" = "purple", "PA" = "purple", "ME" = "bisque", "ACADEMY" = "cyan", "FUND" = "chartreuse4", "PH" = "coral" ) 

com_frec_RBSK_plot <- ggraph(frec_RBSK, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1.2) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in RBSK - Frecuency of communication")

com_frec_RBSK_plot


set.seed(123)
ggraph(frec_RBSK, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()

#layout_with_mds(tipo_RBSK)




##### ---- 3.3 Colaboracion   ######

collab_RBSK_adj <- read.csv("./Data/Colaboracion_RBSK.csv", header = TRUE, row.names = 1) 

collab_RBSK <- graph_from_adjacency_matrix(as.matrix(collab_RBSK_adj), mode = "directed",diag = FALSE, add.rownames = "name", weighted = TRUE)



###### ---- Global indexes   ######

collab_RBSK
#IGRAPH ba6c21d DNW- 56 170 -- 
#  + attr: name (v/c), weight (e/n)

vcount(collab_RBSK)
ecount(collab_RBSK) 
edge_density(graph = collab_RBSK, loops = FALSE) 
ecount(collab_RBSK) / (vcount(collab_RBSK)*(vcount(collab_RBSK)-1)) 

dist_collab_RBSK <- distances(graph = collab_RBSK)  
dist_collab_RBSK
mean_distance(graph = collab_RBSK) #average Path Length
diameter(graph = collab_RBSK)
max(dist_collab_RBSK)

transitivity(collab_RBSK) 

sort(degree(collab_RBSK_adj))
grado_collab_RBSK <- degree(collab_RBSK_adj)
sort(grado_collab_RBSK)
mean(grado_collab_RBSK)
hist(grado_collab_RBSK) 


###### ---- Centrality indexes   ####


collab_RBSK_centrality <- data.frame(Grado = degree(collab_RBSK_adj), Eigencentralidad = eigen_centrality(collab_RBSK, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(collab_RBSK_adj), Intermediacion = betweenness(collab_RBSK_adj))

write.csv(collab_RBSK_centrality, "./Results/Colaboracion.RBSK_centrality_values.csv")



###### ---- Plotting the network  ####


V(collab_RBSK)$type <- attr_nodes_RBSK$type 

colores <- c("OSC" = "blue", "CONANP" = "brown", "CONAPESCA" = "brown", "INAPESCA" = "brown", "SEMAR" = "brown", "CECIMS" = "darkgoldenrod1", "SCPP.CZ" = "bisque", "SCPP.JMA" = "black", "SCPP.VCH" = "purple", "PA" = "purple", "ME" = "bisque", "ACADEMY" = "cyan", "FUND" = "chartreuse4", "PH" = "coral" ) 

collab_RBSK_plot <- ggraph(collab_RBSK, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1.2) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in RBSK - Collaboration")

collab_RBSK_plot


set.seed(123)
ggraph(colab_RBSK, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()

#layout_with_mds(tipo_RBSK)




##### ---- 3.4 Confianza   ######

trust_RBSK_adj <- read.csv("./Data/Confianza_RBSK.csv", header = TRUE, row.names = 1) 

trust_RBSK <- graph_from_adjacency_matrix(as.matrix(trust_RBSK_adj), mode = "directed",
                                         diag = FALSE, add.rownames = "name",       
                                         weighted = TRUE)


###### ---- Global indexes   ######


trust_RBSK
#IGRAPH 92f8ec8 DNW- 56 177 -- 
#+ attr: name (v/c), weight (e/n)

attr_nodes_RBSK$type

vcount(trust_RBSK)
ecount(trust_RBSK) 
edge_density(graph = trust_RBSK, loops = FALSE) 
ecount(trust_RBSK) / (vcount(trust_RBSK)*(vcount(trust_RBSK)-1)) 

dist_trust_RBSK <- distances(graph = trust_RBSK)  
dist_trust_RBSK
mean_distance(graph = trust_RBSK) #average Path Length
diameter(graph = trust_RBSK)
max(dist_trust_RBSK)

transitivity(trust_RBSK) 

sort(degree(trust_RBSK_adj))
grado_trust_RBSK <- degree(trust_RBSK_adj)
sort(grado_trust_RBSK)
mean(grado_trust_RBSK)
hist(grado_trust_RBSK) 


###### ---- Centrality indexes   ####


trust_RBSK_centrality <- data.frame(Grado = degree(trust_RBSK_adj), Eigencentralidad = eigen_centrality(trust_RBSK, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(trust_RBSK_adj), Intermediacion = betweenness(trust_RBSK_adj))

write.csv(trust_RBSK_centrality, "./Results/Confianza.RBSK_centrality_values.csv")




###### ---- Plotting the network  ####


V(trust_RBSK)$type <- attr_nodes_RBSK$type 

colores <- c("OSC" = "blue", "CONANP" = "brown", "CONAPESCA" = "brown", "INAPESCA" = "brown", "SEMAR" = "brown", "CECIMS" = "darkgoldenrod1", "SCPP.CZ" = "bisque", "SCPP.JMA" = "black", "SCPP.VCH" = "purple", "PA" = "purple", "ME" = "bisque", "ACADEMY" = "cyan", "FUND" = "chartreuse4", "PH" = "coral" ) 

trust_RBSK_plot <- ggraph(trust_RBSK, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1.2) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in RBSK - Trust")

trust_RBSK_plot


set.seed(123)
ggraph(trust_RBSK, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()

#layout_with_mds(tipo_RBSK)






############################################################################## #
###                 4.   PUNTA ALLEN                                      ====
############################################################################## #


##### ---- 4.1 Comunicacion_tipo   ######

tipo_PA_adj <- read.csv("./Data/Comunicacion_tipo_PA.csv", header = TRUE, row.names = 1) 

tipo_PA <- graph_from_adjacency_matrix(as.matrix(tipo_PA_adj), mode = "directed",
                                       diag = FALSE, add.rownames = "name",       
                                       weighted = TRUE)

tipo_PA
#IGRAPH de77f63 DNW- 38 125 -- 
# + attr: name (v/c), weight (e/n)



###### ---- Global indexes   ######

vcount(tipo_PA) 
ecount(tipo_PA)
edge_density(graph = tipo_PA, loops = FALSE) 
ecount(tipo_PA) / (vcount(tipo_PA)*(vcount(tipo_PA)-1)) 

dist_tipo_PA <- distances(graph = tipo_PA) 
dist_tipo_PA
mean_distance(graph = tipo_PA) 
diameter(graph = tipo_PA)
max(dist_tipo_PA)

transitivity(tipo_PA) 

sort(degree(tipo_PA_adj)) 
grado_tipo_PA <- degree(tipo_PA_adj)
sort(grado_tipo_PA)
mean(grado_tipo_PA)
hist(grado_tipo_PA) 



###### ---- Centrality indexes   ####

## Colocar en una matriz los valores de Grado, Eigencentralidad, Cercanía e Intermediación.
tipo_PA_centrality <- data.frame(Grado = degree(tipo_PA_adj), Eigencentralidad = eigen_centrality(tipo_PA, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(tipo_PA_adj), Intermediacion = betweenness(tipo_PA_adj))

write.csv(tipo_PA_centrality, "./Results/ComTipo.PA_centrality_values.csv")



###### ---- Plotting the network  ####

dev.off()

V(tipo_PA)$type <- attr_nodes_PA$type

com_type_PA_plot <- ggraph(tipo_PA, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in Punta Allen - Type of communication")

com_type_PA_plot


# create management structure as dendrogram (tree)
set.seed(123)
ggraph(tipo_PA, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()






##### ---- 4.2 Comunicacion_frecuencia   ######


frec_PA_adj <- read.csv("./Data/Comunicacion_frecuencia_PA.csv", header = TRUE, row.names = 1) 

frec_PA <- graph_from_adjacency_matrix(as.matrix(frec_PA_adj), mode = "directed",
                                       diag = FALSE, add.rownames = "name",       
                                       weighted = TRUE)



###### ---- Global indexes   ######


frec_PA
#IGRAPH cb28c4d DNW- 56 178 -- 
# + attr: name (v/c), weight (e/n)

attr_nodes_PA$type

vcount(frec_PA) 
ecount(frec_PA) 
edge_density(graph = frec_PA, loops = FALSE) 
ecount(frec_PA) / (vcount(frec_PA)*(vcount(frec_PA)-1)) 

dist_frec_PA <- distances(graph = frec_PA) 
dist_frec_PA
mean_distance(graph = frec_PA) 
diameter(graph = frec_PA)
max(dist_frec_PA)

transitivity(frec_PA) 

sort(degree(frec_PA_adj)) 

grado_frec_PA <- degree(frec_PA_adj)
sort(grado_frec_PA)
mean(grado_frec_PA)
hist(grado_frec_PA) 



###### ---- Centrality indexes   ####

frec_PA_centrality <- data.frame(Grado = degree(frec_PA_adj), Eigencentralidad = eigen_centrality(frec_PA, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(frec_PA_adj), Intermediacion = betweenness(frec_PA_adj))

write.csv(frec_PA_centrality, "ComFrec.PA_centrality_values.csv")



###### ---- Plotting the network  ####


V(frec_PA)$type <- attr_nodes_RBSK$type 

colores <- c("OSC" = "blue", "CONANP" = "brown", "CONAPESCA" = "brown", "INAPESCA" = "brown", "SEMAR" = "brown", "CECIMS" = "darkgoldenrod1", "SCPP.CZ" = "bisque", "SCPP.JMA" = "black", "SCPP.VCH" = "purple", "PA" = "purple", "ME" = "bisque", "ACADEMY" = "cyan", "FUND" = "chartreuse4", "PH" = "coral" ) 

com_frec_PA_plot <- ggraph(frec_RBSK, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1.2) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in RBSK - Frecuency of communication")

com_frec_PA_plot


set.seed(123)
ggraph(frec_PA, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()

#layout_with_mds(tipo_PA)







##### ---- 4.3 Colaboracion   ######



collab_PA_adj <- read.csv("./Data/Colaboracion_PA.csv", header = TRUE, row.names = 1) 

collab_PA <- graph_from_adjacency_matrix(as.matrix(collab_PA_adj), mode = "directed",diag = FALSE, add.rownames = "name", weighted = TRUE)



###### ---- Global indexes   ######

collab_PA
#IGRAPH 47f9611 DNW- 38 119 -- 
#  + attr: name (v/c), weight (e/n)

class(collab_PA) # In igraph, networks are objects of class "igraph"
V(collab_PA)
attr_nodes_PA$type


vcount(collab_PA)
ecount(collab_PA) 
edge_density(graph = collab_PA, loops = FALSE) 
ecount(collab_PA) / (vcount(collab_PA)*(vcount(collab_PA)-1)) 

dist_collab_PA <- distances(graph = collab_PA)  
dist_collab_PA
mean_distance(graph = collab_PA) #average Path Length
diameter(graph = collab_PA)
max(dist_collab_PA)

transitivity(collab_PA) 

sort(degree(collab_PA_adj))
grado_collab_PA <- degree(collab_PA_adj)
sort(grado_collab_PA)
mean(grado_collab_PA)
hist(grado_collab_PA) 


###### ---- Centrality indexes   ####


collab_PA_centrality <- data.frame(Grado = degree(collab_PA_adj), Eigencentralidad = eigen_centrality(colab_PA, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(colab_PA_adj), Intermediacion = betweenness(colab_PA_adj))

write.csv(collab_PA_centrality, "Colaboracion.PA_centrality_values.csv")



###### ---- Plotting the network  ####


V(colab_PA)$type <- attr_nodes_RBSK$type 

colores <- c("OSC" = "blue", "CONANP" = "brown", "CONAPESCA" = "brown", "INAPESCA" = "brown", "SEMAR" = "brown", "CECIMS" = "darkgoldenrod1", "SCPP.CZ" = "bisque", "SCPP.JMA" = "black", "SCPP.VCH" = "purple", "PA" = "purple", "ME" = "bisque", "ACADEMY" = "cyan", "FUND" = "chartreuse4", "PH" = "coral" ) 

collab_PA_plot <- ggraph(colab_RBSK, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1.2) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in Punta Allen - Collaboration")

collab_PA_plot


set.seed(123)
ggraph(colab_PA, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()

#layout_with_mds(tipo_RBSK)






##### ---- 4.4 Confianza   ######


conf_PA_adj <- read.csv("./Data/Confianza_PA.csv", header = TRUE, row.names = 1) 

conf_PA <- graph_from_adjacency_matrix(as.matrix(conf_PA_adj), mode = "directed",
                                         diag = FALSE, add.rownames = "name",       
                                         weighted = TRUE)


###### ---- Global indexes   ######


conf_PA
#IGRAPH 92f8ec8 DNW- 56 177 -- 
#+ attr: name (v/c), weight (e/n)

attr_nodes_PA$type

vcount(conf_PA)
ecount(conf_PA) 
edge_density(graph = conf_PA, loops = FALSE) 
ecount(conf_PA) / (vcount(conf_PA)*(vcount(conf_PA)-1)) 

dist_conf_PA <- distances(graph = conf_PA)  
dist_conf_PA
mean_distance(graph = conf_PA) #average Path Length
diameter(graph = conf_PA)
max(dist_conf_PA)

transitivity(conf_PA) 

sort(degree(conf_PA_adj))
grado_conf_PA <- degree(conf_PA_adj)
sort(grado_conf_PA)
mean(grado_conf_PA)
hist(grado_conf_PA) 


###### ---- Centrality indexes   ####


conf_PA_centrality <- data.frame(Grado = degree(conf_PA_adj), Eigencentralidad = eigen_centrality(conf_PA, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(conf_PA_adj), Intermediacion = betweenness(conf_PA_adj))

write.csv(conf_PA_centrality, "Confianza.PA_centrality_values.csv")




###### ---- Plotting the network  ####


V(conf_PA)$type <- attr_nodes_PA$type 

colores <- c("OSC" = "blue", "CONANP" = "brown", "CONAPESCA" = "brown", "INAPESCA" = "brown", "SEMAR" = "brown", "CECIMS" = "darkgoldenrod1", "SCPP.CZ" = "bisque", "SCPP.JMA" = "black", "SCPP.VCH" = "purple", "PA" = "purple", "ME" = "bisque", "ACADEMY" = "cyan", "FUND" = "chartreuse4", "PH" = "coral" ) 

trust_PA_plot <- ggraph(conf_RBSK, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1.2) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in Punta Allen - Trust")

trust_PA_plot


set.seed(123)
ggraph(conf_PA, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()

#layout_with_mds(tipo_RBSK)






############################################################################## #
###                 5.   MARIA ELENA                                      ====
############################################################################## #


# - - - - - - - - - - - - - - - 
#### ---- 5.1  Comunicacion_tipo_ME ####
# - - - - - - - - - - - - - - - 

tipo_ME_adj <- read.csv("./Data/Comunicacion_tipo_ME.csv", header = TRUE, row.names = 1) 
tipo_ME_adj


tipo_ME <- graph_from_adjacency_matrix(as.matrix(tipo_ME_adj), mode = "directed",
                                       diag = FALSE, add.rownames = "name", 
                                       weighted = TRUE)



###### ---- Global indexes   ######

class(tipo_ME)
tipo_ME
#IGRAPH a6efc6f DNW- 41 129 -- 
#  + attr: name (v/c), weight (e/n)

plot(tipo_ME)

E(tipo_ME) # view edges
V(tipo_ME) # view nodes
edge_attr(tipo_ME)


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

com_type_ME_plot <- ggraph(tipo_ME, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in Maria Elena")

com_type_ME_plot


# create management structure as dendrogram (tree)
set.seed(123)
ggraph(tipo_ME, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()




# - - - - - - - - - - - - - - - - - - 
#### ---- 5.2 Comunicacion_frecuencia   ####
# - - - - - - - - - - - - - - - - - - 


frec_ME_adj <- read.csv("./Data/Comunicacion_frecuencia_PA.csv", header = TRUE, row.names = 1) 

frec_ME <- graph_from_adjacency_matrix(as.matrix(frec_ME_adj), mode = "directed",
                                       diag = FALSE, add.rownames = "name",       
                                       weighted = TRUE)



###### ---- Global indexes   ######


frec_ME
#IGRAPH cb28c4d DNW- 56 178 -- 
# + attr: name (v/c), weight (e/n)

attr_nodes_ME$type

vcount(frec_ME) 
ecount(frec_ME) 
edge_density(graph = frec_ME, loops = FALSE) 
ecount(frec_ME) / (vcount(frec_ME)*(vcount(frec_ME)-1)) 

dist_frec_ME <- distances(graph = frec_ME) 
dist_frec_ME
mean_distance(graph = frec_ME) 
diameter(graph = frec_ME)
max(dist_frec_ME)

transitivity(frec_ME) 

sort(degree(frec_ME_adj)) 

grado_frec_ME <- degree(frec_ME_adj)
sort(grado_frec_ME)
mean(grado_frec_ME)
hist(grado_frec_ME) 



###### ---- Centrality indexes   ####

frec_ME_centrality <- data.frame(Grado = degree(frec_ME_adj), Eigencentralidad = eigen_centrality(frec_ME, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(frec_ME_adj), Intermediacion = betweenness(frec_ME_adj))

write.csv(frec_ME_centrality, "ComFrec.ME_centrality_values.csv")



###### ---- Plotting the network  ####


V(frec_ME)$type <- attr_nodes_ME$type 

colores <- c("OSC" = "blue", "CONANP" = "brown", "CONAPESCA" = "brown", "INAPESCA" = "brown", "SEMAR" = "brown", "CECIMS" = "darkgoldenrod1", "SCPP.CZ" = "bisque", "SCPP.JMA" = "black", "SCPP.VCH" = "purple", "PA" = "purple", "ME" = "bisque", "ACADEMY" = "cyan", "FUND" = "chartreuse4", "PH" = "coral" ) 

com_frec_ME_plot <- ggraph(frec_ME, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1.2) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in Maria Elena - Frecuency of communication")

com_frec_ME_plot


set.seed(123)
ggraph(frec_ME, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()

#layout_with_mds(tipo_RBSK)






# - - - - - - - - - - - - - - - - - - 
#### ---- 5.3 Collaboration      ####
# - - - - - - - - - - - - - - - - - - 



collab_ME_adj <- read.csv("./Data/Colaboracion_ME.csv", header = TRUE, row.names = 1) 

collab_ME <- graph_from_adjacency_matrix(as.matrix(collab_ME_adj), mode = "directed",diag = FALSE, add.rownames = "name", weighted = TRUE)



###### ---- Global indexes   ######

collab_ME
#IGRAPH ba6c21d DNW- 56 170 -- 
#  + attr: name (v/c), weight (e/n)

class(collab_ME) # In igraph, networks are objects of class "igraph"
V(collab_ME)
attr_nodes_ME$type


vcount(collab_ME)
ecount(collab_ME) 
edge_density(graph = collab_ME, loops = FALSE) 
ecount(collab_ME) / (vcount(collab_ME)*(vcount(collab_ME)-1)) 

dist_collab_ME <- distances(graph = collab_ME)  
dist_collab_ME
mean_distance(graph = collab_ME) #average Path Length
diameter(graph = collab_ME)
max(dist_collab_ME)

transitivity(collab_ME) 

sort(degree(collab_ME_adj))
grado_collab_ME <- degree(collab_ME_adj)
sort(grado_collab_ME)
mean(grado_collab_ME)
hist(grado_collab_ME) 


###### ---- Centrality indexes   ####


collab_ME_centrality <- data.frame(Grado = degree(collab_ME_adj), Eigencentralidad = eigen_centrality(collab_ME, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(collab_ME_adj), Intermediacion = betweenness(collab_ME_adj))

write.csv(collab_ME_centrality, "Colaboracion.ME_centrality_values.csv")



###### ---- Plotting the network  ####


V(collab_ME)$type <- attr_nodes_RBSK$type 

colores <- c("OSC" = "blue", "CONANP" = "brown", "CONAPESCA" = "brown", "INAPESCA" = "brown", "SEMAR" = "brown", "CECIMS" = "darkgoldenrod1", "SCPP.CZ" = "bisque", "SCPP.JMA" = "black", "SCPP.VCH" = "purple", "PA" = "purple", "ME" = "bisque", "ACADEMY" = "cyan", "FUND" = "chartreuse4", "PH" = "coral" ) 

collab_ME_plot <- ggraph(colab_RBSK, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1.2) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in Punta Allen - Collaboration")

collab_ME_plot


set.seed(123)
ggraph(collab_ME, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()

#layout_with_mds(tipo_RBSK)




# - - - - - - - - - - - - - - - - - - 
#### ---- 5.4 Confianza         ####
# - - - - - - - - - - - - - - - - - - 



conf_conf_ME_adj <- read.csv("./Data/Confianza_ME.csv", header = TRUE, row.names = 1) 

conf_ME <- graph_from_adjacency_matrix(as.matrix(conf_ME_adj), mode = "directed",
                                         diag = FALSE, add.rownames = "name",       
                                         weighted = TRUE)


###### ---- Global indexes   ######


conf_ME
#IGRAPH 92f8ec8 DNW- 56 177 -- 
#+ attr: name (v/c), weight (e/n)

attr_nodes_ME$type

vcount(conf_ME)
ecount(conf_ME) 
edge_density(graph = conf_ME, loops = FALSE) 
ecount(conf_ME) / (vcount(conf_ME)*(vcount(conf_ME)-1)) 

dist_conf_ME <- distances(graph = conf_ME)  
dist_conf_ME
mean_distance(graph = conf_ME) #average Path Length
diameter(graph = conf_ME)
max(dist_conf_ME)

transitivity(conf_ME) 

sort(degree(conf_ME_adj))
grado_conf_ME <- degree(conf_ME_adj)
sort(grado_conf_ME)
mean(grado_conf_ME)
hist(grado_conf_ME) 


###### ---- Centrality indexes   ####


conf_ME_centrality <- data.frame(Grado = degree(conf_ME_adj), Eigencentralidad = eigen_centrality(conf_ME, directed = FALSE, scale = FALSE)$vector, Cercania = closeness(conf_ME_adj), Intermediacion = betweenness(conf_ME_adj))

write.csv(conf_ME_centrality, "Confianza.ME_centrality_values.csv")




###### ---- Plotting the network  ####


V(conf_ME)$type <- attr_nodes_ME$type 

colores <- c("OSC" = "blue", "CONANP" = "brown", "CONAPESCA" = "brown", "INAPESCA" = "brown", "SEMAR" = "brown", "CECIMS" = "darkgoldenrod1", "SCPP.CZ" = "bisque", "SCPP.JMA" = "black", "SCPP.VCH" = "purple", "PA" = "purple", "ME" = "bisque", "ACADEMY" = "cyan", "FUND" = "chartreuse4", "PH" = "coral" ) 

trust_ME_plot <- ggraph(conf_ME, layout = "fr") + 
  geom_edge_link(color = "grey", alpha = 1.2) + 
  geom_node_point(aes(color = type), size = 4) +
  scale_color_manual(values = colores) +
  theme_void() +
  labs(title = "Social Analysis Network in Maria Elena - Trust")

trust_ME_plot


set.seed(123)
ggraph(conf_ME, layout = 'dendrogram') + 
  geom_edge_elbow() +
  geom_node_label(aes(label = name), fill = "lightblue") +
  theme_void()

#layout_with_mds(tipo_RBSK)






############################################################################## #
###                 6.   PLOTS                                              ====
############################################################################## #

com_tipo_RBSK_plot
com_frec_RBSK_plot
collab_RBSK_plot
conf_RBSK_plot

com_type_PA_plot
com_frec_PA_plot
collab_PA_plot
trust_PA_plot

com_frec_ME_plot
com_type_ME_plot
collab_ME_plot
trust_ME_plot
