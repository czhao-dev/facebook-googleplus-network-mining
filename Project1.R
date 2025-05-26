library("igraph")

# QUESTION 1

# data file path below could be different in different PC and may need to be changed
g <- read.graph("H:/Courses_UCLA/232E/project1/facebook_combined.txt", format=c("edgelist"), directed = FALSE)
is.connected(g)
diameter(g)

deg_g <- degree(g)

h <- hist(deg_g, breaks=seq(min(deg_g),max(deg_g)), xlab="Degree of Nodes", main="Degree Distribution")

deg_x <- h$breaks
deg_x <- deg_x[1:(length(deg_x)-1)]
deg_y <- h$counts

plot(deg_x,deg_y, ylab="Node Count", col="black", xlab="Degree of Nodes", main="Degree Distribution")

# Exopnential Model
model_1 <- nls(deg_y ~ exp(a+b*deg_x), start=list(a=0,b=0))
model_1_fit <- data.frame(x2 = seq(min(deg_x),max(deg_x)))
deg_y_1 <- predict(model_1, newdata = model_1_fit)

# Reciprocal Model
model_2 <- nls(deg_y ~ a/deg_x+b, start=list(a=0,b=0))
model_2_fit <- data.frame(x3 = seq(min(deg_x),max(deg_x)))
deg_y_2 <- predict(model_2, newdata = model_2_fit)

lines(model_1_fit$x2, deg_y_1, col='green', lwd=3)
lines(model_2_fit$x3, deg_y_2, ylim=c(0, 200), col='red', lwd=3)

# Calculate MSE
mse <- 0
for (i in 1:length(deg_y_1)){
  mse <- mse + (deg_y[i]-deg_y_1[i])^2
}
mse <- mse/length(deg_y_1)

# Average Degree
ave_deg <- sum(deg_g)/length(deg_g)

##########################################################################################################################
##########################################################################################################################
# QUESTION 2

# Consider whether this network is directed or undirected
# Facebook might be undirected

# node = 1 is performed for node 1
# order = 1 is node 1 plus its immediate neighbors
node1_scope = neighborhood(g, nodes = 1, order = 1)
print(node1_scope)

# Creat the graph of persenal network of node 1
pnn_1 = induced_subgraph(g, vids = unlist(node1_scope), impl = "auto")
print(pnn_1)

# Total nodes of this graph 
pnn_1_nodes_count = vcount(pnn_1)
print(pnn_1_nodes_count)

# Total edges of this graph 
pnn_1_edges_count = ecount(pnn_1)
print(pnn_1_edges_count)

# Network plotting
pnn_1$names = sort(unlist(node1_scope))
node_clr = rep("cornflowerblue", pnn_1_nodes_count)
node_size = rep(3, pnn_1_nodes_count)
node_clr[pnn_1$names == 1] = "red"
node_size = 5

plot(pnn_1, vertex.size = node_size, vertex.color = node_clr, vertex.label=NA)


###########################################################################################################################
###########################################################################################################################
# QUESTION 3

core_nodes <- c()          
neighbor_num <- list()
degrees <- list()
j <- 1
for(i in 1:length(V(g))){
  n <- length(neighbors(g, i, mode=c("all")))
  if(n > 200){
    print(j)
    core_nodes <- c(core_nodes,i)          
    neighbor_num[[j]] <- n
    degrees[[j]] <- degree(g, i)
    j <- j+1
  }
}
neighbor_num <- unlist(neighbor_num)
length(neighbor_num)
ave_deg_cores <- mean(unlist(degrees))

# Finding community structure
# Fast-Greedy Algorithm
fgc <- fastgreedy.community(pnn_1)
plot(fgc, pnn_1, vertex.size=5, vertex.label=NA)

# Edge-Betweenness Algorithm
ebc <- edge.betweenness.community(pnn_1)
plot(ebc, pnn_1, vertex.size=5, vertex.label=NA)

# Infomap Algorithm
imc <- infomap.community(pnn_1)
plot(imc, pnn_1, vertex.size=5, vertex.label=NA)


###########################################################################################################################
###########################################################################################################################
# QUESTION 4

pnn_1_removed_core <- delete.vertices(pnn_1, 1)

# Fast-Greedy Algorithm
fgc2 <- fastgreedy.community(pnn_1_removed_core)
plot(fgc2, pnn_1_removed_core, vertex.size=5, vertex.label=NA)

# Edge-Betweenness Algorithm
ebc2 <- edge.betweenness.community(pnn_1_removed_core)
plot(ebc2, pnn_1_removed_core, vertex.size=5, vertex.label=NA)

# Infomap Algorithm
imc2 <- infomap.community(pnn_1_removed_core)
plot(imc2, pnn_1_removed_core, vertex.size=5, vertex.label=NA)


##########################################################################################################################
##########################################################################################################################
# QUESTION 5

tot_embeddedness_arr <- c()
tot_dispersion_arr <- c()
max_embeddedness_id_arr <- c()
max_dispersion_id_arr <- c()
max_dis_emb_id_arr <- c()

for (n in core_nodes){
  print(n)
  node_scope = neighborhood(g, nodes = n, order = 1)
  pnn <- induced_subgraph(g, vids = unlist(node_scope), impl = "auto")
  V(pnn)$name <- seq(1,length(V(pnn)))
  
  # Embeddedness
  pnn_removed_core <- delete.vertices(pnn, which.max(degree(pnn)))
  embeddedness_arr <- degree(pnn_removed_core)
  max_embeddedness_n <- as.numeric(names(which(V(pnn_removed_core)==which.max(embeddedness_arr))))
  
  tot_embeddedness_arr <- c(tot_embeddedness_arr, embeddedness_arr)
  max_embeddedness_id_arr <- c(max_embeddedness_id_arr, max_embeddedness_n)
  
  
  # Dispersion
  pnn_rc_size <- length(V(pnn_removed_core))
  
  dispersion_arr <- c()
  for (i in 1:pnn_rc_size){
    node_i_nb <- neighborhood(pnn_removed_core, nodes = i, order = 1)
    if (length(node_i_nb[[1]])<=2){
      dispersion_arr <- c(dispersion_arr,0)
    } else {
      nb_arr<-names(node_i_nb[[1]])[2:length(node_i_nb[[1]])]
      mutual_friends <- induced_subgraph(g, vids = nb_arr, impl = "auto")
      
      #dt_frinds <- distance_table(mutual_frinds, directed = FALSE)$res
      dt_friends <- distances(mutual_friends, v = V(mutual_friends), to = V(mutual_friends))
      dt_friends[dt_friends==Inf] = 0
      dispersion_i <- sum(dt_friends)
      dispersion_arr <- c(dispersion_arr,dispersion_i)
    }
  }
  max_dispersion_n <- as.numeric(names(which(V(pnn_removed_core)==which.max(dispersion_arr))))
  
  tot_dispersion_arr <- c(tot_dispersion_arr, dispersion_arr)
  max_dispersion_id_arr <- c(max_dispersion_id_arr, max_dispersion_n)
  
  # Dispersion/Embeddedness
  dis_emb_arr <- dispersion_arr/embeddedness_arr
  max_dis_emb_n <- as.numeric(names(which(V(pnn_removed_core)==which.max(dis_emb_arr))))
  max_dis_emb_id_arr <- c(max_dis_emb_id_arr, max_dis_emb_n)
}

hist(tot_embeddedness_arr,breaks = 100, main = 'Embeddedness Distribution', xlab = 'Embeddedness', ylab = 'Frequency')
hist(tot_dispersion_arr,breaks = 100, main = 'Dispersion Distribution', xlab = 'Dispersion', ylab = 'Frequency')


# Plot 3 community plots
plot_personalnetwork<-function(node_id, alg, option){
  network_id <- node_id
  node_scope = neighborhood(g, nodes = network_id, order = 1)
  pnn <- induced_subgraph(g, vids = unlist(node_scope), impl = "auto")

  if(alg == 'fastgreedy'){
    comm <- fastgreedy.community(pnn)
  }
  else if(alg == 'infomap'){
    comm <- infomap.community(pnn)
  }
  else if(alg == 'edgebetweenness'){
    comm <- cluster_edge_betweenness(pnn, directed = FALSE)
  }
  
  if(option == 'dispersion'){
    max_id <- max_dispersion_id_arr[which(core_nodes==network_id)]
  }
  else if(option == 'embeddedness'){
    max_id <- max_embeddedness_id_arr[which(core_nodes==network_id)]
  }
  else if(option == 'dispersion/embeddedness'){
    max_id <- max_dis_emb_id_arr[which(core_nodes==network_id)]
  }
  
  left_end <- ends(pnn,E(pnn))[,1]  # edge's left ends
  right_end <- ends(pnn,E(pnn))[,2] # edge's right ends
  
  color_arr_v <- comm$membership
  color_arr_v[max_id] <- "red"
  color_arr_e <- rep("gray", length(left_end))
  width_arr_e <- rep(1, length(left_end))
  
  for(i in 1:length(left_end)){
    if ((left_end[i] == max_id) || (right_end[i] == max_id)){
      color_arr_e[i] <- 10
      width_arr_e[i] <- 10
    }
  }
  plot.igraph(pnn, vertex.size=3, vertex.color=color_arr_v, edge.color=color_arr_e, edge.width=width_arr_e, vertex.label=NA)
}

plot_personalnetwork(349, alg='fastgreedy', option='dispersion')
plot_personalnetwork(349, alg='fastgreedy', option='embeddedness')
plot_personalnetwork(349, alg='fastgreedy', option='dispersion/embeddedness')

plot_personalnetwork(352, alg='edgebetweenness', option='dispersion')
plot_personalnetwork(352, alg='edgebetweenness', option='embeddedness')
plot_personalnetwork(352, alg='edgebetweenness', option='dispersion/embeddedness')

plot_personalnetwork(1491, alg='infomap', option='dispersion')
plot_personalnetwork(1491, alg='infomap', option='embeddedness')
plot_personalnetwork(1491, alg='infomap', option='dispersion/embeddedness')

################################################################################################################################
################################################################################################################################
# QUESTION 6

# Create lists to store different statistical features
# Including: size, density, modularity, clustering coefficient, betweenness, and assortativity
all_size_list <- list()
all_density_list <- list()
all_modularity_list <- list()
all_cc_list <- list()
all_betweenness_list <- list()
all_assortativity_list <- list()

# Loop through all personal networks
# For each personal network, use fastgreedy algorithm to find communities
# For each community, calculate the different statistical features
for(i in 1:length(core_nodes)){
  print(i)
  node_scope <- neighborhood(g, nodes = core_nodes[i], order = 1)
  pnn <- induced_subgraph(g, vids=unlist(node_scope), impl = "auto")
  fgc <- fastgreedy.community(pnn)
  
  size_arr <- c()
  density_arr <- c()
  modularity_arr <- c()
  cc_arr <- c()
  betweenness_arr <- c()
  assortativity_arr <- c()
  
  for(j in 1:length(fgc)){
    if(sizes(fgc)[j] >= 10){
      community_j <- communities(fgc)[[j]]
      community_j_g <- induced_subgraph(g, vids = community_j, impl = "auto")
      
      size_j <- sizes(fgc)[j]
      density_j <- sizes(fgc)[j]/vcount(pnn)
      cc_j <- transitivity(community_j_g)
      modularity_j <- modularity(fastgreedy.community(community_j_g))
      betweenness_j <- mean(betweenness(pnn, v = community_j, directed = FALSE))
      assortativity_j <- assortativity(community_j_g, types1 = unlist(community_j), directed = FALSE)
      
      if((size_j < 10)){
        cc_j <- 0
        betweenness_j <- 0
        assortativity_j <- 0
      }
      size_arr <- c(size_arr, size_j)
      density_arr <- c(density_arr, density_j)
      cc_arr <- c(cc_arr, cc_j)
      modularity_arr <- c(modularity_arr, modularity_j)
      betweenness_arr <- c(betweenness_arr, betweenness_j)
      assortativity_arr <- c(assortativity_arr, assortativity_j )
      
    }
  }
  all_size_list[[i]] <- size_arr
  all_density_list[[i]] <- density_arr
  all_modularity_list[[i]] <- modularity_arr
  all_cc_list[[i]] <- cc_arr
  all_betweenness_list[[i]] <- betweenness_arr
  all_assortativity_list[[i]] <- assortativity_arr
}

# Printing all the calculated statistical features for each community in each personal network
for(i in 1:length(core_nodes)){
  cat('Personal network ', i, ':\n')
  for(j in 1:length(all_density_list[[i]])){
    if(all_size_list[[i]][j] >= 10){
      cat('Community ', j, ': size=', all_size_list[[i]][j], 
          ', den.=', all_density_list[[i]][j], 
          ', modul.=', all_modularity_list[[i]][j], 
          ', clustering coeff.=', all_cc_list[[i]][j],
          ', betweenness=', all_betweenness_list[[i]][j],
          ', assortativity=', all_assortativity_list[[i]][j],'\n')
    }
  }
}

# Define a 'type1' community with the maximum betweenness and a high density
# Define a 'type2' community with the maximum clustering coefficient and a small modularity
# Search through all communities and categorize them into 'type1' and 'type2' communities
# Print out the community types
type1_comm <- c()
type2_comm <- c()
num_comm <- c()
for(i in 1:length(core_nodes)){
  cat('Personal network ', i, ':\n')
  comm_count <- 0
  for(j in 1:length(all_density_list[[i]])){
    if(all_size_list[[i]][j] >= 10){
      if(!is.na(all_assortativity_list[[i]][j]) && all_assortativity_list[[i]][j] != 0){
        if(j==which.max(all_betweenness_list[[i]]) && all_density_list[[i]][j] > 0.5*mean(all_density_list[[i]])){
          type1_comm <- c(type1_comm, j)
          cat('Community ', j, 'is type1', '\n')
        }
        if((is.na(all_cc_list[[i]][j]) || j==which.max(all_cc_list[[i]])) && all_modularity_list[[i]][j] < 1.5*mean(all_modularity_list[[i]])){
          type2_comm <- c(type2_comm, j)
          cat('Community ', j, 'is type2', '\n')
        }
        comm_count <- comm_count + 1
      }
    }
  }
  cat('\n')
  num_comm <- c(num_comm, comm_count)
}

##############################################################################################################################
##############################################################################################################################
# QUESTION 7

# Change the working directory
# Get the names of all .edges and .circles files
setwd("H:/Courses_UCLA/232E/project1/gplus/")
edgesFiles <- dir(pattern="*.edges")
circlesFiles <- dir(pattern="*.circles")

# Read .edges files, create graphs from each .edges file, and store the graphs in a list
g_list <- list()
for(i in 1:length(edgesFiles)){
  print(i)
  g_list[[i]] <- read_graph(edgesFiles[i], format="ncol", directed=TRUE)
}

# Read .circles files, manually parse them, and store them into a list
circles_list <- list()
num_circles <- 1
for(i in 1:length(circlesFiles)){
  print(circlesFiles[i])
  con <- file(circlesFiles[i], "rt")
  
  n <-  1
  circle_i <- list()
  while(TRUE){
    l <- readLines(con, 1)
    if(length(l)==0){
      break
    }
    circle_i[[n]] <- unlist(strsplit(l, "[\t]"))[-1]
    n <- n + 1
  }
  circles_list[[num_circles]] <- circle_i
  num_circles <- num_circles + 1
  close(con)
}

# Search for users' IDs with more than 2 circles
circle_2_id <- c()
for(i in 1:length(circlesFiles)){
  circle_num <- length(circles_list[[i]])
  if (circle_num>2){
    circle_2_id <- c(circle_2_id, i)
  }
}

# Compute the overlapping number of the user's circles and communities
# alg is the selected community finding algorithm, can be 'walktrap' or 'infomap'
compute_overlaps <- function(alg){
  overlap_mat_list <- list()
  for(n in 1:length(circle_2_id)){
    print(n)
    person_id <- circle_2_id[n]
    gn <- g_list[[person_id]]
    circle_n <- circles_list[[person_id]]
    
    if(alg == 'walktrap'){
      community <- cluster_walktrap(gn)
    }
    else if(alg == 'infomap'){
      community <- infomap.community(gn)
    }
    
    overlap_mat <- list()
    for(i in 1:length(circle_n)){
      overlap_arr <- c()
      for(j in 1:length(community)){
        a <- length(intersect(unlist(circle_n[i]), unlist(community[j])))
        overlap_arr <- c(overlap_arr, a)
      }
      overlap_mat[[i]] <- overlap_arr
    }
    overlap_mat_list[[n]] <- overlap_mat
  }
  overlap_mat_list
}

walktrap_overlap_mat_list <- compute_overlaps(alg = 'walktrap')
infomap_overlap_mat_list <- compute_overlaps(alg = 'infomap')

# Calculate the match scores between the true labels (Google+ circles) and proposed labels (communities)
calculate_match_score <- function(network_index, temp_overlap_mat_list){
  temp_overlap_mat <- temp_overlap_mat_list[[network_index]]
  score_arr <- c()
  for(i in 1:length(temp_overlap_mat)){
    score <- max(temp_overlap_mat[[i]])/sum(temp_overlap_mat[[i]])
    score_arr <- c(score_arr, score)
  }
  mean(score_arr)
}

# Calculate match scores of communities found by walktrap algorithm for each user
walktrap_match_scores <- c()
for(n in 1:length(circle_2_id)){
  s <- calculate_match_score(n, walktrap_overlap_mat_list)
  walktrap_match_scores <- c(walktrap_match_scores, s)
  print(s)
}

# Calculate match scores of communities found by infomap algorithm for each user
infomap_match_scores <- c()
for(n in 1:length(circle_2_id)){
  s <- calculate_match_score(n, infomap_overlap_mat_list)
  infomap_match_scores <- c(infomap_match_scores, s)
  print(s)
}
