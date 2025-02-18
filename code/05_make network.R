coop <- readRDS('~/OneDrive - University of Exeter/data/grin/cooperators.RDS')
vars <- readRDS('~/OneDrive - University of Exeter/data/grin/developed_deposits.RDS')
vars <- vars[!is.na(vars$coop_id)]
df <- dplyr::left_join(vars, coop)

el <- unique(df[,c('accession', 'coop_id')])
colnames(el) <- c('crop', 'cooperator')
el$crop <- as.character(el$crop)
nl <- coop

# currently el is two mode, need to project the nodelist
library(network)
net2 <- network::network(x = el, 
                             bipartite = T,  
                             directed = F)
mem.maxVSize(vsize = 30000)
net1 <- manynet::to_mode1(net2)


# 1. Create count table where organizations are columns (make your mode of
#    interest the 2nd/y argument in the table)
tbl <- table(el)
# 2. Extract column names -- these are mode 1 names (in our case, orgs)
ids <- colnames(tbl)
# 3. Take the cross-product of the table to get a co-occurence matrix
comat <- crossprod(tbl)
# 4. Assign self-co-occurrences 0
diag(comat) <- 0
#   Check: This should be symmetrical
isSymmetric(comat)
# 5. Make a data frame from the matrix and add ids as the column names
comat <- data.frame(comat)
colnames(comat) <- ids
#.  Check: This should be square
dim(comat)
# 6. Remove those with no co-occurrence at all
comat <- comat[rowSums(comat, na.rm = T) != 0, 
               colSums(comat, na.rm = T) != 0]
# 7. Make co-occur data frame into matrix object
comat <- as.matrix(comat)
# 8. Create a graph so that it can be converted into a weighted edge list
g <- igraph::graph_from_adjacency_matrix(comat, weighted = T, mode = 'undirected')
el_proj <- igraph::get.data.frame(g)
order <- unique(c(unique(el_proj$from), unique(el_proj$to)))

#keep <- which(nl$coop_id %in% order)
#nl <- nl[keep,]
check <- match(order, nl$coop_id)
iso <- nl$coop_id[!(1:nrow(nl) %in% check)]
keep <- as.numeric(c(check, iso))
nl <- nl[keep,]
table(nl$coop_id[1:length(order)] == order)
net <- network(el_proj, vertex.attr = nl, directed = F)
net
head(net %v% 'vertex.names')
head(net %v% 'country')

library(ggraph)
ggraph(net, layout = 'stress') +
  geom_edge_link(color = "gray80") +
  geom_node_point(aes(color = country), alpha = 0.2) +
  theme_void() +
  theme(legend.position = "none")
