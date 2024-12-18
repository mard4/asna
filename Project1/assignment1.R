library(igraph) 
library(sna)
library(ggplot2)

setwd("C:/01_Learning/01_Data_science/01_University/01_UniTrento/01_Classes/Semester/3/Advanced_social_network_analysis/Exam/assignment_1/")
#importing the data
load("Borgatti_Scientists504.RDA")
#the network
network_info <- Borgatti_Scientists504[["NetworkInfo"]] 
#the attribute file
attributes <- Borgatti_Scientists504[["Attributes"]]  


################################################################

collaboration <- Borgatti_Scientists504$Collaboration

# Assuming 'network_info' is your adjacency matrix
collaboration_matrix <- as.matrix(collaboration)

# PART 1
################################################

# Apply the cutoff
collaboration_cutoff <- collaboration_matrix
collaboration_cutoff[collaboration_cutoff <= 3] <- 0

# Convert to binary adjacency matrix
collaboration_binary <- collaboration_cutoff
collaboration_binary[collaboration_binary > 0] <- 1

# Create the cutoff graph
g_cutoff <- igraph::graph_from_adjacency_matrix(
  collaboration_binary, 
  mode = "undirected", 
  weighted = FALSE, 
  diag = FALSE
)

# Set seed to obtain always the same plot
set.seed(42)

plot(
  g_cutoff, 
  layout = layout_with_fr, 
  vertex.label = NA, 
  vertex.size = 5,        # Smaller node size
  vertex.shape = "sphere" # Interesting shape (e.g., "sphere", "circle", "square")
)

# PART 2
###################################

attributes$DeptGroup <- ifelse(attributes$DeptID %in% c(1, 2, 5), 1, 0)
igraph::V(g_cutoff)$DeptGroup <- attributes$DeptGroup[match(igraph::V(g_cutoff)$name, attributes$NodeName)]

igraph::V(g_cutoff)$color <- ifelse(igraph::V(g_cutoff)$DeptGroup == 1, "green", "orange")

# Plot the network with vertices colored by DeptGroup
plot(
  g_cutoff, 
  vertex.label = NA,  
  vertex.size = 5,
  vertex.shape = 'sphere',
  vertex.color = igraph::V(g_cutoff)$color,      # Color based on DeptGroup
  edge.color = "gray",                           # Gray edges
  main = "Collaboration Network by DeptGroup", 
  layout = layout_with_fr(g_cutoff)              # Use Fruchterman-Reingold layout
)

legend("bottomleft", 
       legend = c("Others", "Management+Economics+Behavioural"), 
       col = c("orange", "green"), 
       pch = 19, 
       pt.cex = 1.5,
       cex = 0.9,
       bty = "n")

# PART 3
############################

# Compute position measures  
beta_value <- 0.1 
igraph::V(g_cutoff)$BetaCentrality <- power_centrality(g_cutoff, 
                                                       nodes = V(g_cutoff),   
                                                       exponent = beta_value, ) 
igraph::V(g_cutoff)$Betweenness <- igraph::betweenness(g_cutoff)  

# Create position data frame  

position_df <- data.frame(DeptGroup = igraph::V(g_cutoff)$DeptGroup, 
                          BetaCentrality = igraph::V(g_cutoff)$BetaCentrality,                           
                          Betweenness = igraph::V(g_cutoff)$Betweenness)  

# Identify nodes with highest position measures  

max_beta_centrality <- max(igraph::V(g_cutoff)$BetaCentrality)  
nodes_max_beta_centrality <- igraph::V(g_cutoff)$name[igraph::V(g_cutoff)$BetaCentrality == max_beta_centrality]  
cat("Node(s) with highest BetaCentrality Centrality:", nodes_max_beta_centrality, "\n")  
cat("BetaCentrality Centrality Value:", max_beta_centrality, "\n")    

max_Betweenness <- max(igraph::V(g_cutoff)$Betweenness)  
nodes_max_Betweenness <- igraph::V(g_cutoff)$name[igraph::V(g_cutoff)$Betweenness == max_Betweenness]  
cat("Node(s) with highest max_Betweenness Centrality:", nodes_max_Betweenness, "\n")  +
cat("max_Betweenness Centrality Value:", max_Betweenness, "\n\n")

ggplot(position_df, aes(x = as.factor(DeptGroup), y = BetaCentrality, fill = as.factor(DeptGroup))) +
  geom_boxplot(color = "black") +
  theme_minimal() +
  labs(
    title = "Beta Centrality by Department Group",
    x = "Department Group",
    y = "Beta Centrality",
    fill = "Department Group"
  ) + 
  scale_fill_manual(
    values = c("Orange", "Green"),
    labels = c("Others", "Management+Economics+Behavioural")
  ) + 
  scale_x_discrete(
    labels = c("0" = "Others", "1" = "Management+Economics+Behavioural")  # Define x-tick labels
  )

ggplot(position_df, aes(x = as.factor(DeptGroup), y = Betweenness, fill = as.factor(DeptGroup))) +
  geom_boxplot(color = "black") +
  theme_minimal() +
  labs(
    title = "Betweenness by Department Group",
    x = "Department Group",
    y = "Betweenness",
    fill = "Department Group"
  ) + 
  scale_fill_manual(
    values = c("Orange", "Green"),
    labels = c("Others", "Management+Economics+Behavioural")
  ) +
  scale_x_discrete(
    labels = c("0" = "Others", "1" = "Management+Economics+Behavioural")  # Define x-tick labels
  )

# PART 4
############################
# Correlate Betweenness and BetaCentrality 
correlation <- cor.test(position_df$Betweenness, position_df$BetaCentrality)
print("Correlation between Betweenness and BetaCentrality Centrality:")
print(correlation)  

# PART 5
############################
# Correlate DeptGroup with position measures 
cor_dept_betweenness <- cor.test(position_df$DeptGroup, position_df$Betweenness) 
print("Correlation between DeptGroup and Betweenness:") 
print(cor_dept_degree)  
cor_dept_betacentrality <- cor.test(position_df$DeptGroup, position_df$BetaCentrality) 
print("Correlation between DeptGroup and BetaCentrality:") 
print(cor_dept_betacentrality)

######### PERMUTATION TEST ##########

# Permutation Test for DeptGroup vs. Betweenness
n_permutations <- 10000

# Compute the observed correlation
observed_correlation_dept_betw <- cor(position_df$DeptGroup, position_df$Betweenness)

set.seed(42)
permuted_correlations_dept_betw <- numeric(n_permutations)

for (i in 1:n_permutations) {
  # Shuffle one of the variables, e.g., Betweenness
  permuted_betw <- sample(position_df$Betweenness)
  permuted_correlations_dept_betw[i] <- cor(position_df$DeptGroup, permuted_betw)
}

p_value_dept_betw <- mean(abs(permuted_correlations_dept_betw) >= abs(observed_correlation_dept_betw))

cat("Observed correlation (DeptGroup vs. Betweenness):", observed_correlation_dept_betw, "\n")
cat("P-value from permutation test:", p_value_dept_betw, "\n")

hist(permuted_correlations_dept_betw, 
     main = "Permutation Test for Correlation (DeptGroup vs. Betweenness)",
     xlab = "Correlation", col = "lightblue", border = "white")
abline(v = observed_correlation_dept_betw, col = "red", lwd = 2)
legend("topright", legend = "Observed Correlation", col = "red", lwd = 2, cex = 0.8, pt.cex = 0.2, bty = "n")

# Permutation Test for DeptGroup vs. BetaCentrality
n_permutations <- 10000

# Compute the observed correlation
observed_correlation_dept_beta <- cor(position_df$DeptGroup, position_df$BetaCentrality)

set.seed(42)
permuted_correlations_dept_beta <- numeric(n_permutations)

for (i in 1:n_permutations) {
  # Shuffle BetaCentrality this time
  permuted_beta <- sample(position_df$BetaCentrality)
  permuted_correlations_dept_beta[i] <- cor(position_df$DeptGroup, permuted_beta)
}

p_value_dept_beta <- mean(abs(permuted_correlations_dept_beta) >= abs(observed_correlation_dept_beta))

cat("Observed correlation (DeptGroup vs. BetaCentrality):", observed_correlation_dept_beta, "\n")
cat("P-value from permutation test:", p_value_dept_beta, "\n")

hist(permuted_correlations_dept_beta, 
     main = "Permutation Test for Correlation (DeptGroup vs. BetaCentrality)",
     xlab = "Correlation", col = "lightblue", border = "white")
abline(v = observed_correlation_dept_beta, col = "red", lwd = 2)
legend("topleft", legend = "Observed Correlation", col = "red", lwd = 2, cex = 0.8, pt.cex = 0.2, bty = "n")

# CONSIDERATIONS:
# - Betweenness centrality identifies scientists who act as "bridges" or intermediaries between different groups or clusters 
#   in the network.
# - beta centrality not correlated with department group: Scientists’ influence in the network 
#   (as measured by their direct and indirect connections) is not restricted to or dominated by their 
#   departmental group. Collaborations likely transcend departmental boundaries, and a scientist's structural 
#   importance depends more on their position in the overall network rather than their departmental affiliation. 
#   It might also suggest that departmental groupings are not a strong factor in collaboration patterns, 
#   indicating a culture of cross-group interaction or research driven by non-departmental factors 
#   (e.g., shared projects, funding sources, or individual preferences).
# - degree correlated with department group: While the effect size (correlation) is small, it indicates that 
#   departmental affiliation influences how many direct collaborations scientists have.
# - While direct collaborations (degree centrality) are slightly associated with departmental group, the 
#   broader influence (beta-centrality) is not. This could suggest that while scientists tend to collaborate 
#   more within their department, the network's global structure is not strongly shaped by departmental 
#   boundaries. The weak and significant correlation of degree centrality with department suggests potential 
#   siloing in direct collaborations. The lack of correlation for beta-centrality implies that structural
#   importance and influence are driven by broader network factors rather than departmental affiliation.