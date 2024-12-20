setwd("C:/01_Learning/01_Data_science/01_University/01_UniTrento/01_Classes/Semester/3/Advanced_social_network_analysis/Exam/assignment_2")


### ================== Read dataset ================== ###
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)
attributes <- readxl::read_excel('Data_Gender_4.xlsx')
print(str(attributes))

EIES_T2<-read.csv("EIES_t2.csv",row.names=1)
EIES_T2F<-EIES_T2>2

### ================== Part I ================== ###
### Run three models using the cutoff friends (or higher)
library(sna)
if (!requireNamespace("ergm", quietly = TRUE)) {
  install.packages("ergm")
}
library(ergm)
library(e1071)

## mail ok 
par(mar=c(1,1,1,1))
par(mfrow=c(1,1))
gplot(EIES_T2F)
par(mfrow=c(4,2))
EIES_T2F_n<-as.network(EIES_T2F)
## mail ok 

head(EIES_T2F)
head(attributes)

EIES_Model1<-ergm(EIES_T2F_n ~ edges+mutual
                  +gwidegree(decay=.3, fixed=TRUE) +gwodegree(decay=.3, fixed=TRUE)
                  +dgwesp(type="OTP", decay=.5, fixed=TRUE)
                  +dgwesp(type="ITP", decay=.5, fixed=TRUE),
                  control=control.ergm(seed=102, MCMC.runtime.traceplot=TRUE), 
                  verbose=TRUE)

set.vertex.attribute(EIES_T2F_n, "Gender", attributes$Gender)
EIES_Model2<-ergm(EIES_T2F_n ~ edges+mutual
                  +nodeicov("Gender")+nodeocov("Gender")+nodematch("Gender"), 
                  control=control.ergm(seed=102, MCMC.runtime.traceplot=TRUE), 
                  verbose=TRUE)
EIES_Model3<-ergm(EIES_T2F_n ~ edges+mutual
                  +gwidegree(decay=.3, fixed=TRUE) +gwodegree(decay=.3, fixed=TRUE)
                  +dgwesp(type="OTP", decay=.5, fixed=TRUE)
                  +dgwesp(type="ITP", decay=.5, fixed=TRUE)
                  +nodeicov("Gender")+nodeocov("Gender")+nodematch("Gender"), 
                  control=control.ergm(seed=102, MCMC.runtime.traceplot=TRUE), 
                  verbose=TRUE)

# edges: baseline density of edges in the network. It models the probability of any tie 
# existing, controlling for other terms
# mutual: models reciprocity in the network (accounts for the likelihood of ties being bidirectional)
# gwidegree (geometrically weighted):Models the distribution of in-degrees
# gwodegree (geometrically weighted): Models the distribution of out-degrees
## decay=.3: how much higher-order degrees are weighted
## fixed=TRUE: Indicates the decay parameter is fixed and not estimated during model fitting.
# dgwesp: Models shared partner effects, representing how ties are more likely when two nodes 
#         share many partners.
## type="OTP": Outgoing two-paths, capturing shared partnerships formed through outgoing ties
## type="ITP": Incoming two-paths, capturing shared partnerships formed through incoming ties.
## decay=.5: Specifies the decay parameter for weighting higher-order shared partner configurations
## fixed=TRUE: Indicates the decay parameter is not estimated but held constant.
# control.ergm(seed=102, MCMC.runtime.traceplot=TRUE)
## Sets a seed for reproducibility of results.
## Produces trace plots to visualize Markov Chain Monte Carlo (MCMC) diagnostics during estimation.
# influence of node-level attributes on the network's structure
## nodeicov("Gender"): relationship between a node's in-degree and its value for the attribute "Gender"
##                     Captures whether nodes with certain values for "Gender" tend to receive more ties in the network.
## nodeocov("Gender"): relationship between a node's out-degree and its value for the attribute "Gender"
##                     Captures whether nodes with certain values for "Gender" tend to send more ties in the network.         
## nodematch("Gender"): tendency of nodes to form ties with others that have the same value for the attribute
##                      Measures homophily, the tendency for similar nodes (same gender) to connect 

# Present the results in a table

### ================== Part II ================== ###
### Part II: Discuss the results focusing in particular on Model 3 
# (with structural and attribute effects). Only discuss Models 1 and 2 
# where there are differences (an effect became significant in Model 3 
# when it was not in Models 1 or 2, or in Model 3 it became non-significant when it 
# was in Models 1 and 2). 
# Discuss all parameters of Model 3 substantively even if they are not 
# significant taking into account that this is a friendship network. 
# The evaluation will consider whether you showed that you understand the effects in the case of friendship.



### ================== Part IIi ================== ###
### Part 3. Discuss the Goodness of Fit for Model 3 
# (see the example script from the section which is for Model 1 however: 
# “#Goodness of Fit part (**here for EIES_Model1**)”. 
# Discuss the different parts – histograms, trace plots, boxplots.
# Draw a conclusion Are you satisfied with the model? Why (not)?
                                                   
gof.choices<-control.gof.ergm(nsim=2000)
# number of simulated networks to generate to compare against the observed network
# simulations will estimate how well the fitted model reproduces the observed network's structural features
EIES_Model3sim2000<-gof(EIES_Model3, 
                        GOF=~model+idegree+odegree+distance+triadcensus, 
                        control=gof.choices)
# GOF
## GoF test evaluates whether the model can also reproduce other key features of the network, such as degree distributions, 
## geodesic distances, and triadic structures, even if these features weren't explicitly modeled. 
## The fitted ERGM generates networks based on the model's parameters (see ergm function)
## The GoF test compares the structural statistics of these simulated networks to those of the observed network
## This ensures that the model is not only theoretically well-fitted but also capable of generating networks that 
## closely resemble the observed one
## A well-fitting model not only supports your theoretical understanding of the network's formation process but also 
## ensures that the model is capable of generating realistic networks

## gof parameters:
### model: Checks whether the simulated networks match the terms of the fitted model
### idegree: Compares the in-degree distribution of the observed network and simulated networks
### odegree: Compares the out-degree distribution
### distance: Evaluates the geodesic (shortest path) distance distribution
### triadcensus: Compares the triad census (types of three-node subgraphs) in the observed and simulated networks
### inluding these unmodeled featues in the GoF ensures that the model is evaluated on how well it captures 
### broader structural patterns in the network

EIES_Model3sim2000$summary.model
# Goodness-of-Fit (GoF) summary statistics for the model terms, comparing the observed values in the network to the 
# values generated by the simulated networks based on the model
## obs: observed value of the statistic in the original network
## min, mean, max: stats across all simulated networks
## MC p-value: Monte Carlo p-value, which indicates how well the observed value matches the distribution of simulated values.
##             calculated as the proportion of simulated values that are as extreme as or more extreme than the observed value
##             A value close to 1.000 suggests that the model fits the data well for that term
## The model successfully reproduces the observed network's key structural features, including edge density, 
## reciprocity, degree distributions, shared partners, and the influence of Gender.
## If there were terms with low MC p-values (e.g., < 0.05), they would indicate poor fit, suggesting the need for 
## additional terms in the model. In this case, no adjustments seem necessary.

## Plot histograms for all variables in GoF

par(mfrow=c(3,3), mar=c(4,4,2,1))

hist(EIES_Model3sim2000$sim.model[,1] + 0.01, nclass=20, main=paste("Edges distribution"), probability=TRUE, xlab=NA, ylab="Density")
abline(v=EIES_Model3sim2000$summary.model[1,1], col="red", lwd=3)
abline(v=EIES_Model3sim2000$summary.model[1,3], col="blue", lwd=3, lty=2)
legend("topleft", legend=c("Observed Mean", "Simulation Mean"), col=c("red", "blue"), 
       lwd=3, lty=c(3, 3), bty="n", cex=1.1, x.intersp=0.1, y.intersp=0.5)

hist(EIES_Model3sim2000$sim.model[,2] + 0.01, nclass=20, main=paste("Mutual distribution"), probability=TRUE, xlab=NA, ylab=NA)
abline(v=EIES_Model3sim2000$summary.model[2,1], col="red", lwd=3)
abline(v=EIES_Model3sim2000$summary.model[2,3], col="blue", lwd=3, lty=2)

hist(EIES_Model3sim2000$sim.model[,3] + 0.01, nclass=20, main=paste("gw-indegree distribution"), probability=TRUE, xlab=NA, ylab=NA)
abline(v=EIES_Model3sim2000$summary.model[3,1], col="red", lwd=3)
abline(v=EIES_Model3sim2000$summary.model[3,3], col="blue", lwd=3, lty=2)

hist(EIES_Model3sim2000$sim.model[,4] + 0.01, nclass=20, main=paste("gw-outdegree distribution"), probability=TRUE, xlab=NA, ylab="Density")
abline(v=EIES_Model3sim2000$summary.model[4,1], col="red", lwd=3)
abline(v=EIES_Model3sim2000$summary.model[4,3], col="blue", lwd=3, lty=2)

hist(EIES_Model3sim2000$sim.model[,5] + 0.01, nclass=20, main=paste("Shared partner effects distribution (outgoing, gwesp.OTP)"), probability=TRUE, xlab=NA, ylab=NA)
abline(v=EIES_Model3sim2000$summary.model[5,1], col="red", lwd=3)
abline(v=EIES_Model3sim2000$summary.model[5,3], col="blue", lwd=3, lty=2)

hist(EIES_Model3sim2000$sim.model[,6] + 0.01, nclass=20, main=paste("Shared partner effects distribution (incoming, gwesp.ITP)"), probability=TRUE, xlab=NA, ylab=NA)
abline(v=EIES_Model3sim2000$summary.model[6,1], col="red", lwd=3)
abline(v=EIES_Model3sim2000$summary.model[6,3], col="blue", lwd=3, lty=2)

hist(EIES_Model3sim2000$sim.model[,7] + 0.01, nclass=20, main=paste("Gender-indegree distribution (nodeicov)"), probability=TRUE, xlab=NA, ylab="Density")
abline(v=EIES_Model3sim2000$summary.model[7,1], col="red", lwd=3)
abline(v=EIES_Model3sim2000$summary.model[7,3], col="blue", lwd=3, lty=2)

hist(EIES_Model3sim2000$sim.model[,8] + 0.01, nclass=20, main=paste("Gender-outdegree distribution (nodeocov)"), probability=TRUE, xlab=NA, ylab=NA)
abline(v=EIES_Model3sim2000$summary.model[8,1], col="red", lwd=3)
abline(v=EIES_Model3sim2000$summary.model[8,3], col="blue", lwd=3, lty=2)

hist(EIES_Model3sim2000$sim.model[,9] + 0.01, nclass=20, main=paste("Homophily distribution (gendermatch)"), probability=TRUE, xlab=NA, ylab=NA)
abline(v=EIES_Model3sim2000$summary.model[9,1], col="red", lwd=3)
abline(v=EIES_Model3sim2000$summary.model[9,3], col="blue", lwd=3, lty=2)

gw_indegree_skewness <- skewness(EIES_Model3sim2000$sim.model[,3])
gw_odegree_skewness <- skewness(EIES_Model3sim2000$sim.model[,4])

### diagnostic tool to evaluate how well the model reproduces the observed network statistics
### almost all histograms shows that the model 3 is a well-fitted model since it simulates values that reflect the variability observed in the real network
### the spread of the histogram is neither excessively narrow (overfitting) nor wide (underfitting)
### the fact that the observed mean is close to the simulated mean further supports the fact that the model does a good job
### the only exceptions seem to be gw-idegree and gw-odegree which are both notably negatively skewed (skewness:-0.99 and -0.50)
### this implies that the model generates simulated networks where these statistics consistently deviates towards lower values
### which might signal that the model terms do not fully capture the underlying structural processes of the observed network 

# Trace plots
plot(EIES_Model1sim2000$sim.model[,1], type="l", main="Edges trace plot", ylab="", xlab="")
plot(EIES_Model1sim2000$sim.model[,2], type="l", main="Mutual trace plot", ylab="", xlab="")
plot(EIES_Model1sim2000$sim.model[,3], type="l", main="gw-indegree trace plot", ylab="", xlab="")
plot(EIES_Model1sim2000$sim.model[,4], type="l", main="gw-odegree trace plot", ylab="", xlab="")
plot(EIES_Model1sim2000$sim.model[,5], type="l", main="Shared partner effects trace plot (outgoing, gwesp.OTP)", ylab="", xlab="")
plot(EIES_Model1sim2000$sim.model[,6], type="l", main="Shared partner effects trace plot (incoming, gwesp.ITP)", ylab="", xlab="")
plot(EIES_Model1sim2000$sim.model[,7], type="l", main="Gender-indegree (nodeicov) trace plot", ylab="", xlab="")
plot(EIES_Model1sim2000$sim.model[,8], type="l", main="Gender-outdegree (nodeocov) trace plot", ylab="", xlab="")
plot(EIES_Model1sim2000$sim.model[,9], type="l", main="Homophily trace plot (nodematch)", ylab="", xlab="")

## trace plot is a line chart that has:     
### time on the x-axis (draw)
### the values taken by one of the coordinates of the draws on the y-axis
## trace plots can help check the presence of serial correlation (prev values influence future ones) which in MCMC means fewer independent samples
## reducing the efficiency of the sampling process
## all trace plots show no apparent anomalies. The chain seems to explore the sample space many times, the default burn-in values seem to be appropriate
## and there might be only a mild serial correlation. 
## perhaps gw-indegree and gw-odegree being the only exception since the trace plot seems too compact and it might indicate that the sample space is
## not correctly explored. However, it might just be an artifact of the method itself and more rigorous statistical tests are required to further 
## analyse the results. 
## the trace plots seem to be quite stable, aside from gw-indegree which has large spikes downward 
## note that this is also reflected in the histogram of gw-indegree which is clearly left-skewed (smaller sample space explored + "negative" spikes)

# boxplots
par(mfrow=c(2,3))
EIES_Model3sim2000
plot(EIES_Model3sim2000)
boxplot(EIES_Model3sim2000$sim.odeg[,1:11])  # WARNING adjust 1 to 11 if needed

## NOTE: I'm very unsure about what the boxplots represent i.e. Dio cane non si trova neanch un pò di documentazione 
## The black line could be the observed proportion of the statistics over the different values 
## if u print the object "EIES_Model3sim2000" u will see that for example for odegree3 has p-value close to 0.05 bc the observed value is significantly
## different from the simulated one 
## in general the fit of odegree and idegree seems mediocre (as was kind of suggested by the histograms and the traceplots) while it looks very good
## for triad census and ok on minimum geodesic distance
## I think that boxplot() is redundant with the plot at row 1, col 2

# For the triad census – to make it easier to inspect
# A triad census is a count of all the distinct three-node subgraphs (or triads) in a network, categorized according to their structural types. 
# These structural types are labeled using a specific coding scheme.
## triadcensus.003: The empty triad – no edges between the three nodes
## triadcensus.012: A dyadic closure – two nodes are connected, and one is disconnected
## triadcensus.102: A two-path triad – three nodes in a path with no triangles formed
## triadcensus.021D: A directed two-path triad – a directed edge connects the first two nodes, and the last node is connected back to one 
##                   of the earlier nodes (no directed cycle)
## triadcensus.021U: A undirected two-path triad – a two-step path connecting the three nodes in an undirected graph (but this is directed, so doesn't make any sense, does it?)
## triadcensus.021C: A cyclic three-path triad – a cycle of three nodes.
## triadcensus.111D: A directed triangle – all nodes are connected with directed edges in a triangle.
## triadcensus.111U: An undirected triangle – all three nodes are connected in an undirected triangle.
## triadcensus.030T: A three-path – nodes connected in a three-step directed path.
## triadcensus.030C: A three-path with a cycle – a three-step cycle path.
## triadcensus.201: A two-path with one edge forming a closed loop – two nodes are connected with a direct edge and a longer indirect path.
## triadcensus.120D: A directed two-path forming a directed triangle.
## triadcensus.120U: A undirected two-path forming a triangle.
## triadcensus.120C: A cyclic two-path forming a triangle.
## triadcensus.210: A directed triangle.
## triadcensus.300: A directed three-node chain.
# the number associated to each triad if the frequency 

# The histogram 
EIES_Model1sim2000$obs.odeg
par(mfrow=c(4,4))
for (k in 1:16) {
  # Create the histogram with x-axis label suppressed
  hist(EIES_Model1sim2000$sim.triadcensus[,k], 
       main=colnames(EIES_Model1sim2000$sim.triadcensus)[k],
       xlab=NA, ylab=NA)  # Suppress both x and y axis labels
  
  # Add the observed value line (blue dashed)
  abline(v = EIES_Model1sim2000$obs.triadcensus[k], col = "blue", lwd = 3, lty = 2)
  
  if (k == 1){
    legend("topleft", legend=c("Observed Value"), col=c("blue"), 
           lwd=3, lty=3, bty="n", cex=1.1, x.intersp=0.1, y.intersp=0.2)
  }
  
  # If it's one of the specified plots, add the y-axis label
  if (k %in% c(1, 5, 9, 13)) {
    mtext("Frequency", side = 2, line = 2, cex = 0.8)  # Add y-axis label on 1st, 5th, 9th, and 13th plots
  }
}
EIES_Model1sim2000$obs.triadcensus 

## If the blue dashed line is close to the peak of the histogram, it indicates that the observed network's triad count is similar to what is 
## expected in the simulation (i.e., the simulated network matches the observed network in terms of that triad type)
## If the blue dashed line is far from the peak of the histogram (i.e., in the tail or outside the main density), it suggests 
## that the observed network exhibits a triad count that is significantly different from what the simulation predicts, which could 
## indicate a structural anomaly or pattern in the real data that the model doesn't capture well
## are certain types of triads over/under-represented in the simulation?
## what can we say about the single triads?
## if the many types of triads are off then the model is not doing a good job 
## in this case, the only triads that are poorly modeled without a doubt are: 021U, 021C, 111D, 111U, 120D
## pls try to give an interpretation of this

##### 
#NOTES
# 1 - in part-1 we could also comment the values of the coefficients found for EIES_Model1, EIES_Model2 and EIES_Model3 (especially the third one)
#     bc for example they give info about whether there's a tendency to build (positive) or refrain (negative) from building ties
#     see lines 40-48 for an example of interpretation
# 2 - we can also comment on how the coefficients are obtained and what is MCMC mathematically speaking