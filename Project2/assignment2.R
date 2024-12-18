setwd()

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
##                      Measures homophily, the tendency for similar nodes (same gender) to connec
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
EIES_Model1sim2000<-gof(EIES_Model1, 
                        GOF=~model+idegree+odegree+distance+triadcensus, control=gof.choices)
EIES_Model1sim2000$summary.model

hist(EIES_Model1sim2000$sim.model[,1]+.01, nclass=20, main = paste("Histogram of edges"), probability = T, xlab = NA)
abline(v = EIES_Model1sim2000$summary.model[1,1], col = "red", lwd = 3)
abline(v = EIES_Model1sim2000$summary.model[1,3], col = "blue", lwd = 3, lty=2)
######… [MORE HERE…]

plot(EIES_Model1sim2000$sim.model[,1], type="l", main = paste("Trace plot for edges"), ylab="", xlab="")
######à… [MORE HERE…]

plot(EIES_Model1sim2000)
boxplot(EIES_Model1sim2000$sim.odeg[,1:11])  # WARNING adjust 1 to 11 if needed

# For the triad census – to make it easier to inspect
EIES_Model1sim2000$obs.odeg
for (k in 1:16)
{
  hist(EIES_Model1sim2000$sim.triadcensus[,k], main=colnames(EIES_Model1sim2000$sim.triadcensus)[k])
  abline(v = EIES_Model1sim2000$obs.triadcensus[k], col = "blue", lwd = 3, lty=2)
}
EIES_Model1sim2000$obs.triadcensus

