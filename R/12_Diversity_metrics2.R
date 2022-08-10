# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# First version 2022-08-03
# Class 13: Diversity metrics 2: From taxonomic to functional and phylogenetic diversity
# --------------------------------------------------#
#Load packages
library(vegan)
library(ggplot2)

Community.A <- c(10, 6, 4, 1)
Community.B <- c(17, rep(1, 7))

diversity(Community.A, "shannon")
diversity(Community.B, "shannon")
diversity(Community.A, "invsimpson")
diversity(Community.B, "invsimpson")
#inv Simpson gets the difference, but Shannon doesn't

?renyi #curves that provide information on species richness and equability
ren_comA <- renyi(Community.A)
ren_comB <- renyi(Community.B)

ren_AB <- rbind(ren_comA, ren_comB)
ren_AB
#plot a matrix
matplot(ren_AB)
#transpor
matplot(t(ren_AB))
#line shape
matplot(t(ren_AB), type = "l")

#changing the axis
matplot(t(ren_AB), type = "l", axes = F, ylab = "Rényi diversity")
box()
axis(side = 2)
axis(side = 1, labels = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "Inf"), at = 1:11)
legend("topright", legend = c("Community A", "Community B"),
       lty = c(1, 2), col = c(1,2))

#Understanding
abcd <- matrix(c(1,1,4,2,1,1,2,2,1,1,1,2,0,1,0,0,0,1,0,0), nrow=4,
               dimnames=list(c("A", "B","C","D"), c("sp1","sp2","sp3","sp4","sp5")))
abcd

reu <- renyi(abcd)
matplot(t(reu), type = "l", axes = F, ylab = "Rényi diversity")
box()
axis(side = 2)
axis(side = 1, labels = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "Inf"), at = 1:11)

#Three sites have horizontal profiles, which indicates that the species are equally
#distributed by site (maximum evenness).
#Site C has a curve declining from left to right, indicating that the distribution
#of species at this site is uneven (dominant).
#The profile that starts higher on the graph indicates greater species richness
#The order of diversity in the locations is B>A=D>C.
