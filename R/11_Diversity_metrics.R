# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# First version 2022-07-28
# Class 12: Diversity metrics: Introduction to biological diversity analyses
# --------------------------------------------------#
library(vegan)

comm <- read.csv("data/raw/cestes/comm.csv")

dim(comm)
class(comm)
head(comm)

#Which are the most abundant species overall in the dataset?
sort(colSums(comm[,-1]), decreasing = TRUE)

#How many species are there in each site?
#transform in a list of presence and ausence
comm_pres <- comm[,-1]
comm_pres[comm_pres>0] <- 1

#abundance with sites
comm_abd <- cbind(comm$Sites,rowSums(comm_pres))
comm_abd <- as.data.frame(comm_abd)
colnames(comm_abd) <- c("Sites", "Abundance")

# Creating Functions
#Shannon Diversity Index
shannon <- function(x){
  pi <- x/sum(x)
  H <-  -sum(pi * log(pi))
  return(H)
}

#Simpson's Diversity Index
simpson <- function(x){
  pi <- x/sum(x)
  Simp <-  1 - sum(pi^2)
  return(Simp)
}

#Inverse Simpson
inv.simpson <- function(x){
  pi <- x/sum(x)
  inv.simp <-  1 / sum(pi^2)
  return(inv.simp)
}

diversity <- function(x,index){
  pi <-  x/sum(x)
  if (index == "shannon") d <- -sum(pi * log(pi))
  if (index == "simpson") d <- 1 - sum(pi^2)
  if (index == "invSimpson") d <- 1 / sum(pi^2)
  return(d)
}


shannon(comm_abd$Abundance)
diversity(comm_abd$Abundance, index = "shannon")

#Compare with diversity vegan function
vegan::diversity(comm_abd$Abundance, index = "shannon")
