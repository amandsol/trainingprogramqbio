# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# First version 2022-08-04
# Class 14: Continuation: From taxonomic to functional and phylogenetic diversity
# --------------------------------------------------#
#Load packages
library(vegan)
library(cluster)
library(FD)
library(SYNCSA)
library(taxize)
library(dplyr)

# Reading data -----------------------------------------------------------------

# Listing all files
cestes_files <- list.files(path = "data/raw/cestes",
                           pattern = "csv$",
                           full.names = TRUE)
#replace the csv
cestes_names <- gsub(".csv", "", basename(cestes_files), fixed = TRUE)

comm <- read.csv(cestes_files[1])
coord <- read.csv(cestes_files[2])
envir <- read.csv(cestes_files[3])
splist <- read.csv(cestes_files[4])
traits <- read.csv(cestes_files[5])

head(comm)[,1:6]

#Renomear linhas do comm
rownames(comm) <- paste0("Site", comm[,1])
comm <- comm[,-1]
head(comm)[,1:6]

#Species richness can be calculated with the vegan package
richness <- vegan::specnumber(comm)

#Taxonomic measures can be calculated using diversity() function
shannon <- vegan::diversity(comm)
shannon

simpson <- vegan::diversity(comm, index = "simpson")
simpson

### Functional diversity
# Taxonomic diversity indices are based on the assumption that species belong to
#one species or the other.

# This can be thought as a distance matrix between individuals, where individuals of
#the same species have a distance of zero, individuals of different species have a distance of 1 between them.

# When analyzing functional traits the distance between individuals is no longer
#determined by their belonging to a species, but to their position in the trait space.

# These traits can be continuous, but vary in different scales, or they can be categorical,
#and appropriate distance measures have to be used to deal with this difference.

#Gower distance is a common distance metric used in trait-based ecology
traits <- traits[,-1]

gow <- cluster::daisy(traits, metric = "gower")
gow2 <- FD::gowdis(traits)

#implementations in R vary and the literature reports extensions and modifications
identical(gow, gow2) #not the same but why?

class(gow) #different classes

class(gow2)

plot(gow, gow2, asp = 1) #same values
# In the end its the same, just the class that makes it different
#These classes are created by the function's authors


##################### Rao’s quadratic entropy
# Now using SYNCSA package
traits <- read.csv(cestes_files[5])
rownames(traits) <- traits$Sp
traits <- traits[,-1]

tax <- rao.diversity(comm)
fun <- rao.diversity(comm, traits = traits)

plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
abline(a = 0, b = 1)


############## Calculating Functional Diversity indices with package FD #####
#we can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)
#the returned object has Villéger's indices and Rao calculation

## dbFD implements a flexible distance-based framework to compute multidimensional
#functional diversity (FD) indices.
#dbFD returns the three FD indices of Villéger et al.(2008)
names(FuncDiv1)

#We can also do the calculation using the traits matrix directly
FuncDiv <- dbFD(x = traits, a = comm, messages = F)

names(FuncDiv)  # has one more output than the last, called CWM (see above)
#How to we summarize visually, interpret community composition and trait data?


##### Species Names and Families #####
# library in this section is taxize
classification_data <- classification(splist$TaxonName, db = "ncbi")

# View(classification.data)

family_name <- classification_data[[1]] %>%
  filter(rank == "family") %>%
  pull(name) #returns a value ---> select() returns a data.frame


# turn that into a function!
extract_family <- function(x){
  if(!is.null(dim(x))){ # is.na does not work here, but we can use dimensions because the dimensions are NULL for an NA list in classification.data
    y <- x %>%
      filter(rank == 'family') %>%
      pull(name)
    return(y)
  }
}

extract_family(classification_data[[1]])
extract_family(classification_data[[4]])

families <- list()


for(i in 1:length(classification_data)){
  f <- extract_family(classification_data[[i]])
  if(length(f) > 0) families[i] <- f
}

head(families)
