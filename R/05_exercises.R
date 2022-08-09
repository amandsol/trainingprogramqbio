# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
## Monday Exercises
# --------------------------------------------------#


data("iris")
iris

oi<- filter(iris, Species == "setosa" | Species == "virginica")


taxon <- data.frame(Family = "Iridaceae", Genus = "Iris",
                      Species = c("setosa", "versicolor", "virginica"))

t_taxon <- full_join(taxon,iris, by= "Species")

ggplot(iris) +
geom_boxplot(aes(y= Petal.Length))

ggplot(iris) +
  geom_boxplot(aes(y=Petal.Length, fill = Species))

ggplot(iris)+
  geom_point(mapping = aes(x=Sepal.Length, y= Sepal.Width))+
  facet_wrap(~Species)
