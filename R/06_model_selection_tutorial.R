# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Inplementing glm and glmm in R
# First version 2022-07-20
# Class 08: General linear models
# --------------------------------------------------#

library(bbmle)
cuckoo <- read.csv("data/raw/valletta_cuckoo.csv")

h1 <- glm(Beg ~ Mass, data = cuckoo,
          family = poisson(link = log))

h2 <- glm(Beg ~ Mass + Species, data = cuckoo,
          family = poisson(link = log))

h3 <- glm(Beg ~ Mass * Species, data = cuckoo,
          family = poisson(link = log))

h0 <- glm(Beg ~ 0, data = cuckoo,
          family = poisson(link = log))

summary(h3)
summary(h2)
summary(h1)
summary(h0)

bbmle::AICtab(h0, h1, h2, h3, base = TRUE, weights = TRUE)
#dAIC the lower

#Calculating the predicted values
#creating a new data frame
newdata <- expand.grid(Mass = seq(min(cuckoo$Mass), max(cuckoo$Mass), length.out = 200),
                       Species = unique(cuckoo$Species))
#new column
newdata$Beg <- predict(h3, newdata, type = 'response')
summary(newdata)

p <- ggplot(mapping = aes(x = Mass, y = Beg, colour = Species)) +
  geom_point(data = cuckoo) +  geom_line(data = newdata) +
  theme_classic()
p
