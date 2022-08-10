# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# First version 2022-07-21
# Class 09: Creating functions with deSolve package
# --------------------------------------------------#

###Population ecology
library(deSolve)
library(ggplot2) # because we will plot things
library(tidyr) # because we will manipulate some data

###Logistic Growth-------------------------------------------------------------
source("fct/logGrowth.R")
#We will specify the parameters and then solve the ODE.
# named vector with parameters
p <- c(r = 1, a = 0.001)
# initial condition
y0 <- c(N = 10)
# time steps
t <- 1:20

#Then, we can use the ode() function to solve the model for our defined parameters.
# give the function and the parameters to the ode function
out_log <- ode(y = y0, times = t, func = logGrowth, parms = p)
class(out_log)
head(out_log)

#convert the out object into a data.frame and plot
df_log <- as.data.frame(out_log)
class(df_log)


ggplot(df_log) +
  geom_line(aes(x = time, y = N)) +
  theme_classic()

###Lotka-Volterra competition model---------------------------------------------
#we have to define the α matrix with the competition coefficients
source("fct/LVComp.R")
# LV parameters
a <- matrix(c(0.02, 0.01, 0.01, 0.03), nrow = 2)
r <- c(1, 1)
p2 <- list(r, a)
N0 <- c(10, 10)
t2 <- c(1:100)

#Solving the system of ODE.
out_lv <- ode(y = N0, times = t2, func = LVComp, parms = p2)
head(out_lv)
#Notice that now we have two columns because we have two state variables
class(out_lv)

#Convert out data in a format in which every variable is represented in a column
#and every observation is represented in a row
#pivot_longer is used with data frame
df_lv <- pivot_longer(as.data.frame(out_lv), cols = 2:3)
class(df_lv)

ggplot(df_lv) +
  geom_line(aes(x = time, y = value, color = name)) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()



