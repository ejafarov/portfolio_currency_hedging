rm(list=ls())

setwd("~/Documents/R/datasets")

#working libraries
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI)
library(quadprog)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(Rglpk)
library(doParallel)
library("doSNOW")
library(foreach)
library(tcltk)
library(DEoptim)
library(GenSA)
library(pso)
library(xts)
library(tibble)
library(ggplot2)
library(magrittr)
library(dplyr)
library(progress)
library(tidyr)

#may be needed
library(ROI.plugin.symphony)


#working packages
install.packages("PortfolioAnalytics")
install.packages("ROI", dependencies=TRUE)
install.packages("ROI.plugin.glpk", dependencies=TRUE)
install.packages("doSNOW", dependencies=TRUE)
install.packages("Rglpk", dependencies=TRUE) #requires libglpk-lib in ubuntu
install.packages("quadprog", dependencies=TRUE)
install.packages("doParallel", dependencies=TRUE)

install.packages("DEoptim", dependencies=TRUE)
install.packages("GenSA", dependencies=TRUE)
install.packages("pso", dependencies=TRUE)
install.packages("xts", dependencies=TRUE)
install.packages("tibble", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("magrittr", dependencies=TRUE)
install.packages("dplyr", dependencies=TRUE)
install.packages("progress", dependencies=TRUE)

install.packages("tydir", dependencies=TRUE)

#not working



install.packages("tidyquant", dependencies=TRUE)
install.packages("tidyverse", dependencies=TRUE)


#not working
library(tidyverse)
library(corrplot)
library(tidyquant)


library(BLCOP)


library(robustbase)

library(tibbletime)
library(gganimate)
library(h2o)
library(plotly)
library(gapminder)
