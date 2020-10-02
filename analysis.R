library(EpiEstim)
library(ggplot2)
library(ggplot)
library(tidyverse)
library(magrittr)
library(incidence)

#load data
data <- read.csv("data source/DOH Data Drop 20200614.csv")
save (data, file = "rda/data.rda")
data %<>% mutate(DateRepConf=as.Date(DateRepConf, format="%Y-%m-%d"))
#convert to incidence object
incid <- incidence(data$DateRepConf)
#parametric estimation of R
#mean and SD serial interval are based on the study of Nishiura et al (2020)
parametric_si <- estimate_R(incid, method="parametric_si",
                                config = make_config(list(
                                  mean_si = 4.8, 
                                  std_si = 2.3)))

#NCR data
NCRsubset <- as.data.frame(subset(data, RegionRes == "NCR",
                                  select=DateRepConf))
incidNCR <- incidence(NCRsubset$DateRepConf) 
NCR_parametric_si <- estimate_R(incidNCR, method="parametric_si",
                                config = make_config(list(
                                  mean_si = 4.8, 
                                  std_si = 2.3)))
png("figures/NCR_R0.png")
plot(NCR_parametric_si, "R")
dev.off()
#RegionVII data
RegVIIsubset <- as.data.frame(subset(data, RegionRes == "Region VII: Central Visayas"
                                     & DateRepConf >="2020-04-02",
                                  select=DateRepConf))
incidRegVII <- incidence(RegVIIsubset$DateRepConf) 
RegVII_parametric_si <- estimate_R(incidRegVII, method="parametric_si",
                                config = make_config(list(
                                  mean_si = 4.8, 
                                  std_si = 2.3)))
png("figures/RegVII_R0.png")
plot(RegVII_parametric_si, "R")
dev.off()
#RegionIVA data
RegIVAsubset <- as.data.frame(subset(data, RegionRes == "Region IV-A: CALABARZON",
                                     select=DateRepConf))
incidRegIVA <- incidence(RegIVAsubset$DateRepConf) 
RegIVA_parametric_si <- estimate_R(incidRegIVA, method="parametric_si",
                                   config = make_config(list(
                                     mean_si = 4.8, 
                                     std_si = 2.3)))
plot("figures/RegIVA.png")
plot(RegIVA_parametric_si, "R")
dev.off()
#RegionIII data
RegIIIsubset <- as.data.frame(subset(data, RegionRes == "Region III: Central Luzon"
                                     & DateRepConf >="2020-03-09",
                                     select=DateRepConf))
incidRegIII <- incidence(RegIIIsubset$DateRepConf) 
RegIII_parametric_si <- estimate_R(incidRegIII, method="parametric_si",
                                   config = make_config(list(
                                     mean_si = 4.8, 
                                     std_si = 2.3)))
png("figures/REGIII_R0.png")
plot(RegIII_parametric_si, "R")
dev.off()
#RegionXI data
RegXIsubset <- as.data.frame(subset(data, RegionRes == "Region XI: Davao Region"
                                      & DateRepConf >= "2020-03-10",
                                     select=DateRepConf))
incidRegXI <- incidence(RegXIsubset$DateRepConf) 
RegXI_parametric_si <- estimate_R(incidRegXI, method="parametric_si",
                                   config = make_config(list(
                                     mean_si = 4.8, 
                                     std_si = 2.3)))
png("figures/RegXI_R0.png")
plot(RegXI_parametric_si, "R")
dev.off()

