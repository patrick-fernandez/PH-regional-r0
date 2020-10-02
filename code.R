library(EpiEstim)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(incidence)

#load data
data <- read.csv("DOH Data Drop 20200614.csv")
data %<>% mutate(DateRepConf=as.Date(DateRepConf, format="%Y-%m-%d"))
#convert to incidence object
incid <- incidence(data$DateRepConf)
#parametric estimation of R
#mean and SD serial interval are based on the study of Nishiura et al (2020)
parametric_si <- estimate_R(incid, method="parametric_si",
                                config = make_config(list(
                                  mean_si = 4.8, 
                                  std_si = 2.3)))
plot(parametric_si, "all")
#NCR data
NCRsubset <- as.data.frame(subset(data, RegionRes == "NCR",
                                  select=DateRepConf))
incidNCR <- incidence(NCRsubset$DateRepConf) 
NCR_parametric_si <- estimate_R(incidNCR, method="parametric_si",
                                config = make_config(list(
                                  mean_si = 4.8, 
                                  std_si = 2.3)))
plot(NCR_parametric_si, "R")
#RegionVII data
RegVIIsubset <- as.data.frame(subset(data, RegionRes == "Region VII: Central Visayas"
                                     & DateRepConf >="2020-04-02",
                                  select=DateRepConf))
incidRegVII <- incidence(RegVIIsubset$DateRepConf) 
RegVII_parametric_si <- estimate_R(incidRegVII, method="parametric_si",
                                config = make_config(list(
                                  mean_si = 4.8, 
                                  std_si = 2.3)))
plot(RegVII_parametric_si, "R")
#RegionIVA data
RegIVAsubset <- as.data.frame(subset(data, RegionRes == "Region IV-A: CALABARZON",
                                     select=DateRepConf))
incidRegIVA <- incidence(RegIVAsubset$DateRepConf) 
RegIVA_parametric_si <- estimate_R(incidRegIVA, method="parametric_si",
                                   config = make_config(list(
                                     mean_si = 4.8, 
                                     std_si = 2.3)))
plot(RegIVA_parametric_si, "R")
#RegionIII data
RegIIIsubset <- as.data.frame(subset(data, RegionRes == "Region III: Central Luzon"
                                     & DateRepConf >="2020-03-09",
                                     select=DateRepConf))
incidRegIII <- incidence(RegIIIsubset$DateRepConf) 
RegIII_parametric_si <- estimate_R(incidRegIII, method="parametric_si",
                                   config = make_config(list(
                                     mean_si = 4.8, 
                                     std_si = 2.3)))
plot(RegIII_parametric_si, "R")
#RegionXI data
RegXIsubset <- as.data.frame(subset(data, RegionRes == "Region XI: Davao Region"
                                      & DateRepConf >= "2020-03-10",
                                     select=DateRepConf))
incidRegXI <- incidence(RegXIsubset$DateRepConf) 
RegXI_parametric_si <- estimate_R(incidRegXI, method="parametric_si",
                                   config = make_config(list(
                                     mean_si = 4.8, 
                                     std_si = 2.3)))
plot(RegXI_parametric_si, "R")
