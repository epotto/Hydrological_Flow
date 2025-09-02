# My first script

library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RcppRoll)

post <- read_csv("data/post_data.csv")

post

Mag_timing <- function(data){
  G1 <- data |>
    group_by(m) |>
    summarize(Magnitude_Timing = mean(cfs))
  return(G1)
    }

Mag_timing(post)

one_min <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(d) |>
    filter(cfs == min(cfs))
  dat_out <- dat_filt |>
    summarise(One_Day_Min = mean(cfs))
#dat_filt
#dat_out #<- #final product
  return(dat_out)
}


one_min(post)


one_max <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(d) |>
    filter(cfs == max(cfs))
  dat_out <- dat_filt |>
    summarise(One_Day_Min = mean(cfs))
  return(dat_out)
}

one_max(post)

three_min <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(d) |>
    filter(cfs == min(cfs))
  dat_out <- dat_filt |>
    summarise(One_Day_Min = roll_mean(cfs,3))
  return(dat_out)
}

three_min(post)

# Experimental Three_min and function for it

three_min <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(d) |>
    filter(cfs == max(cfs))
  three_func(data, dat_filt)
#  dat_out <- dat_filt |>
#    summarise(Three_Day_Min = )
#  return(dat_out)
}

three_func <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(d) |>
    filter(cfs == max(cfs))
  for(i in range(round(dat_filt / 3, 0))){
    dat_out <- mean(dat_filt$cfs[i]:dat_filt$cfs[i+2])
  summarise(data, Three_Day_Min = dat_out)
  }
}













