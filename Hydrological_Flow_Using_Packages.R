# My first script

library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RcppRoll)



# Use lubridate functions for time durations





post <- read_csv("data/post_data.csv")
pre <- read_csv("data/pre_data.csv")

# Group 1

Mag_timing <- function(data){
  G1filt <- data |>
    group_by(m) |>
    summarise(Magnitude_Timing = mean(cfs))
  G1 <- G1filt |>
    ungroup() |>
    summarise(value = mean(Magnitude_Timing)) |>
    mutate(group = 1,
           metric = "Mean by Month")
  return(G1)
    }

Mag_timing(post)


Group1 <- function(data){
  mag_timing = Mag_timing(data) |>
    select(metric,value,group)
  return(mag_timing)
}

Group1(post)


# Group 2

one_min <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    filter(cfs == min(cfs))
  dat_mean <- dat_filt |>
    summarise(One_Day_Min = mean(cfs))
  dat_out <- dat_mean |>
    ungroup() |>
    summarise(value = mean(One_Day_Min)) |>
    mutate(group = 2,
           metric = "One Day Min")
#dat_filt
#dat_out #<- #final product
  return(dat_out)
}


one_min(post)


one_max <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    filter(cfs == max(cfs))
  dat_mean <- dat_filt |>
    summarise(One_Day_Max = mean(cfs))
  dat_out <- dat_mean |>
    ungroup() |>
    summarise(value = mean(One_Day_Max)) |>
    mutate(group = 2,
           metric = "One Day Max")
  return(dat_out)
}


#three_min <- post#three_min <- function(data){
#  dat_in <- data
#  dat_filt <- dat_in |>
#    group_by(d) |>
#    filter(cfs == min(cfs))
#  dat_out <- dat_filt |>
#    summarise(Three_Day_Min = roll_mean(cfs,n = 3))
#  return(dat_out)
#}


three_min <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    reframe(Three_Day = roll_mean(cfs, n = 3)) |>
    group_by(y)
  dat_min <- dat_filt |>
    summarise(Three_Day_Min = min(Three_Day))
  dat_out <- dat_min |>
    ungroup() |>
    summarise(value = mean(Three_Day_Min)) |>
    mutate(group = 2,
           metric = "Three Day Min")
  return(dat_out)
}

three_min(post)

three_max <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    reframe(Three_Day = roll_mean(cfs, n = 3)) |>
    group_by(y)
  dat_max <- dat_filt |>
    summarise(Three_Day_Max = max(Three_Day))
  dat_out <- dat_max |>
    ungroup() |>
    summarise(value = mean(Three_Day_Max)) |>
    mutate(group = 2,
           metric = "Three Day Max")
  return(dat_out)
}

three_max(post)



seven_min <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    reframe(Sev_Day = roll_mean(cfs, n = 7)) |>
    group_by(y)
  dat_min <- dat_filt |>
    summarise(Seven_Day_Min = min(Sev_Day))
  dat_out <- dat_min |>
    ungroup() |>
    summarise(value = min(Seven_Day_Min)) |>
    mutate(group = 2,
           metric = "Seven Day Min")
  return(dat_out)
}

seven_min(post)

seven_max <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    reframe(Sev_Day = roll_mean(cfs, n = 7)) |>
    group_by(y)
  dat_max <- dat_filt |>
    summarise(Seven_Day_Max = max(Sev_Day))
  dat_out <- dat_max |>
    ungroup() |>
    summarise(value = min(Seven_Day_Max)) |>
    mutate(group = 2,
           metric = "Seven Day Max")
  return(dat_out)
}

seven_max(post)



thirty_min <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    reframe(Thirty_Day = roll_mean(cfs, n = 30)) |>
    group_by(y)
  dat_min <- dat_filt |>
    summarise(Thirty_Day_Min = min(Thirty_Day))
  dat_out <- dat_min |>
    ungroup() |>
    summarise(value = mean(Thirty_Day_Min)) |>
    mutate(group = 2,
           metric = "Thirty Day Min")
  return(dat_out)
}

thirty_min(post)

thirty_max <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    reframe(Thirty_Day = roll_mean(cfs, n = 30)) |>
    group_by(y)
  dat_max <- dat_filt |>
    summarise(Thirty_Day_Max = max(Thirty_Day))
  dat_out <- dat_max |>
    ungroup() |>
    summarise(value = mean(Thirty_Day_Max)) |>
    mutate(group = 2,
           metric = "Thirty Day Max")
  return(dat_out)
}

thirty_max(post)



ninety_min <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    reframe(Ninety_Day = roll_mean(cfs, n = 90)) |>
    group_by(y)
  dat_min <- dat_filt |>
    summarise(Ninety_Day_Min = min(Ninety_Day))
  dat_out <- dat_min |>
    ungroup() |>
    summarise(value = mean(Ninety_Day_Min)) |>
    mutate(group = 2,
           metric = "Ninety Day Min")
  return(dat_out)
}

ninety_min(post)

ninety_max <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    reframe(Ninety_Day = roll_mean(cfs, n = 90)) |>
    group_by(y)
  dat_max <- dat_filt |>
    summarise(Ninety_Day_Max = max(Ninety_Day))
  dat_out <- dat_max |>
    ungroup() |>
    summarise(value = mean(Ninety_Day_Max)) |>
    mutate(group = 2,
           metric = "Ninety Day Max")
  return(dat_out)
}


ninety_max(post)

group_in <- 1:3
group_target <- 1:5

group_target[group_target %in% group_in]

foo <- function(group_in){
if(1 %in% group_in){
  # do this
  # monthyl calcs

  # save out
}
  if(2 %in% group_in){
    # do this
    # rolling means
  }
}

# fter all groups, join the outputs together

foo <- function(data, group){
  group_n <- length(group)
  for(i in 1:group_n){

  }
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    reframe(Ninety_Day = roll_mean(cfs, n = 90)) |>
    group_by(y)
  dat_out <- dat_filt |>
    summarise(Ninety_Day_Max = max(Ninety_Day))|>
    # not sure about next summarize?
    ungroup() |>
    summarize(mean_ninety = mean(Ninety_Day_Max)) |>
    mutate(group = "group2")

  return(dat_out)
}

#Magnitude_Duration <- function(data){
#  full_data <- one_min(data)
#  full_data |>
#    merge(one_max(data), by = "y")
#  return(full_data)
#}

Group2<- function(data){
  one_min <- one_min(data) |>
    select(metric,value,group)
  one_max <- one_max(data) |>
    select(metric,value,group)
  three_min <- three_min(data) |>
    select(metric,value,group)
  three_max <- three_max(data) |>
    select(metric,value,group)
  seven_min <- seven_min(data) |>
    select(metric,value,group)
  seven_max <- seven_max(data) |>
    select(metric,value,group)
  thirty_min <- thirty_min(data) |>
    select(metric,value,group)
  thirty_max <- thirty_max(data) |>
    select(metric,value,group)
  ninety_min <- ninety_min(data) |>
    select(metric,value,group)
  ninety_max <- ninety_max(data) |>
    select(metric,value,group)
  return(bind_rows(one_min,
                   one_max,
                   three_min,
                   three_max,
                   seven_min,
                   seven_max,
                   thirty_min,
                   thirty_max,
                   ninety_min,
                   ninety_max))
}

Group2(post)

# Group 3

jmax <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    filter(cfs == max(cfs))
  dat_out <- dat_filt |>
    ungroup() |>
    summarise(value = mean(j_date)) |>
    ungroup() |>
    mutate(metric = "Julian Date Max",
           group = 3)
  return(dat_out)
}

jmax(post)

jmin <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    filter(cfs == min(cfs))
  dat_out <- dat_filt |>
    ungroup() |>
    reframe(value = mean(j_date)) |>
    ungroup() |>
    mutate(metric = "Julian Date Min",
           group = 3)
  return(dat_out)
}

jmin(post)

# Group 4

daily_mean <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y)
  dat_mean <- dat_filt |>
    ungroup() |>
    reframe(value = mean(cfs))
  return(dat_mean)
}

daily_mean(post)

hi_pulse <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    mutate(q75 = quantile(cfs, probs = 0.75)) |>
    filter(cfs > q75)
  num <- dat_filt |>
    ungroup() |>
    group_by(y) |>
    summarise(value = sum(cfs/cfs)) |>
    ungroup() |>
    summarise(value = mean(value)) |>
    mutate(group = 4,
           metric = "Number of High Pulses")
  return(num)
}

hi_pulse(post)

low_pulse <- function(data){
  dat_in <- data
  dat_filt <- dat_in |>
    group_by(y) |>
    mutate(q25 = quantile(cfs, probs = 0.25)) |>
    filter(cfs < q25)
  num <- dat_filt |>
    ungroup() |>
    group_by(y) |>
    summarise(value = sum(cfs/cfs)) |>
    ungroup() |>
    summarise(value = mean(value)) |>
    mutate(group = 4,
           metric = "Number of Low Pulses")
  return(num)
}

low_pulse(post)

hi_duration1 <- function(data){
  dat_in <- data
  dat_quantile <- dat_in |>
    group_by(y) |>
    mutate(q75 = (cfs >= quantile(cfs, probs = 0.75)))
  counter = 1
  days[1] = counter
  days_of_year <- data$consecutive_date[nrow(data)] / length(unique(data$y)) #calculating how many days in each year (by avg)
  day_num = 0
  year_num = 1
  year_mean = vector(mode = "numeric", length = (length(unique(dat_quantile$y))))
  days_to_subtract = 0
   for(i in 1:nrow(dat_in) - 1){
     days <- 0
     if(dat_quantile$q75[i] == FALSE){
       return(1)
     }
}
   #   if(dat_quantile$q75[i] == FALSE){ # If it isn't a high flow
   #     days_to_subtract <- days_to_subtract + 1
   #   }
   #   if(dat_quantile$q75[i] == TRUE){ # If it is high flow
   #     counter = counter + 1
   #   }
   #   if(data$y[i+1] == data$y[i]){ # If year hasn't changed
   #     day_num = day_num + 1
   #   }
   #   if(data$d == 365.2*year_num & data$y[i+1] != data$y[i]){ # If year has changed based on both metrics,
   #     year_mean[year_num] = counter / (day_num - days_to_subtract)
   #     year_num = year_num + 1
   #   }
   # }
  return(year_mean)
}

hi_duration1(post)


hi_duration2 <- function(data){
    dat_in <- data
    dat_quantile <- dat_in |>
      group_by(y) |>
      mutate(q75 = (cfs >= quantile(cfs, probs = 0.75)))
    counter = 0
    days = vector(mode = "numeric", length = 366)
    year = vector(mode = "numeric", length = length(unique(data$y)))
    year_count = 1
    mean_duration = c()
    for(i in 1:(nrow(dat_in) - 1)){
      if(dat_quantile$q75[i] == TRUE){
        counter = counter + 1
        days[i] = 0
      }
      if(dat_quantile$q75[i] == FALSE){
        days[i] = counter
        mean_duration[i] =
        counter = 0
      }
      if(dat_quantile$y[i] != dat_quantile$y[i+1]){
        mean_duration = days > 0
        year[year_count] = mean(mean_duration)
        year_count = year_count + 1
        counter = 0
        mean_duration = c()
      }
    }
    return(mean(year))
}

numbers = c(1,2,3,4,5,6,0,0,0)



hi_duration2(post)

#hi_duration <- function(data){
#  # dat_in <- post
#  dat_in <- data
#  dat_quantile <- dat_in |>
#    group_by(y) |>
#    mutate(q75 = (cfs >= quantile(cfs, probs = 0.75)))
#  #dat_filter <- dat_quantile |>
#    #filter(csf > q75)
#  counter = 1
#  counter_year = 1
#  day_placement = 1
#  days = vector(mode = "numeric", length = nrow(dat_in))
#  days[1] = counter
#  day_mean = vector(mode = "numeric", length = (length(unique(dat_quantile$y))))
#  year = vector(mode = "numeric", length = (length(unique(dat_quantile))))
#  # i = 1
#   for(i in 1:nrow(dat_in)){
#     for(i in 1:nrow(dat_in) - 1){
#       if(dat_quantile$q75[i] == FALSE){ # If counter doesn't change
#         day_placement = day_placement + 1
#       }
#     }
#     year[i] = dat_in$y
#     if(dat_quantile$q75[i] == TRUE){
#     counter = counter + 1
#     }
#     if(dat_quantile$q75[i] == FALSE){ # If counter doesn't change
#       day_placement = day_placement + 1
#       days[day_placement] = counter # Put however many consecutive days just counted
#       counter = 0 # Reset the counter
#     }
#     if(year[i+1] - year[i] != 0){ # If data prior is not equal to same year (new year)
#       day_mean[counter_year] = mean(days)
#       counter_year = counter_year + 1
#     }
#   consecutive_days = mean(days)
#   return(consecutive_days)
#   }
# }

hi_duration(post)

counter = 1
days_group[1] = counter
for (i in 2:nrow(g3_days_in_25)){
  c_lag = g3_days_in_25$c_lag
  if(c_lag[i+1] - c_lag[i] < 0){
    counter = counter + 1
  }
  days_group[i] = counter
}
days_group[length(days_group)] = counter
days_group

# Experimental Three_min and function for it

#three_min <- function(data){
#  dat_in <- data
#  dat_filt <- dat_in |>
#    group_by(d) |>
#    filter(cfs == max(cfs))
#  three_func(data, dat_filt)
#  dat_out <- dat_filt |>
#    summarise(Three_Day_Min = )
#  return(dat_out)
#}

#three_func <- function(data){
#  dat_in <- data
#  dat_filt <- dat_in |>
#    group_by(d) |>
#    filter(cfs == max(cfs))
#  for(i in range(round(dat_filt / 3, 0))){
#    dat_out <- mean(dat_filt$cfs[i]:dat_filt$cfs[i+2])
#  summarise(data, Three_Day_Min = dat_out)
#  }
#}













