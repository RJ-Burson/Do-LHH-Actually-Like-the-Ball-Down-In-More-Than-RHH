
library(xgboost)
library(cvms)
library(Ecdat)
library(boot)
library(dplyr)
library(ggplot2)
library(caret)
library(UBL)
library(ROSE)
library(scutr)
library(mclust)
library(parallel)
library(SMOTEWB)
library(cli)
library(digest)
library(fastmap)
library(rlang)
library(htmltools)
library(RMySQL)
library(readxl)
library(ggplot2)                     
library(GGally)
library(caTools)
library(skimr)
library(MASS)
library(caret)
library(class)
library(ROCR)
library(ISLR)
library(boot)
library(tree)
library(randomForest)
library(gbm)
library(tidyverse)
library(cutpointr)
library(car)
library(glmnet)
library(e1071)
library(dplyr)
library(summarytools)
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(dplyr)
library(GLMcat)
library(data.table)
library(neuralnet)
library(stringi)
library(mltools)
library(data.table)
library(kknn)


t= read.csv("C:/Users/014497819/Desktop/Thesis/2021 Pitches.csv")
g= read.csv("C:/Users/014497819/Desktop/Thesis/2022 Pitches Pre All-Star.csv")
h= read.csv("C:/Users/014497819/Desktop/Thesis/2022 Pitches Post All-Star.csv")

all_pitches= rbind(t,g,h)



#######################Right Handed Hitters#########################


#Down and In
RH_data = subset(all_pitches, all_pitches$zone==7 & all_pitches$stand=="R")

RH_data$events = as.factor(RH_data$events)

levels(RH_data$events)

RH_plate_apperances = RH_data[RH_data$events %in% c("catcher_interf", "double", "double_play", "field_error",
                                           "field_out", "fielders_choice", "fielders_choice_out",
                                           "force_out", "grounded_into_double_play", "home_run",
                                           "sac_bunt", "sac_fly", "single", "strikeout", 
                                           "strikeout_double_play", "triple", "walk"), ]

#Anytime a batter comes to the plate
RH_pa = dim(RH_plate_apperances)[1]
# No Sacrifices/No walks/HBP
RH_ab = dim(subset(RH_plate_apperances, RH_plate_apperances$events=="double"))[1] + 
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="double_play"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="field_error"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="field_out"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="fielders_choice"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="fielders_choice_out"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="force_out"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="grounded_into_double_play"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="home_run"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="single"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="strikeout"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="strikeout_double_play"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="triple"))[1] 

RH_hits = dim(subset(RH_plate_apperances, RH_plate_apperances$events=="single"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="double"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="triple"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="home_run"))[1] 

RH_avg = RH_hits/RH_ab
RH_avg


RH_SLG= (dim(subset(RH_plate_apperances, RH_plate_apperances$events=="single"))[1] +
  2*dim(subset(RH_plate_apperances, RH_plate_apperances$events=="double"))[1] +
  3*dim(subset(RH_plate_apperances, RH_plate_apperances$events=="triple"))[1] +
  4*dim(subset(RH_plate_apperances, RH_plate_apperances$events=="home_run"))[1])/RH_ab
RH_SLG


#Down
RH_data = subset(all_pitches, all_pitches$zone %in% c(7,8,9) & all_pitches$stand=="R")

RH_data$events = as.factor(RH_data$events)

levels(RH_data$events)

RH_plate_apperances = RH_data[RH_data$events %in% c("catcher_interf", "double", "double_play", "field_error",
                                                    "field_out", "fielders_choice", "fielders_choice_out",
                                                    "force_out", "grounded_into_double_play", "home_run",
                                                    "sac_bunt", "sac_fly", "sac_fly_double_play", 
                                                    "single", "strikeout", "strikeout_double_play", 
                                                    "triple", "triple_play", "walk"), ]

#Anytime a batter comes to the plate
RH_pa = dim(RH_plate_apperances)[1]
# No Sacrifices/No walks/HBP
RH_ab = dim(subset(RH_plate_apperances, RH_plate_apperances$events=="double"))[1] + 
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="double_play"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="field_error"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="field_out"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="fielders_choice"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="fielders_choice_out"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="force_out"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="grounded_into_double_play"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="home_run"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="single"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="strikeout"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="strikeout_double_play"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="triple"))[1] 

RH_hits = dim(subset(RH_plate_apperances, RH_plate_apperances$events=="single"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="double"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="triple"))[1] +
  dim(subset(RH_plate_apperances, RH_plate_apperances$events=="home_run"))[1] 

RH_avg = RH_hits/RH_ab
RH_avg


RH_SLG= (dim(subset(RH_plate_apperances, RH_plate_apperances$events=="single"))[1] +
           2*dim(subset(RH_plate_apperances, RH_plate_apperances$events=="double"))[1] +
           3*dim(subset(RH_plate_apperances, RH_plate_apperances$events=="triple"))[1] +
           4*dim(subset(RH_plate_apperances, RH_plate_apperances$events=="home_run"))[1])/RH_ab
RH_SLG



#########################Left Handed Hitters###############

# Down and In
LH_data = subset(all_pitches, all_pitches$zone==9 & all_pitches$stand=="L")

LH_data$events = as.factor(LH_data$events)

levels(LH_data$events)

LH_plate_apperances = LH_data[LH_data$events %in% c("catcher_interf", "double", "double_play", "field_error",
                                  "field_out", "fielders_choice", "fielders_choice_out",
                                  "force_out", "grounded_into_double_play", "home_run",
                                  "sac_bunt", "sac_fly", "sac_fly_double_play", "single",
                                  "strikeout", "strikeout_double_play", "triple", "walk"), ]


#Anytime a batter comes to the plate
LH_pa = dim(LH_plate_apperances)[1]
# No Sacrifices/No walks/HBP
LH_ab = dim(subset(LH_plate_apperances, LH_plate_apperances$events=="double"))[1] + 
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="double_play"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="field_error"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="field_out"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="fielders_choice"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="fielders_choice_out"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="force_out"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="grounded_into_double_play"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="home_run"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="single"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="strikeout"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="strikeout_double_play"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="triple"))[1] 

LH_hits = dim(subset(LH_plate_apperances, LH_plate_apperances$events=="single"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="double"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="triple"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="home_run"))[1] 
  
LH_avg = LH_hits/LH_ab
LH_avg


LH_SLG= (dim(subset(LH_plate_apperances, LH_plate_apperances$events=="single"))[1] +
           2*dim(subset(LH_plate_apperances, LH_plate_apperances$events=="double"))[1] +
           3*dim(subset(LH_plate_apperances, LH_plate_apperances$events=="triple"))[1] +
           4*dim(subset(LH_plate_apperances, LH_plate_apperances$events=="home_run"))[1])/LH_ab
LH_SLG


#Down
LH_data = subset(all_pitches, all_pitches$zone  %in% c(7,8,9) & all_pitches$stand=="L")

LH_data$events = as.factor(LH_data$events)

levels(LH_data$events)

LH_plate_apperances = LH_data[LH_data$events %in% c("catcher_interf", "double", "double_play", "field_error",
                                                    "field_out", "fielders_choice", "fielders_choice_out",
                                                    "force_out", "grounded_into_double_play", "home_run",
                                                    "sac_bunt", "sac_fly", "sac_fly_double_play", "single",
                                                    "strikeout", "strikeout_double_play", "triple", 
                                                    "triple_play", "walk"), ]


#Anytime a batter comes to the plate
LH_pa = dim(LH_plate_apperances)[1]
# No Sacrifices/No walks/HBP
LH_ab = dim(subset(LH_plate_apperances, LH_plate_apperances$events=="double"))[1] + 
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="double_play"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="field_error"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="field_out"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="fielders_choice"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="fielders_choice_out"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="force_out"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="grounded_into_double_play"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="home_run"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="single"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="strikeout"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="strikeout_double_play"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="triple"))[1] 

LH_hits = dim(subset(LH_plate_apperances, LH_plate_apperances$events=="single"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="double"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="triple"))[1] +
  dim(subset(LH_plate_apperances, LH_plate_apperances$events=="home_run"))[1] 

LH_avg = LH_hits/LH_ab
LH_avg


LH_SLG= (dim(subset(LH_plate_apperances, LH_plate_apperances$events=="single"))[1] +
           2*dim(subset(LH_plate_apperances, LH_plate_apperances$events=="double"))[1] +
           3*dim(subset(LH_plate_apperances, LH_plate_apperances$events=="triple"))[1] +
           4*dim(subset(LH_plate_apperances, LH_plate_apperances$events=="home_run"))[1])/LH_ab
LH_SLG
