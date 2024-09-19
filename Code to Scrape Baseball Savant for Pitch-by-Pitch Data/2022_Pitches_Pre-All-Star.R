#Install devtools to be able to install from github
install.packages('devtools')
library(devtools)
#Bill Petti's Github Page for baseballr
#http://billpetti.github.io/baseballr/
install_github("BillPetti/baseballr")

#call tidyverse for dplyr purposes
install.packages('tidyverse')
library(tidyverse)

#load Statcast data week by week, since it can only load 10 days at a time or 40,000 observations
#scrape_statcast_savant scrapes data from Savant given the game dates and the player types


date407412 = baseballr::scrape_statcast_savant(start_date = '2022-04-07',
                                               end_date = '2022-04-12', player_type = 'pitcher')

date413417 = baseballr::scrape_statcast_savant(start_date = '2022-04-13',
                                               end_date = '2022-04-17', player_type = 'pitcher')

date418422 = baseballr::scrape_statcast_savant(start_date = '2022-04-18',
                                               end_date = '2022-04-22', player_type = 'pitcher')

date423427 = baseballr::scrape_statcast_savant(start_date = '2022-04-23',
                                               end_date = '2022-04-27', player_type = 'pitcher')

date428501 = baseballr::scrape_statcast_savant(start_date = '2022-04-28',
                                               end_date = '2022-05-01', player_type = 'pitcher')

date502506 = baseballr::scrape_statcast_savant(start_date = '2022-05-02',
                                               end_date = '2022-05-06', player_type = 'pitcher')

date507511 = baseballr::scrape_statcast_savant(start_date = '2022-05-07',
                                               end_date = '2022-05-11', player_type = 'pitcher')

date512516 = baseballr::scrape_statcast_savant(start_date = '2022-05-12',
                                               end_date = '2022-05-16', player_type = 'pitcher')

date517521 = baseballr::scrape_statcast_savant(start_date = '2022-05-17',
                                               end_date = '2022-05-21', player_type = 'pitcher')

date522526 = baseballr::scrape_statcast_savant(start_date = '2022-05-22',
                                               end_date = '2022-05-26', player_type = 'pitcher')

date527531 = baseballr::scrape_statcast_savant(start_date = '2022-05-27',
                                               end_date = '2022-05-31', player_type = 'pitcher')

date601605 = baseballr::scrape_statcast_savant(start_date = '2022-06-01',
                                               end_date = '2022-06-05', player_type = 'pitcher')

date606610 = baseballr::scrape_statcast_savant(start_date = '2022-06-06',
                                               end_date = '2022-06-10', player_type = 'pitcher')

date611615 = baseballr::scrape_statcast_savant(start_date = '2022-06-11',
                                               end_date = '2022-06-15', player_type = 'pitcher')

date616620 = baseballr::scrape_statcast_savant(start_date = '2022-06-16',
                                               end_date = '2022-06-20', player_type = 'pitcher')

date621625 = baseballr::scrape_statcast_savant(start_date = '2022-06-21',
                                               end_date = '2022-06-25', player_type = 'pitcher')

date626630 = baseballr::scrape_statcast_savant(start_date = '2022-06-26',
                                               end_date = '2022-06-30', player_type = 'pitcher')

date701705 = baseballr::scrape_statcast_savant(start_date = '2022-07-01',
                                               end_date = '2022-07-05', player_type = 'pitcher')

date706710 = baseballr::scrape_statcast_savant(start_date = '2022-07-06',
                                               end_date = '2022-07-10', player_type = 'pitcher')

date711717 = baseballr::scrape_statcast_savant(start_date = '2022-07-11',
                                               end_date = '2022-07-17', player_type = 'pitcher')




#combine all data into one data frame
SavantData22_Pre = rbind(date407412, date413417, date418422, date423427, date428501, 
                         date502506, date507511, date512516, date517521, date522526, 
                         date527531, date601605, date606610, date611615, date616620, 
                         date621625, date626630, date701705, date706710, date711717)


write.csv(SavantData22_Pre, "C:/Users/014497819/Desktop/Thesis/2022 Pitches Pre All-Star.csv")


dim(SavantData22_Pre)

