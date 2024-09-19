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


date401 = baseballr::scrape_statcast_savant(start_date = '2021-04-01',
                                               end_date = '2021-04-01', player_type = 'pitcher')

date402407 = baseballr::scrape_statcast_savant(start_date = '2021-04-02',
                                               end_date = '2021-04-07', player_type = 'pitcher')

date408412 = baseballr::scrape_statcast_savant(start_date = '2021-04-08',
                                               end_date = '2021-04-12', player_type = 'pitcher')

date413417 = baseballr::scrape_statcast_savant(start_date = '2021-04-13',
                                               end_date = '2021-04-17', player_type = 'pitcher')

date418422 = baseballr::scrape_statcast_savant(start_date = '2021-04-18',
                                               end_date = '2021-04-22', player_type = 'pitcher')

date423427 = baseballr::scrape_statcast_savant(start_date = '2021-04-23',
                                               end_date = '2021-04-27', player_type = 'pitcher')

date428501 = baseballr::scrape_statcast_savant(start_date = '2021-04-28',
                                               end_date = '2021-05-01', player_type = 'pitcher')

date502506 = baseballr::scrape_statcast_savant(start_date = '2021-05-02',
                                               end_date = '2021-05-06', player_type = 'pitcher')

date507511 = baseballr::scrape_statcast_savant(start_date = '2021-05-07',
                                               end_date = '2021-05-11', player_type = 'pitcher')

date512516 = baseballr::scrape_statcast_savant(start_date = '2021-05-12',
                                               end_date = '2021-05-16', player_type = 'pitcher')

date517521 = baseballr::scrape_statcast_savant(start_date = '2021-05-17',
                                               end_date = '2021-05-21', player_type = 'pitcher')

date522526 = baseballr::scrape_statcast_savant(start_date = '2021-05-22',
                                               end_date = '2021-05-26', player_type = 'pitcher')

date527531 = baseballr::scrape_statcast_savant(start_date = '2021-05-27',
                                               end_date = '2021-05-31', player_type = 'pitcher')

date601605 = baseballr::scrape_statcast_savant(start_date = '2021-06-01',
                                               end_date = '2021-06-05', player_type = 'pitcher')

date606610 = baseballr::scrape_statcast_savant(start_date = '2021-06-06',
                                               end_date = '2021-06-10', player_type = 'pitcher')

date611615 = baseballr::scrape_statcast_savant(start_date = '2021-06-11',
                                               end_date = '2021-06-15', player_type = 'pitcher')

date616620 = baseballr::scrape_statcast_savant(start_date = '2021-06-16',
                                               end_date = '2021-06-20', player_type = 'pitcher')

date621625 = baseballr::scrape_statcast_savant(start_date = '2021-06-21',
                                               end_date = '2021-06-25', player_type = 'pitcher')

date626630 = baseballr::scrape_statcast_savant(start_date = '2021-06-26',
                                               end_date = '2021-06-30', player_type = 'pitcher')

date701705 = baseballr::scrape_statcast_savant(start_date = '2021-07-01',
                                               end_date = '2021-07-05', player_type = 'pitcher')

date706710 = baseballr::scrape_statcast_savant(start_date = '2021-07-06',
                                               end_date = '2021-07-10', player_type = 'pitcher')

date711 = baseballr::scrape_statcast_savant(start_date = '2021-07-11',
                                               end_date = '2021-07-11', player_type = 'pitcher')

date716720 = baseballr::scrape_statcast_savant(start_date = '2021-07-16',
                                               end_date = '2021-07-20', player_type = 'pitcher')

date721725 = baseballr::scrape_statcast_savant(start_date = '2021-07-21',
                                               end_date = '2021-07-25', player_type = 'pitcher')

date726730 = baseballr::scrape_statcast_savant(start_date = '2021-07-26',
                                               end_date = '2021-07-30', player_type = 'pitcher')

date731803 = baseballr::scrape_statcast_savant(start_date = '2021-07-31',
                                               end_date = '2021-08-03', player_type = 'pitcher')

date804808 = baseballr::scrape_statcast_savant(start_date = '2021-08-04',
                                               end_date = '2021-08-08', player_type = 'pitcher')

date809813 = baseballr::scrape_statcast_savant(start_date = '2021-08-09',
                                               end_date = '2021-08-13', player_type = 'pitcher')

date814818 = baseballr::scrape_statcast_savant(start_date = '2021-08-14',
                                               end_date = '2021-08-18', player_type = 'pitcher')

date819823 = baseballr::scrape_statcast_savant(start_date = '2021-08-19',
                                               end_date = '2021-08-23', player_type = 'pitcher')

date824828 = baseballr::scrape_statcast_savant(start_date = '2021-08-24',
                                               end_date = '2021-08-28', player_type = 'pitcher')

date829901 = baseballr::scrape_statcast_savant(start_date = '2021-08-29',
                                               end_date = '2021-09-01', player_type = 'pitcher')

date902906 = baseballr::scrape_statcast_savant(start_date = '2021-09-02',
                                               end_date = '2021-09-06', player_type = 'pitcher')

date907911 = baseballr::scrape_statcast_savant(start_date = '2021-09-07',
                                               end_date = '2021-09-11', player_type = 'pitcher')

date912916 = baseballr::scrape_statcast_savant(start_date = '2021-09-12',
                                               end_date = '2021-09-16', player_type = 'pitcher')

date917921 = baseballr::scrape_statcast_savant(start_date = '2021-09-17',
                                               end_date = '2021-09-21', player_type = 'pitcher')

date922926 = baseballr::scrape_statcast_savant(start_date = '2021-09-22',
                                               end_date = '2021-09-26', player_type = 'pitcher')

date927930 = baseballr::scrape_statcast_savant(start_date = '2021-09-27',
                                               end_date = '2021-09-30', player_type = 'pitcher')

date10011003 = baseballr::scrape_statcast_savant(start_date = '2021-10-01',
                                                 end_date = '2021-10-03', player_type = 'pitcher')



#combine all data into one data frame
SavantData21 = rbind(date401,    date402407, date408412, date413417, date418422,
                     date423427, date428501, date502506, date507511, date512516,
                     date517521, date522526, date527531, date601605, date606610,
                     date611615, date616620, date621625, date626630, date701705,
                     date706710, date711,    date716720, date721725, date726730,
                     date731803, date804808, date809813, date814818, date819823,
                     date824828, date829901, date902906, date907911, date912916,
                     date917921, date922926, date927930, date10011003)


write.csv(SavantData21, "C:/Users/014497819/Desktop/Thesis/2021 Pitches.csv")

dim(SavantData21)

