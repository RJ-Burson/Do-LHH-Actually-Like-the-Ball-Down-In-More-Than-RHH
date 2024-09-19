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



date721725 = baseballr::scrape_statcast_savant(start_date = '2022-07-21',
                                               end_date = '2022-07-25', player_type = 'pitcher')

date726730 = baseballr::scrape_statcast_savant(start_date = '2022-07-26',
                                               end_date = '2022-07-30', player_type = 'pitcher')

date731803 = baseballr::scrape_statcast_savant(start_date = '2022-07-31',
                                               end_date = '2022-08-03', player_type = 'pitcher')

date804808 = baseballr::scrape_statcast_savant(start_date = '2022-08-04',
                                               end_date = '2022-08-08', player_type = 'pitcher')

date809813 = baseballr::scrape_statcast_savant(start_date = '2022-08-09',
                                               end_date = '2022-08-13', player_type = 'pitcher')

date814818 = baseballr::scrape_statcast_savant(start_date = '2022-08-14',
                                               end_date = '2022-08-18', player_type = 'pitcher')

date819823 = baseballr::scrape_statcast_savant(start_date = '2022-08-19',
                                               end_date = '2022-08-23', player_type = 'pitcher')

date824828 = baseballr::scrape_statcast_savant(start_date = '2022-08-24',
                                               end_date = '2022-08-28', player_type = 'pitcher')

date829901 = baseballr::scrape_statcast_savant(start_date = '2022-08-29',
                                               end_date = '2022-09-01', player_type = 'pitcher')

date902906 = baseballr::scrape_statcast_savant(start_date = '2022-09-02',
                                               end_date = '2022-09-06', player_type = 'pitcher')

date907911 = baseballr::scrape_statcast_savant(start_date = '2022-09-07',
                                               end_date = '2022-09-11', player_type = 'pitcher')

date912916 = baseballr::scrape_statcast_savant(start_date = '2022-09-12',
                                               end_date = '2022-09-16', player_type = 'pitcher')

date917921 = baseballr::scrape_statcast_savant(start_date = '2022-09-17',
                                               end_date = '2022-09-21', player_type = 'pitcher')

date922926 = baseballr::scrape_statcast_savant(start_date = '2022-09-22',
                                               end_date = '2022-09-26', player_type = 'pitcher')

date927930 = baseballr::scrape_statcast_savant(start_date = '2022-09-27',
                                               end_date = '2022-09-30', player_type = 'pitcher')

date10011005 = baseballr::scrape_statcast_savant(start_date = '2022-10-01',
                                                 end_date = '2022-10-05', player_type = 'pitcher')




#combine all data into one data frame
SavantData22_Post = rbind(date721725, date726730, date731803, date804808, 
                          date809813, date814818, date819823, date824828, 
                          date829901, date902906, date907911, date912916, 
                          date917921, date922926, date927930, date10011005)


write.csv(SavantData22_Post, "C:/Users/014497819/Desktop/Thesis/2022 Pitches Post All-Star.csv")

dim(SavantData22_Post)

401567+303649
708539-705216
