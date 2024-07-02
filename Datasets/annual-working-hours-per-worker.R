library(dplyr)
working_hour <- read.csv(file = 'stats-oecd-averageworkinghourperweek.csv')
country_name <- unique(working_hour$Entity)
avg_hours <- aggregate(Average.annual.working.hours.per.worker ~ Year, data = working_hour, mean)
#average working hour per year
working_hour_2000_2015 <- filter(working_hour, Year >= 2000 & Year < 2016)
year_counts <- group_by(working_hour_2000_2015, Entity) |> summarize(SUMYEAR = sum(Year))
year_counts <- filter(year_counts, SUMYEAR == 32120)
complete_countries_working_hour_2000_2015 <- subset(working_hour_2000_2015, Entity %in% year_counts$Entity)
working_hour_groupedbycountry <- aggregate(Average.annual.working.hours.per.worker ~ Entity, data = complete_countries_working_hour_2000_2015, FUN = mean) |> arrange(desc(Average.annual.working.hours.per.worker))
working_hour_groupedbycountry

south_korea_workinghour_2000_2015 <- filter(complete_countries_working_hour_2000_2015, Entity == "South Korea")

top10_countries <- head(working_hour_groupedbycountry, n = 20)

bottom10_coutries <- tail(working_hour_groupedbycountry, n = 20)

top_and_bottom_10_coutnries <- merge(top10_countries, bottom10_coutries, all = TRUE)

library(ggplot2)



working_hour2 <- read.csv(file = 'annual-working-hours-per-worker.csv')









