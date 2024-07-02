working_hour_weekly <- read.csv(file = 'stats-oecd-averageworkinghour perweek.csv')

working_hour_weekly <- filter(working_hour_weekly, Employment.status == "Total employment") |> select(COUNTRY, Country, TIME, Value)

working_hour_weekly_Korea <- filter(working_hour_weekly, COUNTRY == "KOR")

korea_weeklyhour_2010_2021 <- filter(working_hour_weekly2, COUNTRY == "KOR")

korea_weeklyhour_2010_2021 <- rename(korea_weeklyhour_2010_2021, Year = TIME)


Happiness
three_happiness <- three_happiness %>% rename(Country = Entity)
three_happiness_2010_2021 <- filter(three_happiness, Year >= 2010)
three_happiness_2010_2021 <- select(three_happiness_2010_2021, Code, Year, Cantril.ladder.score)


#Workinghour
working_hour_weekly <- read.csv(file = 'stats-oecd-averageworkinghour perweek.csv')
working_hour_weekly <- filter(working_hour_weekly, Employment.status == "Total employment") |> select(COUNTRY, Country, TIME, Value)
working_hour_weekly <- rename(working_hour_weekly, Code = COUNTRY)
three_working_hour_weekly_2010_2021 <- rename(three_working_hour_weekly_2010_2021, Year = TIME)
three_working_hour_weekly_2010_2021 <- select(three_working_hour_weekly_2010_2021, Code, Year, Value)

workinghour_happiness <- merge(three_happiness_2010_2021, three_working_hour_weekly_2010_2021, by = c("Code", "Year"), all.x = TRUE)

workinghour_happiness <- rename(workinghour_happiness, Working.hour = Value)


ggplot(workinghour_happiness, aes(x = Working.hour, y = Cantril.ladder.score, color = Code)) + geom_point() + ggtitle("Relationship between Working Hours and Cantril Ladder Scores") + xlab("Working Hours") + ylab("Cantril Ladder Score")
