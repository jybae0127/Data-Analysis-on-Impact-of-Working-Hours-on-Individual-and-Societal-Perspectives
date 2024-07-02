library(dplyr)
library(ggplot2)

#Code == "KOR" | Code == "MEX" | Code == "DEU" | Code == "SWE" | Code == "URY" | Code == "ISL" | Code == "BEL" | Code == "LUX" | Code == "FRA" | Code == "DNK" | Code == "NLD" | Code == "NOR" | Code == "MMR" | Code == "KHM"| Code == "SGP"| Code == "THA"| Code == "CRI"| Code == "MYS"| Code == "HKG"| Code == "VNM")


#Happiness
happiness <- read.csv(file = 'happiness-cantril-ladder.csv')
three_happiness <- filter(happiness, Code == "KOR" | Code == "DEU" | Code == "NOR" | Code == "NLD" | Code == "DNK" | Code == "FRA" | Code == "MEX" | Code == "HKG" | Code == "THA" | Code == "CHN" | Code == "IND")
three_happiness <- three_happiness %>% rename(Country = Entity)
three_happiness_2010_2021 <- filter(three_happiness, Year >= 2010)
three_happiness_2010_2021 <- select(three_happiness_2010_2021, Code, Year, Cantril.ladder.score)


#Workinghour
working_hour_annual <- read.csv(file = 'annual-working-hours-per-worker.csv')
working_hour_annual <- filter(working_hour_annual, Code == "KOR" | Code == "DEU" | Code == "NOR" | Code == "NLD" | Code == "DNK" | Code == "FRA" | Code == "MEX" | Code == "HKG" | Code == "THA" | Code == "CHN" | Code == "IND")
working_hour_annual <- rename(working_hour_annual, Annual.working.hour = Average.annual.working.hours.per.worker)
working_hour_annual <- filter(working_hour_annual, Year >= 2010)
working_hour_annual <- select(working_hour_annual, Code, Year, Annual.working.hour)

workinghour_happiness <- merge(three_happiness_2010_2021, working_hour_annual, by = c("Code", "Year"), all.x = TRUE)



#suiciderate
suicide_rate <- read.csv(file = 'suicide-rates-vs-prevalence-of-mental-and-substance-use-disorders.csv')
suicide_rate <- filter(suicide_rate, Code == "KOR" | Code == "DEU" | Code == "NOR" | Code == "NLD" | Code == "DNK" | Code == "FRA" | Code == "MEX" | Code == "HKG" | Code == "THA" | Code == "CHN" | Code == "IND")
suicide_rate <- filter(suicide_rate, Year >= 2010, Year <= 2019)
suicide_rate <- rename(suicide_rate, Deaths.by.suicide = Deaths...Self.harm...Sex..Both...Age..Age.standardized..Rate.)
suicide_rate_2010_2021 <- select(suicide_rate, Code, Year, Deaths.by.suicide)

workinghour_suicide <- merge(working_hour_annual, suicide_rate_2010_2021, by = c("Code", "Year"), all.x = TRUE)


#gdp per capita
gdppercapita <- read.csv(file = 'gdp-per-capita-worldbank.csv')
gdppercapita <- filter(gdppercapita, Code == "KOR" | Code == "DEU" | Code == "NOR" | Code == "NLD" | Code == "DNK" | Code == "FRA" | Code == "MEX" | Code == "HKG" | Code == "THA" | Code == "CHN" | Code == "IND")
gdppercapita <- filter(gdppercapita, Year >= 2010, Year <= 2019)
gdppercapita <- rename(gdppercapita, GDP.per.capita = GDP.per.capita..PPP..constant.2017.international...)
workinghour_gdppercapita <- merge(working_hour_annual, gdppercapita, by = c("Code", "Year"), all.x = TRUE)
workinghour_gdppercapita <- select (workinghour_gdppercapita, Code, Year, Annual.working.hour, GDP.per.capita)


#gini coefficient
gini <- read.csv(file = 'economic-inequality-gini-index.csv')
gini <- filter(gini, Year >=2010)
gini <- select (gini, Code, Year, Gini.coefficient)
gini <- filter(gini, Code == "KOR" | Code == "DEU" | Code == "NOR" | Code == "NLD" | Code == "DNK" | Code == "FRA" | Code == "MEX" | Code == "HKG" | Code == "THA" | Code == "CHN" | Code == "IND")
workinghour_gini <- merge(working_hour_annual, gini, by = c("Code", "Year"), all.x = TRUE)

lm_model_workinghour_happiness <- lm(Cantril.ladder.score ~ Annual.working.hour, data = workinghour_happiness)
summary(lm_model_workinghour_happiness)
ggplot(workinghour_happiness, aes(x = Annual.working.hour, y = Cantril.ladder.score, color = Code)) + geom_point() + ggtitle("Relationship between Working Hours and Cantril Ladder Scores") + xlab("Annual working hours") + ylab("Cantril ladder score")

lm_model_workinghour_suicide <- lm(Deaths.by.suicide ~ Annual.working.hour, data = workinghour_suicide)
summary(lm_model_workinghour_suicide)
ggplot(workinghour_suicide, aes(x = Annual.working.hour, y = Deaths.by.suicide, color = Code)) + geom_point() + ggtitle("Relationship between Working Hours and Suicide") + xlab("Annual working hours") + ylab("Annual suicide per 100,000 people")


lm_model_workinghour_gdppercapita <- lm(GDP.per.capita ~ Annual.working.hour, data = workinghour_gdppercapita)
summary(lm_model_workinghour_gdppercapita)
ggplot(workinghour_gdppercapita, aes(x = Annual.working.hour, y = GDP.per.capita, color = Code)) + geom_point() + ggtitle("Relationship between Working Hours and GDP per capita") + xlab("nnual working hours") + ylab("GDP per capita")

lm_model_workinghour_gini <- lm(Gini.coefficient ~ Annual.working.hour, data = workinghour_gini)
summary(lm_model_workinghour_gini)
ggplot(workinghour_gini, aes(x = Annual.working.hour, y = Gini.coefficient, color = Code)) + geom_point() + ggtitle("Relationship between Working Hours and Income Inequality") + xlab("nnual working hourss") + ylab("Gini Coefficient")
