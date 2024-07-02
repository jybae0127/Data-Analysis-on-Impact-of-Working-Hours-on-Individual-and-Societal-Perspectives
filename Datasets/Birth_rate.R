birth_rate <- read.csv(file = 'crude-birth-rate.csv')
Korea_brith_rate <- filter(birth_rate, Entity == "South Korea")
library(ggplot2)
plot <- ggplot(Korea_brith_rate, aes(x=Year, y=`Birth.rate...Sex..all...Age..all...Variant..estimates`, group=1)) + 
  geom_line(color="blue", size=1) + 
  
  xlab("Year") + ylab("Crude Birth Rate") +
  
  ggtitle("Crude Birth Rate in South Korea from 1950 to 2021") +
  
  theme_bw() +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12)) +
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5)) +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10)) +
  theme(panel.grid.major.y = element_line(color="gray", linetype = "dashed"))

# Display the plot
plot


year <- c(1960, 1970, 1980, 1990, 2000, 2010, 2020)
birth_rate <- c(38.6, 36.8, 24.7, 18.4, 14.0, 12.5, 10.6)
df <- data.frame(year, birth_rate)



