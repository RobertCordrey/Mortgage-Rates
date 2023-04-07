
library('readxl')
library('dplyr')
library('lubridate')
library("ggplot2")

dat <- read_excel('G:/excel/Freedie Mac mortgage rates and points.xlsx',
                   sheet = 2)

dat <- dat %>%
    na.omit()

yearlyDat <- dat %>%
    group_by(year = year(Date)) %>%
    summarize(avgRate = round(mean(Rate), 2),
              avgPoints = round(mean(Points), 2))

ggplot(yearlyDat, aes(year, avgRate)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = round(seq(min(yearlyDat$year)
                                          , max(yearlyDat$year), by = 4),1)) +
    scale_color_brewer(palette="Paired")+
    theme_minimal() +
    ggtitle("Average Mortage Rate from January 1972 to September 2022") +
    labs(caption = "Blue Hen Analytics - Data from Federal Reserve Economic Database") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption=element_text(hjust = 1)) +
    xlab("Year") +
    ylab("Avg Monthly % Rate")

# shows monthly flucations for each year
ggplot(dat, aes(x = year(Date), y = Rate, group = year(Date))) +
    geom_boxplot() +
    scale_x_continuous(breaks = round(seq(min(yearlyDat$year)
                                          , max(yearlyDat$year), by = 4),1)) +
    scale_color_brewer(palette="Paired")+
    theme_minimal() +
    ggtitle("Monthly Variation Rate from January 1972 to September 2022") +
    labs(subtitle = "Yearly Monthly Rate Variation",
         caption = "Blue Hen Analytics - Data from Federal Reserve Economic Database") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle=element_text(hjust = 0.5),
          plot.caption=element_text(hjust = 1)) +
    xlab("Year") +
    ylab("Monthly % Rate")
