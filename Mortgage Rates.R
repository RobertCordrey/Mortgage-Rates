

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(fredr)

# Get the current month and year
month <- format(Sys.Date(), "%B")
year <- format(Sys.Date(), "%Y")

# Set FRED API key
fredr_set_key("*****")

# Retrieve monthly mortgage rates data from FRED
dat <- fredr(
    series_id = 'MORTGAGE30US',
    observation_start = as.Date('1971-01-01'),
    observation_end = as.Date(Sys.Date())
)

min_date <- format(min(dat$date), '%B %Y')

# View data
head(dat)

# dat <- read_excel(file.choose())
# dat <- read_excel('G:/excel/Freedie Mac 30 year mortgage rates and points.xlsx',
#                    sheet = 2)

dat <- dat %>%
    na.omit() %>%
    select(1, 3) %>%
    rename(Date = 1, Rate = 2)


yearlyDat <- dat %>%
    group_by(year = year(Date)) %>%
    summarize(avgRate = round(mean(Rate), 2))

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
    ggtitle(paste("Monthly Variation Rate from ", min_date, " to ", month, year)) +
    labs(subtitle = "Yearly Monthly Rate Variation",
         caption = "Blue Hen Analytics - Data from Federal Reserve Economic Database") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle=element_text(hjust = 0.5),
          plot.caption=element_text(hjust = 1)) +
    xlab("Year") +
    ylab("Monthly % Rate")


homeSales <- fredr(
    series_id = 'EXHOSLUSM495S'
    )

homeSales1 <- homeSales %>%
    select(1, 3) %>%
    rename(Date = 1, Total = 2) %>%
    na.omit()



