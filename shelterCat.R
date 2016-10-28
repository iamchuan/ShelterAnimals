library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(googleVis)
source("./lib.R")

## set wording directory
setwd("~/Desktop/GitHub/ShelterAnimals/")

## load data
animal_train <- read.csv("train.csv", 
                         header = TRUE, 
                         na.strings = "",
                         stringsAsFactors = FALSE)

table(animal_train$AnimalType)

ggplotly(bar_plot(animal_train, "AnimalType", "OutcomeType"))

## Animal Type -> Cat
cat_train <- animal_train %>% 
  filter(AnimalType == "Cat")

## OutcomeType -> OutcomeSubtype SankyChart
outcomeCount <- cat_train %>% 
  group_by(OutcomeType, OutcomeSubtype) %>% count()

Sankey <- gvisSankey(outcomeCount, from="OutcomeType", to="OutcomeSubtype", weight="n")
plot(Sankey)

## Create hasName column
cat_train <- cat_train %>% 
  mutate(hasName = ifelse(is.na(Name), "No", "Yes"))

ggplotly(bar_plot(cat_train, "hasName", "OutcomeType"))
ggplotly(bar_plot(cat_train, "hasName", "OutcomeType", position = "fill"))

## Separate SexuponOutcome
cat_train <- cat_train %>% 
  mutate(sex = ifelse(grepl("Female", SexuponOutcome), "Female",
                      ifelse(grepl("Male", SexuponOutcome), "Male", NA)),
         isNeutered = ifelse(grepl("Unknown", SexuponOutcome), NA, 
                             ifelse(grepl("Intact", SexuponOutcome), "No", "Yes")))

plt_1 <- ggplotly(bar_plot(cat_train, "sex", "OutcomeType"))
plt_2 <- ggplotly(bar_plot(cat_train, "sex", "OutcomeType", position = "fill"))

grid.

ggplotly(bar_plot(cat_train, "isNeutered", "OutcomeType"))
ggplotly(bar_plot(cat_train, "isNeutered", "OutcomeType", position = "fill"))

## Separate Breed (1) -> mix
cat_train <- cat_train %>% 
  mutate(isMix = ifelse(grepl(" Mix", Breed), "Yes", "No"))

ggplotly(bar_plot(cat_train, "isMix", "OutcomeType"))
ggplotly(bar_plot(cat_train, "isMix", "OutcomeType", position = "fill"))

## Separate Breed (2) -> breedMx
cat_train <- cat_train %>% 
  mutate(breedMx = gsub("/.*", "", gsub(" Mix", "", Breed)))

breedByFreq <- names(sort(table(cat_train$breedMx), decreasing = TRUE))
cat_train$breedMx <- factor(cat_train$breedMx, levels = breedByFreq)

ggplotly(bar_plot(cat_train, "breedMx", "OutcomeType"))

## Separate Breed (3) -> breedTop 4
cat_train <- cat_train %>% 
  mutate(breedTop = ifelse(breedMx %in% breedByFreq[1:4], 
                           as.character(breedMx), 
                           "Other"))

breedTopFreq <- names(sort(table(cat_train$breedTop), decreasing = TRUE))
cat_train$breedTop <- factor(cat_train$breedTop, levels = breedTopFreq)

ggplotly(bar_plot(cat_train, "breedTop", "OutcomeType"))
ggplotly(bar_plot(cat_train, "breedTop", "OutcomeType", position = "fill"))

## Separate Color -> pattern_1, pattern_2
# pattern list
unique(unlist(strsplit(unique(cat_train$Color), "/")))

# Create primary and secondary pattern
cat_train <- cat_train %>% 
  mutate(pattern_1 = ifelse(grepl("/", Color), 
                            gsub("/.*", "", Color), 
                            Color),
         pattern_2 = ifelse(grepl("/", Color), 
                            gsub(".*/", "", Color), 
                            Color))

# (Optional) change levels based on fequency 
# patternFreq_1 <- names(sort(table(cat_train$pattern_1), decreasing = TRUE))
# cat_train$pattern_1 <- factor(cat_train$pattern_1, levels = patternFreq_1)
# 
# patternFreq_2 <- names(sort(table(cat_train$pattern_2), decreasing = TRUE))
# cat_train$pattern_2 <- factor(cat_train$pattern_2, levels = patternFreq_2)

# cat pattern df
cat_pattern <- cat_train %>% 
  group_by(pattern_1, pattern_2) %>% 
  summarise(Adoption = sum(OutcomeType == "Adoption"),
            Died = sum(OutcomeType == "Died"),
            Euthanasia = sum(OutcomeType == "Euthanasia"),
            Return_to_owner = sum(OutcomeType == "Return_to_owner"),
            Transfer = sum(OutcomeType == "Transfer"),
            Total = n())
# pattern freqency heatmap
ggplotly(ggplot(cat_pattern, aes(pattern_1, pattern_2, fill = Total)) + 
           geom_tile() +
           scale_fill_gradient(trans = "log"))
# pattern freqency line chart
ggplotly(ggplot(cat_pattern %>% arrange(desc(Total)), 
                aes(1:nrow(cat_pattern), Total)) + 
           geom_line())

# Create overall patternTop
cat_patternTop <- cat_pattern %>% 
  filter(Total > 50) %>%
  arrange(desc(Total))

ggplotly(heat_map(cat_patternTop, "pattern_1", "pattern_2", 
                  colvar = "Total"))

ggplotly(heat_map(cat_patternTop, "pattern_1", "pattern_2", 
                  colvar = "Adoption / Total"))

ggplotly(heat_map(cat_patternTop, "pattern_1", "pattern_2", 
                  colvar = "Died / Total"))

ggplotly(heat_map(cat_patternTop, "pattern_1", "pattern_2", 
                  colvar = "Euthanasia / Total"))

ggplotly(heat_map(cat_patternTop, "pattern_1", "pattern_2", 
                  colvar = "Return_to_owner / Total"))

ggplotly(heat_map(cat_patternTop, "pattern_1", "pattern_2", 
                  colvar = "Transfer / Total"))

ggplotly(ggplot(cat_patternTop, 
                aes(1:nrow(cat_patternTop))) + 
           geom_line(aes(y = Adoption)))

## Separate age -> ageInDay ageInWeek ageInMonth ageInYear
cat_train <- cat_train %>% 
  mutate(ageInDay = toDay(AgeuponOutcome),
         ageInWeek = toWeek(AgeuponOutcome),
         ageInMonth = toMonth(AgeuponOutcome),
         ageInYear = toYear(AgeuponOutcome))

ggplotly(bar_plot(cat_train %>% 
                    mutate(ageInYear = ifelse(ageInYear > 8, 
                                              "9 and older", 
                                              ageInYear)), 
                  "ageInYear", "OutcomeType"))
ggplotly(bar_plot(cat_train %>% 
                    mutate(ageInYear = ifelse(ageInYear > 8, 
                                              "9 and older",
                                              ageInYear)), 
                  "ageInYear", "OutcomeType", position = "fill"))

ggplotly(bar_plot(cat_train, "ageInMonth", "OutcomeType"))
ggplotly(bar_plot(cat_train, "ageInMonth", "OutcomeType", position = "fill"))

ggplotly(bar_plot(cat_train, "ageInWeek", "OutcomeType"))
ggplotly(bar_plot(cat_train, "ageInWeek", "OutcomeType", position = "fill"))

ggplotly(bar_plot(cat_train, "ageInDay", "OutcomeType"))
ggplotly(bar_plot(cat_train, "ageInDay", "OutcomeType", position = "fill"))

ggplot(cat_train %>% 
         group_by(ageInYear) %>% 
         summarise(total = n()) %>% 
         filter(!is.na(ageInYear)), aes(ageInYear, total)) +
  geom_step()

## Time series on DateTime

# Create timestamp
timestamp <- strptime(cat_train$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "CST6CDT")

# by hour
ggplotly(bar_plot(cat_train %>% 
                    mutate(Hour = timestamp$hour), 
                  "Hour", "OutcomeType"))

ggplotly(bar_plot(cat_train %>% 
                    mutate(Hour = timestamp$hour), 
                  "Hour", "OutcomeType",
                  position = "fill"))
# by weekday
wdayLevel <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

ggplotly(bar_plot(cat_train %>% 
                    mutate(Weekday = factor(weekdays(timestamp), 
                                            levels = wdayLevel)), 
                  "Weekday", "OutcomeType"))

ggplotly(bar_plot(cat_train %>% 
                    mutate(Weekday = factor(weekdays(timestamp), 
                                            levels = wdayLevel)), 
                  "Weekday", "OutcomeType",
                  position = "fill"))
# by month
monLevel <- c("January", "February", "March", "April",
              "May", "June", "July", "August", 
              "September", "October", "November", "December")

ggplotly(bar_plot(cat_train %>% 
                    mutate(Month = factor(months(timestamp),
                                          levels = monLevel)), 
                  "Month", "OutcomeType"))

ggplotly(bar_plot(cat_train %>% 
                    mutate(Month = factor(months(timestamp),
                                          levels = monLevel)), 
                  "Month", "OutcomeType",
                  position = "fill"))

# heatmap Hour vs Weekday
cat_WeekHour <- cat_train %>%
  mutate(Hour = timestamp$hour,
         Weekday = factor(weekdays(timestamp), 
                          levels = wdayLevel)) %>%
  group_by(Hour, Weekday) %>%
  summarise(Adoption = sum(OutcomeType == "Adoption"),
            Died = sum(OutcomeType == "Died"),
            Euthanasia = sum(OutcomeType == "Euthanasia"),
            Return_to_owner = sum(OutcomeType == "Return_to_owner"),
            Transfer = sum(OutcomeType == "Transfer"),
            Total = n())

ggplotly(heat_map(cat_WeekHour, "Hour", "Weekday",
                  colvar = "Adoption", 
                  low = "white",
                  high = "blue"))

# cat_tsHour <- cat_train %>% 
#   group_by(timestamp$hour) %>% 
#   summarise(Adoption = sum(OutcomeType == "Adoption"),
#             Died = sum(OutcomeType == "Died"),
#             Euthanasia = sum(OutcomeType == "Euthanasia"),
#             Return_to_owner = sum(OutcomeType == "Return_to_owner"),
#             Transfer = sum(OutcomeType == "Transfer"),
#             Total = n()) %>%
#   rename(Hour = `timestamp$hour`)
         
