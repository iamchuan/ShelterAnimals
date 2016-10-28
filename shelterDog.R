library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(googleVis)

source("./lib.R")

## set wording directory
setwd("~/Workspace/kaggle/shelterAnimal/")

## load data
animal_train <- read.csv("train.csv", 
                         header = TRUE, 
                         na.strings = "",
                         stringsAsFactors = FALSE)

table(animal_train$AnimalType)
ggplotly(bar_plot(animal_train, "AnimalType", "OutcomeType"))

## Animal Type -> Dog
dog_train <- animal_train %>% 
  filter(AnimalType == "Dog")

## OutcomeType -> OutcomeSubtype SankyChart
outcomeCount <- dog_train %>% 
  group_by(OutcomeType, OutcomeSubtype) %>% count()

Sankey <- gvisSankey(outcomeCount, from="OutcomeType", to="OutcomeSubtype", weight="n")
plot(Sankey)

## Create hasName column
dog_train <- dog_train %>% 
  mutate(hasName = ifelse(is.na(Name), "No", "Yes"))

ggplotly(bar_plot(dog_train, "hasName", "OutcomeType"))
ggplotly(bar_plot(dog_train, "hasName", "OutcomeType", position = "fill"))

## Separate SexuponOutcome
dog_train <- dog_train %>% 
  mutate(sex = ifelse(grepl("Female", SexuponOutcome), "Female",
                      ifelse(grepl("Male", SexuponOutcome), "Male", NA)),
         isNeutered = ifelse(grepl("Unknown", SexuponOutcome), NA, 
                             ifelse(grepl("Intact", SexuponOutcome), "No", "Yes")))

ggplotly(bar_plot(dog_train, "sex", "OutcomeType"))
ggplotly(bar_plot(dog_train, "sex", "OutcomeType", position = "fill"))

ggplotly(bar_plot(dog_train, "isNeutered", "OutcomeType"))
ggplotly(bar_plot(dog_train, "isNeutered", "OutcomeType", position = "fill"))

## Separate Breed (1) -> mix
dog_train <- dog_train %>% 
  mutate(isMix = ifelse(grepl(" Mix", Breed), "Yes", "No"))

ggplotly(bar_plot(dog_train, "isMix", "OutcomeType"))
ggplotly(bar_plot(dog_train, "isMix", "OutcomeType", position = "fill"))

## Separate Breed (2) -> breedMx 
dog_train <- dog_train %>% 
  mutate(breedMx = gsub("/.*", "", gsub(" Mix", "", Breed)))

breedByFreq <- names(sort(table(dog_train$breedMx), decreasing = TRUE))
# dog_train$breedMx <- factor(dog_train$breedMx, levels = breedByFreq)

# (do not use the graph) ggplotly(bar_plot(dog_train, "breedMx", "OutcomeType"))

## Separate Breed (3) -> breedTop 10
dog_train <- dog_train %>% 
  mutate(breedTop = ifelse(breedMx %in% breedByFreq[1:10], 
                           as.character(breedMx), 
                           "Other"))

breedTopFreq <- names(sort(table(dog_train$breedTop), decreasing = TRUE))
dog_train$breedTop <- factor(dog_train$breedTop, levels = breedTopFreq)

ggplotly(bar_plot(dog_train %>% 
                    filter(breedTop != "Other"), 
                  "breedTop", "OutcomeType"))
ggplotly(bar_plot(dog_train %>% 
                    filter(breedTop != "Other"), 
                  "breedTop", "OutcomeType", 
                  position = "fill"))

## Separate Color -> pattern_1, pattern_2
# pattern list
unique(unlist(strsplit(unique(dog_train$Color), "/")))

# Create primary and secondary pattern
dog_train <- dog_train %>% 
  mutate(pattern_pri = ifelse(grepl("/", Color), 
                            gsub("/.*", "", Color), 
                            Color),
         pattern_sec = ifelse(grepl("/", Color), 
                            gsub(".*/", "", Color), 
                            Color))

# (Optional) change levels based on fequency 
# patternFreq_1 <- names(sort(table(cat_train$pattern_1), decreasing = TRUE))
# cat_train$pattern_1 <- factor(cat_train$pattern_1, levels = patternFreq_1)
# 
# patternFreq_2 <- names(sort(table(cat_train$pattern_2), decreasing = TRUE))
# cat_train$pattern_2 <- factor(cat_train$pattern_2, levels = patternFreq_2)

# cat pattern df
dog_pattern <- dog_train %>% 
  group_by(pattern_pri, pattern_sec) %>% 
  summarise(Adoption = sum(OutcomeType == "Adoption"),
            Died = sum(OutcomeType == "Died"),
            Euthanasia = sum(OutcomeType == "Euthanasia"),
            Return_to_owner = sum(OutcomeType == "Return_to_owner"),
            Transfer = sum(OutcomeType == "Transfer"),
            Total = n())
# pattern freqency heatmap
ggplotly(heat_map(dog_pattern, "pattern_pri", "pattern_sec", "Total"))

# pattern freqency line chart
ggplotly(ggplot(dog_pattern %>% arrange(desc(Total)), 
                aes(1:nrow(dog_pattern), Total)) + 
           geom_line())

# Create overall patternTop
dog_patternTop <- dog_pattern %>% 
  filter(Total > 100) %>%
  arrange(desc(Total))

ggplotly(heat_map(dog_patternTop, "pattern_pri", "pattern_sec", 
                  colvar = "Total"))

ggplotly(heat_map(dog_patternTop, "pattern_pri", "pattern_sec", 
                  colvar = "Adoption / Total"))

ggplotly(heat_map(dog_patternTop,"pattern_pri", "pattern_sec", 
                  colvar = "Died / Total"))

ggplotly(heat_map(dog_patternTop, "pattern_pri", "pattern_sec", 
                  colvar = "Euthanasia / Total"))

ggplotly(heat_map(dog_patternTop, "pattern_pri", "pattern_sec", 
                  colvar = "Return_to_owner / Total"))

ggplotly(heat_map(dog_patternTop, "pattern_pri", "pattern_sec", 
                  colvar = "Transfer / Total"))

ggplotly(ggplot(dog_patternTop, 
                aes(1:nrow(dog_patternTop))) + 
           geom_line(aes(y = Adoption)))

## Separate age -> ageInDay ageInWeek ageInMonth ageInYear
dog_train <- dog_train %>% 
  mutate(ageInDay = toDay(AgeuponOutcome),
         ageInWeek = toWeek(AgeuponOutcome),
         ageInMonth = toMonth(AgeuponOutcome),
         ageInYear = toYear(AgeuponOutcome))

ggplotly(bar_plot(dog_train %>% 
                    mutate(ageInYear = ifelse(ageInYear > 8, 
                                              "9 and older", 
                                              ageInYear)), 
                  "ageInYear", "OutcomeType"))

ggplotly(bar_plot(dog_train %>% 
                    mutate(ageInYear = ifelse(ageInYear > 8, 
                                              "9 and older",
                                              ageInYear)), 
                  "ageInYear", "OutcomeType", position = "fill"))

ggplotly(bar_plot(dog_train, "ageInMonth", "OutcomeType"))
ggplotly(bar_plot(dog_train, "ageInMonth", "OutcomeType", position = "fill"))

ggplotly(bar_plot(dog_train, "ageInWeek", "OutcomeType"))
ggplotly(bar_plot(dog_train, "ageInWeek", "OutcomeType", position = "fill"))

ggplotly(bar_plot(dog_train, "ageInDay", "OutcomeType"))
ggplotly(bar_plot(dog_train, "ageInDay", "OutcomeType", position = "fill"))

# do not use
ggplot(dog_train %>% 
         group_by(ageInYear) %>% 
         summarise(total = n()) %>% 
         filter(!is.na(ageInYear)), aes(ageInYear, total)) +
  geom_step()

## Time series on DateTime

# Create timestamp
timestamp <- strptime(dog_train$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "CST6CDT")

# by hour
ggplotly(bar_plot(dog_train %>% 
                    mutate(Hour = timestamp$hour), 
                  "Hour", "OutcomeType"))

ggplotly(bar_plot(dog_train %>% 
                    mutate(Hour = timestamp$hour), 
                  "Hour", "OutcomeType",
                  position = "fill"))
# by weekday
wdayLevel <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

ggplotly(bar_plot(dog_train %>% 
                    mutate(Weekday = factor(weekdays(timestamp), 
                                            levels = wdayLevel)), 
                  "Weekday", "OutcomeType"))

ggplotly(bar_plot(dog_train %>% 
                    mutate(Weekday = factor(weekdays(timestamp), 
                                            levels = wdayLevel)), 
                  "Weekday", "OutcomeType",
                  position = "fill"))
# by month
monLevel <- c("January", "February", "March", "April",
              "May", "June", "July", "August", 
              "September", "October", "November", "December")

ggplotly(bar_plot(dog_train %>% 
                    mutate(Month = factor(months(timestamp),
                                          levels = monLevel)), 
                  "Month", "OutcomeType"))

ggplotly(bar_plot(dog_train %>% 
                    mutate(Month = factor(months(timestamp),
                                          levels = monLevel)), 
                  "Month", "OutcomeType",
                  position = "fill"))

# heatmap Hour vs Weekday
dog_WeekHour <- dog_train %>%
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

ggplotly(heat_map(dog_WeekHour, "Hour", "Weekday",
                  colvar = "Adoption", 
                  low = "white",
                  high = "blue"))

ggplotly(bar_plot(animal_train, "ageInWeek", "OutcomeType") + 
           facet_grid(. ~ AnimalType))
ggplotly(bar_plot(animal_train, "ageInWeek", "OutcomeType", position = "fill") + 
           facet_grid(. ~ AnimalType))

ggplotly(bar_plot(animal_train, "ageInDay", "OutcomeType") + 
           facet_grid(. ~ AnimalType))
ggplotly(bar_plot(animal_train, "ageInDay", "OutcomeType", position = "fill") + 
           facet_grid(. ~ AnimalType))

