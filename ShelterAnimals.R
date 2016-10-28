### <<<<<<<<<< packages 
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(googleVis)

### <<<<<<<<<< loaddata
animal_train <- read.csv("/Users/apple/Desktop/GitHub/ShelterAnimals/train.csv", 
                         header = TRUE, 
                         na.strings = "",
                         stringsAsFactors = FALSE)

head(animal_train)

cat_train <- animal_train %>% 
  filter(AnimalType == "Cat")
dog_train <- animal_train %>% 
  filter(AnimalType == "Dog")



### <<<<<<<<<< functions for age conversion

toDay <- function(x) {
  sapply(x, function(x){
    age <- unlist(strsplit(x, split = " "))
    if(grepl("day", age[2])) 
      return(as.numeric(age[1]))
    else 
      return(NA)
  })
}

toWeek <- function(x) {
  sapply(x, function(x){
    age <- unlist(strsplit(x, split = " "))
    if(grepl("day", age[2])) 
      return(0)
    else if(grepl("week", age[2])) 
      return(as.numeric(age[1]))
    else 
      return(NA)
  })
}

toMonth <- function(x) {
  sapply(x, function(x){
    age <- unlist(strsplit(x, split = " "))
    if(grepl("day", age[2]) | grepl("week", age[2])) 
      return(0)
    else if(grepl("month", age[2])) 
      return(as.numeric(age[1]))
    else return(NA)
  })
}

toYear <- function(x) {
  sapply(x, function(x){
    age <- unlist(strsplit(x, split = " "))
    if(grepl("day", age[2]) | grepl("week", age[2]) | grepl("month", age[2])) 
      return(0)
    else if(grepl("year", age[2])) 
      return(as.numeric(age[1]))
    else return(NA)
  })
}


### <<<<<<<<<< functions for graph
bar_plot <- function(data, xvar, group, 
                     position = "stack", 
                     color = "black",
                     palette = "Spectral") {
  g <- ggplot(data, aes_string(xvar, fill = group)) +
    geom_bar(position = position, color = color) + 
    scale_fill_brewer(palette = palette)
  if(position == "fill") {
    g <- g + ylab("Percentage")
  }
  return(g)
}

heat_map <- function(data, xvar, yvar, colvar, 
                     low = "white",
                     high = "black") {
  ggplot(data, aes_string(xvar, yvar)) + 
    geom_tile(aes_string(fill = colvar)) +
    scale_fill_gradient(low = low, high = high) +
    theme_classic()
}


### <<<<<<<<<< Outcome Types & Outcome Subtypes
outcomeCount <- animal_train %>% 
  group_by(OutcomeType, OutcomeSubtype) %>% count()

Sankey <- gvisSankey(outcomeCount, from="OutcomeType", to="OutcomeSubtype", weight="n")
plot(Sankey)


### <<<<<<<<<< Cats vs. Dogs This dataset contains 11134 cats and 15595 dogs
ggplotly(bar_plot(animal_train, "AnimalType", "OutcomeType"))
ggplotly(bar_plot(animal_train, "AnimalType", "OutcomeType", position = "fill"))


### <<<<<<<<<< Does Name Matter?
# Created a new varible "hasName" ("No"= NA, "Yes"= has a name)

animal_train <- animal_train %>% 
  mutate(hasName = ifelse(is.na(Name), "No", "Yes"))

ggplotly(bar_plot(animal_train, "hasName", "OutcomeType") +
           facet_grid(. ~ AnimalType))

ggplotly(bar_plot(animal_train, "hasName", "OutcomeType", position = "fill") +
           facet_grid(. ~ AnimalType)) # for %


### <<<<<<<<<< Does Sex Matter?
# Seperated "SexType" to "Sex" and "isNeutered"

animal_train <- animal_train %>%
  mutate(sex = ifelse(grepl("Female", SexuponOutcome), "Female",
                      ifelse(grepl("Male", SexuponOutcome), "Male", NA)),
         isNeutered = ifelse(grepl("Unknown", SexuponOutcome), NA,
                             ifelse(grepl("Intact", SexuponOutcome), "No", "Yes")))

ggplotly(bar_plot(animal_train, "sex", "OutcomeType") +
           facet_grid(. ~ AnimalType))

ggplotly(bar_plot(animal_train, "sex", "OutcomeType", position = "fill") +
           facet_grid(. ~ AnimalType)) # for %


### <<<<<<<<<< Does Spaying/Neutering Matter?

# Seperated "SexType" to "Sex" and "isNeutered"

ggplotly(bar_plot(animal_train, "isNeutered", "OutcomeType") +
           facet_grid(. ~ AnimalType))

ggplotly(bar_plot(animal_train, "isNeutered", "OutcomeType", position = "fill") +
           facet_grid(. ~ AnimalType)) # for %

### <<<<<<<<<< Does Mixed Breed Matter?

#Seperated "BreedType" to "isMix" and "Breed"

# Separate Breed (1) -> mix
animal_train <- animal_train %>% 
  mutate(isMix = ifelse(grepl(" Mix", Breed), "Yes", "No"))

ggplotly(bar_plot(animal_train, "isMix", "OutcomeType") +
           facet_grid(. ~ AnimalType))

ggplotly(bar_plot(animal_train, "isMix", "OutcomeType", position = "fill") +
           facet_grid(. ~ AnimalType)) # for %


### <<<<<<<<<< Does Breed Matter? (Cats)

# Top eight popular breeds are selected

animal_train <- animal_train %>% 
  mutate(breed_Top = gsub("/.*", "", gsub(" Mix", "", Breed)))

animalTopBreed <- animal_train %>% 
  group_by(AnimalType, breed_Top) %>% 
  summarise(Total = n()) %>% 
  top_n(8, Total)

breedByFreq <- animalTopBreed$breed_Top[order(animalTopBreed$Total, decreasing = TRUE)]

ggplotly(bar_plot(animal_train %>% 
                    filter(AnimalType == "Cat", breed_Top %in% breedByFreq) %>%
                    mutate(breed_Top = factor(breed_Top, levels = breedByFreq)), 
                  "breed_Top", "OutcomeType") + coord_flip())

ggplotly(bar_plot(animal_train %>% 
                    filter(AnimalType == "Cat", breed_Top %in% breedByFreq) %>%
                    mutate(breed_Top = factor(breed_Top, levels = breedByFreq)), 
                  "breed_Top", "OutcomeType", position = "fill") + coord_flip()) # for %


### <<<<<<<<<< Does Breed Matter? (Dogs)

# Top eight breeds are selected

ggplotly(bar_plot(animal_train %>% 
                    filter(AnimalType == "Dog", breed_Top %in% breedByFreq) %>%
                    mutate(breed_Top = factor(breed_Top, levels = breedByFreq)), 
                  "breed_Top", "OutcomeType") + coord_flip())

ggplotly(bar_plot(animal_train %>% 
                    filter(AnimalType == "Dog", breed_Top %in% breedByFreq) %>%
                    mutate(breed_Top = factor(breed_Top, levels = breedByFreq)), 
                  "breed_Top", "OutcomeType", position = "fill") + coord_flip()) # for %


### <<<<<<<<<< Does Age Matter? (By year)

# Converted "AgeuponOutcome" to "ageInDay", "ageInWeek", "ageInMonth", and "ageInYear"

animal_train <- animal_train %>% 
  mutate(ageInDay = toDay(AgeuponOutcome),
         ageInWeek = toWeek(AgeuponOutcome),
         ageInMonth = toMonth(AgeuponOutcome),
         ageInYear = toYear(AgeuponOutcome))

ggplotly(bar_plot(animal_train %>% 
                    mutate(ageInYear = ifelse(ageInYear > 8, 
                                              "9 and older", 
                                              ageInYear)),
                  "ageInYear", "OutcomeType") + 
           facet_grid(. ~ AnimalType))

ggplotly(bar_plot(animal_train %>% 
                    mutate(ageInYear = ifelse(ageInYear > 8, 
                                              "9 and older", 
                                              ageInYear)), 
                  "ageInYear", "OutcomeType", position = "fill") + 
           facet_grid(. ~ AnimalType))


### <<<<<<<<<< Does Age Matter? (By month)

ggplotly(bar_plot(animal_train, "ageInMonth", "OutcomeType") + 
           facet_grid(. ~ AnimalType))

ggplotly(bar_plot(animal_train, "ageInMonth", "OutcomeType", position = "fill") + 
           facet_grid(. ~ AnimalType))


### <<<<<<<<<< Does Outcome Time Matter? (By month)

# Converted "OutcomeDateTime" to POSIXlt (datetime format)

# Time series on DateTime
# Create timestamp
timestamp <- strptime(animal_train$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "CST6CDT")

monLevel <- c("January", "February", "March", "April",
              "May", "June", "July", "August", 
              "September", "October", "November", "December")

ggplotly(bar_plot(animal_train %>% 
                    mutate(Month = factor(months(timestamp),
                                          levels = monLevel)), 
                  "Month", "OutcomeType") + 
           facet_grid(AnimalType ~.)+
           theme(axis.text.x = element_text(angle = 45, hjust = 1)))

### <<<<<<<<<< Does Outcome Time Matter? (By weekday)

wdayLevel <- c("Sunday", "Monday", "Tuesday", 
               "Wednesday", "Thursday", 
               "Friday", "Saturday")

ggplotly(bar_plot(animal_train %>% 
                    mutate(Weekday = factor(weekdays(timestamp), 
                                            levels = wdayLevel)), 
                  "Weekday", "OutcomeType") + 
           facet_grid(.~ AnimalType)+
           theme(axis.text.x = element_text(angle = 45, hjust = 1)))

### <<<<<<<<<< Does Outcome Time Matter? (By hour)

ggplotly(bar_plot(animal_train %>% 
                    mutate(Hour = timestamp$hour), 
                  "Hour", "OutcomeType") + 
           facet_grid(. ~ AnimalType))

### <<<<<<<<<< Heat Map of Adoption: weekday vs. hour

# heatmap Hour vs Weekday
Outcome_WeekHour <- animal_train %>%
  mutate(Hour = timestamp$hour,
         Weekday = factor(weekdays(timestamp), 
                          levels = wdayLevel)) %>%
  group_by(AnimalType, Hour, Weekday) %>%
  summarise(Adoption = sum(OutcomeType == "Adoption"),
            Died = sum(OutcomeType == "Died"),
            Euthanasia = sum(OutcomeType == "Euthanasia"),
            Return_to_owner = sum(OutcomeType == "Return_to_owner"),
            Transfer = sum(OutcomeType == "Transfer"),
            Total = n())

ggplotly(heat_map(Outcome_WeekHour, "Weekday", "Hour",
                  colvar = "Adoption", 
                  low = "#FEF5E7",
                  high = "#2E86C1") + 
           facet_grid(. ~ AnimalType) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)))


