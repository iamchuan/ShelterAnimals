library(dplyr)
library(ggplot2)

## <<<<<<<<< read data into dataframe
shelter_train <- read.csv("NYCDSA/Project/ShelterAnimals/train.csv", 
                          header = TRUE, 
                          stringsAsFactors = FALSE)

## check obs and variables
str(shelter_train) # 26729 obs. of  10 variables

## convert blank to NA
shelter_train$Name[shelter_train$Name == ""] <- NA
shelter_train$DateTime[shelter_train$DateTime == ""] <- NA
shelter_train$OutcomeType[shelter_train$OutcomeType==""] <- NA
shelter_train$OutcomeSubtype[shelter_train$OutcomeSubtype == ""]<- NA
shelter_train$SexuponOutcome[shelter_train$SexuponOutcome==""] <- NA
shelter_train$AgeuponOutcome[shelter_train$AgeuponOutcome==""] <- NA
shelter_train$Breed[shelter_train$Breed==""] <- NA
shelter_train$Color[shelter_train$Color==""] <- NA

which(apply(shelter_train,2,function(x){any(is.na(x))})) # Name OutcomeSubtype SexuponOutcome AgeuponOutcome 

## seperate cat and dog
cat_train <- shelter_train %>% filter(AnimalType == "Cat") %>% select(-AnimalType)
dog_train <- shelter_train %>% filter(AnimalType == "Dog") %>% select(-AnimalType)

## <<<<<<<<< UDF to convert Age to month
toMonth <- function(x) {
  sapply(x, function(x){
    age <- unlist(strsplit(x, split = " "))
    if(grepl("day", age[2])) 
      return(as.numeric(age[1]) / 30)
    else if(grepl("week", age[2])) 
      return(as.numeric(age[1]) / 4)
    else if(grepl("month", age[2])) 
      return(as.numeric(age[1]))
    else if(grepl("year", age[2])) 
      return(as.numeric(age[1]) * 12)
    else return(NA)
  })
}


## <<<<<<<<<<< CAT
## convert age to month, charactor to factor,  etc.
cat_train <- cat_train %>% mutate(ageInMonth = toMonth(AgeuponOutcome),
                                  OutcomeType = as.factor(OutcomeType),
                                  OutcomeSubtype = as.factor(OutcomeSubtype),
                                  SexuponOutcome = as.factor(SexuponOutcome),
                                  Breed = as.factor(Breed),
                                  Color = as.factor(Color))

sum(is.na(cat_train)) # 8681
which(apply(cat_train,2,function(x){any(is.na(x))}))

cat_train$Mix <- ifelse(grepl('Mix', cat_train$Breed), "Yes", "No")
dog_train$Mix <- ifelse(grepl('Mix', dog_train$Breed), "Yes", "No")

cat_train$BreedOnly <- sub(" Mix", "", cat_train$Breed)
sort(table(cat_train$BreedOnly))

cat_train$BreedTop10 <- ifelse(cat_train$BreedOnly%in% names(sort(table(cat_train$BreedOnly), 
                                                                decreasing = TRUE))[1:10], 
                               cat_train$BreedOnly, "Others")
sort(table(cat_train$BreedTop10), decreasing = TRUE)

unique(unlist(strsplit(sapply(levels(cat_train$Color), function(x) sub("/", " ", x)), 
                              split = " ")))

## 
str(cat_train)
colnames(cat_train)


## EDA
ggplot(cat_train, aes(x=ageInMonth*12)) +
       geom_histogram(binwidth = 20)

ggplot(cat_train, aes(OutcomeType, fill=OutcomeSubtype)) +
  geom_bar(position="fill")
ggplot(cat_train, aes(OutcomeType, fill=SexuponOutcome)) +
  geom_bar(position="fill")

ggplot(cat_train, aes(OutcomeType, fill=IsMix)) +
  geom_bar(position="fill")
ggplot(dog_train, aes(OutcomeType, fill=IsMix)) +
  geom_bar(position="fill")

ggplot(cat_train, aes(IsMix, fill=OutcomeType)) +
  geom_bar(position="fill")
ggplot(dog_train, aes(IsMix, fill=OutcomeType)) +
  geom_bar(position="fill")

ggplot(cat_train, aes(IsMix, fill=OutcomeType)) +
  geom_bar(position="fill")
ggplot(dog_train, aes(IsMix, fill=OutcomeType)) +
  geom_bar(position="fill")


ggplot(cat_train, aes(OutcomeType, fill=BreedTop10)) + 
  geom_bar(position = "fill")
ggplot(dog_train, aes(IsMix, fill=OutcomeType)) +
  geom_bar(position="fill")

