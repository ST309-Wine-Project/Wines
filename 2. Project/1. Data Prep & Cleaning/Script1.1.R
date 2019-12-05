# TO-DO
"
1. Implement 'type' for white wines
2. Implement 'type' for roses
3. -''- 'type' for any other type of drink ('sherry', 'cava')
4. CLEAN OUTLIERS
    a. by vintage (should be more under control)
    b. by price
    (c. by point)
5. Sort out text fuck-ups
6. Look into the missing taster_name

"

library(tidyverse)
library(stringr)

### data inputting
wines0 <- read.csv("winemag-data-130k-v2.csv") # source: https://www.kaggle.com/zynicide/wine-reviews

types <- read.csv("./Wine Varieties by Type/red_types.csv", header = T) # source: https://en.wikipedia.org/wiki/List_of_grape_varieties

wines <- wines0 %>%
  mutate(vintage = as.numeric(str_match(title, "[2|1][0-9]{3}")))

varieties <- data.frame(levels(wines$variety))
names(varieties) <- c("variety")

varieties <- varieties %>%
  
  #detect if a blend
  mutate(blend = str_detect(variety, "-") | str_detect(variety, "(?i)blend")) %>%
  
  #search type explicitly 
  mutate(type = tolower(str_match(variety, "(?i)white"))) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)red")), type)) %>%
  
  #search by membership in type-vector (may be redundant)
  mutate(type = ifelse((is.na(type) & variety %in% types$variety[types$type == "red"]), "red", type)) %>%
  mutate(type = ifelse((is.na(type) & variety %in% types$variety[types$type == "white"]), "white", type)) # to be implemented 

#search by text to capture blends, e.g. "cabernet sauvignon-syrah"
mutate(type = ifelse((is.na(type) & str_detect(variexty, regex(paste(types$variety[types$type == "red"], collapse = "|"), ignore_case = T))), "red", type)) #%>%
mutate(type = ifelse((is.na(type) & str_detect(variety, regex(paste(types$variety[types$type == "white"], collapse = "|"), ignore_case = T))), "white", type))

#use the varieties df to incorporate type into 'wines'
wines$blend <- ifelse(wines$variety %in% varieties$variety[varieties$blend == T], T, F)
wines$type <- ifelse(wines$variety %in% varieties$variety[varieties$type == "red"], "red", NA)
wines$type <- ifelse(wines$variety %in% varieties$variety[varieties$type == "white"], "white", NA)
wines$type <- ifelse(wines$variety %in% varieties$variety[varieties$type == "rose"], "rose", NA)

write.csv(wines, "wine-data-tidied.csv")