# #housecleaning
#rm(list = ls())
#.rs.restartR() #restart r sesion
#gc()

library(tidyverse)
library(janitor)
library(data.table)
library(tictoc)
tic()
setwd('/Users/ariadna/Documents/Projects/Trade_World/Scripts/Roger_scripts')
 

#load data
data <- fread("../../Data/Trade_filtered.csv",drop='V1')
data<- clean_names(data)

#keep useful variables (DONE earlier in notebook from pandas)

glimpse(data)


#change names: origin destination reporter (origin or destination)
#   create new colum: origin_country, destination_country
# non-technical explanation: I create an "origin_country" column, that is filled with different reporter VS partner information depending on whether the element is an import or an export, and below do similarly

# The origin of a trade transaction can be:
# a "Reporter Countries" when the "Element" is "Export Quantity" 
# a "Partner Countries" when the "Element" is "Import Quantity" 
# 
# The destination of a trade transaction can be: 
# a "Partner Countries" when the "Element" is "Export Quantity" 
# a "Reporter Countries" when the "Element" is "Import Quantity" 

# Define origin_country and destin_country

to_add = data[,c('reporter_countries','reporter_country_code','partner_countries','partner_country_code')]
colnames(to_add) = c('origin_country','origin_country_code','destin_country','destin_country_code')
data = cbind(data,to_add)
rm(to_add)

replace_bool = (data$element == "Import Quantity") | (data$element == "Import Value") 

data[replace_bool,c('origin_country','origin_country_code')] <- data[replace_bool,c('partner_countries','partner_country_code')]

data[replace_bool,c('destin_country','destin_country_code')] <- data[replace_bool,c('reporter_countries','reporter_country_code')]


# Estimate duplicated records (with equal values and different values, see paper mentioned above), 
# and duplicated records with same reported value


### duplicated records (both equal values and different values)
data <- data %>%
  add_count(origin_country, destin_country, unit, year, item) 
# the `n` column says how many rows have the same content in the specified 
# columns

data <- data %>%
  mutate(duplicated_record = n > 1)

glimpse(data)

dupli <- length(data$n[data$n > 1])
non_dupli <- length(data$n[data$n <=1])

print(paste0('Percentage of data duplicated: ',round(100*dupli/(dupli+non_dupli),2),'%'))
print( dupli/2 + non_dupli )


#___delete n and create new n adding value
data <- data %>% 
  select(-n)

#names(data) #check all in place

### duplicated records (equal values)
# the counter will add 2 to equally reported duplicates
data <- data %>%
  add_count(origin_country, destin_country, unit, year, item, value) 

glimpse(data)

# duplicate and equal:  T/F

data <- data %>%
  mutate(same_duplicated_record = n > 1)


head_data <- head(data, 100)
View(head_data)

dim(data)


#remove NAs in value, if any
data_relia <- data %>% filter(!is.na(value))
head_data_relia <- head(data_relia, 100)
View(head_data_relia)

dim(data_relia)

# compute the reliability index
# group by sum per reporter_countries, product and year of dupli_equal / dupli

#TESTS
z <- c(TRUE, TRUE, TRUE, FALSE, NA)
sum(z, na.rm = TRUE)


if(FALSE){
  # reliability index per country, 
  reliab_country <- data_relia %>% 
    group_by(reporter_countries) %>% 
    summarize(reli_index_c = 
                (sum(same_duplicated_record, na.rm = TRUE) / sum(duplicated_record, na.rm = TRUE))
    )


  which(is.na(reliab_country$reli_index_c))
  which(is.nan(reliab_country$reli_index_c))
  which(is.null(reliab_country$reli_index_c))
  which(is_empty(reliab_country$reli_index_c))
  # no NAs


  #remove NAs in reli_index_c
  reliab_country <- reliab_country %>% filter(!is.na(reli_index_c))
  view(reliab_country)


  # reliability index per country, year, item product
  reliab_cyi <- data_relia %>% 
    group_by(reporter_countries, year, item) %>% 
    summarize(reli_index_cyi = 
                (sum(same_duplicated_record, na.rm = TRUE) / sum(duplicated_record, na.rm = TRUE))
    )

  which(is.na(reliab_cyi$reli_index_cyi))
  which(is.nan(reliab_cyi$reli_index_cyi))
  # NAs lots, naturally cos not all trade occurs across all countries
  which(is.null(reliab_cyi$reli_index_cyi))
  which(is_empty(reliab_cyi$reli_index_cyi))



  #remove NAs in reliab_cyi
  reliab_cyi <- reliab_cyi %>% filter(!is.na(reli_index_cyi))
  which(is.na(reliab_cyi$reli_index_cyi))
  which(is.nan(reliab_cyi$reli_index_cyi))

  view(reliab_cyi)
  names(reliab_cyi)

  # merge reliab_cyi into data

  #band_members %>% left_join(band_instruments)
  dataset <- left_join(data, reliab_cyi, by = c("reporter_countries", "year", "item" ))

  #check NAs in left side
  #remove NAs in reli_index_c ?
  #NO: this would remove valuable records! 

  head_dataset <- head(dataset, 100)
  View(head_dataset)
  names(dataset)

  # Now fill the NAs gap in the reli_index_cyi with reli_index_c
  #       first, I left join, 
  dataset <- left_join(dataset, reliab_country, by = c("reporter_countries" ))

  head_dataset <- head(dataset, 100)
  View(head_dataset)
  names(dataset)
  #       then  I replace if NA, and remove reli_index_c
  dataset$reli_index_cyi <- ifelse(is.na(dataset$reli_index_cyi), dataset$reli_index_c, dataset$reli_index_cyi)
  head_dataset <- head(dataset, 100)
  View(head_dataset)

  which(is.na(dataset$reli_index_cyi))
  which(is.nan(dataset$reli_index_cyi))
  #clear
  dataset <- dataset %>% 
    select(-reli_index_c)

  # slice_max and slice_min do not work if variable contains NAs
  # remove NAs, remove dollars / keep only quantities 
  #   if dollars needed, similary make a dollars dataset for most expensive reporting and merge

  which(is.na(dataset$value)) # > 435097 entries
  which(is.nan(dataset$value))

  dataset <- dataset %>%
    drop_na(value)

  which(is.na(dataset$value)) #  0 entries

  # 
  # # Now keep the values with higher reli_index_cyi, with two different methods
  # 
  # test2 <- dataset
  #  
  # test2 <- test2 %>% 
  #    group_by(origin_country_code, destin_country_code, year, item, unit) %>%
  #    slice_max(order_by = reli_index_cyi, n = 1, with_ties = FALSE)
  # 
  # test3 <- dataset
  # 
  # test3 <- test3 %>%
  #   arrange(desc(reli_index_cyi)) %>%
  #   group_by(origin_country_code, destin_country_code, year, item, unit) %>%
  #   slice(1:1)
  # 
  # 
  # # both tests give identic results, proportion is similar as reported in ESSD
  # # like test2 method more, slice max
  # head_test <- head(test3, 100)
  # View(head_test)
  # 
  # nrow(test2)
  # nrow(dataset)
  # nrow(test2)/nrow(dataset)
  # View(test2)
  # 
  # nrow(test3)
  # length(test3$reli_index_cyi)
  # 
  # length(test2$reli_index_cyi)
  # length(dataset$reli_index_cyi)
  # 
  # 
  # #   final test
  # test4 <- head(dataset, 10)
  # test4 <- test4 %>% 
  #   select(-element, -n, -reporter_country_code, -origin_country_code, -destin_country_code, -same_duplicated_record, -duplicated_record, -partner_country_code, -partner_countries )
  # #   perform manual edits to create conditions to test the script below for the index
  # View(test4)
  # test4 <- edit(test4) # manipulate into one country has 0, double flows, most in 1 year..
  # 
  # 
  # test5 <- test4 %>% 
  #   group_by(origin_country, destin_country, year, item, unit) %>%
  #   slice_max(order_by = reli_index_cyi, n = 1, with_ties = FALSE)
  # View(test5)
  # 
  # 
  # 
  # 

  # actual index implemented here:

  dataset <- dataset %>% 
    group_by(origin_country, destin_country, year, item, unit) %>%
    slice_max(order_by = reli_index_cyi, n = 1, with_ties = FALSE)


  head_test <- head(dataset, 100)
  View(head_test)

toc()
  # now remove  not needed cols (index, intermediate operations, previous ..)

  dataset <- dataset %>% 
    select(-element, -n, -reli_index_cyi, -same_duplicated_record, -duplicated_record, -partner_country_code, -partner_countries )


  # now save to CSV and move to NetworkX and to CRM team

  write.csv( x = dataset, file = "FAO_2023_10.csv")

}
# #load data (set wd)
# data <- read_csv("FAO_2023_10.csv") 
# 
# colnames(data)
# head(data)








