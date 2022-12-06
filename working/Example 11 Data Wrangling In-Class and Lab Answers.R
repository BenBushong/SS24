


## Data wrangling Example construction
## 
## 
## 3 data sets to process and merge
## ## First is a list of all Corporate bookings at a hotel. Room type, date (1 obs for each nights stay), employee ID
## ## Second is a list of all room rates, wide, with each date in a column and a row corresponding to room type
## ## Third is a list of all corporations with free parking



#### Data creation ####
#######################
#### List of all corporate bookings at a hotel:
#### 
require(lubridate)
require(zoo)
# require(timeDate)
require(tidyverse)
set.seed(2021)

NC = 5
NN = 500
corps = LETTERS[1:NC]
roomtype = c('single','double','suite','royalsuite','economy')
roomrates_dates = tibble(date_occupied = seq.Date(from = make_date(2015, 1, 1), to = make_date(2020, 12, 31), by = '1 day'),
                         weekend_up = weekdays(date_occupied) %in% c('Friday','Saturday'),
                         month_up = month(date_occupied) %in% c(12, 6, 7, 8),
                         period_up = month(date_occupied)==11 & year(date_occupied)==2020,
                         price_single = 100 + rnorm(length(date_occupied), 0, 15) + 50*period_up + 70*month_up + 100*weekend_up,
                         price_double = 180 + rnorm(length(date_occupied), 0, 15) + 50*period_up + 70*month_up + 100*weekend_up,
                         price_suite = 400 + rnorm(length(date_occupied), 0, 100) + 100*period_up + 100*month_up + 200*weekend_up,
                         price_royalsuite = 700 + rnorm(length(date_occupied), 0, 120) + 100*period_up + 100*month_up + 200*weekend_up,
                         price_economy = 75 + rnorm(length(date_occupied), 0, 10) + 50*period_up + 30*month_up + 50*weekend_up,
                         id = 1) %>%
  dplyr::select(-ends_with('_up'))#, wday = c(7, 1:4)))



parking = tibble(corp = corps,
                 FreeParking = c(T,T,F,T,F)) 





books = data.frame(
  corp = sample(corps, NN, replace = T),
  StayID = 1:NN,
  room = sample(roomtype, size = NN, replace = T, prob=c(.5, .2, .1, .03, .17)),
  stay_start = sample(seq.Date(from = make_date(2015, 1, 1), to = make_date(2020, 12, 31), by = '1 day'), replace = T, size = NN),
  length = pmax(rpois(NN, 3), 1)
)

books$room_use = paste0(books$room, '_room')
books$room_use[books$room_use=='royalsuite_room'] = 'royalsuite'
books = as_tibble(books)
books$stay_dates = mapply(function(std, ll) (tibble(date_occupied = seq.Date(from = std, to = std+days(ll), by = '1 day'))),
                          books$stay_start, books$length, SIMPLIFY=FALSE)

books = unnest(books, cols = stay_dates)

books2 = books %>%
  left_join(roomrates_dates, by = 'date_occupied') %>%
  left_join(parking, by = 'corp') %>%
  dplyr::mutate(Amount = case_when(
    room_use=='economy_room' ~ price_economy + 60*(!FreeParking),
    room_use=='single_room' ~ price_single + 60*(!FreeParking),
    room_use=='royalsuite' ~ price_royalsuite + 60*(!FreeParking),
    room_use=='double_room' ~ price_double + 60*(!FreeParking),
    room_use=='suite_room' ~ price_suite + 60*(!FreeParking),
    TRUE ~ as.numeric(NA)))




books_use = books2 %>%
  dplyr::select(corp, StayID, room_use, date_occupied)

parking_use = parking %>%
  dplyr::filter(FreeParking==T)

roomrates_use = roomrates_dates %>%
  pivot_wider(id_cols = id, names_from = date_occupied, values_from =  price_single:price_economy) %>%
  dplyr::select(-1)

write.csv(books_use, file.path('C:/Users/jkirk/OneDrive - Michigan State University','Teaching','SSC442_SS21','static','data','Example11_booking.csv'), row.names = F)
write.csv(parking_use, file.path('C:/Users/jkirk/OneDrive - Michigan State University','Teaching','SSC442_SS21','static','data','Example11_parking.csv'), row.names = F)
write.csv(roomrates_use, file.path('C:/Users/jkirk/OneDrive - Michigan State University','Teaching','SSC442_SS21','static','data','Example11_roomrates.csv'), row.names = F)

# students are to use the three datasets to answer the question: which corporation spends the most with the hotel?
# 
# books_use has the data on bookings but has no prices
# roomrates_use has the data on room prices, but in wide format (and with tricky column names)
# parking_use has the data on which ones have free parking (but missing any who don't so NA's in merge)
# 
# 
# 
# 
## Lab #11 Version
#     I also make a version for lab work building on the class exercises:
roomrates_hw = roomrates_dates %>%
  dplyr::mutate(year = year(date_occupied),
                month = month(date_occupied),
                day = day(date_occupied)) %>%
  dplyr::mutate(year = gsub('^20', '', as.character(year))) %>%
  dplyr::mutate(date_occupied_new = paste0(day, '/',month,'/',year)) %>%
  dplyr::select(date_occupied_new, contains('price')) %>%
  pivot_longer(-c(date_occupied_new) ) %>%
  pivot_wider(names_from = date_occupied_new, id_cols = name, values_from = value) %>%
  dplyr::mutate(name = gsub('royalsuite','royal_suite', .$name))
  
books_hw = books_use %>%
  dplyr::mutate(date_occupied = paste0(as.character(date_occupied), ' 01:00:00'))

parking_hw = parking_use %>%
  bind_rows(tibble(corp = 'C', FreeParking = as.logical(FALSE))) %>%
  dplyr::arrange(corp)
parking_hw[4,'corp'] = 'E'


write.csv(books_hw, file.path('C:/Users/jkirk/OneDrive - Michigan State University','Teaching','SSC442_SS21','static','data','Lab11_booking.csv'), row.names = F)
write.csv(parking_hw, file.path('C:/Users/jkirk/OneDrive - Michigan State University','Teaching','SSC442_SS21','static','data','Lab11_parking.csv'), row.names = F)
write.csv(roomrates_hw, file.path('C:/Users/jkirk/OneDrive - Michigan State University','Teaching','SSC442_SS21','static','data','Lab11_roomrates.csv'), row.names = F)




#### In-Class (Data Wrangling) ####
###################################

library(tidyverse)
library(lubridate)
booking = read.csv('https://ssc442kirkpatrick.netlify.app/data/Example11_booking.csv')
roomrates = read.csv('https://ssc442kirkpatrick.netlify.app/data/Example11_roomrates.csv')
parking = read.csv('https://ssc442kirkpatrick.netlify.app/data/Example11_parking.csv')


# Start by noting that booking contains the main info
#  - corp
#  - room_use

# So, we need to merge prices to room_use. Make roomrates long:

roomrates.clean = gather(roomrates, roomkey, price) %>%
  separate(col = roomkey, into = c('junk','room_use','date_occupied'),  sep = "_", extra = 'merge', fill = 'left')

table(roomrates.clean$room_use, useNA='always')

# Nice. Two issues
# - 1. How about the date format? ymd? ydm?

str(roomrates.clean) # date is character string; price looks good
table(roomrates.clean$date_occupied) # looks like ymd

# First let's fix the date
roomrates.clean = roomrates.clean %>%
  mutate(date_occupied = ymd(date_occupied)) %>%
  dplyr::select(-junk)


# - 2. Will it merge?
#        - The primary key is date_occupied x room_use
intersect(roomrates.clean$room_use, booking$room_use) # !!!!
setdiff(roomrates.clean$room_use, booking$room_use)
setdiff(booking$room_use, roomrates.clean$room_use)

# Let's make roomrates.clean match booking:
roomrates.clean = roomrates.clean %>%
  dplyr::mutate(room_use = case_when(room_use=='royalsuite' ~ room_use,
                                     TRUE ~ paste0(room_use, '_room')))

setdiff(roomrates.clean$room_use, booking$room_use)
setdiff(booking$room_use, roomrates.clean$room_use)
# those are empty sets. everything in booking has a matched entry in roomrates for room_use



# Now, check dates on booking:
str(booking)

setdiff(booking$date_occupied, roomrates.clean$date_occupied) 
# Yikes!

booking = booking %>%
  dplyr::mutate(date_occupied = ymd(date_occupied))

setdiff(booking$date_occupied, roomrates.clean$date_occupied) # empty!

# one last check - note we're checking to make sure all of booking's keys are in roomrates.clean
setdiff(booking$room_use, roomrates.clean$room_use)
setdiff(booking$date_occupied, roomrates.clean$date_occupied)

# Last, we want to make sure that roomrates.clean is UNIQUE on date_occupied + room_use:
  NROW(unique(roomrates.clean %>% dplyr::select(date_occupied, room_use)))
  NROW(roomrates.clean)
  # or
  sum(duplicated(roomrates.clean %>% dplyr::select(date_occupied, room_use)))

# Let's merge!
revenue = booking %>%
  left_join(roomrates.clean, by = c('date_occupied','room_use'))


# Now, parking
# Couple ways we can do this. Note that those that don't have free parking are just not listed
setdiff(revenue$corp, parking$corp)
# Since parking has a primary key of 'corp' we can look to see if it's unique on that:
sum(duplicated(parking %>% dplyr::select(corp)))

# We can merge and fill
revenue.full = revenue %>%
  left_join(parking, by = 'corp') %>%
  replace_na(list(FreeParking = 0)) %>%
  dplyr::mutate(parking_revenue = 60*(!FreeParking))   # $60/night is what we state on the problem

# Or use case_when with is.na()
revenue.full = revenue %>%
  left_join(parking, by = 'corp') %>%
  dplyr::mutate(parking_revenue = case_when(is.na(FreeParking) ~ 60,
                                            TRUE ~ 0))

# Or, we could complete parking (and add the price while we're at it):
parking.full = bind_rows(parking, data.frame(corp = c("C","E"),
                                             FreeParking = FALSE)) %>%
  dplyr::mutate(parking_revenue = 60*(!FreeParking))

revenue.full = revenue %>%
  left_join(parking.full %>% dplyr::select(corp, parking_revenue), by = 'corp')




# Finally, we total things up:
revenue.full %>%
  group_by(corp) %>%
  dplyr::summarize(total_spending = sum(price) + sum(parking_revenue)) %>%
  dplyr::arrange(-total_spending)






#### Lab (Data Wrangling) (usually Lab #13 or #12) ####
#######################################################


library(tidyverse)
library(lubridate)
booking = read.csv('https://ssc442kirkpatrick.netlify.app/data/Lab11_booking.csv')
roomrates = read.csv('https://ssc442kirkpatrick.netlify.app/data/Lab11_roomrates.csv')
parking = read.csv('https://ssc442kirkpatrick.netlify.app/data/Lab11_parking.csv')


# Start by noting that booking contains the main info
#  - corp
#  - room_use

# So, we need to merge prices to room_use. Make roomrates long:

roomrates.clean = roomrates %>%
  separate(col = name, into = c('junk','room_use'), sep = '^price_', fill = 'right') %>% 
  dplyr::mutate(room_use = gsub(room_use, pattern = "_", replacement = "")) %>%
  dplyr::select(-junk) %>%
  gather(date_occupied, price, -room_use) %>%
  dplyr::mutate(date_occupied = dmy(gsub(date_occupied, pattern = 'X', replacement = '')))


# - 2. Will it merge?
#        - The primary key is date_occupied x room_use
intersect(roomrates.clean$room_use, booking$room_use) # !!!!
setdiff(roomrates.clean$room_use, booking$room_use)
setdiff(booking$room_use, roomrates.clean$room_use)


booking.clean = booking %>%
  dplyr::mutate(room_use = gsub(room_use, pattern = '_room', replacement = ''),
                date_occupied = as.Date(ymd_hms(date_occupied)))


setdiff(booking.clean$room_use, roomrates.clean$room_use)
setdiff(booking.clean$date_occupied, roomrates.clean$date_occupied)



# Last, we want to make sure that roomrates.clean is UNIQUE on date_occupied + room_use:
NROW(unique(roomrates.clean %>% dplyr::select(date_occupied, room_use)))
NROW(roomrates.clean)
# or
sum(duplicated(roomrates.clean %>% dplyr::select(date_occupied, room_use)))

# Let's merge!
revenue = booking.clean %>%
  left_join(roomrates.clean, by = c('date_occupied','room_use'))


# Now, parking
# Couple ways we can do this. Note that those that don't have free parking are just not listed
setdiff(revenue$corp, parking$corp)
# Since parking has a primary key of 'corp' we can look to see if it's unique on tha (trivial)t:
sum(duplicated(parking %>% dplyr::select(corp)))



# Merge parking (other ways to do it shown in example above)
revenue.full = revenue %>%
  left_join(parking, by = 'corp') %>%
  dplyr::mutate(parking_revenue = case_when(is.na(FreeParking) ~ 60,
                                            TRUE ~ 0))
           

# Finally, we total things up (Exercise 1.1):
revenue.full %>%
  group_by(corp) %>%
  dplyr::summarize(total_spending = sum(price) + sum(parking_revenue)) %>%
  dplyr::arrange(-total_spending)


# Exercise 1.2 -
revenue.full.12 = revenue.full %>%
  dplyr::mutate(year = year(date_occupied)) %>%
  group_by(corp, year) %>%
  dplyr::summarize(total_annual_spending = sum(price) + sum(parking_revenue)) %>%
  dplyr::arrange(corp, year)

ggplot(revenue.full.12, aes(x = year, y = total_annual_spending, col = corp)) + 
  geom_path()



# Exercise 1.3
# 
# 
roomrates.13 = roomrates.clean %>%
  dplyr::mutate(day = day(date_occupied),
                month = month(date_occupied), 
                year = year(date_occupied),
                dayname = wday(date_occupied, label=TRUE),
                weekday = wday(date_occupied, label = TRUE) %in% c('Sun','Sat'))

plot13.1 = ggplot(roomrates.13 %>% dplyr::filter(room_use=='single' & year==2016 & month==12), aes(x = date_occupied, y = price, col = dayname)) + 
  geom_path()

# Weekends are higher
# Also, months 6-7-8 and 12 are higher
# Also, Nov 2020 is higher