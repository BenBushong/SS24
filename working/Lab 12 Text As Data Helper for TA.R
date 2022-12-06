


# Lab 12 - Text As Data
# TURN echo=T in your first chunk that sets options - we need to see code on this one!


### 12.1
Sales2020 = c('$1,420,142',
              '$438,125.82',
              '120,223.50',
              '42,140')


## solution 
# Must use str_replace_all on commas
# Must escape the $ using \\
# Can also use parse_number from readr (mentioned in lesson)
as.numeric(str_replace_all(Sales2020, '\\$|,', ''))
parse_number(Sales2020)


### 12.2
# Clean these names so that you can set these in correct alphabetical order using `Students[order(Students)]`
# Note that the space in Mc Evoy is part of the name and should not be removed, but the other version McAvoy has no space!
# (in the end, proper alphabetizing should be Mc Evoy > McAvoy
Students = c('Ali',' Meza','McAvoy', 'Mc Evoy', '.Donaldson','Kirkpatrick ')

## Solution
## Clean white spaces and remove \\.
## But use anchor ^ to look for \\s (white spaces) at the beginning 
##  so that Mc Evoy retains its space
StudentsClean = str_replace_all(Students, pattern = '^\\s|\\s$|\\.', replace = '')
StudentsClean[order(StudentsClean)]






### 12.3
# Write a line of code that returns TRUE for each valid, complete price (including cents to two places) in USD:
Prices = c('$12.95',
           '$\beta$',
           '$1944.55',
           '3.14',
           '$CAN',
           '$12.',
           '$109',
           '4,05',
           '$200.00')


## Solution
str_detect(Prices, '^\\$\\d+\\.\\d{2}')




### 12.4
# This will use groups to extract
# Generate a three-column table with acres in the first column, price per acre in the second, and compute the
# total in the third (which requires getting both columns in numeric form. 
# Use str_match
landSales = c('Sold 12 acres at $105 per acre',
              '200 ac. at $58.90 each',
              '.25 acre lot for $1,000.00 ea',
              'Offered 50 acres for $5,000 per')
# First group in () looks for 0-1 .'s (for the 3rd entry), then 1 or more digits.
# Then, after the group, we look for any a-z, A-Z, spaces, or .'s of any number (for the filler between acres and prices)
# Then, we look for a $ after which we start the second group
# That second group must (following a $) contain 1 or more digit OR , OR .
#   The second group has to recognize the ',' but then has to have it removed
# Finally, we make our table using as.numeric
str_match(landSales, '(\\.?\\d+)[a-zA-Z\\s.]*\\$([\\d,.]+)') %>%
  as_tibble() %>%
  dplyr::mutate(Acres = as.numeric(V2), Price = parse_number(V3)) %>%
  dplyr::select(Acres, Price) %>%
  dplyr::mutate(Total = round(Price*Acres, 2)) %>%
  knitr::kable()

