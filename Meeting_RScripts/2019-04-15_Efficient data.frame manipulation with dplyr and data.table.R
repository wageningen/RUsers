

# library(microbenchmark)
library(dplyr)
library(dbplyr)
library(RSQLite)
library(data.table)
library(lubridate)
# library(raster)

# departing/arriving flights from JFK in 2014
flights_dt <- fread("https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv")
flights_df <- as.data.frame(flights_dt)


#### dplyr ####

## mutate: add columns
flights_df <- flights_df %>%
  mutate(YMD = ymd(paste(year, month, day)))
class(flights_df$YMD)

# add multiple columns at once and use them
flights_df <- flights_df %>%
  mutate(YMD = ymd(paste(year, month, day)),
         YMD_1 = as.POSIXct(YMD + hours(1)))


## select: select columns
flights_df %>% 
  select(YMD, carrier)


# de-select with "-"
flights_df %>% 
  select(-YMD)


## arrange: sort by certain column
head(flights_df %>%
  arrange(YMD))


## descending
flights_df %>%
  arrange(desc(YMD))


## group_by: grouping operations

# typical combination with summarise (aka aggregation) 
flights_df %>% 
  group_by(carrier) %>%
  summarise(Delay = sum(dep_delay)) %>%
  ungroup() %>%
  arrange(Delay)

# grouping with multiple variables
flights_df %>% 
  group_by(carrier, origin) %>%
  summarise(Delay = sum(dep_delay)) %>%
  ungroup() %>%
  arrange(Delay)

# also possible: window functions = do something per group without aggregation
flights_df <- flights_df %>% 
  group_by(carrier) %>%
  arrange(dep_delay) %>%
  mutate(RankByCarrier = 1:n()) %>%
  ungroup()
nrow(flights_df) == nrow(flights_dt) # no rows were aggregated


#### dplyr pitfalls and caveats ####

# clash of verb names with other packages due to very generic verbs
flights_df %>% 
  select(carrier)

# over-writing variables
flights_df %>%
  group_by(carrier) %>%
  summarise(distance = mean(distance),
            distance_max = max(distance)) %>% # distance will be min(distance) from the above line
  ungroup()


## not a proper interface?
# from https://github.com/WinVector/Examples/blob/master/dplyr/dplyrQuiz.md
db <- DBI::dbConnect(RSQLite::SQLite(), 
                     ":memory:")
dL <- data.frame(x = 3.077, 
                 k = 'a', 
                 stringsAsFactors = FALSE)
dR <- dplyr::copy_to(db, dL, 'dR')

# pretty difficult to get number of rows from a remote table
nrow(dR)


dbWriteTable(db, 'flights', flights_df)

flights_db <- tbl(db, 'flights')

flights_db %>%
  group_by(carrier) %>%
  summarise(Delay = sum(dep_delay))

# not supported functions; still understandable
flights_db %>%
  mutate(DelayRank = rank(dep_delay))

# no window functions, even though SQLite 3 supports them
flights_db %>%
  collect() %>%
  group_by(carrier) %>%
  mutate(Delay = sum(dep_delay) / n())

# aggreating works
flights_db %>%
  group_by(carrier) %>%
  summarise(Delay = sum(dep_delay))


#### data.table ####

## add columns
flights_dt[, YMD := ymd(paste(year, month, day))]

# add multiple columns at once
flights_dt[, `:=`(YMD = ymd(paste(year, month, day)), YMD_1 = as.POSIXct(YMD + hours(1)))]
# throws error: not possible at once, so in two steps
flights_dt[, YMD := ymd(paste(year, month, day))]
flights_dt[, YMD_1 := as.POSIXct(YMD + hours(1))]


## select columns
flights_dt[,YMD]
flights_dt[['YMD']] # list syntax: data.frames and data.tables are named lists


## sorting
flights_dt[order(origin, -dest)]


## by groups
flights_dt[, .(.N, Delay = sum(dep_delay)), by = .(carrier, origin)]

