#'Econ294 R Lab
# # # # # # # # # # # # # # #
######## Final Examl ########
# # # # # # # # # # # # # # #
# Naihao Guo
# Winter 2016
# https://github.com/guonaihao/Econ294RLab

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
install.packages("nycflights13")
install.packages("RSQLite")
library(dplyr)
library(nycflights13)
library(RSQLite)

# loading and sorting data
db<-src_sqlite("db.sqlite3", create = T)

flights_sqlite <- copy_to(db, flights, temporary = FALSE, 
  indexes = list(c("year", "month", "day"), "carrier", "tailnum"))

airlines_sqlite <- copy_to(db, airlines, temporary = FALSE, indexes = list("carrier"))
airports_sqlite <- copy_to(db, airports, temporary = FALSE, indexes = list("faa"))
planes_sqlite <- copy_to(db, planes, temporary = FALSE, indexes = list("tailnum"))
weather_sqlite <- copy_to(db, weather, temporary = FALSE, 
  indexes = list(c("year", "month","day","hour"), "origin"))

#
df.flight<-tbl(db, sql("SELECT * FROM flights")) %>% collect()
df.plane<-tbl(db, sql("SELECT * FROM planes")) %>% collect()
df.airport<-tbl(db,sql("SELECT * FROM airports"))%>% collect()
df.weather<-tbl(db,sql("SELECT * FROM weather")) %>% collect()