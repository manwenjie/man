flights <- readRDS("data/flights_week10.rds")

flightsData <- flights$data[[1]]$data_frame

dplyr::glimpse(flightsData)


flightsData$DepartureTimeZone |>
  table() |>
  sort(decreasing = T)->tb_departureTimeZone

dplyr::glimpse(flightsData)


library(dplyr)
library(lubridate)

flightsData <- flightsData %>%
  group_by(DepartureTimeZone) %>%
  mutate(DepartureTime = ymd_hms(DepartureTime, tz = DepartureTimeZone[[1]])) |>
  glimpse()


flightsData |> split(flightsData$DepartureTimeZone) -> split_flightsData

head(split_flightsData[[1]], 3)
head(split_flightsData[[2]]$DepartureTimeZone, 3)
head(split_flightsData[[3]]$DepartureTimeZone, 3)

x=1

split_flightsData[[x]] |>dplyr::glimpse()
split_flightsData[[x]] |> View()

.x=1 
split_flightsData[[.x]]$DepartureTime <-
  lubridate::ymd_hm(split_flightsData[[.x]]$DepartureTime, tz = split_flightsData[[.x]]$DepartureTimeZone[[1]]) 

options(encoding = "UTF-8")
Sys.setlocale("LC_CTYPE", "zh_TW.UTF-8")
flights <- readRDS("data/flights_week10.rds")
flightsData <- flights$data[[1]]$data_frame
dplyr::glimpse(flightsData)
Rows: 4,941
Columns: 24
$ AirlineID          �[3m�[90m<fct>�[39m�[23m 3U, 3U, 3U, 3U, 3U, 3U, 3U, 3U, 3U, 3U, 3U, 3U, 5J,…
                                          $ ScheduleStartDate  �[3m�[90m<date>�[39m�[23m 2023-10-13, 2023-10-20, 2023-10-27, 2023-10-13, 20…
                                                                                     $ ScheduleEndDate    �[3m�[90m<chr>�[39m�[23m "2023-10-15", "2023-10-22", "2023-10-27", "2023-10-…
$ FlightNumber       �[3m�[90m<chr>�[39m�[23m "3U3783", "3U3783", "3U3783", "3U3784", "3U3784", "…
                                                                                                                               $ DepartureAirportID �[3m�[90m<chr>�[39m�[23m "CKG", "CKG", "CKG", "TSA", "TSA", "TSA", "TFU", "T…
$ DepartureTime      �[3m�[90m<chr>�[39m�[23m "2023-10-13 15:00", "2023-10-20 15:00", "2023-10-27…
                                                                                                                                                                         $ CodeShare          �[3m�[90m<list>�[39m�[23m [<data.frame[0 x 0]>], [<data.frame[0 x 0]>], [<da…
                                                                                                                                                                                                                                                                       $ ArrivalAirportID   �[3m�[90m<chr>�[39m�[23m "TSA", "TSA", "TSA", "CKG", "CKG", "CKG", "TSA", "T…
$ ArrivalTime        �[3m�[90m<chr>�[39m�[23m "18:00", "18:00", "18:00", "22:15", "22:15", "22:15…
                                                                                                                                                                                                                                                                                                                 $ Monday             �[3m�[90m<lgl>�[39m�[23m FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRU…
                                                                                                                                                                                                                                                                                                                                                           $ Tuesday            �[3m�[90m<lgl>�[39m�[23m FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
                                                                                                                                                                                                                                                                                                                                                                                                     $ Wednesday          �[3m�[90m<lgl>�[39m�[23m FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRU…
                                                                                                                                                                                                                                                                                                                                                                                                                                               $ Thursday           �[3m�[90m<lgl>�[39m�[23m FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         $ Friday             �[3m�[90m<lgl>�[39m�[23m TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRU…
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   $ Saturday           �[3m�[90m<lgl>�[39m�[23m FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             $ Sunday             �[3m�[90m<lgl>�[39m�[23m TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, F…
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       $ UpdateTime         �[3m�[90m<chr>�[39m�[23m "2023-10-10T08:26:07+08:00", "2023-10-10T08:26:07+0…
$ VersionID          �[3m�[90m<int>�[39m�[23m 1111, 1111, 1111, 1111, 1111, 1111, 1111, 1111, 111…
$ Terminal           �[3m�[90m<chr>�[39m�[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "1"…
$ num_codeShare      �[3m�[90m<int>�[39m�[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
$ DepartureTimeZone  �[3m�[90m<chr>�[39m�[23m "Asia/Shanghai", "Asia/Shanghai", "Asia/Shanghai", …
$ ArrivalTimeZone    �[3m�[90m<chr>�[39m�[23m "Asia/Taipei", "Asia/Taipei", "Asia/Taipei", "Asia/…
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 $ DepartTaiwan       �[3m�[90m<lgl>�[39m�[23m FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE…
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           $ ArriveTaiwan       �[3m�[90m<lgl>�[39m�[23m TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, …

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     library(dplyr)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     library(lubridate)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     flightsData <- flightsData %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       group_by(DepartureTimeZone) %>%
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       mutate(DepartureTime = ymd_hms(DepartureTime, tz = DepartureTimeZone[[1]])) |>
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       glimpse()


library(dplyr)
library(lubridate)

flightsData <- flightsData %>%
  group_by(DepartureTimeZone) %>%
  mutate(DepartureTime = ymd_hms(DepartureTime, tz = DepartureTimeZone[[1]])) |>
  glimpse()
      
Rows: 4,941
Columns: 24
Groups: DepartureTimeZone [35]
$ AirlineID          �[3m�[90m<fct>�[39m�[23m 3U, 3U, 3U, 3U, 3U, 3U, 3U, 3U, 3U, 3U, 3U, 3U, 5J,…
                                          $ ScheduleStartDate  �[3m�[90m<date>�[39m�[23m 2023-10-13, 2023-10-20, 2023-10-27, 2023-10-13, 20…
                                                                                     $ ScheduleEndDate    �[3m�[90m<chr>�[39m�[23m "2023-10-15", "2023-10-22", "2023-10-27", "2023-10-…
$ FlightNumber       �[3m�[90m<chr>�[39m�[23m "3U3783", "3U3783", "3U3783", "3U3784", "3U3784", "…
                                                                                                                               $ DepartureAirportID �[3m�[90m<chr>�[39m�[23m "CKG", "CKG", "CKG", "TSA", "TSA", "TSA", "TFU", "T…
$ DepartureTime      �[3m�[90m<dttm>�[39m�[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ CodeShare          �[3m�[90m<list>�[39m�[23m [<data.frame[0 x 0]>], [<data.frame[0 x 0]>], [<da…
$ ArrivalAirportID   �[3m�[90m<chr>�[39m�[23m "TSA", "TSA", "TSA", "CKG", "CKG", "CKG", "TSA", "T…
                                                                                                                                                                         $ ArrivalTime        �[3m�[90m<chr>�[39m�[23m "18:00", "18:00", "18:00", "22:15", "22:15", "22:15…
$ Monday             �[3m�[90m<lgl>�[39m�[23m FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRU…
$ Tuesday            �[3m�[90m<lgl>�[39m�[23m FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
$ Wednesday          �[3m�[90m<lgl>�[39m�[23m FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRU…
$ Thursday           �[3m�[90m<lgl>�[39m�[23m FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
$ Friday             �[3m�[90m<lgl>�[39m�[23m TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRU…
$ Saturday           �[3m�[90m<lgl>�[39m�[23m FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
$ Sunday             �[3m�[90m<lgl>�[39m�[23m TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, F…
$ UpdateTime         �[3m�[90m<chr>�[39m�[23m "2023-10-10T08:26:07+08:00", "2023-10-10T08:26:07+0…
                                                                                                                                                                                                                   $ VersionID          �[3m�[90m<int>�[39m�[23m 1111, 1111, 1111, 1111, 1111, 1111, 1111, 1111, 111…
                                                                                                                                                                                                                                                             $ Terminal           �[3m�[90m<chr>�[39m�[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "1"…
                                                                                                                                                                                                                                                                                                       $ num_codeShare      �[3m�[90m<int>�[39m�[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
                                                                                                                                                                                                                                                                                                                                                 $ DepartureTimeZone  �[3m�[90m<chr>�[39m�[23m "Asia/Shanghai", "Asia/Shanghai", "Asia/Shanghai", …
                                                                                                                                                                                                                                                                                                                                                                                           $ ArrivalTimeZone    �[3m�[90m<chr>�[39m�[23m "Asia/Taipei", "Asia/Taipei", "Asia/Taipei", "Asia/…
$ DepartTaiwan       �[3m�[90m<lgl>�[39m�[23m FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE…
$ ArriveTaiwan       �[3m�[90m<lgl>�[39m�[23m TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, …                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     

flightsData |> split(flightsData$DepartureTimeZone) -> split_flightsData                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
head(split_flightsData[[1]], 3)
head(split_flightsData[[2]]$DepartureTimeZone, 3)
head(split_flightsData[[3]]$DepartureTimeZone, 3)

.x=1 # say the first one

split_flightsData[[.x]]$DepartureTime <- 
  lubridate::ymd_hm(split_flightsData[[.x]]$DepartureTime, tz = split_flightsData[[.x]]$DepartureTimeZone[[1]]) 

data_frame$some_column <- operations on ... other data_frame$column(s)
data_frame |>
  dplyr::mutate(
    some_column = operations on ... other column(s)
  )
  split_flightsData[[.x]] |>
  dplyr::mutate(
    DepartureTime = 
      lubridate::ymd_hm(DepartureTime, tz = DepartureTimeZone[[1]])
  )
  flightsData |>
    dplyr::group_by(DepartureTimeZone) |> # split and do the following on each sub data frame
    dplyr::mutate(
        DepartureTime = lubridate::ymd_hm(DepartureTime, tz = DepartureTimeZone[[1]]) # parse time
    ) |>
    dplyr::ungroup() -> 
    flightsData2
    dplyr::glimpse(flightsData2)
    split_flightsData[[.x]] |>
    dplr::mutate
      DepartureTime =
        lubridate::ymd_hm(
          DepartureTime,
          tz = DepartureTimeZone[[1]]
        )                
     
# for every
     
flightsData <- flights$data[[1]]$data_frame

flightsData |>
  dplyr::group_by(DepartureTimeZone) |>
  dplyr::mutate(
    DepartureTime =
      lubridate::ymd_hm(
        DepartureTime,
        tz = DepartureTimeZone[[1]])
     ) |>
  dplyr::ungroup() ->
  flightsData
 
flightsData |>
  dplyr::group_by(AirlineID) |>
  dplyr::mutate(
   
  )
    
