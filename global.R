
library(shiny)
library(shinyWidgets)
library(siebanxicor)
library(fredr)
library(echarts4r)
library(reactable)
library(reactablefmtr)
library(dplyr)
library(lubridate)


# Banxico
setToken("ed753930baf67bff05f52150712bb82a155a2978ea1b2c2800bed51a8dba72f1")
subyacente <- 
  getSerieDataFrame(getSeriesData(c("SP74660"), 
                                  as.Date('1990-01-01'),
                                  as.Date('2024-07-12')), 
                    c("SP74660"))
no_subyacente <- 
  getSerieDataFrame(getSeriesData(c("SP74663"), 
                                  as.Date('1990-01-01'),
                                  as.Date('2024-07-12')), 
                    c("SP74663"))

datos <- 
subyacente |> 
  rename(
    subyacente = value
  ) |> 
  left_join(
    no_subyacente |> 
      rename(
        no_subyacente = value
      )
  )









