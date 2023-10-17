



mercancias <- 
getSerieDataFrame(getSeriesData(c("SP74633"), 
                                as.Date('1950-01-01'),
                                today() %m+% years(1)), 
                  c("SP74633")) |> 
  setNames(c("Fecha", "mercancias"))


servicios <- 
  getSerieDataFrame(getSeriesData(c("SP74635"), 
                                  as.Date('1950-01-01'),
                                  today() %m+% years(1)), 
                    c("SP74635")) |> 
  setNames(c("Fecha", "servicios"))

agropecuarios <- 
  getSerieDataFrame(getSeriesData(c("SP56378"), 
                                  as.Date('1950-01-01'),
                                  today() %m+% years(1)), 
                    c("SP56378")) |> 
  setNames(c("Fecha", "agropecuarios"))

energeticos <- 
  getSerieDataFrame(getSeriesData(c("SP74638"), 
                                  as.Date('1950-01-01'),
                                  today() %m+% years(1)), 
                    c("SP74638")) |> 
  setNames(c("Fecha", "energeticos"))


componentes <- 
mercancias |> 
  left_join(servicios, by = c("Fecha" = "Fecha")) |> 
  left_join(agropecuarios, by = c("Fecha" = "Fecha")) |> 
  left_join(energeticos, by = c("Fecha" = "Fecha"))


rm(servicios)
rm(mercancias)
rm(energeticos)
rm(agropecuarios)







