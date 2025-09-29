getwd()
setwd("/home/rstudio/project/UFO_Sightings_Assignment")

data <- read.csv('nuforc_sightings.csv')

unique(data$state)

US_data <- function(x){
  x |> mutate(
    country = str_to_upper(country),
    state = str_to_upper(state),
    shape = str_to_lower(str_trim(shape))
  )
   data.frame(
     
   )
}
