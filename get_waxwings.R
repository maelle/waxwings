library("spocc")
library("dplyr")
library("purrr")
library("scrubr")

############################################################
#                                                          #
#                         get data                         ####
#                                                          #
############################################################

get_slice <- function(longitude, year){
  print(paste(year, longitude))
  gbifopts <- list(limit = 200000,
                   year = year)
  waxwings <- occ(query = "Bombycilla",
                  from = c('gbif'), 
                  gbifopts = gbifopts,
                  geometry = c(longitude - 0.5, - 90, 
                               longitude + 0.5, 90))
  waxwings <- occ2df(waxwings)
}
longitudes <- seq( -180, 179, by = 0.5)
years <- rep(2011:2016, length(longitudes))
longitudes <- rep(longitudes,  6)
waxwings <- map2(longitudes, years, get_slice)
for (i in 1:length(waxwings)){
  waxwings[[i]]$latitude <- as.numeric(waxwings[[i]]$latitude)
  waxwings[[i]]$longitude <- as.numeric(waxwings[[i]]$longitude)
}

waxwings <- bind_rows(waxwings)
waxwings <- unique(waxwings)

waxwings <- filter(waxwings, !is.na(name))
readr::write_csv(waxwings, path = "uncleaned_waxwings.csv")
############################################################
#                                                          #
#                          clean                           ####
#                                                          #
############################################################


cleanup <- function(df){
  print(df$date[1])
  df <- df %>%
    coord_impossible() %>%
    coord_incomplete() %>%
    coord_unlikely() 

    if(nrow(df) > 1){
    
      df <- dedup(df)
    }
  df <- df %>%
    date_standardize("%Y-%m-%d") %>%
    date_missing()
    return(df)
} 
waxwings <- readr::read_csv("uncleaned_waxwings.csv")


waxwings <- split(waxwings, 
                  waxwings$date)

waxwings <- lapply(waxwings, cleanup)

waxwings <- bind_rows(waxwings)
waxwings <- unique(waxwings)
readr::write_csv(waxwings, path = "waxwings.csv")
nrow(waxwings)
