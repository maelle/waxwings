library("spocc")
library("dplyr")
library("purrr")
library("scrubr")
library("ggplot2")
library("ggalt") 
library("magick")
library("ggmap")
library("ggthemes")
############################################################
#                                                          #
#                         get data                         ####
#                                                          #
############################################################

get_slice <- function(longitude){
  print(longitude)
  gbifopts <- list(limit = 200000,
                   year = 2016)
  inatopts <- list(maxresults = 100000, year = 2016)
  waxwings <- occ(query = "Bombycilla garrulus",
                  from = c('gbif', 'inat', 'ebird'), 
                  gbifopts = gbifopts,
                  inatopts = inatopts,
                  geometry = c(longitude, - 90, longitude + 5, 90))
  waxwings <- occ2df(waxwings)
}
longitudes <- seq(-180, 175, by = 5)
waxwings <- lapply(longitudes, get_slice)
for (i in 1:length(waxwings)){
  waxwings[[i]]$latitude <- as.numeric(waxwings[[i]]$latitude)
  waxwings[[i]]$longitude <- as.numeric(waxwings[[i]]$longitude)
}

waxwings <- bind_rows(waxwings)
waxwings <- unique(waxwings)

readr::write_csv(waxwings, path = "uncleaned_waxwings.csv")
############################################################
#                                                          #
#                          clean                           ####
#                                                          #
############################################################


cleanup <- function(df){
  print(df$date[1])
  df %>%
    coord_impossible() %>%
    coord_incomplete() %>%
    coord_unlikely() %>%
    dedup() %>%
    date_standardize("%Y-%m-%d") %>%
    date_missing()
} 

cleanup(waxwings)

waxwings <- split(waxwings, 
                  lubridate::week(waxwings$date))

waxwings <- lapply(waxwings, cleanup)

waxwings <- bind_rows(waxwings)
waxwings <- unique(waxwings)
readr::write_csv(waxwings, path = "waxwings.csv")
nrow(waxwings)
############################################################
#                                                          #
#                           map                            ####
#                                                          #
############################################################
wax_map <- map_data("world")
wax_map <- wax_map[wax_map$region != "Antarctica",]
p <- ggplot()
p <- p + geom_map(data = wax_map,
           map = wax_map,
           aes(x = long, y = lat, map_id = region),
           color = "white", fill = "#7f7f7f",
           size = 0.05, alpha = 1/4)
p <- p + theme_map() 
p <- p + geom_point(aes(longitude, latitude,
                 col = lubridate::month(date)),
             data = waxwings)
p <- p + ylim(0, 80)
p + theme(legend.position = "none")

ggsave(p, file = "map.png",
       width = 8, height = 8)

############################################################
#                                                          #
#                        second map                        ####
#                                                          #
############################################################

waxwings <- readr::read_csv("waxwings.csv")

waxwings <- mutate(waxwings,
                   breeding = 
                     lubridate::month(date) %in%
                     c(3:9))
wax_map <- map_data("world")
wax_map <- wax_map[wax_map$region != "Antarctica",]

p <- ggplot()
p <- p + geom_map(data = wax_map,
                  map = wax_map,
                  aes(x = long, y = lat, map_id = region),
                  color = "white", fill = "#7f7f7f",
                  size = 0.05, alpha = 1/4)
p <- p + theme_map() 
p <- p + geom_point(aes(longitude, latitude),
                    data = waxwings,
                    size = .5)
p <- p + ylim(40, 80)
p <- p + facet_grid(breeding ~ .) +
  ggtitle("Breeding season (March-Sept)")
p +  geom_bkde2d(bandwidth=c(5, 5),
                 aes(longitude, latitude),
                 data = waxwings[waxwings$breeding,],
                 col = "red")+  
  geom_bkde2d(bandwidth=c(5, 5),
            aes(longitude, latitude),
            data = waxwings[!waxwings$breeding,],
            col = "blue")

