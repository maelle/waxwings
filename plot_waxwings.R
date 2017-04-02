library("ggplot2")
library("ggalt") 
library("magick")
library("ggmap")
library("ggthemes")
library("dplyr")
waxwings <- readr::read_csv("waxwings.csv")
waxwings <- filter(waxwings, name != "Bombycilla japonica",
                   longitude < 50)
waxwings <- filter(waxwings, !(longitude == 0 & latitude == 0))
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
                        col = name),
                    data = waxwings)
p <- p + ylim(0, 80) +
  xlim(-165, 40)
p + theme(legend.position = "none")

ggsave(p, file = "map.png",
       width = 8, height = 5)

############################################################
#                                                          #
#                        second map                        ####
#                                                          #
############################################################

waxwings <- readr::read_csv("waxwings.csv")

waxwings <- mutate(waxwings,
                   breeding = 
                     lubridate::month(date) %in%
                     c(3:9),
                   month = lubridate::month(date))
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
p <- p + facet_grid(month ~ name) +
  ggtitle("Breeding season (March-Sept)")
p <- p +  geom_bkde2d(bandwidth=c(5, 5),
                      aes(longitude, latitude),
                      data = waxwings[waxwings$breeding,],
                      col = "red")+  
  geom_bkde2d(bandwidth=c(5, 5),
              aes(longitude, latitude),
              data = waxwings[!waxwings$breeding,],
              col = "blue")

ggsave(p, file = "map2.png",
       width = 20, height = 5)