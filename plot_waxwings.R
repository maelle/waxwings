library("ggplot2")
library("ggalt") 
library("magick")
library("ggmap")
library("ggthemes")
library("dplyr")
library("albersusa")
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
#                     day of the week                      ####
#                                                          #
############################################################
waxwings <- mutate(waxwings, 
                   wday = lubridate::wday(date, label = TRUE))
waxwings %>%
  group_by(update(date, wday = 1), wday) %>%
  summarize(n = n()) %>%
ggplot() +
  geom_boxplot(aes(wday, n))

ggsave(file = "wday.png", width = 8, height = 6)
############################################################
#                                                          #
#                        second map                        ####
#                                                          #
############################################################
waxwings <- mutate(waxwings,
                   month = lubridate::month(date))
cedrorum <- filter(waxwings,
                   name == "Bombycilla cedrorum")
plot_month_cedrorum <- function(df, cedrorum){
  p <- ggplot()
  p <- p + ggtitle(df$month[1])
  p <- p + geom_map(data = wax_map,
                    map = wax_map,
                    aes(x = long, y = lat, map_id = region),
                    color = "white", fill = "#7f7f7f",
                    size = 0.05, alpha = 1/4)
  p <- p + theme_map() 
  p <- p + geom_point(aes(longitude, latitude),
                      data = df,
                      size = .5,
                      col = "red")
  p <- p + ylim(min(cedrorum$latitude), 
                max(cedrorum$latitude))
  p <- p + xlim(min(cedrorum$longitude), 
                max(cedrorum$longitude))
  outfil <- paste0("cedrorum_", df$month[1], ".png")
  ggsave(outfil, p, width=5, height=5)
  
  outfil
  
}

cedrorum_l <- split(cedrorum, cedrorum$month)


cedrorum_l %>%
  map(plot_month_cedrorum, cedrorum = cedrorum) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=1) %>%
  image_write("cedrorum.gif")

garrulus <- filter(waxwings,
                   name == "Bombycilla garrulus")
garrulus_l <- split(garrulus, garrulus$month)

garrulus_l %>%
  map(plot_month_cedrorum, cedrorum = garrulus) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=1) %>%
  image_write("garrulus.gif")