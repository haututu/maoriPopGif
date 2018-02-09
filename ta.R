library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)


#Import data
dat <- read.csv("TABLECODE7566_Data_6180744e-7be3-4a58-8954-9ec4437db375.csv")

#Get proportion of Maori
dat <- tidyr::spread(dat, Ethnicity, Value) %>%
  mutate(percMaori = round(Maori / `Total population`, 2)) %>%
  rename(TA2017_NAM = Area)

#Load TA file and do some goblin magic
ta <- st_read(dsn = ".", layer = "territorial-authority-2017-generalised-version")

ta <- st_set_crs(ta, 2193) #data is in NZMG
ta <- st_transform(ta, 4326) #convert to WGS84
  
ta <- rmapshaper::ms_simplify(as(ta, "Spatial"), keep=0.5) %>% st_as_sf()

dat$TA2017_NAM <- tolower(dat$TA2017_NAM)
ta$TA2017_NAM <- tolower(ta$TA2017_NAM)

#Piece of crap dplyr with non-regular expressions means I manually ran each image... terrible I know...
#It's on my to do list to fix up.
yearMap <- function(year) {
  
  mapData <- merge(ta, filter(dat, Year == 2038) %>% select(TA2017_NAM, percMaori), by="TA2017_NAM")
  
  bins <- c(quantile(mapData$percMaori, seq(0, 1, 0.10), na.rm = TRUE), Inf)
  pal <- colorBin("YlOrRd", domain = mapData$percMaori, bins = bins)
  
  #Plot
  leaflet() %>% 
    addProviderTiles("Esri.WorldGrayCanvas") %>% 
    #fitBounds(168.70047, -32.94, 176.52966, -48.33) %>% #allNZ
    fitBounds(169.74,-33.69,179.93,-42.33) %>% #north island
    addPolygons(data=mapData,
                label=~TA2017_NAM,
                fillColor=~pal(percMaori),
                opacity=1, 
                weight=2,
                color = "white",
                dashArray = "1",
                fillOpacity = 0.7) %>%
    addLegend(pal = pal, values = mapData$percMaori, opacity = 0.7, title = "2038 Maori",
              position = "bottomright")
  
  
}

#Animate the images
library(magick)
image1 <- image_read("ni2013.jpeg")
image2 <- image_read("ni2018.jpeg")
image3 <- image_read("ni2023.jpeg")
image4 <- image_read("ni2028.jpeg")
image5 <- image_read("ni2033.jpeg")
image6 <- image_read("ni2038.jpeg")

niGif <- image_animate(c(image1, image2, image3, image4, image5, image6), fps = 1)

image_write(niGif, "niMaori.gif")
