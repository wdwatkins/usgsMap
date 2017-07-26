library(readxl)
library(maps)
library(maptools)
library(sp)
library(ggplot2)
library(magrittr)

source('mapHelper.R')
mapData <- read_excel('Map Data.xlsx')

#need to set state for carribbean WSC
mapData$STATE[mapData$CITY == "GUAYNABO"] <- "PR"

conus <- to_sp('state')

# thanks to Bob Rudis (hrbrmstr):
# https://github.com/hrbrmstr/rd3albers

# -- if moving any more states, do it here: --
move_variables <- list(
  AK = list(scale=0.6, shift = c(40,-450), rotate=-50),
  HI = list(scale=1, shift=c(520, -110), rotate=-35),
  PR = list(scale=2.5, shift = c(-140, 90), rotate=20)
)

stuff_to_move <- list(
  AK = to_sp("world", "USA:alaska"),
  HI = to_sp("world", "USA:hawaii"),
  PR = to_sp("world", "Puerto Rico")
)

states.out <- conus

wgs84 <- "+init=epsg:4326"
coords = cbind(mapData$LONGITUDE, mapData$LATITUDE)
 sites = SpatialPoints(coords, proj4string = CRS(wgs84)) %>% 
   spTransform(CRS(proj4string(states.out)))

 sites.df <- as.data.frame(sites)
 
for(i in names(move_variables)){
  shifted <- do.call(shift_sp, c(sp = stuff_to_move[[i]], 
                                 move_variables[[i]],  
                                 proj.string = proj4string(conus),
                                 row.names = i))
  states.out <- rbind(shifted, states.out, makeUniqueIDs = TRUE)
  
  shifted.sites <- do.call(shift_sp, c(sp = sites[mapData$STATE == i,],
                                       move_variables[[i]],
                                       proj.string = proj4string(conus),
                                       ref=stuff_to_move[[i]])) %>%
    as.data.frame %>%
    coordinates()

  sites.df[mapData$STATE == i, ] <- shifted.sites
  
}

#create regions
regions <- list( NE = states.out[c('maine', 'new hampshire', 'vermont', 
                                   'massachusetts', 'rhode island', 'connecticut',
                                   'new york', 'delaware', 'new jersey', 'maryland',
                                   'pennsylvania', 'west virginia', 'virginia', 
                                   'district of columbia')],
                 SE = states.out[c('north carolina', 'south carolina', 'georgia', 
                                   'florida', 'mississippi', 'alabama', 'tennessee',
                                   'arkansas', 'louisiana', 'PR')],
                 SW = states.out[c('arizona', 'utah', 'colorado', 'new mexico', 
                                   'texas', 'oklahoma', 'kansas')],
                 MW = states.out[c('north dakota', 'south dakota', 'nebraska',
                                   'minnesota', 'wisconsin', 'iowa', 'missouri',
                                   'illinois', 'indiana', 'ohio', 'michigan', 
                                   'kentucky')],
                 PAC = states.out[c("california", "nevada", "HI")],
                 NW = states.out[c("washington", "oregon", "idaho",
                                   "montana", "wyoming")],
                 AK = states.out[c("AK")])

colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
regionNames <- c("NE", "SE", "SW", "MW", "PAC", "NW", "AK")
color_region_df <- data.frame(regionNames, colors, stringsAsFactors = FALSE)
gsMap <- ggplot() +
  # geom_polygon(aes(x = long, y = lat, group = group),
  #              data = tri, fill = "blue",
  #              alpha = 0.5, color = "white") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) 

for(i in 1:nrow(color_region_df)) {
  gsMap <- gsMap + geom_polygon(aes(x = long, y = lat, group = group),
                                      data = regions[[color_region_df$regionName[i]]], 
                        fill = color_region_df$color[i],
                                      alpha = 0.5, color = "white")
}

gsMap <- gsMap + geom_point(data = sites.df,
           aes(x = coords.x1, y=coords.x2),
           colour = "black", size = 1) 
gsMap