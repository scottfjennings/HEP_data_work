

library(sf)
library(rgdal)
subreg_line <- readOGR(dsn = "C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_spatial/subregions", layer = "subregion_line")

subreg_line_ll <- spTransform(subreg_line, CRS("+proj=longlat +datum=WGS84"))
writeOGR(subreg_line_ll, "subreg_line.kml", layer = "subreg_line", driver="KML")


study_area_line <- read_sf(dsn = "C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_spatial/subregions", layer = "atlas_boundary_line")