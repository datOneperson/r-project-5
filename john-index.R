#Project 5 covid analysis

rm(list = ls())

library(tmap)
library(sp)  
library(sf)
library(dplyr) 

#SET Working Directory TO SOURCE FILE LOCATION

# 1 
# 1.2 map by blocks
tracts <- st_read('./data for class project 5/Boundaries - Census Tracts - 2010', layer = 'geo_export_38874380-fddb-4065-ab12-7133a0816806')
tracts <- st_transform(tracts, CRS("+init=epsg:32616")) # to WGS #contains geometry for tracts
tracts$tractArea <- st_area(tracts$geometry)

blocks <- st_read('./data for class project 5/Boundaries - Census Blocks - 2010', 'geo_export_f65e2281-ba3c-481c-aa82-df8a1bb02963')
blocks <- st_transform(blocks, CRS("+init=epsg:32616")) # to WGS
blocks$blockArea <- st_area(blocks$geometry)

zipCodeAreas <- st_read('./data for class project 5/Boundaries - ZIP Codes', layer = 'geo_export_e4eb90d1-6775-4b67-9358-6cba79ffd8cf')
zipCodeAreas <- st_transform(zipCodeAreas, CRS("+init=epsg:32616")) #to WGS
zipCodeAreas$zipArea <- st_area(zipCodeAreas$geometry) #get area by the geomtry column instead of the area given for ZIP
zipCodeAreas$numericZip <-as.numeric(zipCodeAreas$zip)
table(zipCodeAreas$zip) # TWO COUNTIES HAVE TWO POLYGONS, this is why there arent 61 zip codes in later maps


#READ IN COVID DATA
covidCases <- read.csv('./data for class project 5/cases/COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code.csv')
covidCases$numericZip <-as.numeric(covidCases$zip) #ifrom character to numeric
                                   
table(covidCases$zip)

#merge zipcode shapefile data with covid number data
casesByZip <- merge(
  as.data.frame(covidCases), 
  as.data.frame(zipCodeAreas), #This inclused zip, case data, POINT, and geometry of zip code polygon
  by = "numericZip"
)

#plot(zipCodeAreas) #check zip codes

#Zip codes to sf, to WGS, GET AREA of zip codes in square meters
casesByZip <- st_as_sf(casesByZip) #as sf
casesByZip <- st_transform(casesByZip, CRS("+init=epsg:32616")) # to WGS


#Intersect Zip code with block and tracts
casesBlocksByZip <- st_intersection(casesByZip, blocks)
casesTractsByZip <- st_intersection(casesByZip, tracts) #includes zip code, case data, POINT, ZIP area m2, GEOID, TRACTCE, geometry

#CHECKS
#----
#qtm(casesTractsByZip) #check
#plot(select("casesTractsByZip", c("geometry"))) #check
#tm_shape(casesTractsByZip)+tm_fill("zipArea") #check


#-----
#CASES IN ZIP/ZIP AREA * block AREA = CASES IN TRACT
casesBlocksByZip$casesWeekly <- casesBlocksByZip$Cases...Weekly/casesBlocksByZip$zipArea *
  casesBlocksByZip$blockArea

#AGGREGATE BY BLOCK, SUM
casesBlocksAggregate <- casesBlocksByZip %>%
  group_by(blockce10) %>% #group by block
  summarise(sum_weekly = sum(casesWeekly, na.rm = TRUE)) #Sum

#CASES IN ZIP/ZIP AREA * TRACT AREA = CASES IN TRACT
casesTractsByZip$casesWeekly <- casesTractsByZip$Cases...Weekly/casesTractsByZip$zipArea *
  casesTractsByZip$tractArea

#AGGREGATE BY tract, SUM
casestractAggregate <- casesTractsByZip %>%
  group_by(tractce10) %>% #group by tract
  summarise(sum_weekly = sum(casesWeekly, na.rm = TRUE)) #Sum

#AGGREGATE BY zip, SUM
casesZipAggregate <- casesByZip %>%
  group_by(numericZip) %>% #group by tract
  summarise(sum_weekly = sum(Cases...Weekly, na.rm = TRUE)) #Sum


tmap_mode("plot")

# WEEKLY case MAP BLOCK, TRACT, ZIP
#------ 

#BY BLOCK
#Jenks <- 
tm_shape(casesBlocksAggregate) +
  tm_polygons("sum_weekly", id = "blockce10", title = "Cases", border.alpha = 0, style = "jenks", n=5) +
  tm_layout(main.title = "Weekly COVID Cases by Chicago Census Block",
            inner.margins = c(.2, .4, .1, .1),
            main.title.position = "center",
            panel.labels = c("Classification method: Jenks"),
            panel.label.color = "black",
            bg.color = "white",
            legend.outside = FALSE)+
  tm_compass(
    north = '0',
    type = 'arrow')+
  tm_credits(text = "Map created by Alyssa Besser", size = 0.6, position = c("left", "BOTTOM"))+ 
  tm_credits(text = "Source: US Census Data 2010 and CityofChicago.org", size = 0.6, position = c("left", "BOTTOM"))+
  tm_credits(text = "Map Projection WGS84 UTM 16N", size = 0.6, position = c("left", "BOTTOM"))+
  tm_scale_bar()+
  tm_legend(position = c("left", "top"),
            frame = TRUE)     


#WEEKLY MAP BY TRACT
tm_shape(casestractAggregate) +
  tm_polygons("sum_weekly", id = "tractce10", title = "Cases", border.alpha = 0, style = "jenks", n=5) +
  tm_layout(main.title = "Weekly COVID Cases by Chicago Census Tract",
            inner.margins = c(.2, .4, .1, .1),
            main.title.position = "center",
            panel.labels = c("Classification method: Jenks"),
            panel.label.color = "black",
            bg.color = "white",
            legend.outside = FALSE)+
  tm_compass(
    north = '0',
    type = 'arrow')+
  tm_credits(text = "Map created by Alyssa Besser", size = 0.6, position = c("left", "BOTTOM"))+ 
  tm_credits(text = "Source: US Census Data 2010 and CityofChicago.org", size = 0.6, position = c("left", "BOTTOM"))+
  tm_credits(text = "Map Projection WGS84 UTM 16N", size = 0.6, position = c("left", "BOTTOM"))+
  tm_scale_bar()+
  tm_legend(position = c("left", "top"),
            frame = TRUE)     

#WEEKLY MAP BY zip
tm_shape(casesZipAggregate) +
  tm_polygons("sum_weekly", id = "tractce10", title = "Cases", border.alpha = 0, style = "jenks", n=5) +
  tm_layout(main.title = "Weekly COVID Cases by Chicago Zip Code",
            inner.margins = c(.2, .4, .1, .1),
            main.title.position = "center",
            panel.labels = c("Classification method: Jenks"),
            panel.label.color = "black",
            bg.color = "white",
            legend.outside = FALSE)+
  tm_compass(
    north = '0',
    type = 'arrow')+
  tm_credits(text = "Map created by Alyssa Besser", size = 0.6, position = c("left", "BOTTOM"))+ 
  tm_credits(text = "Source: US Census Data 2010 and CityofChicago.org", size = 0.6, position = c("left", "BOTTOM"))+
  tm_credits(text = "Map Projection WGS84 UTM 16N", size = 0.6, position = c("left", "BOTTOM"))+
  tm_scale_bar()+
  tm_legend(position = c("left", "top"),
            frame = TRUE)     

#-----
#WEEKLY DEATHS BY BLOCK, TRACT, ZIP
#----
head(casesBlocksByZip)
#DEATHS IN ZIP/ZIP AREA * block AREA = CASES IN TRACT
casesBlocksByZip$deathsWeekly <- casesBlocksByZip$Deaths...Weekly/casesBlocksByZip$zipArea *
  casesBlocksByZip$blockArea

#AGGREGATE BY BLOCK, SUM
casesBlocksAggregate <- casesBlocksByZip %>%
  group_by(blockce10) %>% #group by block
  summarise(sum_weekly = sum(deathsWeekly, na.rm = TRUE)) #Sum

#Map blocks death <- 
tm_shape(casesBlocksAggregate) +
  tm_polygons("sum_weekly", id = "blockce10", title = "Deaths", border.alpha = 0, style = "jenks", n=5) +
  tm_layout(main.title = "Weekly COVID Deaths by Chicago Census Block",
            inner.margins = c(.2, .4, .1, .1),
            main.title.position = "center",
            panel.labels = c("Classification method: Jenks"),
            panel.label.color = "black",
            bg.color = "white",
            legend.outside = FALSE)+
  tm_compass(
    north = '0',
    type = 'arrow')+
  tm_credits(text = "Map created by Alyssa Besser", size = 0.6, position = c("left", "BOTTOM"))+ 
  tm_credits(text = "Source: US Census Data 2010 and CityofChicago.org", size = 0.6, position = c("left", "BOTTOM"))+
  tm_credits(text = "Map Projection WGS84 UTM 16N", size = 0.6, position = c("left", "BOTTOM"))+
  tm_scale_bar()+
  tm_legend(position = c("left", "top"),
            frame = TRUE)     

#WEEKLY DEATH BY TRACT
#DEATHS IN ZIP/ZIP AREA * TRACT AREA = CASES IN TRACT
casesTractsByZip$deathsWeekly <- casesTractsByZip$Deaths...Weekly/casesTractsByZip$zipArea *
  casesTractsByZip$tractArea

#AGGREGATE BY tract, SUM
casestractAggregate <- casesTractsByZip %>%
  group_by(tractce10) %>% #group by tract
  summarise(sum_weekly = sum(deathsWeekly, na.rm = TRUE)) #Sum

#WEEKLY deaths MAP BY TRACT
tm_shape(casestractAggregate) +
  tm_polygons("sum_weekly", id = "tractce10", title = "Deaths", border.alpha = 0, style = "jenks", n=5) +
  tm_layout(main.title = "Weekly COVID Deaths by Chicago Census Tract",
            inner.margins = c(.2, .4, .1, .1),
            main.title.position = "center",
            panel.labels = c("Classification method: Jenks"),
            panel.label.color = "black",
            bg.color = "white",
            legend.outside = FALSE)+
  tm_compass(
    north = '0',
    type = 'arrow')+
  tm_credits(text = "Map created by Alyssa Besser", size = 0.6, position = c("left", "BOTTOM"))+ 
  tm_credits(text = "Source: US Census Data 2010 and CityofChicago.org", size = 0.6, position = c("left", "BOTTOM"))+
  tm_credits(text = "Map Projection WGS84 UTM 16N", size = 0.6, position = c("left", "BOTTOM"))+
  tm_scale_bar()+
  tm_legend(position = c("left", "top"),
            frame = TRUE)     

#WEEKLY DEATH BY ZIP
#AGGREGATE BY zip, SUM
head(casesBlocksByZip)
#TEST IN ZIP/ZIP AREA * block AREA = CASES IN TRACT
casesBlocksByZip$testsWeekly <- casesBlocksByZip$Deaths...Weekly/casesBlocksByZip$zipArea *
  casesBlocksByZip$blockArea

#AGGREGATE BY BLOCK, SUM
casesBlocksAggregate <- casesBlocksByZip %>%
  group_by(blockce10) %>% #group by block
  summarise(sum_weekly = sum(deathsWeekly, na.rm = TRUE)) #Sum

 #WEEKLY DEATH BY ZIP<- 

#WEEKLY MAP DEATHS BY zip
tm_shape(casesZipAggregate) +
  tm_polygons("sum_weekly", id = "tractce10", title = "Deaths", border.alpha = 0, style = "jenks", n=5) +
  tm_layout(main.title = "Weekly COVID Deaths by Chicago Zip Code",
            inner.margins = c(.2, .4, .1, .1),
            main.title.position = "center",
            panel.labels = c("Classification method: Jenks"),
            panel.label.color = "black",
            bg.color = "white",
            legend.outside = FALSE)+
  tm_compass(
    north = '0',
    type = 'arrow')+
  tm_credits(text = "Map created by Alyssa Besser", size = 0.6, position = c("left", "BOTTOM"))+ 
  tm_credits(text = "Source: US Census Data 2010 and CityofChicago.org", size = 0.6, position = c("left", "BOTTOM"))+
  tm_credits(text = "Map Projection WGS84 UTM 16N", size = 0.6, position = c("left", "BOTTOM"))+
  tm_scale_bar()+
  tm_legend(position = c("left", "top"),
            frame = TRUE)     

#TEST BY ZIP, TRACT, BLOCK
#-----
#TEST BY BLOCK
head(casesBlocksByZip)

#tests IN ZIP/ZIP AREA * block AREA = CASES IN TRACT
casesBlocksByZip$testsWeekly <- casesBlocksByZip$Tests...Weekly/casesBlocksByZip$zipArea *
  casesBlocksByZip$blockArea

#AGGREGATE BY BLOCK, SUM
casesBlocksAggregate <- casesBlocksByZip %>%
  group_by(blockce10) %>% #group by block
  summarise(sum_weekly = sum(testsWeekly, na.rm = TRUE)) #Sum
#MapJenks <- 
tm_shape(casesBlocksAggregate) +
  tm_polygons("sum_weekly", id = "blockce10", title = "Tests", border.alpha = 0, style = "jenks", n=5) +
  tm_layout(main.title = "Weekly COVID Tests by Chicago Census Block",
            inner.margins = c(.2, .4, .1, .1),
            main.title.position = "center",
            panel.labels = c("Classification method: Jenks"),
            panel.label.color = "black",
            bg.color = "white",
            legend.outside = FALSE)+
  tm_compass(
    north = '0',
    type = 'arrow')+
  tm_credits(text = "Map created by Alyssa Besser", size = 0.6, position = c("left", "BOTTOM"))+ 
  tm_credits(text = "Source: US Census Data 2010 and CityofChicago.org", size = 0.6, position = c("left", "BOTTOM"))+
  tm_credits(text = "Map Projection WGS84 UTM 16N", size = 0.6, position = c("left", "BOTTOM"))+
  tm_scale_bar()+
  tm_legend(position = c("left", "top"),
            frame = TRUE)     

#WEEKLY TEST BY ZIP, TRACT, BLOCK
#-----

casesTractsByZip$testsWeekly <- casesTractsByZip$Tests...Weekly/casesTractsByZip$zipArea *
  casesTractsByZip$tractArea

#AGGREGATE BY tract, SUM
casestractAggregate <- casesTractsByZip %>%
  group_by(tractce10) %>% #group by tract
  summarise(sum_weekly = sum(testsWeekly, na.rm = TRUE)) #Sum

#WEEKLY TEST MAP BY TRACT
tm_shape(casestractAggregate) +
  tm_polygons("sum_weekly", id = "tractce10", title = "Tests", border.alpha = 0, style = "jenks", n=5) +
  tm_layout(main.title = "Weekly COVID Tests by Chicago Census Tract",
            inner.margins = c(.2, .4, .1, .1),
            main.title.position = "center",
            panel.labels = c("Classification method: Jenks"),
            panel.label.color = "black",
            bg.color = "white",
            legend.outside = FALSE)+
  tm_compass(
    north = '0',
    type = 'arrow')+
  tm_credits(text = "Map created by Alyssa Besser", size = 0.6, position = c("left", "BOTTOM"))+ 
  tm_credits(text = "Source: US Census Data 2010 and CityofChicago.org", size = 0.6, position = c("left", "BOTTOM"))+
  tm_credits(text = "Map Projection WGS84 UTM 16N", size = 0.6, position = c("left", "BOTTOM"))+
  tm_scale_bar()+
  tm_legend(position = c("left", "top"),
            frame = TRUE)     

#zip
#AGGREGATE BY zip, SUM
casesZipAggregate <- casesByZip %>%
  group_by(numericZip) %>% #group by tract
  summarise(sum_weekly = sum(Tests...Weekly, na.rm = TRUE)) #Sum

#WEEKLY TEST MAP BY zip
tm_shape(casesZipAggregate) +
  tm_polygons("sum_weekly", id = "tractce10", title = "Tests", border.alpha = 0, style = "jenks", n=5) +
  tm_layout(main.title = "Weekly COVID Tests by Chicago Zip Code",
            inner.margins = c(.2, .4, .1, .1),
            main.title.position = "center",
            panel.labels = c("Classification method: Jenks"),
            panel.label.color = "black",
            bg.color = "white",
            legend.outside = FALSE)+
  tm_compass(
    north = '0',
    type = 'arrow')+
  tm_credits(text = "Map created by Alyssa Besser", size = 0.6, position = c("left", "BOTTOM"))+ 
  tm_credits(text = "Source: US Census Data 2010 and CityofChicago.org", size = 0.6, position = c("left", "BOTTOM"))+
  tm_credits(text = "Map Projection WGS84 UTM 16N", size = 0.6, position = c("left", "BOTTOM"))+
  tm_scale_bar()+
  tm_legend(position = c("left", "top"),
            frame = TRUE)     

#WHAT I think is interesting, political map over COVID map
#-----

WARDshape <- read.csv('C:/Users/abesser/OneDrive - purdue.edu/CE 597/Project 5/WARDS_2015.csv')
politicaldata <- read.csv('C:/Users/abesser/OneDrive - purdue.edu/CE 597/Project 5/politcal.csv')

plot(WARDshape)

#merge zipcode shapefile data with covid number data
politicalmerge <- merge(
  as.data.frame(politicaldata), 
  as.data.frame(WARDshape), #This inclused zip, case data, POINT, and geometry of zip code polygon
  by = ""
