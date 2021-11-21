
# SETTING UP --------------------------------------------------------------


# remotes::install_github("r-spatial/rgee")
library(rgee)

## It is necessary just once
ee_install()

# Initialize Earth Engine!
ee_Initialize()

ee_check() # Check non-R dependencies
# ee_clean_credentials() # Remove credentials of a specific user
# ee_clean_pyenv() # Remove reticulate system variables

# INITIALIZE --------------------------------------------------------------


library(rgee)
#ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# traditional R character
print("Hello world!")

# Earth Engine Python Style
ee$String("Hello World from Earth Engine!")$getInfo()
ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")$getInfo()

# Earth Engine Pipes Style
ee$String("Hello World from Earth Engine!") %>%
  ee$String$getInfo()

ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318") %>%
  ee$Image$getInfo()


# LOAD AND MAP AN IMAGE ---------------------------------------------------


# Load an image.
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")

# Display the image.
Map$centerObject(image)
Map$addLayer(image, name = "Landsat 8 original image")

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = c("B5", "B4", "B3"),
  min = 5000, max = 15000, gamma = 1.3
)

Map$addLayer(image, vizParams, "Landsat 8 False color")

# Use Map to add features and feature collections to the map. For example,
counties <- ee$FeatureCollection("TIGER/2016/Counties")

Map$addLayer(
  eeObject = counties,
  visParams = vizParams,
  name = "counties"
)



# MAP NDVI WITH CLOUDY MASK -----------------------------------------------

library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# This function gets NDVI from Landsat 8 imagery.
addNDVI <- function(image) {
  return(image$addBands(image$normalizedDifference(c("B5", "B4"))))
}

# This function masks cloudy pixels.
cloudMask <- function(image) {
  clouds <- ee$Algorithms$Landsat$simpleCloudScore(image)$select("cloud")
  return(image$updateMask(clouds$lt(10)))
}

# Load a Landsat collection, map the NDVI and cloud masking functions over it.
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(c(-122.262, 37.8719)))$
  filterDate("2014-03-01", "2014-05-31")$
  map(addNDVI)$
  map(cloudMask)

# Reduce the collection to the mean of each pixel and display.
meanImage <- collection$reduce(ee$Reducer$mean())
vizParams <- list(
  bands = c("B5_mean", "B4_mean", "B3_mean"),
  min = 0,
  max = 0.5
)

Map$addLayer(
  eeObject = meanImage,
  visParams = vizParams,
  name = "mean"
)

# Load a region in which to compute the mean and display it.
counties <- ee$FeatureCollection("TIGER/2016/Counties")
santaClara <- ee$Feature(counties$filter(
  ee$Filter$eq("NAME", "Santa Clara")
)$first())

Map$addLayer(
  eeObject = santaClara,
  visParams = list(palette = "yellow"),
  name = "Santa Clara"
)

# Get the mean of NDVI in the region.
mean <- meanImage$select("nd_mean")$reduceRegion(
  reducer = ee$Reducer$mean(),
  geometry = santaClara$geometry(),
  scale = 30
)

# Print mean NDVI for the region.
cat("Santa Clara spring mean NDVI:", mean$get("nd_mean")$getInfo())


# GET CHANGE IN WATER -----------------------------------------------------


library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

###############################
# Asset List
###############################

gsw = ee$Image('JRC/GSW1_1/GlobalSurfaceWater')
occurrence = gsw$select('occurrence')
change = gsw$select("change_abs")
roi = ee$Geometry$Polygon(
  list(
    c(-74.17213, -8.65569),
    c(-74.17419, -8.39222),
    c(-74.38362, -8.36980),
    c(-74.43031, -8.61293)
  )
)

###############################
# Constants
###############################

VIS_OCCURRENCE = list(
  min = 0,
  max = 100,
  palette = c('red', 'blue')
)

VIS_CHANGE = list(
  min = -50,
  max = 50,
  palette = c('red', 'black', 'limegreen')
)

VIS_WATER_MASK = list(
  palette = c('white', 'black')
)

###############################
# Calculations
###############################

# Create a water mask layer, and set the image mask so that non-water areas are transparent.
water_mask = occurrence$gt(90)$mask(1)

###############################
# Initialize Map Location
###############################

# Uncomment one of the following statements to center the map on
# a particular location.
# Map$setCenter(-90.162, 29.8597, 10)   # New Orleans, USA
# Map$setCenter(-114.9774, 31.9254, 10) # Mouth of the Colorado River, Mexico
# Map$setCenter(-111.1871, 37.0963, 11) # Lake Powell, USA
# Map$setCenter(149.412, -35.0789, 11)  # Lake George, Australia
# Map$setCenter(105.26, 11.2134, 9)     # Mekong River Basin, SouthEast Asia
# Map$setCenter(90.6743, 22.7382, 10)   # Meghna River, Bangladesh
# Map$setCenter(81.2714, 16.5079, 11)   # Godavari River Basin Irrigation Project, India
# Map$setCenter(14.7035, 52.0985, 12)   # River Oder, Germany & Poland
# Map$setCenter(-59.1696, -33.8111, 9)  # Buenos Aires, Argentina\
Map$setCenter(-74.4557, -8.4289, 11)  # Ucayali River, Peru

###############################
# Map Layers
###############################

Map$addLayer(water_mask, VIS_WATER_MASK, '90% occurrence water mask', FALSE) +
  Map$addLayer(occurrence$updateMask(occurrence$divide(100)),  VIS_OCCURRENCE, "Water Occurrence (1984-2015)") +
  Map$addLayer(change, VIS_CHANGE,'occurrence change intensity')