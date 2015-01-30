#
#  Purpose: Reads in a data file.  Contents of the file are expected to be
#   the pixel values for an image of some given size.  Example, pixel values
#   ranging from 0 to 255 in a 512 x 512.  These might have been in an Excel
#   sheet then exported in a csv format.  Perhaps an image that was read into
#   python or MATLAB and then exported.
#
#   Author               Date               Notes
#   T. D. Kelly          29 JAN 2015        Original version of code
#

# Requires the raster library.  IF this is not already installed in your
# R software, you will need to install.packages("raster")
library(raster)

# You will need to edit this part of the code to provide your data file
# The data file will be read in
# Converted to a matrix
# Then rastered and stored in DataImage
DataImage <- as.matrix(read.csv("AFMSputData.csv", header=FALSE))

# You can always create a SUBSET of your image if you want to find other
#  regions of the image after finding the first "big" region
#temp <- c(1:100,1:100)

RasterImage <- raster(DataImage)

# So that the image doesn't squish itself to a 1 x 1 change extent
extent(RasterImage) = extent( c(0,nrow(DataImage),0,ncol(DataImage)) + 0.5)

# Take a look to make sure this is what you wanted
#View(DataFile) # Commented out for now, it can be quite large

# Function to find the maximum of the argument passed
MaxValue <- function( rimage ) {
  max( rimage, na.rm = TRUE)
}

# This part needs to be edited to get weighting matrix 3x3, 3x5, 9x7, etc
weights <- matrix(1,3,3)

# Now store the localmaxima that are found using the focal function
localMaxima <- focal( x = RasterImage, weights, fun = MaxValue, pad = TRUE, padValue = NA)

# Extract the x,y coordinates of those maxima
maximumCoordinates <- xyFromCell(localMaxima, Which(localMaxima <= 0.1, cells=TRUE))
# You may need to execute this command after source("findLocalMaxima.R")
head(maximumCoordinates) # gives a preview
View(maximumCoordinates) # You can uncomment this as needed

# Store the Image of the localmaxima
MaximaImage <- RasterImage == localMaxima

par(mfrow=c(1,2))
plot(RasterImage)
plot(MaximaImage)