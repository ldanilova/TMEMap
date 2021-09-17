#' @import interp
#' @import SpatialExperiment
#import description end
0


#===================
#' addVoronoiArea
#' Calculate Voronoi area in a SpatialExperiment object and add it to the reducedDim slot
#'
#' This function calculates Voronoi area for every image in a SpatialExperiment object and adds it to the reducedDim slot
#'   Voronoi should be calculated for each image separately, so if data come from several cores, slides, images, and so on
#'   then spatialCoords should have a column that specifies how to split.
#'
#' @export
#'
#' @param se 	a SpatialExperiment object.
#' @param splitBy 	a character string or interger that specifies a column in spatialData to use to split coordiatnes
#'   if they correspond to several images. NULL means there is only one image in the object.
#'
#' @return a SpatialExperiment object.

addVoronoiArea <- function(se, splitBy = NULL)
{
	# extract coordinates to calculate area
	coord <- spatialCoords(se)
	# names of columns with x and y coordinates to calculate area
	c_names <- spatialCoordsNames(se)
	# get spatial data
	spData = spatialData(se)
	# Voronoi area
	v_area <- c()
	# if there is only one image, then calculate Voronoi using all coordinates without spliting
	if (is.null(splitBy))
	{
		# calculate area
		v_area <- voronoi.mosaic(coord[,c_names[1]],coord[,c_names[2]]) %>% voronoi.area
	}else{
	# if there is more than one image, split coordinates, calculate area, merge back and add to reducedDim
		# check if there is the splitBy column

		if(is(splitBy,'character') && !(splitBy %in% spatialDataNames(se)))
		{
			stop(paste("There is no column", splitBy,'in spatialData'))
			return(se)
		}
		if(is(splitBy,'numeric') && !(splitBy %in% seq_len(ncol(spData))))
		{
			stop(paste("There is no column", splitBy,'in spatialData'))
			return(se)
		}
		#-------------------------
		# split coordinates by region/images to calculate area per region
		tryCatch({
			coordByRegion <- split(x = data.frame(coord), f = factor(spData[, splitBy]))
		}, error = function(ex) {
			message(paste("Cannot split the spatialData by column", splitBy))
  			print(ex);
			return(se)
		})
		#--------------------------
		# calculate area
		tryCatch({
			v_area <- lapply(coordByRegion, function(x)
			{
				voronoi.mosaic(x[,c_names[1]],x[,c_names[2]]) %>% voronoi.area
			})
			# merge all areas to a single vector
			v_area <- unlist(v_area)
		}, error = function(ex) { # if area wasn't caclulated, return an error
			message("Cannot calculate Voronoi area")
  			print(ex);
		})
	}
	# if Vornoi area was calculated, add it to reducedDim
	if(length(v_area) != 0) {
		reducedDim(se, "Voronoi") <- data.frame(v_area, row.names = rownames(coord))
		return(se)
	}else{ # if area wasn't calculated, return a warning
		warning("Voronoi area was not calculated")
	    return(se)
	}
	return(se)
}
