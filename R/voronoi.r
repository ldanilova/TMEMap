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
#' @param splitBy 	a character string or interger that specifies a column in spatialCoords to use to split coordiatnes
#'   if they correspond to several images. NULL means there is only one image in the object.
#'
#' @return a SpatialExperiment object.

addVoronoiArea <- function(se, splitBy = NULL)
{
	# extract coordinates to calculate area
	coord <- spatialCoords(se)
	# Voronoi area
	v_area <- c()
	# if there is only one image, then calculate Voronoi using all coordinates without spliting
	if (is.null(splitBy))
	{
		# calculate area
		v_area <- voronoi.mosaic(coord[,'x'],coord[,'y']) %>% voronoi.area
	}else{
	# if there is more than one image, split coordinates, calculate area, merge back and add to reducedDim
		# check if there is the splitBy column

		if(is(splitBy,'character') && !(splitBy %in% colnames(coord)))
		{
			stop(paste("There is no column", splitBy,'in spatialCoords'))
			return(se)
		}
		if(is(splitBy,'numeric') && !(splitBy %in% seq_len(ncol(coord))))
		{
			stop(paste("There is no column", splitBy,'in spatialCoords'))
			return(se)
		}
		#-------------------------
		# split coordinates by region/images to calculate area per region
		tryCatch({
			coordByRegion <- split(x = coord, f = factor(coord[, splitBy]))
		}, error = function(ex) {
			message(paste("Cannot split the spatialCoords by column", splitBy))
  			print(ex);
			return(se)
		})
		#--------------------------
		# calculate area
		tryCatch({
			v_area <- lapply(coordByRegion, function(x)
			{
				voronoi.mosaic(x[,'x'],x[,'y']) %>% voronoi.area
			})
			# merge all areas to a single vector
			v_area <- unlist(v_area)
		}, error = function(ex) {
			message("Cannot calculate Voronoi area")
  			print(ex);
		})
	}
	# if Vornoi area was calculated, add it to reducedDim
	if(length(v_area) != 0) {
		reducedDim(se, "Voronoi") <- data.frame(v_area)
		return(se)
	}else{
		warning("Voronoi area was not calculated")
	}
}
