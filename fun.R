# Copyright 2016 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(rgeos)
#' Compare more than two things
#' 
#' @param ... things to compare
#' @param fun the function to use to compare (default \code{all.equal}). 
#'   \code{identical} is the other option. Must be named as it comes after
#'   \code{...}
#'   
#' @return logical
#' @export
#' 
#' @examples
compare_all <- function(..., fun = all.equal) {
  if (!as.character(substitute(fun)) %in% c("all.equal", "identical")) {
    stop("fun must be either all.equal or identical")
  }
  dots <- list(...)
  
  if (length(dots) < 2) stop("You need at least two things to compare!")
  
  equal_to_first <- vapply(dots[-1], FUN = function(z) {isTRUE(fun(z, dots[[1]]))}, 
                           FUN.VALUE = logical(1))
  all(equal_to_first)
}

#' Get year from character field
#'
#' Basically just finds four consecutive digits starting with 18, 19, or 20
#'
#' @param x 
#'
#' @return integer
#'
#' @examples
get_year <- function(x) {
  yr_char <- gsub(".*(18|19|20)(\\d{2}).*", "\\1\\2", as.character(x))
  as.integer(yr_char)
}

#' Summarize occurrences of points within a polygon
#'
#' <full description of function>
#'
#' @import sp
#' @param  polys SpatialPolygonsDataFrame to summarise by
#' @param  points SpatialPointsDataFrame to summarise
#' @param  var Variable (column name) in \code{points} to summarise
#' @param  fn The summary function with which to summarise point data in each polygon
#' 
#' @return A vector of the same length as the SpatialPolygonsDataFrame.
poly_summary <- function(polys, points, var, fn, ...) {
  
  if (!class(points) == "SpatialPointsDataFrame") {
    stop("points is not of class SpatialPointsDataFrame")
  }
  
  if (!class(polys) == "SpatialPolygonsDataFrame") {
    stop("polys is not of class SpatialPolygonsDataFrame")
  }
  
  if (!var %in% names(points)) {
    stop(var, " is not a column in points spatial data frame")
  }
  
  if (!class(fn) == "function") {
    stop("fn is not a valid function")
  }
  
  in_fun <- function(x) {
    vec <- x[[var]]
    if (length(vec) > 0) {
      ret <- fn(vec, ...)
    } else {
      ret <- 0
    }
    return(ret)  
  }
  
  over_list <- over(polys, points, returnList = TRUE)
  
  out_list <- lapply(over_list, in_fun)
  unlist(out_list)
}

n_sites <- function(x) length(x)

n_spp <- function(x) {
  collapsed <- paste(x, collapse = " ")
  x <- strsplit(collapsed, " ")[[1]]
  length(unique(x))
}

#' Find the nearest features and get indices/attributes
#' 
#' Works the same as \code{\link[sp]{over}} but instead of of doing an overlay, 
#' retrieves the indexes or attributes from the spatial object \code{y} that is nearest to x.
#'
#' @param x the features to receive the attributes
#' @param y the features which contain the attributes
#' @param returnList only works if \code{y} is geometries only (not a 
#' \code{Spatial*DataFrame}). If \code{TRUE}, returns a named list, otherwise returns 
#' a named vector.
#'
#' @return If \code{y} has attributes, a \code{data.frame} with row names 
#' corresponding to indices of \code{x}. If \code{y} is only geometries, returns 
#' a named list or vector of the indices of \code{y}, depending on the value of 
#' \code{returnList}. Names are indicies of \code{x}.
#' 
#' @export
nearest <- function(x, y, returnList = FALSE) {
  if (!identicalCRS(x, y)) {
    stop("Source and target are not in the same spatial projection!")
  }
  dist_mat <- gDistance(x, y, byid = TRUE)
  closest_feat <- apply(dist_mat, 2, which.min)
  
  if ("data" %in% slotNames(y)) {
    ret <- y@data[closest_feat, ]
    row.names(ret) <- names(closest_feat)
  } else {
    ret <- closest_feat
    if (returnList) {
      ret <- as.list(ret)
    }
  }
  ret
}

#' simplify
#' 
#' Simplify spatial objects, allowing topologically-aware simplification.
#' 
#' Uses \href{https://github.com/mbloch/mapshaper}{mapshaper} to simplify polygons. 
#' It is a Node library, so you need to have Node installed to use it: 
#' \url{https://nodejs.org/download/}. Then install mapshaper on the command 
#' line with: \code{npm install -g mapshaper}.
#' 
#' @importFrom rgdal readOGR writeOGR ogrListLayers
#' @importFrom sp proj4string proj4string<- 
#'
#' @param sp_obj spatial object to simplify
#' @param keep proportion of points to retain (0-1; default 0.05)
#' @param method simplification method to use: \code{"dp"} for Douglas-Peuker 
#'   algorithm, or \code{"vis"} for Visvalingam algorithm. See this 
#'   \url{https://github.com/mbloch/mapshaper/wiki/Simplification-Tips}{link} 
#'   for more information.
#' @param keep_shapes Prevent polygon features from disappearing at high 
#'   simplification (default \code{TRUE})
#' @param no_repair disable intersection repair after simplification (default 
#'   \code{FALSE}).
#' @param auto_snap Snap together vertices within a small distance threshold to 
#'   fix small coordinate misalignment in adjacent polygons. Default \code{TRUE}.
#'   
#' @return an \code{sp} object
#' @export
simplify <- function(sp_obj, keep = 0.05, method = "vis", keep_shapes = TRUE, 
                     no_repair = FALSE, auto_snap = TRUE) {
  
  if (system("mapshaper --version") == 127L) {
    stop("You do not appear to have mapshaper installed. If you have node.js ", 
         "installed on your system, type 'npm install -g mapshaper' on the ", 
         "command line. If you don't have node installed, install it from ", 
         "http://nodejs.org/, then run the above command.")
  }
  
  if (!is(sp_obj, "Spatial")) stop("sp_obj must be a spatial object")
  if (keep > 1 || keep < 0) stop("keep must be in the range 0-1")
  
  if (method == "vis") {
    method <- "visvalingam"
  } else if (!(method == "dp")) {
    stop("method should be one of 'vis' or 'dp'")
  }
  
  if (keep_shapes) keep_shapes <- "keep-shapes" else keep_shapes <- ""
  
  if (no_repair) no_repair <- "no-repair" else no_repair <- ""
  
  if (auto_snap) auto_snap <- "auto-snap" else auto_snap <- ""
  
  tmp_dir <- tempdir()
  infile <- file.path(tmp_dir, "tempfile.shp")
  writeOGR(sp_obj, dsn = tmp_dir, layer = "tempfile", driver = "ESRI Shapefile", 
           overwrite_layer = TRUE, delete_dsn = TRUE)
  
  outfile <- file.path(tempdir(), "myoutfile.geojson")
  
  call <- sprintf("mapshaper %s %s -simplify %s %s %s %s -o %s force", infile, 
                  auto_snap, keep, method, keep_shapes, no_repair, outfile)
  call <- gsub("\\s+", " ", call)
  system(call)
  
  lyr <- ogrListLayers(outfile)[1]
  ret <- readOGR(outfile, layer = lyr, verbose = FALSE, stringsAsFactors = FALSE)
  
  # Need to reassign the projection as it is lost
  proj4string(ret) <- suppressWarnings(proj4string(sp_obj))
  ret
  
}
