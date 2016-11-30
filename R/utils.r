#' Add vertices to a linear path to ensure curvature 
#'
#' This function takes a matrix of coordinates in longlat that define a pathy, and returns a denser matrix with more vertices based on a minimum allowed distance. 
#' 
#' The function assumes each segment is a great circle, on the Vincenty ellipsoid \code{\link[geosphere]{distVincentyEllipsoid}}. 
#' @param x matrix of coordinates, long/lat
#' @param minm default distance in metres on a great cicle
#'
#' @return matrix
#'
#' @importFrom geosphere distVincentyEllipsoid
densifyMat <- function(x, minm = 1852 * 10) {
  ## assume this is the closed polygon
  l <- vector("list", nrow(x) - 1)
  for (i in seq_along(l)) {
    dist <- distVincentyEllipsoid(x[i, ], x[i + 1, ])
    nsegs <- max(c(dist %/% minm, 3))
    l[[i]] <- cbind(seq(x[i,1], x[i+1,1], length = nsegs), seq(x[i,2], x[i+1,2], length = nsegs))
  }
  do.call(rbind, l)
}