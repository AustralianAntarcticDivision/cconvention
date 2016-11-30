# ---- division-b ----




## ASD database 
## here we simply define a matrix for every named ASD that we might want

.getASD <- function(asd) {
  if (length(asd) > 1) stop("only one named ASD can be returned")
  x <- NULL
  if (asd == "58.4.1") {
    # Division 58.4.1
    # The waters bounded by a line starting at 64°S 80°E; thence due east to 64°S 86°E; thence due north to 55°S 86°E; thence due east to 150°E longitude; thence due south to the Antarctic Continent; thence westward along the coast of the Antarctic Continent to 80°E longitude; thence due north to the starting point.
    v  <- 
    c(80, -90, 
      80, -64, 
      86, -64, 
      86, -55, 
      150, -55, 
      150, -90)
    x <- matrix(v, ncol = 2, byrow = TRUE)
  }
  if (asd == "58.4.2") {
    # Division 58.4.2
    # The waters bounded by a line starting at 62°S 30°E; thence due east to 62°S 73°10'E; thence due south to 64°S 73°10'E; thence due east to 64°S 80°E; thence due south to the Antarctic Continent; thence westward along the coast of the Antarctic Continent to 30°E longitude; thence due north to the starting point.
    
    #The waters bounded by a line starting at 62°S 30°E
    # thence due east to 62°S 73°10'E
    # thence due south to 64°S 73°10'E
    # thence due east to 64°S 80°E
    # thence due south to the Antarctic Continent
    # thence westward along the coast of the Antarctic Continent to 30°E longitude
    # thence due north to the starting point.
    
    v <- 
      c(30, -90, 
        30, -62, 
        73 + 10/60, -62, 
        73 + 10/60, -64, 
        80, -64, 
        80, -90)
    x <- matrix(v, ncol = 2, byrow = TRUE)
  }
  
#   if (asd == "88.1") {
#     ##The waters bounded by a line commencing from a point on the coast of Antarctica between Oates Land and George V Land at 150°00'E longitude; thence due north to 60°00'S latitude; thence due east to 170°00'W longitude; thence due south to Dufek coast in Antarctica; thence running in a westerly direction along the coast of Antarctica to the point of departure.
#     v <- 
#       c(150, -60, 
#         180, -60)
#   }
  
  if (is.null(x)) stop(sprintf("%s not found", asd))
 
   return(x)
}




#' Define polygon object of CCAMLR divisions. 
#'
#' Currently only builds objects for 58.4.1, ensuring they are topologically clean and with sufficient density for fishing impact analysis. 
#' @param name for future use, currently ignored
#' @param intsct optional poylgon object to use for cropping via intersection
#'
#' @return SpatialPolygonsDataFrame
#' @export
#' @examples \dontrun{
#' #cm <- readOGR(file.path(dp, "data", "add_ver6"), "cst01_polygon")
#' #cmu <- gUnionCascaded(cm)
#' library(aceecostats)
#' subarea <- division(intsct = aes_region)
#' }
#' @importFrom sp CRS Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame spTransform
#' @importFrom raster cover projection
division <- function(name = "58.4.1", intsct = NULL) {
  llproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  listofasd <- vector("list", length(name))
  for (i in seq_along(name)) {
    listofasd[[i]] <- .getASD(name[i])
  }
  clp <- function(x) rbind(x, x[1,])
  
  listofasd1 <- lapply(listofasd, function(x) list(Polygon(densifyMat(clp(x)))))
  dv <- SpatialPolygonsDataFrame(SpatialPolygons(lapply(seq_along(listofasd1), function(xi) Polygons(listofasd1[[xi]], as.character(xi))), 
                                           proj4string = CRS(llproj)), data.frame(name = name, stringsAsFactors = FALSE))         

  
  ##cm <- raadtools::subset(coastmap("cst01_polygon"), cst01srf == "land")
  
  if (!is.null(intsct)) {
    cv <- spTransform(subset(cover(spTransform(dv, projection(intsct)), intsct), !is.na(name)), llproj)
  } else {
    cv <- dv
  }
 cv
}

