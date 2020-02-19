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
    
  }
  
  
  ## 48 copied from http://www.fao.org/fishery/area/Area48/en  2020-02-19 MSumner
  if (asd == "48.1") {
    #Peninsular (Subarea 48.1)
    # The waters bounded by a line commencing from a point at 70°00'W longitude on the coast of Antarctica at Palmer Land; 
    # thence running across the George VI Sound to a point at 70°00'W longitude on the south coast of Alexander Island; 
    # thence along the east coast of this island to a point on the northeast coast at 70°00'W longitude; 
    # thence due north to 60°00'S latitude; 
    # thence due east to 50°00'W longitude; 
    # thence due south to 65°00'S latitude; 
    # thence due west to a point on the east coast of the Antarctic Peninsula at 65°00'S latitude; 
    # thence running in a northeasterly and then southwesterly direction along the coast of the Antarctic Peninsula to the point of departure.
    v <- c(-70, -90, 
      -70, -60, # ignore alexander island
      -50, -60, 
      -50, -65, 
      -62, -65,   ## dummy points for peninsula
      -66, -67,   ## dummy points for peninsula
      -67, -74,    ## dummy points for peninsula
      -96, -78,   ## dummy points for peninsula
      -66, -90
      )

  } 
  if (asd == "48.2") {
    # South Orkney (Subarea 48.2)
    # The waters bounded by a line running from a point at 64°00'S latitude and 50°00'W longitude; 
    # thence due north to a point at 57°00'S latitude and 50°00'W longitude; 
    # thence due east to 30°00'W longitude; 
    # thence due south to 64°00'S latitude; thence due west to the point of departure.
   v <- c(-50, -64, 
          -50, -57, 
          -30, -57, 
          -30, -64) 

  }
  
  if (asd == "48.3") {
    # South Georgia (Subarea 48.3)
    
    # The waters bounded by a line running from a point at 57°00'S latitude and 50°00'W longitude; 
    # thence due north along meridian 50°00'W to parallel 50°00'S; 
    # thence due east to 30°00'W longitude; 
    # thence due south to 57°00'S latitude; 
    # thence due west to the point of departure.
    
    v <- 
    c(-50, -57, 
      -50, -50, 
      -30, -50, 
      -30, -57)
  }
  
  if (asd == "48.4") {
    # South Sandwich (Subarea 48.4)
    
    # The waters bounded by a line running from a point at 64°00'S latitude and 30°00'W longitude; 
    # thence due north along meridian 30°00'W to parallel 50°00'S; 
    # thence due east to 20°00'W longitude; 
    # thence due south to 64°00'S latitude; 
    # thence due west to the point of departure.
    v <- c(
      -30, -64,
      -30, -50, 
      -20, -50, 
      -20, -64
    )
  }
  
  if (asd == "48.5") {
    # Weddel Sea (Subarea 48.5)
    
    # The waters bounded by a line running from a point at 65°00'S latitude on the east coast of the Antarctic Peninsula; 
    # thence due east to 50°00'W longitude; 
    # thence due north to 64°00'S latitude; 
    # thence running due east along this parallel to 20°00'W longitude; 
    # thence due south to the coast of Antarctica near Coats Land; 
    # thence running in a southwesterly and then northerly direction along the coast of Antarctica and the Antarctic Peninsula to the point of departure.
    v <- c(
      -66, -67,   ## dummy points for peninsula
      -62, -65,   ## dummy points for peninsula
      -50, -65, 
      -50, -64, 
      -20, -64,
      -20, -90, 
      -96, -78,   ## dummy points for peninsula
      -67, -74    ## dummy points for peninsula
      
    )
    
  }
  
  if (asd == "48.6") {
    # Bouvet (Subarea 48.6)
    
    # The waters bounded by a line running from a point at 20°00'W longitude on the coast of Antarctica near Coats Land; 
    # thence running due north along meridian 20°00'W to 50°00'S latitude; 
    # thence due east to 30°00'E longitude; 
    # thence due south to Princess Ragnhild coast in Antarctica; 
    # thence in a westerly direction along the coast of Antarctica to the point of departure.
    v <- c(
      -20, -90, 
      -20, -50, 
      -30, -50, 
      -30, -90
    )
  }
#   if (asd == "88.1") {
#     ##The waters bounded by a line commencing from a point on the coast of Antarctica between Oates Land and George V Land at 150°00'E longitude; thence due north to 60°00'S latitude; thence due east to 170°00'W longitude; thence due south to Dufek coast in Antarctica; thence running in a westerly direction along the coast of Antarctica to the point of departure.
#     v <- 
#       c(150, -60, 
#         180, -60)
#   }
  
  if (is.null(v)) stop(sprintf("%s not found", asd))
  x <- matrix(v, ncol = 2, byrow = TRUE)
  x
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

