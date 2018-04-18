pixsize <- function(x) {
  if (missing(x)) {
    bb <- par("usr")
  } else {
    bb <- sf::st_bbox(x)[c(1, 3, 2, 4)]
  }
  max(apply(matrix(bb, 2), 2, diff) /  dev.size("px") ) 
}


library(sf)
f <- raadfiles::thelist_files(pattern = "parcels_hobart")$fullname[1]
x <- read_sf(f)[1]


dev_filter <- function(x, detail = 1, ..., add = FALSE) {
 if (add) {
   px <- pixsize()
 } else {
   px <- pixsize(x)
 }
  x[as.numeric(sf::st_area(sf::st_set_crs(x, NA))) > (px * px) * detail, ]  
}

