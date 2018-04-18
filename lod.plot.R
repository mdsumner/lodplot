lod_plot <- function(x, ..., add = FALSE) {
  bb <- sf::st_bbox(x)
  ll <- grepl("longlat", sf::st_crs(x)$proj4string)
   if (!add) {
     plot.new()
     plot.window(bb[c(1, 3)], bb[c(2, 4)], asp = c(1, 1/cos(mean(bb[c(2, 4)]) * pi/180))[ll + 1])
     add <- TRUE
   }
  ## find the device size, an overestimate probably
  ddm <- dev.size("px")
  r <- raster::raster(raster::extent(bb[c(1, 3, 2, 4)]), ncols = ddm[1], nrow = ddm[2])
  x$ID <- seq_len(nrow(x))
  r <- fasterize::fasterize(x, r, field = "ID")
  ## map colours
  col <- list(...)[["col"]]
  if (is.null(col)) {
    col <- sf::sf.colors()
  }

  plot(r, add = add, col = col)
  invisible(NULL)
}

library(sf)
example(st_read)
png(width = 1000, height = 500)
lod_plot(nc)
dev.off()


