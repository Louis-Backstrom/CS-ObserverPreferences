# Expand an st_bbox object to have neat boundaries
expand_bbox <- function(bbox, units = 1) {
  bbox[1] <- floor(bbox[1] / units) * units
  bbox[2] <- floor(bbox[2] / units) * units
  bbox[3] <- ceiling(bbox[3] / units) * units
  bbox[4] <- ceiling(bbox[4] / units) * units
  
  return(bbox)
}

# Create a %notin% operator (opposite of %in% operator)
`%notin%` <- Negate(`%in%`)

# Suppress print output (e.g. from model summary) of a function
# https://stackoverflow.com/a/49945753
quiet_output <- function(x) {
  sink("NUL")
  temp <- x
  sink()
  return(temp)
}