rescale <- function(x, na.rm = FALSE, ...) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  rng <- range(x, ...)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  rng <- range(x, ...)
  (x - rng[1]) / (rng[2] - rng[1])
}

# Uso de la función rescale
data <- c(1, 2, NA, 4, 5)
rescaled_data <- rescale(data, na.rm = TRUE)
print(rescaled_data)
