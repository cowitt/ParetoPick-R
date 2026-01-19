## ----eval = TRUE, include = FALSE---------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>',
  warning = FALSE,
  message = FALSE
)


## ----precision_table,  message=FALSE, warning=FALSE,  echo=FALSE--------------
data.frame(
  `Geohash Length` = 1:8,
  `KM error`= c(2500.0, 630.0, 78.0, 20.0, 2.4, 0.61, 0.076, 0.019)
)

## ----tayrona------------------------------------------------------------------
library(geohashTools)
gh_encode(11.3113917, -74.0779006)

## ----tayrona_zoom_out---------------------------------------------------------
gh_encode(11.3113917, -74.0779006, precision = 5L)

## -----------------------------------------------------------------------------
coords = data.frame(
  x=rnorm(20L),
  y=rnorm(20L)
)
gh <- gh_encode(coords$x, coords$y)
gh

## ----yirgacheffe--------------------------------------------------------------
gh_decode('sc54v')

## ----yirgacheffe_delta--------------------------------------------------------
gh_decode('sc54v', include_delta = TRUE)

## ----gh_delta-----------------------------------------------------------------
gh_delta(5L)

## ----decode_vector------------------------------------------------------------
gh_decode(gh)

## ----neighbors----------------------------------------------------------------
gh_neighbors('w21z74nz')

## ----merlion_nbhd, fig.width = 3, fig.height = 3, out.width = '\\textwidth'----
library(sf)
merlion_ghs <- gh_neighbors('w21z74')
merlion_nbhd <- gh_to_sf(merlion_ghs)

# Example plot of geohashes neighbouring w21z74
plot(merlion_nbhd, col = NA, reset = FALSE, key.pos = NULL)
text(
  st_coordinates(st_centroid(merlion_nbhd)),
  labels = row.names(merlion_nbhd)
)

## ----meuse, fig.width = 4, fig.height = 4, out.width = '\\textwidth'----------
if (!requireNamespace('ggplot2', quietly = TRUE)) {
  install.packages('ggplot2')
}

library(ggplot2)

data(meuse, package = 'sp')
meuse_sf = st_as_sf(meuse, coords = c('x', 'y'), crs = 28992L, agr = 'constant')
meuse_sf <- st_transform(meuse_sf, crs = 4326L)

ggplot() +
  geom_sf(data = meuse_sf, aes(colour = cadmium)) +
  theme_void()

## ----meuse_covering-----------------------------------------------------------
meuse_gh <- gh_covering(meuse_sf)
meuse_gh

## ----meuse_covering_plot, fig.width = 4, fig.height = 4, out.width = '\\textwidth'----
ggplot() +
  geom_sf(data = meuse_sf) +
  geom_sf(data = meuse_gh, fill  = NA) +
  geom_sf_text(data = meuse_gh, aes(label = rownames(meuse_gh))) +
  theme_void()

## ----meuse_minimal, fig.width = 4, fig.height = 4, out.width = '\\textwidth'----
meuse_gh <- gh_covering(meuse_sf, minimal = TRUE)

ggplot() +
  geom_sf(data = meuse_sf) +
  geom_sf(data = meuse_gh, fill  = NA) +
  geom_sf_text(data = meuse_gh, aes(label = rownames(meuse_gh))) +
  theme_void()

