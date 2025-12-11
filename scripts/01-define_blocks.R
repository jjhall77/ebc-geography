
#01

#-------------------------------------------------
# 00. Libraries
#-------------------------------------------------

library(dplyr)
library(sf)
library(stringr)
library(leaflet)
library(htmltools)

#-------------------------------------------------
# 01. LION data: define blocks and filter to city streets
#-------------------------------------------------

# Quick exam of LION structure
glimpse(lion)

# Segments per physical block
lion |>
  st_drop_geometry() |>
  count(physical_id) |>
  summary()

# Real streets only
lion_streets <- lion |>
  filter(
    feature_typ %in% c("0","6"),  # mapped streets
    status == "2",                # active
    segment_typ == "U",           # undivided standard geometry
    traf_dir %in% c("T","A","W")  # vehicular
  )

cat("Unique physical_id (lion_streets):",
    lion_streets |> pull(physical_id) |> n_distinct(), "\n")

# Remove highways, bridges, tunnels, ramps
lion_city_streets <- lion_streets |>
  filter(!rw_type %in% c("2","3","4","9"))

cat("Unique physical_id (lion_city_streets):",
    lion_city_streets |> pull(physical_id) |> n_distinct(), "\n")

#-------------------------------------------------
# 02. Build ZIP adjacency from left/right ZIPs
#-------------------------------------------------

zip_pairs <- lion_city_streets |>
  st_drop_geometry() |>
  transmute(
    l_zip = str_trim(l_zip),
    r_zip = str_trim(r_zip)
  ) |>
  filter(
    (!is.na(l_zip) & l_zip != "") |
      (!is.na(r_zip) & r_zip != "")
  ) |>
  distinct()

zip_edges_sym <- zip_pairs |>
  filter(
    !is.na(l_zip), !is.na(r_zip),
    l_zip != "", r_zip != "",
    l_zip != r_zip
  ) |>
  distinct(l_zip, r_zip) |>
  (\(df) bind_rows(
    df |> rename(zip1 = l_zip, zip2 = r_zip),
    df |> rename(zip1 = r_zip, zip2 = l_zip)
  ))() |>
  distinct(zip1, zip2)

zip_neighbors <- zip_edges_sym |>
  group_by(zip1) |>
  summarise(
    neighbors = list(unique(zip2)),
    degree    = length(unique(zip2)),
    .groups   = "drop"
  )

#-------------------------------------------------
# 03. Helper: pick a cluster of contiguous ZIP codes
#-------------------------------------------------

pick_zip_cluster <- function(n_cluster = 4) {
  
  candidates <- zip_neighbors |>
    filter(degree >= (n_cluster - 1))
  
  if (nrow(candidates) == 0)
    stop("No ZIP has enough neighbors to form a cluster.")
  
  seed     <- candidates |> slice_sample(n = 1)
  seed_zip <- seed$zip1
  neighs   <- unlist(seed$neighbors)
  
  c(seed_zip, sample(neighs, n_cluster - 1)) |> sort()
}

sample_zips <- pick_zip_cluster(4)
cat("Sample ZIPs:", paste(sample_zips, collapse = ", "), "\n")

#-------------------------------------------------
# 04. Leaflet: inspect filtered ZIP cluster
#-------------------------------------------------

streets_subset <- lion_city_streets |>
  filter(
    str_trim(l_zip) %in% sample_zips |
      str_trim(r_zip) %in% sample_zips
  )

streets_subset_linear <- streets_subset |>
  st_cast("MULTILINESTRING") |>
  st_make_valid() |>
  st_transform(4326)

streets_subset_linear$label_text <- sprintf(
  "Street: %s<br>Physical ID: %s<br>Segment ID: %s<br>Generic ID: %s",
  streets_subset_linear$street,
  streets_subset_linear$physical_id,
  streets_subset_linear$segment_id,
  streets_subset_linear$generic_id
)

leaflet() |>
  addTiles() |>
  addPolylines(
    data   = streets_subset_linear,
    weight = 2,
    opacity = 0.7,
    label  = ~ lapply(label_text, HTML),
    highlightOptions = highlightOptions(
      weight = 4,
      opacity = 1,
      bringToFront = TRUE
    )
  ) |>
  addLegend(
    "bottomright",
    colors = "#3388ff",
    labels = paste("Streets in ZIPs:", paste(sample_zips, collapse = ", "))
  )

#-------------------------------------------------
# 05. Summary counts for lion_city_streets
#-------------------------------------------------

lion_city_streets |>
  st_drop_geometry() |>
  summarise(
    n_segment_id  = n_distinct(segment_id),
    n_physical_id = n_distinct(physical_id),
    n_generic_id  = n_distinct(generic_id)
  )

#-------------------------------------------------
# 06. Leaflet QA: generic_id mapping to >1 physical_id
#-------------------------------------------------

generic_multi_phys <- lion_city_streets |>
  st_drop_geometry() |>
  filter(!is.na(generic_id)) |>
  group_by(generic_id) |>
  summarise(
    n_physical = n_distinct(physical_id),
    .groups = "drop"
  ) |>
  filter(n_physical > 1)

multi_segments <- lion_city_streets |>
  filter(generic_id %in% generic_multi_phys$generic_id)

multi_segments_4326 <- multi_segments |>
  st_cast("MULTILINESTRING") |>
  st_make_valid() |>
  st_transform(4326)

multi_segments_4326$label_text <- sprintf(
  "Street: %s<br>Generic ID: %s<br>Physical ID: %s<br>Segment ID: %s",
  multi_segments_4326$street,
  multi_segments_4326$generic_id,
  multi_segments_4326$physical_id,
  multi_segments_4326$segment_id
)

leaflet() |>
  addTiles() |>
  addPolylines(
    data   = multi_segments_4326,
    weight = 2,
    opacity = 0.7,
    label  = ~ lapply(label_text, HTML),
    highlightOptions = highlightOptions(
      weight = 4,
      opacity = 1,
      bringToFront = TRUE
    )
  ) |>
  addLegend(
    "bottomright",
    colors = "#3388ff",
    labels = "Segments where generic_id maps to >1 physical_id"
  )




#-------------------------------------------------
# Streets with NYPDID in the sampled ZIP cluster
#Have no idea what this is
#-------------------------------------------------

nypd_streets <- lion_city_streets |>
  filter(
    !is.na(nypdid),
    str_trim(nypdid) != ""
  )

# Handle curved geometries + project to WGS84 for leaflet
nypd_streets_4326 <- nypd_streets |>
  st_cast("MULTILINESTRING") |>
  st_make_valid() |>
  st_transform(4326)

# Mouseover label: NYPDID + IDs + street name
nypd_streets_4326$label_text <- sprintf(
  "Street: %s<br>NYPDID: %s<br>Physical ID: %s<br>Segment ID: %s<br>Generic ID: %s",
  nypd_streets_4326$street,
  nypd_streets_4326$nypdid,
  nypd_streets_4326$physical_id,
  nypd_streets_4326$segment_id,
  nypd_streets_4326$generic_id
)

# Leaflet map
leaflet() |>
  addTiles() |>
  addPolylines(
    data   = nypd_streets_4326,
    weight = 2,
    opacity = 0.8,
    label  = ~ lapply(label_text, HTML),
    highlightOptions = highlightOptions(
      weight = 4,
      opacity = 1,
      bringToFront = TRUE
    )
  ) |>
  addLegend(
    "bottomright",
    colors = "#3388ff",
    labels = paste("Streets with NYPDID in ZIPs:", paste(sample_zips, collapse = ", "))
  )


