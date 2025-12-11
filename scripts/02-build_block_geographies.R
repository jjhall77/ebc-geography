#02- build_block_geographies.R

#-------------------------------------------------
#Build and export segment → physical_id crosswalk
#-------------------------------------------------

seg_to_physical <- lion_city_streets |>
  st_drop_geometry() |>
  select(
    segment_id,
    physical_id,
    generic_id,
    nypdid,
    fdnyid,
    street,
    saf_street_name,
    from_left,
    to_left,
    from_right,
    to_right,
    l_zip,
    r_zip,
    l_boro,
    r_boro,
    l_cd,
    r_cd,
    lct2020,
    rct2020,
    lcb2020,
    rcb2020,
    l_block_face_id,
    r_block_face_id,
    shape_length
  ) |>
  distinct()

write_csv(seg_to_physical, here("output/seg_to_physical.csv"))

cat("Segments:", nrow(seg_to_physical), "\n")
cat("Physical blocks:", n_distinct(seg_to_physical$physical_id), "\n")
cat("Generic IDs:", n_distinct(seg_to_physical$generic_id), "\n")
cat("NYPD IDs:", n_distinct(seg_to_physical$nypdid), "\n")

#-------------------------------------------------
# 08. Create physical block geometries
#-------------------------------------------------
lion_city_streets_linear <- lion_city_streets |>
  st_cast("MULTILINESTRING") |>
  st_make_valid()

physical_blocks <- lion_city_streets_linear |>
  group_by(physical_id) |>
  summarise(
    geometry = st_union(SHAPE),
    n_segments = n(),
    total_length_ft = sum(shape_length, na.rm = TRUE),
    streets = paste(unique(street), collapse = "; "),
    .groups = "drop"
  )

# Add borough (mode across segments)
block_boro <- lion_city_streets_linear |>
  st_drop_geometry() |>
  count(physical_id, l_boro) |>
  group_by(physical_id) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(physical_id, boro = l_boro)

# Add community district
block_cd <- lion_city_streets_linear |>
  st_drop_geometry() |>
  count(physical_id, l_cd) |>
  group_by(physical_id) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(physical_id, cd = l_cd)

# Add census tract (2020)
block_ct <- lion_city_streets_linear |>
  st_drop_geometry() |>
  count(physical_id, lct2020) |>
  group_by(physical_id) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(physical_id, ct2020 = lct2020)

physical_blocks <- physical_blocks |>
  left_join(block_boro, by = "physical_id") |>
  left_join(block_cd, by = "physical_id") |>
  left_join(block_ct, by = "physical_id")

st_write(physical_blocks, here("output/physical_blocks.gpkg"), delete_dsn = TRUE)

cat("Physical blocks created:", nrow(physical_blocks), "\n")

