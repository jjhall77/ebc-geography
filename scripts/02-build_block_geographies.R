#02-build_block_geographies.R
# Modified to preserve node IDs for intersection handling

#-------------------------------------------------
# Build and export segment → physical_id crosswalk
# NOW INCLUDES NODE IDs
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
    shape_length,
    # NEW: Node IDs for intersection handling
    node_id_from,
    node_id_to
  ) |>
  distinct()

write_csv(seg_to_physical, here("output/seg_to_physical.csv"))

cat("Segments:", nrow(seg_to_physical), "\n")
cat("Physical blocks:", n_distinct(seg_to_physical$physical_id), "\n")
cat("Generic IDs:", n_distinct(seg_to_physical$generic_id), "\n")
cat("NYPD IDs:", n_distinct(seg_to_physical$nypdid), "\n")

#-------------------------------------------------
# NEW: Build physical_id <-> node_id crosswalk
# This is the key for fast intersection handling
#-------------------------------------------------

physical_to_node <- lion_city_streets |>
  st_drop_geometry() |>
  select(physical_id, node_id_from, node_id_to) |>
  pivot_longer(
    cols = c(node_id_from, node_id_to),
    names_to = "node_position",
    values_to = "node_id"
  ) |>
  filter(!is.na(node_id), node_id != "") |>
  distinct(physical_id, node_id)

write_csv(physical_to_node, here("output/physical_to_node.csv"))

cat("Physical-to-node crosswalk rows:", nrow(physical_to_node), "\n")
cat("Unique nodes:", n_distinct(physical_to_node$node_id), "\n")

#-------------------------------------------------
# NEW: Identify endpoint nodes for each physical block
# Endpoints appear once in node list; internal nodes appear twice
#-------------------------------------------------

node_appearances <- lion_city_streets |>
  st_drop_geometry() |>
  select(physical_id, node_id_from, node_id_to) |>
  pivot_longer(
    cols = c(node_id_from, node_id_to),
    values_to = "node_id"
  ) |>
  filter(!is.na(node_id), node_id != "") |>
  count(physical_id, node_id, name = "n_appearances")

# Endpoint nodes: appear exactly once per physical block
endpoint_nodes <- node_appearances |>
  filter(n_appearances == 1)

# Summarize endpoints per physical block
block_endpoints <- endpoint_nodes |>
  group_by(physical_id) |>
  summarise(
    endpoint_node_1 = first(node_id),
    endpoint_node_2 = if(n() > 1) nth(node_id, 2) else NA_character_,
    n_endpoints = n(),
    .groups = "drop"
  )

cat("Blocks with 2 endpoints:", sum(block_endpoints$n_endpoints == 2), "\n")
cat("Blocks with 1 endpoint (dead ends):", sum(block_endpoints$n_endpoints == 1), "\n")
cat("Blocks with >2 endpoints (complex):", sum(block_endpoints$n_endpoints > 2), "\n")

#-------------------------------------------------
# Create physical block geometries
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

# Join all attributes including NEW endpoint nodes
physical_blocks <- physical_blocks |>
  left_join(block_boro, by = "physical_id") |>
  left_join(block_cd, by = "physical_id") |>
  left_join(block_ct, by = "physical_id") |>
  left_join(block_endpoints, by = "physical_id")

st_write(physical_blocks, here("output/physical_blocks.gpkg"), delete_dsn = TRUE)

cat("Physical blocks created:", nrow(physical_blocks), "\n")

#-------------------------------------------------
# NEW: Build intersection lookup table
# Maps intersection nodes to adjacent physical blocks
#-------------------------------------------------

# Load nodes from LION
lion_gdb <- here("data", "lion", "lion.gdb")

nodes <- st_read(lion_gdb, layer = "node", quiet = TRUE) |>
  st_transform(2263) |>
  clean_names() |>
  mutate(nodeid = as.character(nodeid))

# Count street names per node to identify intersections
node_street_counts <- lion_city_streets |>
  st_drop_geometry() |>
  select(node_id_from, node_id_to, street) |>
  pivot_longer(cols = c(node_id_from, node_id_to), values_to = "node_id") |>
  filter(!is.na(node_id), node_id != "") |>
  distinct(node_id, street) |>
  count(node_id, name = "n_streets")

# Intersections = nodes with 2+ street names meeting
intersection_nodes <- node_street_counts |>
  filter(n_streets >= 2)

cat("Intersection nodes:", nrow(intersection_nodes), "\n")

# Build intersection -> adjacent blocks lookup
# This is the key table for splitting intersection crimes
intersection_to_blocks <- physical_to_node |>
  inner_join(intersection_nodes, by = "node_id") |>
  group_by(node_id, n_streets) |>
  summarise(
    adjacent_blocks = list(physical_id),
    n_adjacent_blocks = n_distinct(physical_id),
    .groups = "drop"
  )

# Save as RDS (list columns don't work in CSV)
saveRDS(intersection_to_blocks, here("output/intersection_to_blocks.rds"))

# Also save a flat version for easier inspection
intersection_to_blocks_flat <- physical_to_node |>
  inner_join(intersection_nodes, by = "node_id")

write_csv(intersection_to_blocks_flat, here("output/intersection_to_blocks_flat.csv"))

cat("Intersections with block lookup:", nrow(intersection_to_blocks), "\n")

# Summary of intersection complexity
intersection_to_blocks |>
  count(n_adjacent_blocks, name = "n_intersections") |>
  arrange(n_adjacent_blocks) |>
  print()

#-------------------------------------------------
# NEW: Add intersection node geometries
# Useful for spatial joins when snapping crimes
#-------------------------------------------------

intersection_nodes_sf <- nodes |>
  filter(nodeid %in% intersection_nodes$node_id) |>
  left_join(intersection_nodes, by = c("nodeid" = "node_id")) |>
  left_join(
    intersection_to_blocks |> select(node_id, n_adjacent_blocks),
    by = c("nodeid" = "node_id")
  )

st_write(intersection_nodes_sf, here("output/intersection_nodes.gpkg"), delete_dsn = TRUE)

cat("Intersection nodes with geometry:", nrow(intersection_nodes_sf), "\n")

#-------------------------------------------------
# Summary
#-------------------------------------------------

cat("\n=== OUTPUT FILES ===\n")
cat("1. physical_blocks.gpkg - Street segments with endpoint nodes\n")
cat("2. seg_to_physical.csv - Segment to physical block crosswalk (with nodes)\n")
cat("3. physical_to_node.csv - Physical block to node crosswalk\n")
cat("4. intersection_to_blocks.rds - Intersection -> adjacent blocks lookup\n")
cat("5. intersection_to_blocks_flat.csv - Flat version for inspection\n")
cat("6. intersection_nodes.gpkg - Intersection point geometries\n")
