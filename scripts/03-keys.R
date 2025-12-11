
#-------------------------------------------------
# 03-keys.R
# Create block names, ID crosswalks, and master reference tables
# Includes manual override system for block names
#-------------------------------------------------

library(dplyr)
library(sf)
library(stringr)
library(readr)
library(here)

#-------------------------------------------------
# PART 1: Human-readable block names (with manual override support)
#-------------------------------------------------

# === STEP A: Calculate address info per segment ===

seg_addr_info <- lion_city_streets_linear |>
  st_drop_geometry() |>
  mutate(
    fl = as.numeric(from_left),
    tl = as.numeric(to_left),
    fr = as.numeric(from_right),
    tr = as.numeric(to_right),
    addr_min = pmin(fl, tl, fr, tr, na.rm = TRUE),
    addr_max = pmax(fl, tl, fr, tr, na.rm = TRUE)
  ) |>
  select(physical_id, segment_id, street, addr_min, addr_max, shape_length)

# === STEP B: Aggregate to physical_id + street level ===

block_street_ranges <- seg_addr_info |>
  filter(!is.na(addr_min), !is.na(addr_max), is.finite(addr_min), is.finite(addr_max)) |>
  group_by(physical_id, street) |>
  summarise(
    addr_low = min(addr_min, na.rm = TRUE),
    addr_high = max(addr_max, na.rm = TRUE),
    addr_span = addr_high - addr_low,
    street_length = sum(shape_length, na.rm = TRUE),
    n_segments = n(),
    .groups = "drop"
  )

# === STEP C: Generate algorithmic names ===

block_names_auto <- block_street_ranges |>
  group_by(physical_id) |>
  slice_max(order_by = tibble(street_length, addr_span), n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    block_name = paste0(addr_low, "-", addr_high, " ", street),
    name_source = "auto"
  ) |>
  select(physical_id, block_name, primary_street = street, addr_low, addr_high, name_source)

# Handle blocks with no valid addresses
blocks_no_addr <- physical_blocks |>
  st_drop_geometry() |>
  filter(!physical_id %in% block_names_auto$physical_id) |>
  select(physical_id, streets) |>
  mutate(
    primary_street = str_extract(streets, "^[^;]+"),
    block_name = paste0("[No addr] ", str_trunc(primary_street, 40)),
    addr_low = NA_real_,
    addr_high = NA_real_,
    name_source = "auto_no_addr"
  ) |>
  select(physical_id, block_name, primary_street, addr_low, addr_high, name_source)

block_names_auto <- bind_rows(block_names_auto, blocks_no_addr)

# === STEP D: Create/load manual overrides file ===

manual_overrides_path <- here("data/block_names_manual.csv")

if (!file.exists(manual_overrides_path)) {
  
  manual_template <- tibble(
    physical_id = character(),
    block_name_manual = character(),
    primary_street_manual = character(),
    addr_low_manual = numeric(),
    addr_high_manual = numeric(),
    notes = character(),
    surveyed_date = character(),
    surveyed_by = character()
  )
  
  # Ensure data directory exists
  
  if (!dir.exists(here("data"))) dir.create(here("data"))
  
  write_csv(manual_template, manual_overrides_path)
  cat("Created manual overrides template at:", manual_overrides_path, "\n")
  cat("Add rows to this file after field surveys.\n\n")
}

manual_overrides <- read_csv(manual_overrides_path, col_types = cols(
  physical_id = col_integer(),
  block_name_manual = col_character(),
  primary_street_manual = col_character(),
  addr_low_manual = col_double(),
  addr_high_manual = col_double(),
  notes = col_character(),
  surveyed_date = col_character(),
  surveyed_by = col_character()
))

cat("Manual overrides loaded:", nrow(manual_overrides), "entries\n")

# === STEP E: Merge auto names with manual overrides ===

block_names <- block_names_auto |>
  left_join(
    manual_overrides |> select(physical_id, block_name_manual, primary_street_manual, 
                               addr_low_manual, addr_high_manual, notes),
    by = "physical_id"
  ) |>
  mutate(
    block_name = coalesce(block_name_manual, block_name),
    primary_street = coalesce(primary_street_manual, primary_street),
    addr_low = coalesce(addr_low_manual, addr_low),
    addr_high = coalesce(addr_high_manual, addr_high),
    name_source = if_else(!is.na(block_name_manual), "manual", name_source)
  ) |>
  select(physical_id, block_name, primary_street, addr_low, addr_high, name_source, notes)

write_csv(block_names, here("output/block_names.csv"))

# QA summary
cat("\n--- Block Names Summary ---\n")
cat("Blocks with addresses:", sum(!is.na(block_names$addr_low)), "\n")
cat("Blocks without addresses:", sum(is.na(block_names$addr_low)), "\n\n")

block_names |>
  count(name_source, name = "n_blocks") |>
  print()

cat("\nSample block names:\n")
block_names |> 
  filter(!is.na(addr_low)) |>
  sample_n(min(15, sum(!is.na(block_names$addr_low)))) |> 
  select(physical_id, block_name, name_source) |>
  print()


#-------------------------------------------------
# PART 2: Flag blocks needing manual review
#-------------------------------------------------

blocks_to_review <- block_names |>
  filter(
    name_source == "auto_no_addr" |
      str_detect(block_name, "^\\[No addr\\]") |
      (addr_high - addr_low > 500 & !is.na(addr_low)) |
      (addr_high - addr_low == 0 & !is.na(addr_low)) |
      is.na(addr_low)
  ) |>
  left_join(
    physical_blocks |> st_drop_geometry() |> select(physical_id, boro, cd, streets),
    by = "physical_id"
  ) |>
  mutate(
    review_reason = case_when(
      is.na(addr_low) ~ "no_addresses",
      addr_high - addr_low > 500 ~ "large_address_span",
      addr_high - addr_low == 0 ~ "single_address",
      TRUE ~ "other"
    )
  )

write_csv(blocks_to_review, here("output/blocks_needing_review.csv"))

cat("\n--- Blocks Flagged for Manual Review ---\n")
cat("Total:", nrow(blocks_to_review), "\n\n")

blocks_to_review |>
  count(review_reason, name = "n_blocks") |>
  print()


#-------------------------------------------------
# PART 3: ID crosswalk (segment-level)
#-------------------------------------------------

id_crosswalk <- lion_city_streets_linear |>
  st_drop_geometry() |>
  select(
    segment_id,
    physical_id,
    generic_id,
    fdnyid,
    l_block_face_id,
    r_block_face_id,
    lct2020,
    rct2020,
    lcb2020,
    rcb2020
  ) |>
  distinct()

write_csv(id_crosswalk, here("output/id_crosswalk.csv"))

cat("\n--- ID Crosswalk ---\n")
cat("Segments:", nrow(id_crosswalk), "\n")
cat("Physical blocks:", n_distinct(id_crosswalk$physical_id), "\n")
cat("Generic IDs:", n_distinct(id_crosswalk$generic_id), "\n")


#-------------------------------------------------
# PART 4: Block-level ID summary
#-------------------------------------------------

block_id_summary <- id_crosswalk |>
  group_by(physical_id) |>
  summarise(
    segment_ids = paste(unique(segment_id), collapse = ";"),
    n_segments = n_distinct(segment_id),
    generic_ids = paste(unique(generic_id), collapse = ";"),
    fdny_ids = paste(unique(fdnyid), collapse = ";"),
    block_face_ids = paste(unique(c(l_block_face_id, r_block_face_id)), collapse = ";"),
    census_tracts_2020 = paste(unique(c(lct2020, rct2020)), collapse = ";"),
    census_blocks_2020 = paste(unique(c(lcb2020, rcb2020)), collapse = ";"),
    .groups = "drop"
  )

write_csv(block_id_summary, here("output/block_id_summary.csv"))


#-------------------------------------------------
# PART 5: Master block reference table
#-------------------------------------------------

master_blocks <- physical_blocks |>
  st_drop_geometry() |>
  select(physical_id, n_segments, total_length_ft, boro, cd, ct2020) |>
  left_join(
    block_names |> select(physical_id, block_name, primary_street, addr_low, addr_high, name_source), 
    by = "physical_id"
  ) |>
  left_join(
    block_id_summary |> select(physical_id, census_tracts_2020, census_blocks_2020),
    by = "physical_id"
  )

write_csv(master_blocks, here("output/master_block_list.csv"))

# Summary stats
cat("\n--- Master Block List Summary ---\n")
master_blocks |>
  summarise(
    total_blocks = n(),
    total_segments = sum(n_segments),
    total_length_miles = sum(total_length_ft, na.rm = TRUE) / 5280,
    blocks_with_addresses = sum(!is.na(addr_low)),
    blocks_no_addresses = sum(is.na(addr_low)),
    n_boroughs = n_distinct(boro)
  ) |>
  print()

cat("\nBorough breakdown:\n")
master_blocks |>
  count(boro, name = "n_blocks") |>
  arrange(desc(n_blocks)) |>
  print()

cat("\nName source breakdown:\n")
master_blocks |>
  count(name_source, name = "n_blocks") |>
  print()


#-------------------------------------------------
# PART 6: Output file inventory
#-------------------------------------------------

cat("\n--- Output Files Created ---\n")
cat("output/block_names.csv          - Human-readable block names\n")
cat("output/blocks_needing_review.csv - Flagged for manual review\n
")
cat("output/id_crosswalk.csv         - Segment-level ID mappings\n")
cat("output/block_id_summary.csv     - Block-level ID summary\n")
cat("output/master_block_list.csv    - Master reference table\n")
cat("\nManual overrides file:\n")
cat("data/block_names_manual.csv     - Add manual entries here\n")
