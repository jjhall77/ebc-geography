#-------------------------------------------------
# 04-pluto-pad-crosswalks.R
# Build BBL/BIN/Address → physical_id crosswalks
# Uses multi-wave matching: PAD → APPBBL → Billing → Buffer → Nearest
#-------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(sf)
  library(purrr)
  library(readr)
  library(here)
})

options(dplyr.summarise.inform = FALSE)

#-------------------------------------------------
# PART 0: Helper functions
#-------------------------------------------------

make_bbl_chr <- function(boro, block, lot) {
  b  <- str_replace_all(as.character(boro),  "\\D", "")
  bl <- str_replace_all(as.character(block), "\\D", "")
  lt <- str_replace_all(as.character(lot),   "\\D", "")
  ifelse(
    is.na(b) | is.na(bl) | is.na(lt) | b == "" | bl == "" | lt == "",
    NA_character_,
    paste0(b, str_pad(bl, 5, pad = "0"), str_pad(lt, 4, pad = "0"))
  )
}

make_bbl_chr_from_num <- function(x) {
  ifelse(is.na(x), NA_character_, {
    s <- format(x, scientific = FALSE, trim = TRUE)
    s <- gsub("\\D", "", s)
    s <- str_pad(s, 10, pad = "0")
    paste0(substr(s,1,1), substr(s,2,6), substr(s,7,10))
  })
}

as_segid_chr <- function(x) {
  ifelse(is.na(x), NA_character_, str_pad(as.character(x), 7, pad = "0"))
}


#-------------------------------------------------
# PART 1: Load and prepare data
#-------------------------------------------------

cat("Loading data...\n")


# Verify CRS matches physical_blocks
if (st_crs(pluto) != st_crs(physical_blocks)) {
  pluto <- st_transform(pluto, st_crs(physical_blocks))
}

# Build PLUTO keys
pluto_key <- pluto |>
  mutate(
    bbl_chr = make_bbl_chr(boro_code, block, lot),
    appbbl_chr = make_bbl_chr_from_num(appbbl)
  )

all_bbls <- pluto_key |> st_drop_geometry() |> distinct(bbl_chr)
cat("Total PLUTO BBLs:", nrow(all_bbls), "\n\n")

# Build PLUTO keys — ADD bbl_chr TO pluto_sf itself
pluto <- pluto |>
  mutate(
    bbl_chr = make_bbl_chr(boro_code, block, lot),
    appbbl_chr = make_bbl_chr_from_num(appbbl)
  )


#-------------------------------------------------
# PART 2: Prepare LION segment → physical_id lookup
#-------------------------------------------------

# Get segment → physical mapping from your filtered LION
seg_to_physical_lookup <- lion_city_streets_linear |>
  st_drop_geometry() |>
  select(segment_id, physical_id, legacy_id) |>
  distinct()

# LION lines for spatial operations (de-curved)
lion_lines <- lion_city_streets_linear |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_cast("MULTILINESTRING") |>
  st_make_valid() |>
  select(segment_id, physical_id, legacy_id)

# Build lookup: ANY 7-digit segid → canonical segment_id → physical_id
lion_lookup <- bind_rows(
  seg_to_physical_lookup |> transmute(segid_chr = as_segid_chr(segment_id), segment_id, physical_id),
  seg_to_physical_lookup |> transmute(segid_chr = as_segid_chr(legacy_id), segment_id, physical_id)
) |>
  filter(!is.na(segid_chr), !is.na(segment_id)) |>
  distinct(segid_chr, segment_id, physical_id)

cat("LION lookup built:", nrow(lion_lookup), "segid mappings\n\n")

#-------------------------------------------------
# PART 3: Wave 1 - PAD direct match (bobaadr)
#-------------------------------------------------

cat("=== Wave 1: PAD (bobaadr) direct match ===\n")

wave1_key <- bobaadr |>
  transmute(
    bbl_chr = make_bbl_chr(boro, block, lot),
    segid_chr = as_segid_chr(segid)
  ) |>
  filter(!is.na(bbl_chr), !is.na(segid_chr)) |>
  inner_join(lion_lookup, by = "segid_chr") |>
  distinct(bbl_chr, segment_id, physical_id) |>
  mutate(source = "W1_PAD")

w1_bbl <- wave1_key |> distinct(bbl_chr)
remaining_after_w1 <- all_bbls |> anti_join(w1_bbl, by = "bbl_chr")

cat("  BBL-segment pairs added:", nrow(wave1_key), "\n")
cat("  Distinct BBLs matched:", nrow(w1_bbl), "\n")
cat("  Remaining unmatched:", nrow(remaining_after_w1), "\n\n")

#-------------------------------------------------
# PART 4: Wave 2A - APPBBL fallback (condos/coops)
#-------------------------------------------------

cat("=== Wave 2A: APPBBL fallback ===\n")

wave2a_key <- remaining_after_w1 |>
  inner_join(
    pluto_key |> st_drop_geometry() |> filter(!is.na(appbbl_chr)) |> select(bbl_chr, appbbl_chr),
    by = "bbl_chr"
  ) |>
  inner_join(
    wave1_key |> select(base_bbl = bbl_chr, segment_id, physical_id),
    by = c("appbbl_chr" = "base_bbl")
  ) |>
  transmute(bbl_chr, segment_id, physical_id) |>
  distinct() |>
  mutate(source = "W2_APPBBL")

w2a_bbl <- wave2a_key |> distinct(bbl_chr)
remaining_after_w2a <- remaining_after_w1 |> anti_join(w2a_bbl, by = "bbl_chr")

cat("  BBL-segment pairs added:", nrow(wave2a_key), "\n")
cat("  Distinct BBLs matched:", nrow(w2a_bbl), "\n")
cat("  Remaining unmatched:", nrow(remaining_after_w2a), "\n\n")

#-------------------------------------------------
# PART 5: Wave 2B - Billing chain (bobabbl)
#-------------------------------------------------

if (exists("bobabbl")) {
  cat("=== Wave 2B: Billing chain (bobabbl) ===\n")
  
  bbbl <- bobabbl |>
    transmute(
      billbbl = make_bbl_chr(billboro, billblock, billlot),
      lobbl = make_bbl_chr(loboro, loblock, lolot)
    ) |>
    filter(!is.na(billbbl), !is.na(lobbl))
  
  wave2b_key <- remaining_after_w2a |>
    inner_join(bbbl, by = c("bbl_chr" = "billbbl")) |>
    inner_join(
      bobaadr |> transmute(
        bbl_chr = make_bbl_chr(boro, block, lot),
        segid_chr = as_segid_chr(segid)
      ),
      by = c("lobbl" = "bbl_chr")
    ) |>
    filter(!is.na(segid_chr)) |>
    inner_join(lion_lookup, by = "segid_chr") |>
    transmute(bbl_chr, segment_id, physical_id) |>
    distinct() |>
    mutate(source = "W2_BILLING")
  
  w2b_bbl <- wave2b_key |> distinct(bbl_chr)
  remaining_after_w2 <- remaining_after_w2a |> anti_join(w2b_bbl, by = "bbl_chr")
  
  cat("  BBL-segment pairs added:", nrow(wave2b_key), "\n")
  cat("  Distinct BBLs matched:", nrow(w2b_bbl), "\n")
  cat("  Remaining unmatched:", nrow(remaining_after_w2), "\n\n")
  
} else {
  wave2b_key <- tibble(bbl_chr = character(), segment_id = integer(), 
                       physical_id = integer(), source = character())
  remaining_after_w2 <- remaining_after_w2a
  cat("=== Wave 2B: Skipped (bobabbl not found) ===\n\n")
}

#-------------------------------------------------
# PART 6: Wave 3 - 50ft buffer spatial intersection
#-------------------------------------------------

cat("=== Wave 3: 50ft buffer intersection ===\n")

pluto_remaining_w3 <- pluto |>
  semi_join(remaining_after_w2, by = "bbl_chr") |>
  select(bbl_chr)

if (nrow(pluto_remaining_w3) > 0) {
  
  pluto_buf50 <- st_buffer(pluto_remaining_w3, 50) |> st_make_valid()
  
  wave3_join <- st_join(
    pluto_buf50 |> select(bbl_chr),
    lion_lines |> select(segment_id, physical_id),
    join = st_intersects,
    left = FALSE
  )
  
  wave3_key <- wave3_join |>
    st_drop_geometry() |>
    distinct(bbl_chr, segment_id, physical_id) |>
    mutate(source = "W3_BUFFER50")
  
  w3_bbl <- wave3_key |> distinct(bbl_chr)
  remaining_after_w3 <- remaining_after_w2 |> anti_join(w3_bbl, by = "bbl_chr")
  
  cat("  BBL-segment pairs added:", nrow(wave3_key), "\n")
  cat("  Distinct BBLs matched:", nrow(w3_bbl), "\n")
  cat("  Remaining unmatched:", nrow(remaining_after_w3), "\n\n")
  
} else {
  wave3_key <- tibble(bbl_chr = character(), segment_id = integer(),
                      physical_id = integer(), source = character())
  remaining_after_w3 <- remaining_after_w2
  cat("  No BBLs to process in Wave 3\n\n")
}

#-------------------------------------------------
# PART 7: Wave 4 - Nearest segment ≤150ft
#-------------------------------------------------

cat("=== Wave 4: Nearest segment ≤150ft ===\n")

pluto_remaining_w4 <- pluto |>
  semi_join(remaining_after_w3, by = "bbl_chr") |>
  select(bbl_chr)

if (nrow(pluto_remaining_w4) > 0) {
  
  lion_min <- lion_lines |> select(segment_id, physical_id)
  idx <- st_nearest_feature(pluto_remaining_w4, lion_min)
  dft <- as.numeric(st_distance(pluto_remaining_w4, lion_min[idx, ], by_element = TRUE))
  
  wave4_key <- tibble(
    bbl_chr = pluto_remaining_w4$bbl_chr,
    segment_id = lion_min$segment_id[idx],
    physical_id = lion_min$physical_id[idx],
    dist_ft = dft
  ) |>
    filter(is.finite(dist_ft), dist_ft <= 150) |>
    distinct(bbl_chr, segment_id, physical_id, .keep_all = TRUE) |>
    mutate(source = "W4_NEAREST150")
  
  w4_bbl <- wave4_key |> distinct(bbl_chr)
  remaining_after_w4 <- remaining_after_w3 |> anti_join(w4_bbl, by = "bbl_chr")
  
  cat("  BBL-segment pairs added:", nrow(wave4_key), "\n")
  cat("  Distinct BBLs matched:", nrow(w4_bbl), "\n")
  cat("  Remaining unmatched:", nrow(remaining_after_w4), "\n\n")
  
} else {
  wave4_key <- tibble(bbl_chr = character(), segment_id = integer(),
                      physical_id = integer(), dist_ft = numeric(), source = character())
  remaining_after_w4 <- remaining_after_w3
  cat("  No BBLs to process in Wave 4\n\n")
}

#-------------------------------------------------
# PART 8: Combine all waves → BBL to physical_id crosswalk
#-------------------------------------------------

cat("=== Building final crosswalks ===\n")

# Full key with source tracking
bbl_segment_physical_key <- bind_rows(
  wave1_key,
  wave2a_key,
  wave2b_key,
  wave3_key |> select(-any_of("dist_ft")),
  wave4_key |> select(-any_of("dist_ft"))
) |>
  distinct(bbl_chr, segment_id, physical_id, source)

# BBL → physical_id (many-to-many: BBL can touch multiple physical blocks)
bbl_to_physical <- bbl_segment_physical_key |>
  distinct(bbl_chr, physical_id, source) |>
  arrange(bbl_chr, physical_id)

# BBL → segment (for NYPD if they need segment-level)
bbl_to_segment <- bbl_segment_physical_key |>
  distinct(bbl_chr, segment_id, source) |>
  arrange(bbl_chr, segment_id)

write_csv(bbl_to_physical, here("output/bbl_to_physical.csv"))
write_csv(bbl_to_segment, here("output/bbl_to_segment.csv"))

# Summary
cat("\n--- BBL Crosswalk Summary ---\n")
cat("Total BBL-physical pairs:", nrow(bbl_to_physical), "\n")
cat("Distinct BBLs matched:", n_distinct(bbl_to_physical$bbl_chr), "\n")
cat("Distinct physical_ids with BBLs:", n_distinct(bbl_to_physical$physical_id), "\n")
cat("BBLs still unmatched:", nrow(remaining_after_w4), "\n\n")

# Match rate by source
bbl_to_physical |>
  count(source, name = "n_pairs") |>
  mutate(pct = round(n_pairs / sum(n_pairs) * 100, 1)) |>
  print()


#-------------------------------------------------
# PART 9: BIN → physical_id crosswalk (FIXED)
#-------------------------------------------------

cat("\n=== Building BIN crosswalk ===\n")

# From PAD (bobaadr): BIN → BBL
bin_to_bbl <- bobaadr |>
  transmute(
    bin = bin,
    bbl_chr = make_bbl_chr(boro, block, lot)
  ) |>
  filter(!is.na(bin), bin != 0, !is.na(bbl_chr)) |>
  distinct()

cat("BIN-BBL pairs from PAD:", nrow(bin_to_bbl), "\n")

# Join to get BIN → physical_id
bin_to_physical <- bin_to_bbl |>
  inner_join(bbl_to_physical |> select(bbl_chr, physical_id), by = "bbl_chr") |>
  distinct(bin, bbl_chr, physical_id)

write_csv(bin_to_physical, here("output/bin_to_physical.csv"))

cat("Total BIN-physical pairs:", nrow(bin_to_physical), "\n")
cat("Distinct BINs:", n_distinct(bin_to_physical$bin), "\n")
cat("Distinct physical_ids with BINs:", n_distinct(bin_to_physical$physical_id), "\n\n")
#-------------------------------------------------
# PART 10: Address list for CJENS (from PAD/bobaadr)
#-------------------------------------------------

cat("=== Building address list ===\n")

# Build address list from bobaadr
address_list <- bobaadr |>
  transmute(
    bbl_chr = make_bbl_chr(boro, block, lot),
    bin = bin,
    # Build full address from components
    house_num = coalesce(lhnd, hhnd),
    street_name = stname,
    address = paste(house_num, street_name),
    boro = boro,
    zip = zipcode
  ) |>
  filter(!is.na(bbl_chr)) |>
  inner_join(bbl_to_physical |> select(bbl_chr, physical_id), by = "bbl_chr") |>
  distinct(physical_id, address, bin, bbl_chr, boro, zip) |>
  arrange(physical_id, address)

#write_csv(address_list, here("output/block_address_list.csv"))

cat("Total addresses:", nrow(address_list), "\n")
cat("Distinct physical_ids with addresses:", n_distinct(address_list$physical_id), "\n")

# Addresses per block summary
addr_per_block <- address_list |>
  count(physical_id, name = "n_addresses")

cat("Addresses per block:\n")
summary(addr_per_block$n_addresses)
cat("\n")

#-------------------------------------------------
# PART 11: Update master block list
#-------------------------------------------------

cat("=== Updating master block list ===\n")

# Count BBLs, BINs, addresses per physical_id
bbl_counts <- bbl_to_physical |>
  count(physical_id, name = "n_bbls")

bin_counts <- bin_to_physical |>
  count(physical_id, name = "n_bins")

addr_counts <- address_list |>
  count(physical_id, name = "n_addresses")

# Update master_blocks (from 03-keys.R)
master_blocks_full <- master_blocks |>
  left_join(bbl_counts, by = "physical_id") |>
  left_join(bin_counts, by = "physical_id") |>
  left_join(addr_counts, by = "physical_id")

write_csv(master_blocks_full, here("output/master_block_list_full.csv"))

cat("Master block list updated with BBL/BIN/address counts\n")

# Summary
cat("\n--- Master Block Summary ---\n")
master_blocks_full |>
  summarise(
    total_blocks = n(),
    blocks_with_bbls = sum(n_bbls > 0, na.rm = TRUE),
    blocks_with_bins = sum(n_bins > 0, na.rm = TRUE),
    blocks_with_addresses = sum(n_addresses > 0, na.rm = TRUE),
    median_bbls = median(n_bbls, na.rm = TRUE),
    median_bins = median(n_bins, na.rm = TRUE),
    median_addresses = median(n_addresses, na.rm = TRUE)
  ) |>
  print()

#-------------------------------------------------
# PART 12: Export unmatched BBLs for QA
#-------------------------------------------------

cat("\n=== Exporting unmatched BBLs for QA ===\n")

unmatched_bbls <- pluto |>
  semi_join(remaining_after_w4, by = "bbl_chr") |>
  select(bbl_chr, boro_code, block, lot, address, land_use, owner_type, owner_name) |>
  st_drop_geometry()

write_csv(unmatched_bbls, here("output/unmatched_bbls.csv"))

cat("Unmatched BBLs exported:", nrow(unmatched_bbls), "\n")

# Breakdown by land use
if (nrow(unmatched_bbls) > 0) {
  cat("\nUnmatched by land use:\n")
  unmatched_bbls |>
    count(land_use, name = "n") |>
    arrange(desc(n)) |>
    head(10) |>
    print()
}

#-------------------------------------------------
# PART 13: Output file inventory
#-------------------------------------------------

cat("\n--- Output Files Created ---\n")
cat("output/bbl_to_physical.csv      - BBL → physical_id crosswalk\n")
cat("output/bbl_to_segment.csv       - BBL → segment_id crosswalk\n")
cat("output/bin_to_physical.csv      - BIN → physical_id crosswalk\n")
cat("output/block_address_list.csv   - All addresses per block (CJENS)\n")
cat("output/master_block_list_full.csv - Master with BBL/BIN/address counts\n")
cat("output/unmatched_bbls.csv       - BBLs that couldn't be matched\n")
