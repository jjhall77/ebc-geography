library(here)
library(tidyverse)
library(sf)
library(janitor)

list.files(here("data/lion"))

# Path to the geodatabase
lion_gdb <- here("data", "lion", "lion.gdb")

# List available layers inside the .gdb
st_layers(lion_gdb)

lion_gdb <- here("data", "lion", "lion.gdb")

lion <- st_read(lion_gdb, layer = "lion") %>%
  st_transform(2263) %>% 
  clean_names() # NYC coordinates, ft



# Load MapPLUTO
pluto <- st_read(here("data/nyc_mappluto_25v3_1_fgdb/MapPLUTO25v3_1.gdb")) %>%  
  clean_names()


# 1. Property Address Directory -----------------------------------------------
bobaadr <- here("data", "pad_25d", "bobaadr.txt") |>
  read_csv(show_col_types = FALSE) |>
  clean_names()

# 2. BBL Crosswalk -------------------------------------------------------------
bobabbl <- here("data", "pad_25d", "bobabbl.txt") |>
  read_csv(show_col_types = FALSE) |>
  clean_names()

# 3. Street Name Dictionary ----------------------------------------------------
snd25Ccow <- here("data", "pad_25d", "snd25Dcow.txt") |>
  read_csv(show_col_types = FALSE) |>
  clean_names()

