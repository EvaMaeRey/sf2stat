nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))

nc |>
  dplyr::select(county_name = NAME, fips = FIPS) |>
  sf2stat:::sf_df_prep_for_stat(id_col_name = "county_name") ->
nc_geo_reference
