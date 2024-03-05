sf_df_add_xy_center_coords <- function(sf_df){

sf_df |>
    dplyr::pull(geometry) |>
    sf::st_zm() |>
    sf::st_point_on_surface() ->
  points_sf

the_coords <- do.call(rbind, sf::st_geometry(points_sf)) |>
  tibble::as_tibble() |> setNames(c("x","y"))

cbind(sf_df, the_coords)

}
