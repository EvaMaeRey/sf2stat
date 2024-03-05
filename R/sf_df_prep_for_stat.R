sf_df_prep_for_stat <- function(sf_df, id_col_name = NULL){
  
  sf_df |>
    # using purrr allows us to get bb for each row
    dplyr::mutate(bb =
                    purrr::map(geometry,
                               sf_df_return_bbox_df)) |>
    tidyr::unnest(bb) |>
    data.frame() |>
    sf_df_add_xy_center_coords() ->
  sf_df_w_bb_and_centers

  # use first column as keep/drop column unless otherwise specified
  if(is.null(id_col_name)){id_col_name <- 1} 
  
  sf_df_w_bb_and_centers$id_col <- sf_df_w_bb_and_centers[,id_col_name]

  return(sf_df_w_bb_and_centers)
  
  }
