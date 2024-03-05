template_compute_panel_code <- function(){
  
"compute_panel_geo_XXXX <- function(data, scales, keep_id = NULL, drop_id = NULL){
  
  if(!is.null(keep_id)){ data <- filter(data, id_col %in% keep_id) }
  if(!is.null(drop_id)){ data <- filter(data, !(id_col %in% drop_id)) }
  
  if(!stamp){data <- dplyr::inner_join(data, geo_ref_XXXX)}
  if( stamp){data <- geo_ref_XXXX }
  
  data
  
}" |> cat()
  
}
