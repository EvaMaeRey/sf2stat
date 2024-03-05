template_stat_code <- function(){
  
'StatXXXXsf <- ggplot2::ggproto(`_class` = "StatXXXXsf",
                                `_inherit` = ggplot2::Stat,
                                required_aes = c("fips|county_name|XXXX"),
                                compute_panel = compute_panel_geo_XXXX,
                               default_aes = c(label = ggplot2::after_stat(id_col)))' |> cat()
}
