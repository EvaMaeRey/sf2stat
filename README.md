
- [Part 0. Proposal](#part-0-proposal)
- [Package build Part I. Work out functionality
  ✅](#package-build-part-i-work-out-functionality-)
  - [Select toy sf data](#select-toy-sf-data)
- [wrapping up more](#wrapping-up-more)
  - [stat_region and friends](#stat_region-and-friends)
  - [stat_subregion and friends](#stat_subregion-and-friends)
- [Geo examples](#geo-examples)
  - [North Carolina counties](#north-carolina-counties)
  - [Chile regiones](#chile-regiones)
  - [World countries rnaturalearth](#world-countries-rnaturalearth)
  - [Canadian provinces](#canadian-provinces)
  - [Netherlands province](#netherlands-province)
  - [US states](#us-states)
  - [US counties](#us-counties)
  - [aseg brain segments](#aseg-brain-segments)
  - [German voting districts](#german-voting-districts)
- [sf2stat Proposed usage](#sf2stat-proposed-usage)

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# Part 0. Proposal

Proposing the {sf2stat} package! 🦄
<!-- (typical package introduction write up; but actually aspirational) -->

The goal of {sf2stat} is to make it easier to prep *sf data* for use in
a ggproto Stat computation; the Stat then can be used for creating a
stat/geom function to be used in ggplot2 plots.

Without the package, we live in the effortful world, in which we’d have
to prep our own data including figuring out the bounding box for each
geometry, and, if we want labeling functionality, the centroid for each
geometry.

With the {sf2stat} package, we’ll live in a different world (🦄 🦄 🦄)
where the task is a snap 🫰:

Proposed API is:

    library(ggregion)
    --
    --
    read.csv("nc-midterms.csv") |>
      ggplot() + 
      aes(county_name = str_to_title(desc_county)) + 
      geom_region() + 
      aes(fill = cd_party) +
      geom_region_text()

# Package build Part I. Work out functionality ✅

In this section we’ll use the nc sf dataframe to check out how our
functions work.

## Select toy sf data

``` r
nc_ref <- sf::st_read(system.file("shape/nc.shp", package="sf")) |>
  select(county_name = NAME, fips = FIPS)
#> Reading layer `nc' from data source 
#>   `/Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/sf/shape/nc.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27

read.csv("nc-midterms.csv") |>
  mutate(county_name = str_to_title(desc_county)) |>
  left_join(nc_ref) %>% 
  ggplot() + 
  geom_sf() +
  aes(fill = cd_party, 
      label = county_name,
      geometry = geometry)+
  geom_sf_text(check_overlap = T)
#> Joining with `by = join_by(county_name)`
```

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

``` r
# we want our stat to do stuff that StatSf and StatSfCoordinates does.
prep_geo_reference <- function(ref_data, id_index = 1){
  
  ref_data |>
  ggplot2::StatSf$compute_panel(coord = ggplot2::CoordSf) |>
  ggplot2::StatSfCoordinates$compute_group(coord = ggplot2::CoordSf) %>% 
    mutate(id_col = .[[id_index]])
  
}


compute_panel_aggregation <- function(data, scales, fun = sum, non_grouping = c("fill", "wt", "within")){
  
  grp_cols <-  names(data)[!names(data) %in% non_grouping]
  
  # Thanks June! https://github.com/teunbrand/ggplot-extension-club/discussions/15
  data %>% 
    group_by(group_by(pick(any_of(grp_cols)))) ->   
  data
  
  if(is.null(data$fill)){data <- mutate(data, fill = 1)}
  if(is.null(data$wt)){data$wt <- 1}
  
  data %>% 
    summarize(fill = fun(.data$fill*.data$wt), .groups = 'drop') |>
    mutate(summary = fill) ->
  data
    
  if(is.null(data$within)){data$within <- 1}

  data %>%   
    group_by(.data$within) %>% 
    mutate(prop = .data$fill/sum(.data$fill)) %>%
    mutate(percent = round(.data$prop*100)) -> 
  data
  
  data
  
}

# Flip the script... prepare compute (join) to happen in layer (NEW!)
compute_panel_region <- function(data, scales, ref_data, id_index = 1,
                                 stamp = FALSE, keep_id = NULL,
                                 drop_id = NULL, fun = sum){
  
  fill_is_category <- is.character(data$fill)|is.factor(data$fill)|is.logical(data$fill)
  
  if(!(fill_is_category)){
  
  data <- data |> compute_panel_aggregation(scales, fun = fun, non_grouping = c("fill", "wt", "within")) 
  
  }
  
  ref_data %>% 
    prep_geo_reference(id_index = id_index) ->
  ref_data
  
  if(!is.null(keep_id)){
  
  ref_data %>% 
    filter(id_col %in% keep_id) ->
  ref_data
  
    }
  
  if(!is.null(drop_id)){
  
  ref_data %>% 
    filter(!(id_col %in% drop_id)) ->
  ref_data
  
    }
  
  if(stamp){
    
    ref_data |>
      mutate(fill = ifelse(fill_is_category, NA, NA |> as.numeric()))
    
  }else{
  
 ref_data %>% 
    inner_join(data) 
      
  
  }
    
}
```

``` r
nc_ref <- sf::st_read(system.file("shape/nc.shp", package="sf")) |>
  select(county_name = NAME, fips = FIPS)
#> Reading layer `nc' from data source 
#>   `/Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/sf/shape/nc.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27

read.csv("nc-midterms.csv") |>
  mutate(county_name = str_to_title(desc_county)) |>
  select(county_name) |>
  compute_panel_region(ref_data = nc_ref)
#> Joining with `by = join_by(county_name)`
#> Simple feature collection with 98 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
#> First 10 features:
#>    county_name  fips      xmin      xmax     ymin     ymax         x        y
#> 1         Ashe 37009 -84.32385 -75.45698 33.88199 36.58965 -81.49496 36.42112
#> 2    Alleghany 37005 -84.32385 -75.45698 33.88199 36.58965 -81.13241 36.47396
#> 3        Surry 37171 -84.32385 -75.45698 33.88199 36.58965 -80.69280 36.38828
#> 4    Currituck 37053 -84.32385 -75.45698 33.88199 36.58965 -75.93852 36.30697
#> 5  Northampton 37131 -84.32385 -75.45698 33.88199 36.58965 -77.36988 36.35211
#> 6     Hertford 37091 -84.32385 -75.45698 33.88199 36.58965 -77.04217 36.39709
#> 7       Camden 37029 -84.32385 -75.45698 33.88199 36.58965 -76.18290 36.36249
#> 8        Gates 37073 -84.32385 -75.45698 33.88199 36.58965 -76.72199 36.43576
#> 9       Warren 37185 -84.32385 -75.45698 33.88199 36.58965 -78.11342 36.42681
#> 10      Stokes 37169 -84.32385 -75.45698 33.88199 36.58965 -80.23459 36.40106
#>         id_col fill summary within prop percent                       geometry
#> 1         Ashe    1       1      1 0.01       1 MULTIPOLYGON (((-81.47276 3...
#> 2    Alleghany    1       1      1 0.01       1 MULTIPOLYGON (((-81.23989 3...
#> 3        Surry    1       1      1 0.01       1 MULTIPOLYGON (((-80.45634 3...
#> 4    Currituck    1       1      1 0.01       1 MULTIPOLYGON (((-76.00897 3...
#> 5  Northampton    1       1      1 0.01       1 MULTIPOLYGON (((-77.21767 3...
#> 6     Hertford    1       1      1 0.01       1 MULTIPOLYGON (((-76.74506 3...
#> 7       Camden    1       1      1 0.01       1 MULTIPOLYGON (((-76.00897 3...
#> 8        Gates    1       1      1 0.01       1 MULTIPOLYGON (((-76.56251 3...
#> 9       Warren    1       1      1 0.01       1 MULTIPOLYGON (((-78.30876 3...
#> 10      Stokes    1       1      1 0.01       1 MULTIPOLYGON (((-80.02567 3...

read.csv("nc-midterms.csv") |>
  mutate(county_name = str_to_title(desc_county)) |>
  select(county_name) |>
  compute_panel_region(ref_data = nc_ref, keep_id = "Mecklenburg")
#> Joining with `by = join_by(county_name)`
#> Simple feature collection with 1 feature and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -81.06555 ymin: 35.00202 xmax: -80.53964 ymax: 35.50912
#> Geodetic CRS:  NAD27
#>   county_name  fips      xmin      xmax     ymin     ymax         x        y
#> 1 Mecklenburg 37119 -84.32385 -75.45698 33.88199 36.58965 -80.82771 35.25729
#>        id_col fill summary within prop percent                       geometry
#> 1 Mecklenburg    1       1      1 0.01       1 MULTIPOLYGON (((-81.0493 35...
```

# wrapping up more

## stat_region and friends

``` r
# same as geom_sf but geom (and stat) is flexible
qlayer_sf_crs <- function (mapping = NULL, data = NULL, geom = "sf", 
                           stat = "sf", position = "identity", 
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, 
                           crs, ...) {
  
    c(layer_sf(geom = geom, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, 
            ...)), 
      coord_sf(crs = crs))
}

stat_region <- function(ref_data = getOption("sf2stat.ref_data", nc_ref), 
                        id_index = 1, 
                        required_aes = getOption("sf2stat.required_aes", "fips|county_name"),
                        geom = GeomSf, ...){
  
  StatSfJoin <- ggproto("StatSfJoin", Stat, 
                        compute_panel = compute_panel_region, 
                        default_aes = aes(label = after_stat(id_col), 
                                          fill = after_stat(fill)),
                        required_aes = required_aes)
  
  qlayer_sf_crs(stat = StatSfJoin, 
                geom = geom,
                ref_data = ref_data, 
                crs = sf::st_crs(ref_data), 
                id_index = id_index, ...)
  
}

#  geom_sf  # want to look at quieting the coord message...


GeomOutline <- ggproto("GeomOutline", GeomSf,
                       default_aes = aes(!!!modifyList(GeomSf$default_aes,
                                                       aes(fill = "transparent", 
                                                           color = "black"))))

geom_region_sf <- function(mapping = NULL, ...){stat_region(geom = GeomSf, mapping = mapping, ...)}
geom_region <- geom_region_sf   # convenience short name
geom_region_outline <- function(mapping = NULL, ...){stat_region(geom = GeomOutline, mapping = mapping, ...)}
geom_region_label <- function(mapping = NULL, ...){stat_region(geom = GeomLabel,mapping = mapping,...)}
geom_region_text <- function(mapping = NULL, ...){stat_region(geom = GeomText, mapping = mapping,...)}
geom_region_textrepel <- function(mapping = NULL, ...){stat_region(geom = ggrepel::GeomTextRepel, mapping = mapping, ...)}


stamp_region_sf <- function(...){geom_region_sf(stamp = T, required_aes = Stat$required_aes, ...)}
stamp_region <- stamp_region_sf
stamp_region_outline <- function(...){geom_region_outline(stamp = T, required_aes = Stat$required_aes, ...)}
stamp_region_label <- function(...){geom_region_label(stamp = T, required_aes = Stat$required_aes, ...)}
stamp_region_text <- function(...){geom_region_text(stamp = T, required_aes = Stat$required_aes, ...)}
```

## stat_subregion and friends

``` r
stat_subregion <- function(ref_data = getOption("sf2stat.ref_data_subregion", nc_ref), 
                        id_index = 1, 
                        required_aes = getOption("sf2stat.required_aes_subregion", "fips|county_name"),
                        geom = GeomSf, ...){
  
  StatSfJoin <- ggproto("StatSfJoin", Stat, 
                        compute_panel = compute_panel_region, 
                        default_aes = aes(label = after_stat(id_col)),
                        required_aes = required_aes)
  
  qlayer_sf_crs(stat = StatSfJoin, 
                geom = geom,
                ref_data = ref_data, 
                crs = sf::st_crs(ref_data), 
                id_index = id_index, ...)
  
}


geom_subregion_sf <- function(mapping = NULL, ...){stat_subregion(geom = GeomSf, mapping = mapping, ...)}
geom_subregion <- geom_subregion_sf   # convenience short name
geom_subregion_outline <- function(mapping = NULL, ...){stat_subregion(geom = GeomOutline, mapping = mapping, ...)}
geom_subregion_label <- function(mapping = NULL, ...){stat_subregion(geom = GeomLabel,mapping = mapping,...)}
geom_subregion_text <- function(mapping = NULL, ...){stat_subregion(geom = GeomText, mapping = mapping,...)}
geom_subregion_textrepel <- function(mapping = NULL, ...){stat_subregion(geom = ggrepel::GeomTextRepel,mapping = mapping, ...)}


stamp_subregion_sf <- function(...){geom_subregion_sf(stamp = T, required_aes = Stat$required_aes, ...)}
stamp_subregion <- stamp_subregion_sf
stamp_subregion_outline <- function(...){geom_subregion_outline(stamp = T, required_aes = Stat$required_aes, ...)}
stamp_subregion_label <- function(...){geom_subregion_label(stamp = T, required_aes = Stat$required_aes, ...)}
stamp_subregion_text <- function(...){geom_subregion_text(stamp = T, required_aes = Stat$required_aes, ...)}
```

``` r
set_region <- function(ref_data, required_aes = NULL, region, level = "region", return_region_names = F){
  
  if(is.null(required_aes)){
    
    names_ref_data <- names(ref_data)
    required_aes <- paste(names_ref_data[names_ref_data != "geometry"],
                          collapse = "|")
    
  }
  
  if(level == "region"){
options(sf2stat.ref_data = ref_data,
        sf2stat.required_aes = required_aes)

message(paste0("Region is", region, "\nRequired aes: '", required_aes, "'"))
  }  
  
  if(level == "subregion"){
options(sf2stat.ref_data_subregion = ref_data,
        sf2stat.required_aes_subregion = required_aes)

message(paste0("Subregion is ", region, "\nRequired aes: '", required_aes, "'"))

  }  

if(return_region_names){
  ref_data[,1]
}

}
```

# Geo examples

many data packages linked to from here
<https://github.com/ipeaGIT/geobr>

## North Carolina counties

``` r
set_region_sf_nc_counties <- function(return_region_names = F, region = "county"){

sf::st_read(system.file("shape/nc.shp", package="sf")) |>
  select(county_name = NAME, fips = FIPS) |>
  set_region(region = region, return_region_names = return_region_names)

}

nc_midterms <- read.csv("nc-midterms.csv")
head(nc_midterms)
#>   desc_county     n  cd_party  ind_vote
#> 1      ONSLOW 24406 0.2059283 0.3862985
#> 2     ROBESON 36367 0.5061306 0.4066599
#> 3    RANDOLPH 15867 0.1651505 0.4230793
#> 4       ANSON  9028 0.5674062 0.4267833
#> 5     HALIFAX 21875 0.5865712 0.4337829
#> 6       ROWAN 23667 0.2424922 0.4338108

set_region_sf_nc_counties()
#> Reading layer `nc' from data source 
#>   `/Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/sf/shape/nc.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
#> Region iscounty
#> Required aes: 'county_name|fips'

nc_midterms |>
  ggplot() + 
  aes(county_name = str_to_title(desc_county)) + 
  stamp_region(fill = 'darkgrey') + 
  geom_region() + 
  aes(fill = n/1000) + 
  stamp_region_outline(
    keep_id = "Mecklenburg",
    color = "orange",
    linewidth = 1) + 
  geom_region_text(check_overlap = T,
                   color = "whitesmoke")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(county_name)`
#> Joining with `by = join_by(county_name)`
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

``` r

layer_data() |> head()
#> Joining with `by = join_by(county_name)`
#> Joining with `by = join_by(county_name)`
#>       fill       label PANEL county_name  fips                       geometry
#> 1 darkgrey        Ashe     1        Ashe 37009 MULTIPOLYGON (((-81.47276 3...
#> 2 darkgrey   Alleghany     1   Alleghany 37005 MULTIPOLYGON (((-81.23989 3...
#> 3 darkgrey       Surry     1       Surry 37171 MULTIPOLYGON (((-80.45634 3...
#> 4 darkgrey   Currituck     1   Currituck 37053 MULTIPOLYGON (((-76.00897 3...
#> 5 darkgrey Northampton     1 Northampton 37131 MULTIPOLYGON (((-77.21767 3...
#> 6 darkgrey    Hertford     1    Hertford 37091 MULTIPOLYGON (((-76.74506 3...
#>        xmin      xmax     ymin     ymax         x        y      id_col linetype
#> 1 -84.32385 -75.45698 33.88199 36.58965 -81.49496 36.42112        Ashe        1
#> 2 -84.32385 -75.45698 33.88199 36.58965 -81.13241 36.47396   Alleghany        1
#> 3 -84.32385 -75.45698 33.88199 36.58965 -80.69280 36.38828       Surry        1
#> 4 -84.32385 -75.45698 33.88199 36.58965 -75.93852 36.30697   Currituck        1
#> 5 -84.32385 -75.45698 33.88199 36.58965 -77.36988 36.35211 Northampton        1
#> 6 -84.32385 -75.45698 33.88199 36.58965 -77.04217 36.39709    Hertford        1
#>   alpha stroke    colour linewidth
#> 1    NA    0.5 #595959FF       0.2
#> 2    NA    0.5 #595959FF       0.2
#> 3    NA    0.5 #595959FF       0.2
#> 4    NA    0.5 #595959FF       0.2
#> 5    NA    0.5 #595959FF       0.2
#> 6    NA    0.5 #595959FF       0.2
```

## Chile regiones

``` r
set_region_region_chilemapas <- function(return_region_names = F){

chilemapas::generar_regiones() %>% 
  mutate(region_numerico = as.numeric(codigo_region)) %>%
  select(region_codigo = codigo_region,
         region_numerico)  |>
  set_region(region = "region", return_region_names = return_region_names)
  
}
```

``` r
set_region_region_chilemapas()
#> Region isregion
#> Required aes: 'region_codigo|region_numerico'
options(scipen = 10)

chilemapas::censo_2017_comunas %>% 
  mutate(region = str_extract(codigo_comuna, "..")) %>% 
  summarise(pop = sum(poblacion), .by = c(region, sexo)) %>% 
  ggplot() + 
  aes(region_codigo = region, fill = pop/100000) +
  geom_region(linewidth = .01, color = "white") + 
  facet_wrap(~sexo) + 
  scale_fill_viridis_c(transform = "log") + 
  stamp_region_outline(color = "red", 
                       keep_id = "05")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(region_codigo)`
#> Joining with `by = join_by(region_codigo)`
```

![](man/figures/README-unnamed-chunk-10-1.png)<!-- -->

``` r

last_plot() + 
  geom_region_textrepel()
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(region_codigo)`
#> Joining with `by = join_by(region_codigo)`
#> Joining with `by = join_by(region_codigo)`
#> Joining with `by = join_by(region_codigo)`
```

![](man/figures/README-unnamed-chunk-10-2.png)<!-- -->

``` r
library(ggstats)
chilemapas::censo_2017_comunas %>% 
  mutate(region = str_extract(codigo_comuna, "..")) %>% 
  summarise(pop = sum(poblacion), .by = c(region, sexo)) %>% 
  ggplot() +
  aes(fill = sexo, y = region, weight = pop) + 
  ggstats::geom_pyramid(fill = "grey") +
  ggstats::geom_pyramid()
```

![](man/figures/README-unnamed-chunk-11-1.png)<!-- -->

``` r


chilemapas::censo_2017_comunas %>% 
  mutate(region = str_extract(codigo_comuna, "..")) %>% 
  summarise(pop = sum(poblacion), .by = c(region, sexo)) %>% 
  ggplot() +
  aes(fill = sexo, y = region, weight = pop) + 
  ggstats::geom_likert(fill = "grey") + 
  ggstats::geom_likert() + 
  ggstats::geom_likert_text()
```

![](man/figures/README-unnamed-chunk-11-2.png)<!-- -->

## World countries rnaturalearth

``` r
library(tidyverse)

set_region_country_rnaturalearth <- function(scale = "small", return_region_names = F){

rnaturalearth::ne_countries(
  scale = scale, returnclass = "sf") |> 
  select(country_name = sovereignt, iso3c = iso_a3) |>
  mutate(country_name = ifelse(country_name == "United States of America", "United States", country_name)) |>
  mutate(iso3c = ifelse(country_name == "France", "FRA", iso3c)) |>
  set_region(region = "country", return_region_names = return_region_names)
  
}
```

``` r
set_region_country_rnaturalearth()
#> Region iscountry
#> Required aes: 'country_name|iso3c'

ggplot() + 
  aes(iso3c = 1, fill = NULL) + # this shouldn't be required for stamp, but is
  stamp_region() + 
  stamp_region(keep_id = c("United States", "Brazil", "Canada", 
                           "France", "South Korea", "United Kingdom",
                           "Netherlands", "Austria", "Australia", 
                           "Uganda", "Germany", "Denmark",
                           "Sweden"), fill = "midnightblue") +
  labs(title = "extenders tuning in ...")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
```

![](man/figures/README-unnamed-chunk-13-1.png)<!-- -->

``` r
set_region_country_rnaturalearth()
#> Region iscountry
#> Required aes: 'country_name|iso3c'

countries_csv_url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/countries.csv'

readr::read_csv(countries_csv_url) |>
  ggplot() + 
  aes(iso3c = alpha_3, # iso3c or country_name are required...
      fill = numeric,
      label = numeric) + 
  geom_region() +
  geom_region_text(check_overlap = T,
                   size = 2, 
                   color = "whitesmoke") + 
  labs(title = "ISO numeric country codes")
#> Rows: 249 Columns: 6
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (5): alpha_2, alpha_3, name, official_name, common_name
#> dbl (1): numeric
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(iso3c)`
#> Joining with `by = join_by(iso3c)`
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="33%" />

``` r
last_plot() +
  aes(fill = rank(name)) + 
  labs(title = "Are numeric codes basically spaced alphabetical orderings")
#> Joining with `by = join_by(iso3c)`
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-15-1.png)<!-- -->

``` r

last_plot() + 
  aes(fill = log(abs(rank(numeric) - rank(name)) + 1)) + 
  labs(title = "Which codes are furthest from expected if alphabetical dominates?")
#> Joining with `by = join_by(iso3c)`
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-15-2.png)<!-- -->

``` r

last_plot() + 
  aes(fill = str_extract(name, "...") |> toupper() == alpha_3) +
  labs(title = "Which countries iso3c is exact match with first three letters?") %>% 
  labs(fill = "First three letters is ISO3")
#> Joining with `by = join_by(iso3c)`
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-15-3.png)<!-- -->

``` r


readr::read_csv(countries_csv_url) |>
  ggplot() + 
  aes(x = rank(name), 
      y = numeric, 
      label = name) + 
  geom_point(color = "black") +
  stat_smooth(method = "lm")
#> Rows: 249 Columns: 6
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (5): alpha_2, alpha_3, name, official_name, common_name
#> dbl (1): numeric
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> `geom_smooth()` using formula = 'y ~ x'
```

![](man/figures/README-unnamed-chunk-15-4.png)<!-- -->

``` r
set_region_country_rnaturalearth()
#> Region iscountry
#> Required aes: 'country_name|iso3c'

heritage <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv')
#> Rows: 3 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (1): country
#> dbl (2): 2004, 2022
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

heritage %>% 
  pivot_longer(cols = `2004`:`2022`, names_to = "year", values_to = "n_awards") %>% 
  ggplot() +
  aes(country_name = country) + 
  geom_region() + 
  aes(fill = n_awards) + 
  facet_wrap(~year)
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
```

![](man/figures/README-unnamed-chunk-16-1.png)<!-- -->

``` r
democracy_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv')
#> Rows: 14768 Columns: 43
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (11): country_name, country_code, regime_category, monarch_name, preside...
#> dbl (15): year, regime_category_index, monarch_accession_year, monarch_birth...
#> lgl (17): is_monarchy, is_commonwealth, is_female_monarch, is_democracy, is_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

set_region_country_rnaturalearth()
#> Region iscountry
#> Required aes: 'country_name|iso3c'

democracy_data %>% 
  filter(year == 2020) %>% 
  ggplot() + 
  aes(iso3c = country_code) +
  geom_region() + 
  aes(fill = regime_category)
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-17-1.png)<!-- -->

``` r

last_plot() + 
  aes(fill = NULL) + 
  geom_region(fill = "darkred", 
              data = . %>% filter(regime_category %>% stringr::str_detect("dictatorship"))) + 
  labs(title = "Countries classified as dictatorships in 2020 in 2024-11-05/democracy_data.csv")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(iso3c)`
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-17-2.png)<!-- -->

``` r
set_region_country_rnaturalearth()
#> Region iscountry
#> Required aes: 'country_name|iso3c'

worlds_fairs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-13/worlds_fairs.csv')
#> Rows: 70 Columns: 14
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (6): name_of_exposition, country, city, category, theme, notables
#> dbl (8): start_month, start_year, end_month, end_year, visitors, cost, area,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

worlds_fairs %>%
  count(country) %>% 
  ggplot() + 
  aes(country_name = country, wt = n) + 
  stamp_region(drop_id = "Antarctica") +
  geom_region() +
  labs(title = "number of World Fairs, worlds_fairs.csv")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(country_name)`
```

![](man/figures/README-unnamed-chunk-18-1.png)<!-- -->

``` r
set_region_country_rnaturalearth()
#> Region iscountry
#> Required aes: 'country_name|iso3c'

stackoverflow_survey_single_response <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-03/stackoverflow_survey_single_response.csv')
#> Rows: 65437 Columns: 28
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (2): country, currency
#> dbl (26): response_id, main_branch, age, remote_work, ed_level, years_code, ...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

stackoverflow_survey_single_response %>%
  mutate(iso3c = 
           countrycode::countrycode(country, "country.name", "iso3c")) %>% 
  count(iso3c, r_used) %>% 
  filter(!is.na(r_used)) %>% 
  ggplot() + 
  aes(iso3c = iso3c, fill = n) + 
  stamp_region(drop_id = "Antarctica") +
  geom_region() + 
  scale_fill_viridis_c(transform = "log", 
                       breaks = 10^(0:4)) + 
  facet_wrap(~ ind2cat::ind_recode(r_used, 
                                   cat_true = "Has used R", 
                                   cat_false = "Hasn't used R"),
             ncol = 1) + 
  geom_region_text(aes(label = n), 
                   color = "grey",
                   check_overlap = T,
                   size = 2)
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(iso3c)`
#> Joining with `by = join_by(iso3c)`
#> Joining with `by = join_by(iso3c)`
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-19-1.png)<!-- -->

``` r

library(countrycode)
stackoverflow_survey_single_response %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>% 
  filter(!is.na(r_used)) %>% 
  summarise(r_percent = 100 * sum(r_used)/n(), 
            total_responding = n(),
            .by = iso3c) %>% 
  filter(total_responding > 10) %>% 
  ggplot() + 
  aes(fill = r_percent, iso3c = iso3c) + 
  stamp_region() +
  geom_region() + 
  scale_fill_viridis_c()
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-19-2.png)<!-- -->

``` r


stackoverflow_survey_single_response %>% 
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>% 
  filter(!is.na(r_used)) %>% 
  filter(n() > 10, .by = iso3c) %>% 
  ggplot() + 
  aes(iso3c = iso3c, fill = r_used) + 
  stamp_region() +
  geom_region(fun = mean) + 
  scale_fill_viridis_c()
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-19-3.png)<!-- -->

``` r

layer_data(i = 2) |> head()
#> Joining with `by = join_by(iso3c)`
#>      fill                       label                country_name iso3c xmin
#> 1 #2B9289 United Republic of Tanzania United Republic of Tanzania   TZA -180
#> 2 #404D88                      Canada                      Canada   CAN -180
#> 3 #3E5489               United States               United States   USA -180
#> 4 #440154                  Kazakhstan                  Kazakhstan   KAZ -180
#> 5 #462167                  Uzbekistan                  Uzbekistan   UZB -180
#> 6 #3E5589                   Indonesia                   Indonesia   IDN -180
#>   xmax ymin     ymax          x          y                      id_col PANEL
#> 1  180  -90 83.64513   34.14207 -6.2078294 United Republic of Tanzania     1
#> 2  180  -90 83.64513 -110.24381 56.7019200                      Canada     1
#> 3  180  -90 83.64513  -99.31483 37.2367450               United States     1
#> 4  180  -90 83.64513   66.31159 48.0689612                  Kazakhstan     1
#> 5  180  -90 83.64513   63.44288 41.3532772                  Uzbekistan     1
#> 6  180  -90 83.64513  113.26946 -0.1785159                   Indonesia     1
#>   group    summary within        prop percent                       geometry
#> 1   115 0.10714286      1 0.019963705       2 MULTIPOLYGON (((33.90371 -0...
#> 2    20 0.04912451      1 0.009153268       1 MULTIPOLYGON (((-122.84 49,...
#> 3   119 0.05446683      1 0.010148691       1 MULTIPOLYGON (((-122.84 49,...
#> 4    63 0.00000000      1 0.000000000       0 MULTIPOLYGON (((87.35997 49...
#> 5   120 0.01639344      1 0.003054556       0 MULTIPOLYGON (((55.96819 41...
#> 6    51 0.05539359      1 0.010321371       1 MULTIPOLYGON (((141.0002 -2...
#>   linetype alpha stroke    colour linewidth
#> 1        1    NA    0.5 #595959FF       0.2
#> 2        1    NA    0.5 #595959FF       0.2
#> 3        1    NA    0.5 #595959FF       0.2
#> 4        1    NA    0.5 #595959FF       0.2
#> 5        1    NA    0.5 #595959FF       0.2
#> 6        1    NA    0.5 #595959FF       0.2
```

``` r
set_region_country_rnaturalearth()  
#> Region iscountry
#> Required aes: 'country_name|iso3c'

eclipse_total_2024 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_total_2024.csv')
#> Rows: 3330 Columns: 10
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (2): state, name
#> dbl  (2): lat, lon
#> time (6): eclipse_1, eclipse_2, eclipse_3, eclipse_4, eclipse_5, eclipse_6
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.


eclipse_total_2024 %>% 
  ggplot() + 
  aes(country_name = 1) + 
  stamp_region(keep_id = "United States") +
  geom_point(aes(y = lat, x = lon, color = eclipse_1))
```

![](man/figures/README-unnamed-chunk-20-1.png)<!-- -->

``` r
set_region_country_rnaturalearth()
#> Region iscountry
#> Required aes: 'country_name|iso3c'

outer_space_objects <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-23/outer_space_objects.csv')
#> Rows: 1175 Columns: 4
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (2): Entity, Code
#> dbl (2): Year, num_objects
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

outer_space_objects |>
  summarise(num_objects = sum(num_objects), .by = Code) |>
  ggplot() + 
  aes(iso3c = Code, 
      fill = num_objects) +
  stamp_region() +
  geom_region() +
  scale_fill_viridis_c(transform = "log",
                       breaks = 10^(0:4)) + 
  labs(title = "Number of outerspace objects in outer_space_objects.csv")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-21-1.png)<!-- -->

``` r

outer_space_objects |>
  ggplot() + 
  aes(iso3c = Code, fill = num_objects) +
  stamp_region() +
  geom_region() +
  scale_fill_viridis_c(transform = "log",
                       breaks = 10^(0:4)) + 
  labs(title = "Number of outerspace objects in outer_space_objects.csv")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-21-2.png)<!-- -->

``` r

layer_data(i = 2) |> head()
#> Joining with `by = join_by(iso3c)`
#>      fill            label     country_name iso3c xmin xmax ymin     ymax
#> 1 #46276C           Canada           Canada   CAN -180  180  -90 83.64513
#> 2 #FDE725    United States    United States   USA -180  180  -90 83.64513
#> 3 #440556       Kazakhstan       Kazakhstan   KAZ -180  180  -90 83.64513
#> 4 #440154 Papua New Guinea Papua New Guinea   PNG -180  180  -90 83.64513
#> 5 #440556        Indonesia        Indonesia   IDN -180  180  -90 83.64513
#> 6 #450958        Argentina        Argentina   ARG -180  180  -90 83.64513
#>            x           y           id_col PANEL group    summary within
#> 1 -110.24381  56.7019200           Canada     1    15  25.003548      1
#> 2  -99.31483  37.2367450    United States     1    93 255.467576      1
#> 3   66.31159  48.0689612       Kazakhstan     1    46   2.197225      1
#> 4  144.22612  -6.6678356 Papua New Guinea     1    71   0.000000      1
#> 5  113.26946  -0.1785159        Indonesia     1    38   2.079442      1
#> 6  -64.08055 -37.2391995        Argentina     1     3   3.871201      1
#>          prop percent                       geometry linetype alpha stroke
#> 1 0.018074709       2 MULTIPOLYGON (((-122.84 49,...        1    NA    0.5
#> 2 0.184673881      18 MULTIPOLYGON (((-122.84 49,...        1    NA    0.5
#> 3 0.001588342       0 MULTIPOLYGON (((87.35997 49...        1    NA    0.5
#> 4 0.000000000       0 MULTIPOLYGON (((141.0002 -2...        1    NA    0.5
#> 5 0.001503199       0 MULTIPOLYGON (((141.0002 -2...        1    NA    0.5
#> 6 0.002798436       0 MULTIPOLYGON (((-68.63401 -...        1    NA    0.5
#>      colour linewidth
#> 1 #595959FF       0.2
#> 2 #595959FF       0.2
#> 3 #595959FF       0.2
#> 4 #595959FF       0.2
#> 5 #595959FF       0.2
#> 6 #595959FF       0.2
```

``` r
set_region_country_rnaturalearth()
#> Region iscountry
#> Required aes: 'country_name|iso3c'
 

wwbi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_data.csv')
#> Rows: 141985 Columns: 4
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (2): country_code, indicator_code
#> dbl (2): year, value
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
wwbi_series <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_series.csv')
#> Rows: 302 Columns: 2
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (2): indicator_code, indicator_name
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.


wwbi_data %>% 
  filter(year == 2007, indicator_code == "BI.WAG.PRVS.FM.SM") %>% 
  ggplot() + 
  aes(iso3c = country_code, fill = value) + 
  stamp_region() +
  geom_region() + 
  labs(title = wwbi_series %>% 
         filter(indicator_code =="BI.WAG.PRVS.FM.SM") %>%
         pull(indicator_name)) +
  scale_fill_viridis_c(option = "cividis") 
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-22-1.png)<!-- -->

``` r

wwbi_data %>% 
  filter(year == 2007, indicator_code == "BI.WAG.PRVS.FM.SM") %>% 
  filter(value < 3) %>% 
  ggplot() + 
  aes(iso3c = country_code, fill = value) + 
  stamp_region() +
  geom_region() + 
  labs(title = wwbi_series %>% 
         filter(indicator_code =="BI.WAG.PRVS.FM.SM") %>%
         pull(indicator_name)) +
  scale_fill_viridis_c(option = "magma") 
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-22-2.png)<!-- -->

``` r
set_region_country_rnaturalearth()
#> Region iscountry
#> Required aes: 'country_name|iso3c'
   


wwbi_country <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_country.csv')
#> Rows: 202 Columns: 29
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (21): country_code, short_name, table_name, long_name, x2_alpha_code, cu...
#> dbl  (7): national_accounts_base_year, national_accounts_reference_year, sys...
#> lgl  (1): vital_registration_complete
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.


wwbi_country %>% 
  ggplot() + 
  aes(iso3c = country_code, fill = region) + 
  geom_region()
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-23-1.png)<!-- -->

``` r

last_plot() + 
  aes(fill = income_group)
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-23-2.png)<!-- -->

``` r
set_region_country_rnaturalearth()
#> Region iscountry
#> Required aes: 'country_name|iso3c'
 
cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv')
#> Rows: 1187 Columns: 19
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (17): cheese, url, milk, country, region, family, type, fat_content, cal...
#> lgl  (2): vegetarian, vegan
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

cheeses %>% 
  filter(!str_detect(milk, ",")) %>% 
  distinct(country, milk) %>% 
  ggplot() + 
  aes(country_name = country) + 
  stamp_region() + 
  geom_region(fill = "goldenrod3") +
  facet_wrap(~milk) + 
  labs(title = "Cheese Origins! Country and animal in cheeses.csv")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
```

![](man/figures/README-unnamed-chunk-24-1.png)<!-- -->

``` r
set_region_country_rnaturalearth()
#> Region iscountry
#> Required aes: 'country_name|iso3c'


tidyr::world_bank_pop %>% 
  filter(indicator == "SP.POP.GROW") %>% 
  mutate(pop_growth = `2000`) %>% 
  ggplot() + 
  aes(iso3c = country) + 
  geom_region() + 
  aes(fill = pop_growth) + 
  scale_fill_viridis_c() + 
  labs(title = "Population Growth, 2000") +
  geom_region_outline(data = . %>% filter(pop_growth<0), 
                      color = "red")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(iso3c)`
#> Joining with `by = join_by(iso3c)`
```

![](man/figures/README-unnamed-chunk-25-1.png)<!-- -->

``` r
set_region_country_rnaturalearth(scale = "large")  
#> Region iscountry
#> Required aes: 'country_name|iso3c'

orcas <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-15/orcas.csv')
#> Rows: 775 Columns: 19
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (10): encounter_sequence, duration, vessel, observers, pods_or_ecotype,...
#> dbl   (6): year, encounter_number, begin_latitude, begin_longitude, end_lati...
#> date  (1): date
#> time  (2): begin_time, end_time
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

orcas |>
  ggplot() + 
  geom_point(aes(y = begin_latitude,
                 x = begin_longitude),
             size = 2, pch = 21,
             fill = "white",
             color = "black"
             ) +
  stamp_region_outline(keep_id = c("United States", "Canada")) +
  coord_sf(crs = "NAD83", 
           xlim = c(min(orcas$begin_longitude, na.rm = T), 
                     max(orcas$begin_longitude, na.rm = T)), 
           ylim = c(min(orcas$begin_latitude, na.rm = T), 
                     max(orcas$begin_latitude, na.rm = T))) +
  labs(title = "Orcas begin latitude and longitude in orcas.csv ")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
```

![](man/figures/README-unnamed-chunk-26-1.png)<!-- -->

``` r
set_region_country_rnaturalearth()  
#> Region iscountry
#> Required aes: 'country_name|iso3c'

cia_factbook <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-22/cia_factbook.csv')  
#> Rows: 259 Columns: 11
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (1): country
#> dbl (10): area, birth_rate, death_rate, infant_mortality_rate, internet_user...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
  
cia_factbook %>% 
  ggplot() + 
  aes(country_name = country) + 
  stamp_region(drop_id = "Antarctica") +
  geom_region() + 
  aes(fill = internet_users/population)
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(country_name)`
```

![](man/figures/README-unnamed-chunk-27-1.png)<!-- -->

``` r

last_plot() + 
  geom_region_text(data = . %>% filter(area > 500000),
                   check_overlap = T,
                   size = 2,
                   color = "whitesmoke") + 
  aes(label = round(100*internet_users/population)|>paste0("%")) + 
  labs(title = "Internet usership") + 
  guides(fill = "none")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
```

![](man/figures/README-unnamed-chunk-27-2.png)<!-- -->

``` r
set_region_country_rnaturalearth()  
#> Region iscountry
#> Required aes: 'country_name|iso3c'

nato_names <- c("Albania", "Belgium", "Bulgaria", "Canada", "Croatia", "Czech Republic", "Denmark", "Estonia", "France", "Germany", "Greece", "Hungary",  
                "Iceland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Turkey", "United Kingdom", "United States")  

library(gapminder)  
gapminder %>%  
  filter(year == 2002) %>%  
  rename(name = country) %>%   
  ggplot() +  
  aes(country_name = name) +
  geom_region(keep_id = nato_names) + 
  aes(fill = gdpPercap)
#> Joining with `by = join_by(country_name)`
```

![](man/figures/README-unnamed-chunk-28-1.png)<!-- -->

``` r

set_region_country_rnaturalearth()  
#> Region iscountry
#> Required aes: 'country_name|iso3c'

country_results_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-24/country_results_df.csv')
#> Rows: 3780 Columns: 18
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (3): country, leader, deputy_leader
#> dbl (14): year, team_size_all, team_size_male, team_size_female, p1, p2, p3,...
#> lgl  (1): p7
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

country_results_df %>% 
  filter(year == 2024) %>% 
  mutate(country = ifelse(country == "People's Republic of China", "China", country)) %>% 
  mutate(points = p1+p2+p3+p4+p5+p6) %>% 
  ggplot() + 
  aes(country_name = country) + 
  stamp_region(drop_id = "Antarctica") + 
  geom_region() + 
  aes(fill = points) + 
  aes(label = paste0("#", rank(-points),"\n",
                     country, "\n", points,"\n")) + 
  geom_region_text(size = 1.5,
                   lineheight = .7,
                   check_overlap = T) + 
  labs(title = "International Math Olympiad points earned in country_results_df.csv")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
```

![](man/figures/README-unnamed-chunk-29-1.png)<!-- -->

``` r

last_plot() + 
  aes(fill = points/team_size_all) + 
  aes(label = round((points)/team_size_all, 1))
#> Joining with `by = join_by(country_name)`
#> Joining with `by = join_by(country_name)`
```

![](man/figures/README-unnamed-chunk-29-2.png)<!-- -->

``` r
  
ggplot2::theme_set
#> function (new) 
#> {
#>     check_object(new, is.theme, "a {.cls theme} object")
#>     old <- ggplot_global$theme_current
#>     ggplot_global$theme_current <- new
#>     invisible(old)
#> }
#> <bytecode: 0x7fec8011ba18>
#> <environment: namespace:ggplot2>
```

## Canadian provinces

<https://mountainmath.github.io/cancensus/index.html>

``` r
nhl_player_births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_player_births.csv')
#> Rows: 8474 Columns: 9
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (5): first_name, last_name, birth_city, birth_country, birth_state_prov...
#> dbl  (3): player_id, birth_year, birth_month
#> date (1): birth_date
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

head(nhl_player_births) |> names()
#> [1] "player_id"            "first_name"           "last_name"           
#> [4] "birth_date"           "birth_city"           "birth_country"       
#> [7] "birth_state_province" "birth_year"           "birth_month"

# library(cancensus)
# provinces_data <- get_statcan_geographies(
#   census_year = 2021,
#   level = "PR",
#   type = "cartographic",
#   cache_path = NULL,
#   timeout = 1000,
#   refresh = FALSE,
#   quiet = FALSE
# )

set_region_province_canada_rnaturalearth <- function(){
  
  provinces_data <- rnaturalearth::ne_states(country = "Canada", returnclass = "sf")

  provinces_data %>% 
    select(prov_name = name) %>% 
    mutate(prov_name = ifelse(prov_name == "Québec", "Quebec", prov_name)) %>% 
    mutate(prov_name = ifelse(prov_name == "Yukon", "Yukon Territory", prov_name)) ->
  ref_data
  
  ref_data %>% 
    options(sf2stat.ref_data = ., 
          sf2stat.required_aes = "prov_name")
  
  message("Region has been set to canadian provinces\n
           required aes 'prov_name'")
  
  ref_data %>% sf::st_drop_geometry() %>% .[,1]
  
}

# provinces_data <- get_statcan_geographies(
#   census_year = 2021,
#   level = "PR",
#   type = "cartographic",
#   cache_path = NULL,
#   timeout = 1000,
#   refresh = FALSE,
#   quiet = FALSE
# )

# provinces_data <- rnaturalearth::ne_states(country = "Canada", returnclass = "sf")



set_region_province_canada_rnaturalearth()
#> Region has been set to canadian provinces
#> 
#>            required aes 'prov_name'
#>  [1] "British Columbia"          "Alberta"                  
#>  [3] "Saskatchewan"              "Manitoba"                 
#>  [5] "Ontario"                   "Quebec"                   
#>  [7] "New Brunswick"             "Yukon Territory"          
#>  [9] "Nunavut"                   "Newfoundland and Labrador"
#> [11] "Nova Scotia"               "Northwest Territories"    
#> [13] "Prince Edward Island"

library(ggplot2)

nhl_player_births |>
  filter(birth_country == "CAN") |>
  count(birth_state_province) |> 
  ggplot() +
  aes(prov_name = birth_state_province) + 
  geom_region() +
  aes(fill = n) +
  geom_region_text(aes(label = paste0(str_wrap(birth_state_province, 6),"\n", n)),
                   size = 2,
                   lineheight = .8)
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(prov_name)`
#> Joining with `by = join_by(prov_name)`
```

![](man/figures/README-unnamed-chunk-30-1.png)<!-- -->

## Netherlands province

``` r
library(tmap)
#> Breaking News: tmap 3.x is retiring. Please test v4, e.g. with
#> remotes::install_github('r-tmap/tmap')

data("NLD_prov")
data("NLD_muni")

set_region_province_netherlands_tmap <- function(){

NLD_prov %>% 
  select(prov_name = name, prov_code = code) %>% 
  options(sf2stat.ref_data = ., 
          sf2stat.required_aes = "prov_code|prov_name")
  
  message("Region has been set to netherland provinces\n
          required aes 'prov_code|prov_name")
}


set_subregion_municipality_netherlands_tmap <- function(){

NLD_muni %>% 
  select(muni_name = name, 
         muni_code = code, 
         prov_name = province
         ) %>% 
  options(sf2stat.ref_data_subregion = ., 
          sf2stat.required_aes_subregion = "muni_code|muni_name")
  
  message("Subregion has been set to netherland municipality\n
          required aes 'muni_code|muni_name")
}

set_region_province_netherlands_tmap()
#> Region has been set to netherland provinces
#> 
#>           required aes 'prov_code|prov_name
set_subregion_municipality_netherlands_tmap()
#> Subregion has been set to netherland municipality
#> 
#>           required aes 'muni_code|muni_name

NLD_prov %>% 
  sf::st_drop_geometry() %>% 
  ggplot() + 
  aes(prov_code = code) + 
  geom_region() + 
  aes(fill = population/100000) + 
  geom_region_text(check_overlap = T,
                   size = 2, 
                   color = "whitesmoke")
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> 
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> 
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> 
#> Joining with `by = join_by(prov_code)`
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> 
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> 
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> 
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> 
#> Joining with `by = join_by(prov_code)`
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
```

![](man/figures/README-unnamed-chunk-31-1.png)<!-- -->

``` r

last_plot() + 
  aes(label = round(population/100000, 3))
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> Joining with `by = join_by(prov_code)`old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> Joining with `by = join_by(prov_code)`old-style crs object detected; please recreate object with a recent sf::st_crs()
```

![](man/figures/README-unnamed-chunk-31-2.png)<!-- -->

``` r



NLD_muni %>% 
  sf::st_drop_geometry() %>%  
  ggplot() + 
  aes(muni_code = code) + 
  geom_subregion() + 
  aes(fill = population) + 
  geom_subregion_text(check_overlap = T,
                      size = 2, 
                      color = "whitesmoke") + 
  scale_fill_viridis_c(transform = "log", breaks = c(10000, 100000, 1000000))
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> Joining with `by = join_by(muni_code)`old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> Joining with `by = join_by(muni_code)`old-style crs object detected; please recreate object with a recent sf::st_crs()
```

![](man/figures/README-unnamed-chunk-31-3.png)<!-- -->

``` r

last_plot() + 
  stamp_region_outline(aes(prov_name = 1), linewidth = .25)
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> Joining with `by = join_by(muni_code)`old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> Joining with `by = join_by(muni_code)`old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
#> old-style crs object detected; please recreate object with a recent sf::st_crs()
```

![](man/figures/README-unnamed-chunk-31-4.png)<!-- -->

## US states

``` r
set_region_state_usmapdata <- function(){
  
  required_aes <- "state_name|state_abb|fips"
  
usmapdata::us_map() |> 
  select(state_name = full, state_abb = abbr, fips,
         geometry = geom) |>
  options(sf2stat.ref_data = _,
          sf2stat.required_aes = "state_name|state_abb|fips")
  
  message("required aes are 'state_name|state_abb|fips'")
  
}


set_region_state_usmapdata()
#> required aes are 'state_name|state_abb|fips'

USArrests  %>% 
  rownames_to_column("state") |>
  ggplot() + 
  aes(state_name = state) + 
  geom_region(alpha = .75) + 
  aes(fill = UrbanPop) + 
  scale_fill_viridis_c()
#> Joining with `by = join_by(state_name)`
```

![](man/figures/README-unnamed-chunk-32-1.png)<!-- -->

``` r

set_region_state_usmapdata()
#> required aes are 'state_name|state_abb|fips'


nhl_player_births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_player_births.csv')
#> Rows: 8474 Columns: 9
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (5): first_name, last_name, birth_city, birth_country, birth_state_prov...
#> dbl  (3): player_id, birth_year, birth_month
#> date (1): birth_date
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

nhl_player_births |>
  filter(birth_country == "USA") |>
  count(birth_state_province) |>
  ggplot() + 
  aes(state_name = birth_state_province, fill = n) + 
  stamp_region(fill = "darkgrey") +
  geom_region() + 
  geom_region_text(aes(label = n), 
                   color = "whitesmoke",
                   size = 3) + 
  labs(title = "NHL players from US states in nhl_player_births.csv")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(state_name)`
#> Joining with `by = join_by(state_name)`
```

![](man/figures/README-unnamed-chunk-32-2.png)<!-- -->

``` r




set_region_state_usmapdata()
#> required aes are 'state_name|state_abb|fips'


pride_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-11/pride_index.csv')
#> Rows: 238 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (3): campus_name, campus_location, community_type
#> dbl (2): rating, students
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.


pride_index %>% 
  mutate(state_abb = campus_location %>% 
           str_extract("..$")) %>%
  summarise(average_rating = mean(rating), 
            .by = state_abb) %>% 
  ggplot() + 
  aes(state_abb = state_abb, fill = average_rating) + 
  stamp_region() +
  geom_region() + 
  scale_fill_viridis_c() + 
  labs(title = "Mean pride index rating for colleges in pride_index.csv")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(state_abb)`
```

![](man/figures/README-unnamed-chunk-32-3.png)<!-- -->

``` r
  

set_region_state_usmapdata()
#> required aes are 'state_name|state_abb|fips'


eclipse_total_2024 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_total_2024.csv')
#> Rows: 3330 Columns: 10
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (2): state, name
#> dbl  (2): lat, lon
#> time (6): eclipse_1, eclipse_2, eclipse_3, eclipse_4, eclipse_5, eclipse_6
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.


eclipse_total_2024 %>% 
  distinct(state) %>%  
  ggplot() + 
  aes(state_abb = state) + 
  stamp_region() +
  geom_region(data = . %>% distinct(state),
              fill = "midnightblue") + 
  labs(title = "'eclipse_1' US state locations eclipse_total_2024.csv")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(state_abb)`
```

![](man/figures/README-unnamed-chunk-32-4.png)<!-- -->

## US counties

``` r
set_region_county_usmapdata <- function(){
  
  required_aes <- "state_name|state_abb|fips|county_name"
  
usmapdata::us_map("county" ) |> 
  select(county_name = county,
         state_name = full, 
         state_abb = abbr, 
         fips,     
         geometry = geom) |>
  mutate(county_name = str_remove(county_name, " County| Census Area| Municipality| Borough")) |> 
  options(sf2stat.ref_data = _,
          sf2stat.required_aes = "state_name|state_abb|fips")
  
  message("required aes are 'state_name|state_abb|fips|county_name'")
  
}

set_region_county_usmapdata()
#> required aes are 'state_name|state_abb|fips|county_name'

ggplot() + 
  aes(fips = 1) + 
  stamp_region()
```

![](man/figures/README-unnamed-chunk-33-1.png)<!-- -->

``` r
set_region_county_usmapdata()
#> required aes are 'state_name|state_abb|fips|county_name'

polling_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-16/polling_places.csv')
#> Rows: 461445 Columns: 15
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (13): state, county_name, jurisdiction, jurisdiction_type, precinct_id,...
#> date  (2): election_date, source_date
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

polling_places |>
  filter(year(election_date) == 2020) %>% 
  mutate(county_name = 
           stringr::str_to_title(county_name)) %>% 
  count(state, county_name) %>% 
  ggplot() + 
  aes(state_abb = state, 
      county_name = county_name,
      fill = n) + 
  stamp_region() + 
  geom_region() +
  labs(title = "Number of polling places by county, 2020 in polling_places.csv") +
  scale_fill_viridis_c(transform = "log", 
                       option = "magma",
                       breaks = c(1, 10, 100, 1000, 10000)) 
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(county_name, state_abb)`
```

![](man/figures/README-unnamed-chunk-34-1.png)<!-- -->

``` r

set_region_state_usmapdata()
#> required aes are 'state_name|state_abb|fips'


polling_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-16/polling_places.csv')
#> Rows: 461445 Columns: 15
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (13): state, county_name, jurisdiction, jurisdiction_type, precinct_id,...
#> date  (2): election_date, source_date
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

polling_places |>
  filter(year(election_date) == 2020) %>% 
  count(state) %>%  
  ggplot() + 
  aes(state_abb = state, fill = n) + 
  stamp_region() + 
  geom_region() + 
  labs(title = "Number of polling places, 2020")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(state_abb)`
```

![](man/figures/README-unnamed-chunk-34-2.png)<!-- -->

## aseg brain segments

``` r
set_region_aseg_ggseg <- function(){
  
  ggseg::aseg$data %>% 
  # filter(!is.na(label)) %>% 
  select(region) %>% 
  options(sf2stat.ref_data = ., 
          sf2stat.required_aes = "region")
  
}

set_region_aseg_ggseg()

ggplot() + 
  aes(region = 1) +
  stamp_region() + 
  stamp_region(keep_id = "amygdala", 
               fill = "orange") + 
  stamp_region(keep_id = "hippocampus",
               fill = "cadetblue")
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Error in st_transform.sfc(bbox, 4326): cannot transform sfc object with missing crs

set_region_aseg_ggseg()

ggplot() + 
  aes(region = 1) + 
  stamp_region() + 
  stamp_region(keep_id = c("amygdala", "hippocampus"),
               aes(fill = after_stat(region))) + 
  theme(legend.position = "top",
        legend.justification = "left") + 
  labs(fill = NULL)
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Error in st_transform.sfc(bbox, 4326): cannot transform sfc object with missing crs
```

## German voting districts

``` r
download.file("https://www.bundeswahlleiter.de/dam/jcr/f92e42fa-44f1-47e5-b775-924926b34268/btw17_geometrie_wahlkreise_geo_shp.zip", "btw17_geometrie_wahlkreise_geo_shp.zip")
unzip("btw17_geometrie_wahlkreise_geo_shp.zip")
```

``` r
set_region_wahlkreis17_germany_bundeswahlleiter_de <- function(){
  
  wahlkreis_17 <- sf::st_read("Geometrie_Wahlkreise_19DBT_geo.shp")
  
  
  wahlkreis_17 %>% 
  # filter(!is.na(label)) %>% 
  select(wahlkreis_num = WKR_NR,
         land_num = LAND_NR,
         land_name = LAND_NAME,
         wahlkreis_name = WKR_NAme) %>% 
  options(sf2stat.ref_data = ., 
          sf2stat.required_aes =
            "wahlkreis_num|land_num|land_name|wahlkreis_name")
  
}




# wahlkreis_17
```

# sf2stat Proposed usage

<https://twitter.com/EmilyRiederer/status/1816820773581127781>

``` r
# given some flatfile data of interest
read.csv("nc-midterms.csv") |> head()
#>   desc_county     n  cd_party  ind_vote
#> 1      ONSLOW 24406 0.2059283 0.3862985
#> 2     ROBESON 36367 0.5061306 0.4066599
#> 3    RANDOLPH 15867 0.1651505 0.4230793
#> 4       ANSON  9028 0.5674062 0.4267833
#> 5     HALIFAX 21875 0.5865712 0.4337829
#> 6       ROWAN 23667 0.2424922 0.4338108


# and being aware of geographic data with geometry shape column
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/sf/shape/nc.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27

# select relevant id columns (this will keep geometry column)
nc_ref <- nc |>
  select(county_name = NAME, fips = FIPS)



# do this routine, change out the ref data, required_aes and 'county' in convenience funtion names.
stat_county <- function(...){stat_region(ref_data = nc_ref, required_aes = "county_name|fips", id_index = 1, ...)}  # uses GeomSf as default

GeomOutline <- ggproto("GeomOutline", GeomSf,
                       default_aes = aes(!!!modifyList(GeomSf$default_aes,
                                                       aes(fill = NA, 
                                                           color = "black"))))

geom_county_sf <- function(...){stat_county(geom = GeomSf,...)}
geom_county <- geom_county_sf   # convenience short name
geom_county_outline <- function(...){stat_county(geom = GeomOutline, ...)}
geom_county_label <- function(...){stat_county(geom = GeomLabel,...)}
geom_county_text <- function(...){stat_county(geom = GeomText, ...)}

stamp_county_sf <- function(...){geom_county_sf(stamp = T, ...)}
stamp_county <- stamp_county_sf
stamp_county_outline <- function(...){geom_county_outline(stamp = T, ...)}
stamp_county_label <- function(...){geom_county_label(stamp = T, ...)}
stamp_county_text <- function(...){geom_county_text(stamp = T, ...)}
```

A first NC map shows when we map desc_county to county name.

``` r
read.csv("nc-midterms.csv") |>
  ggplot() + 
  aes(county_name = str_to_title(desc_county)) + 
  geom_county()
#> Joining with `by = join_by(county_name)`
```

![](man/figures/README-unnamed-chunk-39-1.png)<!-- -->

We see that there are actually undiscovered counties, as exact name
matching can be a little dicy. Using fips which would probably perform
better, which is possible with any plot data with an input fips column
(I decided to skip adding fips even though I had a cross-walk)

We can do the following ‘stamp’ convenience layer to get the full map. I
think of this as an annotation layer - it doesn’t refer to global data,
but ‘brings its own data’. annotate_county is just too long.

``` r
read.csv("nc-midterms.csv") |>
  ggplot() + 
  aes(county_name = str_to_title(desc_county)) + 
  stamp_county(fill = 'darkgrey')
```

![](man/figures/README-unnamed-chunk-40-1.png)<!-- -->

Then we use geom_county(), which reflects your data and the success of
the underlying join process.

``` r
last_plot() + 
  geom_county()
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(county_name)`
```

![](man/figures/README-unnamed-chunk-41-1.png)<!-- -->

Then look at population choropleth (fill = n) and highlight Mecklenburg
with convenience annotation layer ‘stamp_county’

``` r
options(scipen = 10)
last_plot() + 
  aes(fill = n/100000)
#> Joining with `by = join_by(county_name)`
```

![](man/figures/README-unnamed-chunk-42-1.png)<!-- -->

highlight at county of interest…

``` r
last_plot() + 
  stamp_county_outline(keep_id = "Mecklenburg", 
                       color = "orange",
                       linewidth = 1)
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(county_name)`
```

![](man/figures/README-unnamed-chunk-43-1.png)<!-- -->

We can add a text layer defaults to ref_data column 1 (id_index
setting)…

``` r
last_plot() +
   geom_county_text(color = "white", 
                    check_overlap = T, 
                    size = 2)
#> Coordinate system already present. Adding new coordinate system, which will
#> replace the existing one.
#> Joining with `by = join_by(county_name)`
#> Joining with `by = join_by(county_name)`
```

![](man/figures/README-unnamed-chunk-44-1.png)<!-- -->

We can look at another variable…

``` r
last_plot() + 
   aes(fill = cd_party) 
#> Joining with `by = join_by(county_name)`
#> Joining with `by = join_by(county_name)`
```

![](man/figures/README-unnamed-chunk-45-1.png)<!-- -->

And another…

``` r
last_plot() +
  aes(fill = ind_vote)
#> Joining with `by = join_by(county_name)`
#> Joining with `by = join_by(county_name)`
```

![](man/figures/README-unnamed-chunk-46-1.png)<!-- -->

And look at some values for that variable

``` r
last_plot() +
  aes(label = round(ind_vote, 2))
#> Joining with `by = join_by(county_name)`
#> Joining with `by = join_by(county_name)`
```

![](man/figures/README-unnamed-chunk-47-1.png)<!-- -->

``` r
knitr::knit_exit()
```
