Mortality in Sweden
================
Tommi Suvitaival, <tommi.raimo.leo.suvitaival@regionh.dk>, Steno
Diabetes Center Copenhagen
2020-05-11

# Load Data

  - Data from Statistics Sweden SCB:
      - Överdödligheten sjunker i
        Sverige
      - <https://www.scb.se/om-scb/nyheter-och-pressmeddelanden/overdodligheten-sjunker-i-sverige/>
      - Preliminär statistik över döda
        (excelfil)
      - <https://www.scb.se/hitta-statistik/statistik-efter-amne/befolkning/befolkningens-sammansattning/befolkningsstatistik/pong/tabell-och-diagram/preliminar-statistik-over-doda/>

<!-- end list -->

``` r
data.loaded <-
  readxl::read_excel(
    path = "H:/projects/covid19/data/SCB/2020-05-08---preliminar-statistik-over-doda-inkl-eng.xlsx",
    sheet = "Tabell 7",
    skip = 8
  )
```

    ## New names:
    ## * `M 0-64 år` -> `M 0-64 år...3`
    ## * `M 65-79 år` -> `M 65-79 år...4`
    ## * `M 80-89 år` -> `M 80-89 år...5`
    ## * `M 90+ år` -> `M 90+ år...6`
    ## * `K 0-64 år` -> `K 0-64 år...7`
    ## * ...

# Prepare Data

## Add Unassigned Deaths to Total

``` r
data <- data.loaded

data[ data == 0 ] <- NA

tmp <- which( data$"Vecka" == "Uppgift saknas" ) 

idx.data <- 2:19

data.before.adding <- data

for ( i in 1:length( idx.data ) ) {
  
  idx.obs.i <- which( !is.na( data[ -tmp, idx.data[ i ] ] ) )
  
  data[ idx.obs.i, idx.data[ i ] ] <- 
    data[ idx.obs.i, idx.data[ i ] ] +
    unlist( data[ tmp, idx.data[ i ] ] ) / length( idx.obs.i )
  
}
```

## Bind Periods

``` r
tmp <- data.loaded

colnames( tmp ) <-
  stringr::str_replace(
    string = colnames( tmp ),
    pattern = "\\.\\.\\.[0-9]+$",
    replacement = ""
  )

data <- tmp[ , 1:10 ]

data$"Period" <- colnames( data )[ 2 ]

colnames( data )[ 2 ] <- "Total"

tmp <- tmp[ , c( 1, 11:19 ) ]

tmp$"Period" <- colnames( tmp )[ 2 ]

colnames( tmp )[ 2 ] <- "Total"

tmp[ tmp == 0 ] <- NA

data <-
  dplyr::bind_rows(
    data,
    tmp
  )

tmp <- data

data <- data[ , -c( 2, 7:10 ) ]

colnames( data ) <-
  stringr::str_replace(
    string = colnames( data ),
    pattern = "^M ",
    replacement = ""
  )

tmp <- tmp[ , -( 2:6 ) ]

colnames( tmp ) <-
  stringr::str_replace(
    string = colnames( tmp ),
    pattern = "^K ",
    replacement = ""
  )

data <-
  dplyr::bind_rows(
    Male = data,
    Female = tmp,
    .id = "Sex"
  )

data$"Period" <-
  relevel(
    x = as.factor( data$"Period" ),
    ref = "2020"
  )
```

## Translate

``` r
colnames( data )[ colnames( data ) == "Vecka" ] <- "Week"

colnames( data ) <-
  stringr::str_replace(
    string = colnames( data ),
    pattern = "år",
    replacement = "years"
  )
```

## Pivot to Longer Format

``` r
data.plot <- data

data.plot$"Week" <- as.numeric( data.plot$"Week" )
```

    ## Warning: NAs introduced by coercion

``` r
data.plot <- data.plot[ !is.na( data.plot$"Week" ), ]

data.plot <-
  tidyr::pivot_longer(
    data = data.plot,
    cols =
      colnames( data.plot )[
        grep(
          x = colnames( data.plot ),
          pattern = "(Total)|(years)"
        )
        ],
    names_to = "Age",
    values_to = "Deaths"
  )
```

## Omit Incomplete Weeks

``` r
data.plot <-
  dplyr::filter(
    .data = data.plot,
    Week != 1 &
      Week < 51
  )

tmp <- 
  data.plot$"Period" == levels( data.plot$"Period" )[ 1 ] &
  !is.na( data.plot$"Deaths" )

tmp <- data.plot[ tmp, "Week" ]

tmp <- max( unlist( tmp ) )

data.plot <-
  dplyr::filter(
    .data = data.plot,
    # Week < tmp - 1 |
    Week < tmp |
      Period != levels( data.plot$"Period" )[ 1 ]
  )
```

# Plot

## By Sex

``` r
ggplot2::ggplot(
  data = data.plot,
  mapping =
    ggplot2::aes(
      x = Week,
      y = Deaths,
      color = Age,
      linetype = Period,
      shape = Period
    )
) +
  ggplot2::geom_point( alpha = 0.75 ) +
  ggplot2::geom_smooth() +
  ggplot2::scale_y_log10() +
  ggplot2::facet_grid(
    col = ggplot2::vars( Sex )
  ) +
  ggplot2::scale_color_brewer(
    palette = "Set1",
    direction = -1
  ) +
  ggplot2::theme(
    legend.position = "top",
    legend.direction = "vertical"
  )
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

<img src="README_files/figure-gfm/Plot-Sex-1.png" width="75%" />

## By Age Groups

``` r
ggplot2::ggplot(
  data = data.plot,
  mapping =
    ggplot2::aes(
      x = Week,
      y = Deaths,
      color = Period,
      linetype = Sex,
      shape = Sex
    )
) +
  ggplot2::geom_point( alpha = 0.75 ) +
  ggplot2::geom_smooth() +
  ggplot2::scale_y_log10() +
  ggplot2::facet_wrap(
    facets = ggplot2::vars( Age )
  ) +
  ggplot2::scale_color_brewer(
    palette = "Set1"
    # ,
    # direction = -1
  )
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

<img src="README_files/figure-gfm/Plot-Age-1.png" width="75%" />

## By Age Groups and Sex

``` r
ggplot2::ggplot(
  data = data.plot,
  mapping =
    ggplot2::aes(
      x = Week,
      y = Deaths,
      color = Period,
      linetype = Period,
      shape = Period
    )
) +
  ggplot2::geom_point( alpha = 0.75 ) +
  ggplot2::geom_smooth() +
  ggplot2::scale_y_log10() +
  ggplot2::facet_grid(
    cols = ggplot2::vars( Sex ),
    rows = ggplot2::vars( Age ),
    scales = "free_y"
  ) +
  ggplot2::scale_color_brewer(
    palette = "Set1"
    # ,
    # direction = -1
  )
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

<img src="README_files/figure-gfm/Plot-Age-and-Sex-1.png" width="75%" />

``` r
ggplot2::ggplot(
  data = data.plot,
  mapping =
    ggplot2::aes(
      x = Week,
      y = Deaths,
      color = Period,
      linetype = Period,
      shape = Period
    )
) +
  ggplot2::geom_point( alpha = 0.75 ) +
  ggplot2::geom_smooth() +
  ggplot2::scale_y_log10() +
  ggplot2::facet_grid(
    rows = ggplot2::vars( Sex ),
    cols = ggplot2::vars( Age )
  ) +
  ggplot2::scale_color_brewer(
    palette = "Set1"
  ) +
  ggplot2::theme(
    legend.position = "top"
  )
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

<img src="README_files/figure-gfm/Plot-Age-and-Sex-B-1.png" width="75%" />

# Appendix

``` r
utils::sessionInfo()
```

    ## R version 3.6.2 (2019-12-12)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 17763)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.3         knitr_1.27         magrittr_1.5       munsell_0.5.0     
    ##  [5] tidyselect_1.0.0   colorspace_1.4-1   R6_2.4.1           rlang_0.4.6       
    ##  [9] plyr_1.8.5         stringr_1.4.0      dplyr_0.8.3        tools_3.6.2       
    ## [13] grid_3.6.2         gtable_0.3.0       xfun_0.12          htmltools_0.4.0   
    ## [17] ellipsis_0.3.0     lazyeval_0.2.2     yaml_2.2.0         readxl_1.3.1      
    ## [21] digest_0.6.23      assertthat_0.2.1   tibble_3.0.1       lifecycle_0.2.0   
    ## [25] crayon_1.3.4       farver_2.0.3       RColorBrewer_1.1-2 reshape2_1.4.3    
    ## [29] ggplot2_3.2.1      tidyr_1.0.0        purrr_0.3.3        vctrs_0.2.4       
    ## [33] glue_1.3.1         evaluate_0.14      rmarkdown_2.1      labeling_0.3      
    ## [37] stringi_1.4.4      compiler_3.6.2     pillar_1.4.3       cellranger_1.1.0  
    ## [41] scales_1.1.0       pkgconfig_2.0.3
