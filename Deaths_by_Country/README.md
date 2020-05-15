Registered COVID-19 Deaths by Country
================
Tommi Suvitaival, <tommi.raimo.leo.suvitaival@regionh.dk>, Steno
Diabetes Center Copenhagen
2020-05-15

# Introduction

  - Country data from gapminder R-package
  - COVID-19 data from coronavirus R-package (dev)

# Load Packages

``` r
library( magrittr )

# devtools::install_github( "covid19r/coronavirus")
```

# Prepare Data

## Gapminder

``` r
# View( gapminder::gapminder )

data.gm <- 
  gapminder::gapminder %>% 
    dplyr::group_by( country ) %>% 
    dplyr::top_n( n = 1, year ) # suggested by helgebergo
```

## Coronavirus

``` r
data.covid <- 
  coronavirus::coronavirus %>%
  dplyr::group_by(
    Country.Region,
    type
  )  %>%
  dplyr::summarise(
    Deaths = sum( cases )
  )

data.covid$"Country.Region"[ data.covid$"Country.Region" == "US" ] <-
  "United States"
```

## Harmonize

``` r
data.gm$"country" <- as.character( data.gm$"country" )

data.gm[ data.gm$"country" == "Myanmar", "country" ] <- "Burma"
data.gm[ data.gm$"country" == "Czech Republic", "country" ] <- "Czechia"
data.gm[ data.gm$"country" == "Korea, Rep.", "country" ] <- "Korea, South"
data.gm[ data.gm$"country" == "Slovak Republic", "country" ] <- "Slovakia"
data.gm[ data.gm$"country" == "Swaziland", "country" ] <- "Eswatini"
data.gm[ data.gm$"country" == "Yemen, Rep.", "country" ] <- "Yemen"

data.covid[ 
  data.covid$"Country.Region" == "Congo (Kinshasa)",
  "Country.Region"
  ] <- 
  "Congo, Dem. Rep."

data.covid[ 
  data.covid$"Country.Region" == "Congo (Brazzaville)", 
  "Country.Region"
  ] <- 
  "Congo, Rep."

data.covid[ data.covid$"Country.Region" == "Taiwan*", "Country.Region" ] <- 
  "Taiwan"
```

## Integrate

``` r
data <-
  merge(
    x = data.gm,
    y = data.covid[ which( data.covid$"type" == "death" ), ],
    by.x = "country",
    by.y = "Country.Region",
    all = FALSE
  )

data$"Deaths_per_Capita" <- data$"Deaths" / data$"pop"

data$"Deaths_per_100k" <- data$"Deaths_per_Capita" * 100000
```

# Figures

## Deaths vs. Population Size

### All

``` r
ggplot2::ggplot(
  data = data,
  mapping =
    ggplot2::aes(
      x = pop,
      y = Deaths,
      label = country
    )
) +
  ggplot2::geom_point() +
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_log10() +
  ggrepel::geom_text_repel() +
  ggplot2::geom_smooth( method = "lm" ) +
  ggplot2::ylab( label = "Registered COVID-19 deaths\n(log)" ) +
  ggplot2::xlab( label = "Population size\n(log)" )
```

![](README_files/figure-gfm/Deaths-v-Population-1.png)<!-- -->

``` r
summary(
  lm(
    formula = Deaths ~ pop,
    data = data
  )
)
```

    ## 
    ## Call:
    ## lm(formula = Deaths ~ pop, data = data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -10381  -1639  -1536  -1460  71237 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 1.506e+03  7.079e+02   2.127   0.0352 *
    ## pop         9.694e-06  4.529e-06   2.141   0.0341 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7929 on 135 degrees of freedom
    ## Multiple R-squared:  0.03283,    Adjusted R-squared:  0.02566 
    ## F-statistic: 4.582 on 1 and 135 DF,  p-value: 0.0341

### By Continent

``` r
ggplot2::ggplot(
  data = data,
  mapping =
    ggplot2::aes(
      x = pop,
      y = Deaths,
      color = continent,
      label = country
    )
) +
  ggplot2::geom_point() +
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_log10() +
  ggrepel::geom_text_repel() +
  ggplot2::geom_smooth( method = "lm" ) +
  ggplot2::scale_color_brewer( palette = "Set1" ) +
  ggplot2::facet_wrap( facets = ggplot2::vars( continent ) ) +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::ylab( label = "Registered COVID-19 deaths\n(log)" ) +
  ggplot2::xlab( label = "Population size\n(log)" )
```

![](README_files/figure-gfm/Deaths-v-Population-By-Continent-1.png)<!-- -->

## Deaths per Capita vs. Life Expectancy

### All

``` r
ggplot2::ggplot(
  data = data,
  mapping =
    ggplot2::aes(
      x = lifeExp,
      y = Deaths_per_100k,
      label = country
    )
) +
  ggplot2::geom_point() +
  ggplot2::scale_y_log10() +
  ggrepel::geom_text_repel() +
  ggplot2::geom_smooth( method = "lm" ) +
  ggplot2::ylab( label = "Registered COVID-19 deaths\nper 100,000 inhabitants\n(log)" ) +
  ggplot2::xlab( label = "Life expectancy\n(years)" )
```

![](README_files/figure-gfm/Deaths-v-Life-Exp-1.png)<!-- -->

``` r
summary(
  lm(
    formula = Deaths_per_100k ~ lifeExp,
    data = data
  )
)
```

    ## 
    ## Call:
    ## lm(formula = Deaths_per_100k ~ lifeExp, data = data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -9.843 -5.876 -2.705  1.750 71.846 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -20.18839    5.60556  -3.601 0.000443 ***
    ## lifeExp       0.36904    0.08247   4.475 1.61e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.51 on 135 degrees of freedom
    ## Multiple R-squared:  0.1292, Adjusted R-squared:  0.1227 
    ## F-statistic: 20.03 on 1 and 135 DF,  p-value: 1.609e-05

### By Continent

``` r
ggplot2::ggplot(
  data = data,
  mapping =
    ggplot2::aes(
      x = lifeExp,
      y = Deaths_per_100k,
      color = continent,
      label = country
    )
) +
  ggplot2::geom_point() +
  ggplot2::scale_y_log10() +
  ggrepel::geom_text_repel() +
  ggplot2::geom_smooth( method = "lm", se = FALSE ) +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::scale_color_brewer( palette = "Set1" ) +
  ggplot2::ylab( label = "Registered COVID-19 deaths\nper 100,000 inhabitants\n(log)" ) +
  ggplot2::xlab( label = "Life expectancy\n(years)" )
```

![](README_files/figure-gfm/Deaths-v-Life-Exp-by-Continent-1.png)<!-- -->

## Deaths per Capita vs. Gross Domestic Product per Capita

### All

``` r
ggplot2::ggplot(
  data = data,
  mapping =
    ggplot2::aes(
      x = gdpPercap,
      y = Deaths_per_100k,
      label = country
    )
) +
  ggplot2::geom_point() +
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_log10() +
  ggrepel::geom_text_repel() +
  ggplot2::geom_smooth( method = "lm" ) +
  ggplot2::ylab( label = "Registered COVID-19 deaths\nper 100,000 inhabitants\n(log)" ) +
  ggplot2::xlab( label = "Gross domestic product (GDP)\nper capita\n(log)" )
```

![](README_files/figure-gfm/Deaths-v-GDP-1.png)<!-- -->

``` r
data.model <- data

data.model$"Deaths_per_100k_log" <- log10( data.model$"Deaths_per_100k" )

data.model$"gdpPercap_log" <- log10( data.model$"gdpPercap" )

data.model <- data.model[ !is.infinite( data.model$"Deaths_per_100k_log" ), ]

summary(
  lm(
    formula = Deaths_per_100k_log ~ gdpPercap_log,
    data = data.model
  )
)
```

    ## 
    ## Call:
    ## lm(formula = Deaths_per_100k_log ~ gdpPercap_log, data = data.model)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.08411 -0.42733  0.02937  0.48061  1.33608 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    -4.3229     0.3992  -10.83   <2e-16 ***
    ## gdpPercap_log   1.0812     0.1041   10.39   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6742 on 124 degrees of freedom
    ## Multiple R-squared:  0.4653, Adjusted R-squared:  0.461 
    ## F-statistic: 107.9 on 1 and 124 DF,  p-value: < 2.2e-16

``` r
summary(
  lm(
    formula = Deaths_per_100k_log ~ gdpPercap_log + lifeExp,
    data = data.model
  )
)
```

    ## 
    ## Call:
    ## lm(formula = Deaths_per_100k_log ~ gdpPercap_log + lifeExp, data = data.model)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.08045 -0.41306  0.01341  0.49102  1.34078 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -4.389353   0.401448 -10.934  < 2e-16 ***
    ## gdpPercap_log  0.897337   0.175953   5.100 1.25e-06 ***
    ## lifeExp        0.011256   0.008698   1.294    0.198    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6724 on 123 degrees of freedom
    ## Multiple R-squared:  0.4725, Adjusted R-squared:  0.4639 
    ## F-statistic: 55.08 on 2 and 123 DF,  p-value: < 2.2e-16

``` r
ggplot2::ggplot(
  data = data,
  mapping =
    ggplot2::aes(
      x = gdpPercap,
      y = Deaths_per_100k,
      label = country
    )
) +
  ggplot2::geom_smooth( method = "lm" ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes( color = continent )
  ) +
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_log10() +
  ggrepel::geom_text_repel(
    mapping = ggplot2::aes( color = continent )
  ) +
  ggplot2::scale_color_brewer( palette = "Set1" ) +
  ggplot2::ylab( label = "Registered COVID-19 deaths\nper 100,000 inhabitants\n(log)" ) +
  ggplot2::xlab( label = "Gross domestic product (GDP)\nper capita\n(log)" )
```

![](README_files/figure-gfm/Deaths-v-GDP-Color-1.png)<!-- -->

### By Continent

``` r
ggplot2::ggplot(
  data = data,
  mapping =
    ggplot2::aes(
      x = gdpPercap,
      y = Deaths_per_100k,
      color = continent,
      label = country
    )
) +
  ggplot2::geom_point() +
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_log10() +
  ggrepel::geom_text_repel() +
  ggplot2::geom_smooth( method = "lm", se = FALSE ) +
  ggplot2::scale_color_brewer( palette = "Set1" ) +
  ggplot2::ylab( label = "Registered COVID-19 deaths\nper 100,000 inhabitants\n(log)" ) +
  ggplot2::xlab( label = "Gross domestic product (GDP)\nper capita\n(log)" )
```

![](README_files/figure-gfm/Deaths-v-GDP-by-Continent-1.png)<!-- -->

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
    ## other attached packages:
    ## [1] magrittr_1.5
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.3         knitr_1.27         munsell_0.5.0      tidyselect_1.0.0  
    ##  [5] colorspace_1.4-1   R6_2.4.1           rlang_0.4.6        stringr_1.4.0     
    ##  [9] dplyr_0.8.3        tools_3.6.2        grid_3.6.2         gtable_0.3.0      
    ## [13] xfun_0.12          htmltools_0.4.0    ellipsis_0.3.0     coronavirus_0.2.0 
    ## [17] lazyeval_0.2.2     yaml_2.2.0         digest_0.6.23      assertthat_0.2.1  
    ## [21] tibble_3.0.1       lifecycle_0.2.0    crayon_1.3.4       RColorBrewer_1.1-2
    ## [25] farver_2.0.3       ggplot2_3.2.1      purrr_0.3.3        gapminder_0.3.0   
    ## [29] vctrs_0.2.4        ggrepel_0.8.1      glue_1.3.1         evaluate_0.14     
    ## [33] rmarkdown_2.1      labeling_0.3       stringi_1.4.4      compiler_3.6.2    
    ## [37] pillar_1.4.3       scales_1.1.0       pkgconfig_2.0.3
