tidyverse
---------

    library(tidyverse)

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.0.4     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    library(palmerpenguins)

    penguins_tidy <- penguins %>% 
      na.omit() %>% 
      select(species, flipper_length_mm) %>% 
      mutate(species = factor(species)) %>% 
      reduce(
        levels(.$species)[-1],
        ~ mutate(.x, !!paste0("species", .y) := as.integer(species == .y)),
        .init = .
      )

    lm(flipper_length_mm ~ species, data = penguins_tidy)

    ## 
    ## Call:
    ## lm(formula = flipper_length_mm ~ species, data = penguins_tidy)
    ## 
    ## Coefficients:
    ##      (Intercept)  speciesChinstrap     speciesGentoo  
    ##          190.103             5.721            27.133

    lm(flipper_length_mm ~ species, data = na.omit(penguins))

    ## 
    ## Call:
    ## lm(formula = flipper_length_mm ~ species, data = na.omit(penguins))
    ## 
    ## Coefficients:
    ##      (Intercept)  speciesChinstrap     speciesGentoo  
    ##          190.103             5.721            27.133

data.table
----------

    library(data.table)

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

    penguins_dt <- as.data.table(penguins)

    penguins_dt <- na.omit(penguins_dt)

    penguins_dt <- penguins_dt[, .(species, flipper_length_mm)][, `:=`(species = factor(species))]

    treatment_lvls <- levels(penguins_dt$species)[-1]
    treatment_cols <- paste0("species", treatment_lvls)

    penguins_dt[, (treatment_cols) := lapply(treatment_lvls, function(x){as.integer(species == x)})]

    penguins_dt[]

    ##        species flipper_length_mm speciesChinstrap speciesGentoo
    ##   1:    Adelie               181                0             0
    ##   2:    Adelie               186                0             0
    ##   3:    Adelie               195                0             0
    ##   4:    Adelie               193                0             0
    ##   5:    Adelie               190                0             0
    ##  ---                                                           
    ## 329: Chinstrap               207                1             0
    ## 330: Chinstrap               202                1             0
    ## 331: Chinstrap               193                1             0
    ## 332: Chinstrap               210                1             0
    ## 333: Chinstrap               198                1             0

    lm(flipper_length_mm ~ species, data = penguins_dt)

    ## 
    ## Call:
    ## lm(formula = flipper_length_mm ~ species, data = penguins_dt)
    ## 
    ## Coefficients:
    ##      (Intercept)  speciesChinstrap     speciesGentoo  
    ##          190.103             5.721            27.133

    lm(flipper_length_mm ~ species, data = na.omit(penguins))

    ## 
    ## Call:
    ## lm(formula = flipper_length_mm ~ species, data = na.omit(penguins))
    ## 
    ## Coefficients:
    ##      (Intercept)  speciesChinstrap     speciesGentoo  
    ##          190.103             5.721            27.133

benchmark
---------

    ver_tidy <- function() {
      penguins %>% 
        na.omit() %>% 
        select(species, flipper_length_mm) %>% 
        mutate(species = factor(species)) %>% 
        reduce(
          levels(.$species)[-1],
          ~ mutate(.x, !!paste0("species", .y) := as.integer(species == .y)),
          .init = .
        )
    }

    ver_dt <- function() {
      penguins_dt <- as.data.table(penguins)
      
      penguins_dt <- na.omit(penguins_dt)
      
      penguins_dt <- penguins_dt[, .(species, flipper_length_mm)][, `:=`(species = factor(species))]
      
      treatment_lvls <- levels(penguins_dt$species)[-1]
      treatment_cols <- paste0("species", treatment_lvls)
      
      penguins_dt[, (treatment_cols) := lapply(treatment_lvls, function(x){as.integer(species == x)})] 
    }

    microbenchmark::microbenchmark(ver_tidy(), ver_dt())

    ## Unit: milliseconds
    ##        expr    min       lq      mean   median       uq     max neval cld
    ##  ver_tidy() 8.0959 10.38315 13.871701 12.03090 16.48270 31.7803   100   b
    ##    ver_dt() 1.3551  1.77250  2.598675  2.07085  2.62895 23.2842   100  a
