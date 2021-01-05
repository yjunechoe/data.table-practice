tidyverse
---------

    library(tidyverse)
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

    penguins_tidy

    ## # A tibble: 333 x 4
    ##    species flipper_length_mm speciesChinstrap speciesGentoo
    ##    <fct>               <int>            <int>         <int>
    ##  1 Adelie                181                0             0
    ##  2 Adelie                186                0             0
    ##  3 Adelie                195                0             0
    ##  4 Adelie                193                0             0
    ##  5 Adelie                190                0             0
    ##  6 Adelie                181                0             0
    ##  7 Adelie                195                0             0
    ##  8 Adelie                182                0             0
    ##  9 Adelie                191                0             0
    ## 10 Adelie                198                0             0
    ## # ... with 323 more rows

data.table
----------

    library(data.table)

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

benchmark
---------

    microbenchmark::microbenchmark(ver_tidy(), ver_dt())

    ## Unit: milliseconds
    ##        expr    min       lq      mean   median       uq     max neval cld
    ##  ver_tidy() 9.9916 12.83540 18.137178 16.54765 21.76255 49.5573   100   b
    ##    ver_dt() 1.6799  2.13705  3.476468  2.43180  3.50295 24.5549   100  a
