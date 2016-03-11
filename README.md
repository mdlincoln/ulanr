# ulanr

Reconcile artist names to the Getty Union List of Artist Names.

[![Build Status](https://travis-ci.org/mdlincoln/ulanr.svg?branch=master)](https://travis-ci.org/mdlincoln/ulanr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mdlincoln/ulanr?branch=master)](https://ci.appveyor.com/project/mdlincoln/ulanr)

## Installation

You may install `ulanr` directly from GitHub. First install 
[devtools](http://cran.r-project.org/web/packages/devtools/index.html).
Then run the following commands to instal the ulanr package:

```r 
# install.packages("devtools")
devtools::install_github("mdlincoln/ulanr") 
```

Use of the `local` method for finding ULAN matches also requires the download of a data package:

```r
devtools::install_github("mdlincoln/ulanrdata")
```

## Usage

`ulan_match` takes a character vector of names, and returns a named list of candidate matches.

`method = "local"` will search for results from a table of the ULAN's alternate names (available through the [ulanrdata]() package), returning a named list with one data.frame per input name, each listing ULAN entites with alternate names that have a high character-level [cosine similarity](http://nlp.stanford.edu/IR-book/html/htmledition/dot-products-1.html) to the input names.
When the input name matches one of the ULAN alternate names exactly, the `local` method will return only those matches, without running costly string similarity measurements. (Note that it is possible for two separate individuals, e.g. Vincent van Gogh (1820-1888) and Vincent van Gogh (1853-1890) to match the same variant name spelling.)

``` r
library(ulanr)
ulan_match(c("Rembrandt van Rijn", "Vincent van Gogh"), method = "local")
#> $`Rembrandt van Rijn`
#> Source: local data frame [1 x 7]
#> 
#>          id          pref_name birth_year death_year gender nationality
#>       (int)              (chr)      (int)      (int)  (chr)       (chr)
#> 1 500011051 Rembrandt van Rijn       1606       1669   male       Dutch
#> Variables not shown: score (dbl)
#> 
#> $`Vincent van Gogh`
#> Source: local data frame [2 x 7]
#> 
#>          id         pref_name birth_year death_year gender nationality
#>       (int)             (chr)      (int)      (int)  (chr)       (chr)
#> 1 500337743 Gogh, Vincent van       1820       1888   male       Dutch
#> 2 500115588 Gogh, Vincent van       1853       1890   male       Dutch
#> Variables not shown: score (dbl)
```

An alternate method, `method = "sparql"`, works by directly querying the Getty's live [SPARQL endpoint](vocab.getty.edu/sparql), relying on their Lucene index to search for similar matches across the alternate and preferred names for all artists.

``` r
ulan_match(c("Rembrandt", "Vincent van Gogh"), method = "sparql")
#> $Rembrandt
#> Source: local data frame [4 x 7]
#> 
#>          id           pref_name birth_year death_year gender nationality
#>       (int)               (chr)      (int)      (int)  (chr)       (chr)
#> 1 500006691  Bugatti, Rembrandt       1884       1916   male     Italian
#> 2 500049481 Lockwood, Rembrandt       1815       1889   male    American
#> 3 500019719    Peale, Rembrandt       1778       1860   male    American
#> 4 500011051  Rembrandt van Rijn       1606       1669   male       Dutch
#> Variables not shown: score (dbl)
#> 
#> $`Vincent van Gogh`
#> Source: local data frame [5 x 7]
#> 
#>          id         pref_name birth_year death_year gender nationality
#>       (int)             (chr)      (int)      (int)  (chr)       (chr)
#> 1 500337743 Gogh, Vincent van       1820       1888   male       Dutch
#> 2 500341187   Gogh, V. W. van       1890       2010   male       Dutch
#> 3 500115588 Gogh, Vincent van       1853       1890   male       Dutch
#> 4 500339434    Gogh, Theo van       1857       1891   male       Dutch
#> 5 500099450   Gogh, Peter van       1900       2050   male       Dutch
#> Variables not shown: score (dbl)
```

`ulan_id` and `ulan_data` are wrapper functions for `ulan_match` that return a vector and data frame, respectively, of only the top matches for each name.

See the vignette included with this package for more information on restricting searches by life dates and maximum similarity scores.

# License

ulanr contains information from Union List of Artist Names (ULAN)® which is made available under the [ODC Attribution License](http://opendatacommons.org/licenses/by/1.0/).
