# ulanr

Reconcile artist names to the Getty Union List of Artist Names.

## Installation

You may install `ulanr` directly from GitHub. First install 
[devtools](http://cran.r-project.org/web/packages/devtools/index.html).
Then run the following commands to instal the ulanr package:

```r 
# install.packages("devtools")
devtools::install_github("mdlincoln/ulanr") 
```

## Usage

ulanr's main method is `ulan_id` which takes a name as a character vector, along with an optional date range.
This will construct a SPARQL query fired to the [Getty's endpoint](http://vocab.getty.edu/sparql), and return a canonical ULAN id in the form of a 9-digit number.
You may restrict the search results to artists whose life dates intersect with a range of years.
This can be useful to differentiate, for example, between ["Rembrandt van Rijn"](http://vocab.getty.edu/ulan/500011051) and ["Rembrandt Peale"](http://vocab.getty.edu/ulan/500019719).

```r
> ulan_id("Rembrandt")
[1] 500011051
> ulan_id("Rembrandt", years = c(1700, 1800))
[1] 500019719
> ulan_id(c("Mark Rothko", "Rembrandt"))
[1] 500014869 500011051
```

The `ulan_data` function returns a dataframe containing the input names vector, and a series of columns with ULAN id, preferred name, and other artist attributes.

```r
> ulan_data(c("Mark Rothko", "Rembrandt van Rijn"))
Source: local data frame [2 x 7]

                name        id          pref_name birth_year death_year gender nationality
1        Mark Rothko 500014869       Rothko, Mark       1903       1970   male    American
2 Rembrandt van Rijn 500011051 Rembrandt van Rijn       1606       1669   male       Dutch
```

# License

ulanr contains information from Union List of Artist Names (ULAN)® which is made available under the [ODC Attribution License](http://opendatacommons.org/licenses/by/1.0/).
