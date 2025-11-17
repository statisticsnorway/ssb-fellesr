# SSB-format

Klassen `ssb_format` gir funksjonalitet for å mappe kategoriske og
intervallbaserte verdier i henhold til en forhåndsdefinert kodeliste.

## Public fields

- `frmt_list`:

  En navngitt liste som spesifiserer mappingen.

- `is_range_format`:

  Boolsk verdi som angir om formatet er intervallbasert.

- `na_value`:

  Verdi som representerer manglende data, utledet fra `frmt_list`.

- `breaks`:

  En vektor av grenseverdier for intervallbasert mapping, utledet fra
  `frmt_list`.

- `labels`:

  En vektor med labeler for intervallene, utledet fra `frmt_list`.

## Methods

### Public methods

- [`ssb_format$new()`](#method-ssb_format-new)

- [`ssb_format$map_range()`](#method-ssb_format-map_range)

- [`ssb_format$map_cat()`](#method-ssb_format-map_cat)

- [`ssb_format$clone()`](#method-ssb_format-clone)

------------------------------------------------------------------------

### Method `new()`

Initialiserer SSB Format-klassen

#### Usage

    ssb_format$new(frmt_list, is_range_format)

#### Arguments

- `frmt_list`:

  En "named list" (tilsvarende dictionary i python) som spesifiserer
  mappingen.

- `is_range_format`:

  Boolsk verdi som angir om formatet er intervallbasert.

------------------------------------------------------------------------

### Method `map_range()`

Mapper numeriske verdier til kategoriske

#### Usage

    ssb_format$map_range(vec)

#### Arguments

- `vec`:

  En vektor med numeriske verdier som skal mappes.

#### Returns

En vektor med kategoriske verdier som er mappet.

#### Examples

    \dontrun{
    format <- ssb_format$new(frmt_list = list("10-20" = "A", "21-30" = "B", "other" = "C"), is_range_format = TRUE)
    format$map_range(c(15, 25, 35))
    }

------------------------------------------------------------------------

### Method `map_cat()`

Mapper kategorier til kategorier

#### Usage

    ssb_format$map_cat(vec)

#### Arguments

- `vec`:

  En vektor med kategoriske verdier som skal mappes.

#### Returns

En vektor med de mappede verdiene.

#### Examples

    \dontrun{
    format <- ssb_format$new(frmt_list = list("A" = "Alpha", "B" = "Beta"), is_range_format = FALSE)
    format$map_cat(c("A", "B"))
    }

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ssb_format$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `ssb_format$map_range`
## ------------------------------------------------

if (FALSE) { # \dontrun{
format <- ssb_format$new(frmt_list = list("10-20" = "A", "21-30" = "B", "other" = "C"), is_range_format = TRUE)
format$map_range(c(15, 25, 35))
} # }

## ------------------------------------------------
## Method `ssb_format$map_cat`
## ------------------------------------------------

if (FALSE) { # \dontrun{
format <- ssb_format$new(frmt_list = list("A" = "Alpha", "B" = "Beta"), is_range_format = FALSE)
format$map_cat(c("A", "B"))
} # }
```
