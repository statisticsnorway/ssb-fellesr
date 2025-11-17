# Funksjon for aa laste inn .json-fil fra Google Cloud Storage

Funksjonen `read_json` kan brukes til aa lese inn .json-filer Google
Cloud Storage.

## Usage

``` r
read_json(file, ...)
```

## Arguments

- file:

  Full sti og navn paa filen som skal leses inn fra Google Cloud Storage
  bucket.

- ...:

  Flere parametere (se:
  https://arrow.apache.org/docs/r/reference/read_json_arrow.html)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- read_json("ssb-prod-spesh-personell-data-kilde/example_1.json")
} # }
```
