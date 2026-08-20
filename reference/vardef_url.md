# Hent URL til tjenesten for variabeldefinisjoner

Returnerer endepunktet til SSBs tjeneste for variabeldefinisjoner.

## Usage

``` r
vardef_url(status = "ekstern")
```

## Arguments

- status:

  En tekststreng som angir hvilken tjeneste URL-en skal peke til.
  Gyldige verdier er `"ekstern"` og `"intern"`.

## Value

En tekststreng med URL-en til den valgte tjenesten.

## Examples

``` r
vardef_url()
#> [1] "https://metadata.ssb.no/public/variable-definitions"

vardef_url(status = "intern")
#> [1] "https://metadata.intern.ssb.no/variable-definitions"
```
