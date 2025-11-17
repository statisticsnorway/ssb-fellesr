# Funksjon for aa sjekke hvilke filer som finnes i en Google Cloud Storage bucket

Funksjonen `gcs_list_objects` kan brukes til aa sjekke hvilke filer som
finnes i en Google Cloud Storage bucket. I tillegg ser man stoerrelsen
til filene og tidspnktet filen sist ble endret.

## Usage

``` r
gcs_list_objects(bucket)
```

## Arguments

- bucket:

  Full sti til Google Cloud Storage bucket.

## Examples

``` r
if (FALSE) { # \dontrun{
gcs_list_objects("ssb-prod-dapla-felles-data-delt")

gcs_list_objects("ssb-prod-dapla-felles-data-delt") %>%
dplyr::filter(grepl("GIS", name) == T)
dplyr::arrange(desc(updated))

} # }
```
