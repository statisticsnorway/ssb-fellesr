# Erstatt filsti i en Datadoc-fil

Oppdaterer filstien som er lagret i datasettmetadataene i en Datadoc
JSON-fil.

## Usage

``` r
replace_file_path(filsti, ny_filsti)
```

## Arguments

- filsti:

  Tekststreng. Filsti til Datadoc JSON-filen som skal oppdateres.

- ny_filsti:

  Tekststreng. Ny filsti som skal lagres i `datadoc$dataset$file_path`.

## Value

Det oppdaterte Datadoc-objektet som en liste. Objektet returneres
usynlig.

## Details

Funksjonen leser inn en eksisterende Datadoc JSON-fil, erstatter verdien
i `datadoc$dataset$file_path` og skriver de oppdaterte metadataene
tilbake til den samme JSON-filen.

Datadoc JSON-filen angitt i `filsti` overskrives med den oppdaterte
versjonen. Den eksisterende verdien i `datadoc$dataset$file_path`
erstattes med verdien angitt i `ny_filsti`.

## Examples

``` r
if (FALSE) { # \dontrun{
replace_file_path(
  filsti = "data/eksempel__DOC.json",
  ny_filsti = "data/eksempel.parquet"
)
} # }
```
