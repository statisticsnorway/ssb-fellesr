# Erstatt filsti i Datadoc-metadata

Oppdaterer filstien som er lagret i datasettmetadataene i en Datadoc
JSON-fil.

## Usage

``` r
replace_file_path(filsti_datadoc, file_path)
```

## Arguments

- filsti_datadoc:

  Tekststreng. Filsti til Datadoc JSON-filen som skal oppdateres.

- file_path:

  Tekststreng. Ny filsti som skal lagres i `datadoc$dataset$file_path`.

## Value

Det oppdaterte Datadoc-objektet som en liste. Objektet returneres
usynlig.

## Details

Funksjonen leser inn en eksisterende Datadoc JSON-fil, erstatter verdien
i `datadoc$dataset$file_path` og skriver de oppdaterte metadataene
tilbake til den samme JSON-filen.

Datadoc JSON-filen angitt i `filsti_datadoc` overskrives med den
oppdaterte versjonen. Den eksisterende verdien i
`datadoc$dataset$file_path` erstattes med verdien angitt i `file_path`.

## Examples

``` r
if (FALSE) { # \dontrun{
replace_file_path(
  filsti_datadoc = "data/eksempel__DOC.json",
  file_path = "data/eksempel.parquet"
)
} # }
```
