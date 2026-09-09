# Kopier metadata på datasett-nivå mellom Datadoc-filer

Kopierer metadata på datasett-nivå fra én Datadoc-fil til en annen.
Metadata for variabler blir ikke endret.

## Usage

``` r
copy_metadata_dataset(
  filsti_datadoc_egen,
  filsti_datadoc_original,
  overwrite = FALSE
)
```

## Arguments

- filsti_datadoc_egen:

  En tekststreng med filsti til Datadoc-filen som skal oppdateres. Kan
  også være filstien til den tilhørende Parquet-filen.

- filsti_datadoc_original:

  En tekststreng med filsti til Datadoc-filen metadata skal kopieres
  fra. Kan også være filstien til den tilhørende Parquet-filen.

- overwrite:

  Logisk verdi som angir om eksisterende metadata på datasett-nivå i
  målfilen skal overskrives. Standard er `FALSE`, slik at kun tomme
  eller manglende felt fylles ut. Tekniske felt som identifiserer
  målfilen overskrives ikke uavhengig av verdien til `overwrite`.

## Value

Returnerer den oppdaterte Datadoc-strukturen usynlig som en liste.
Datadoc-filen som er angitt i `filsti_datadoc_egen` oppdateres samtidig
på disk.

## Details

Tekniske felt som identifiserer målfilen beholdes fra målfilen og
overskrives ikke med verdier fra kildefilen. Dette gjelder blant annet
datasettets kortnavn, ID, filsti, eier og metadata om opprettelse.

Dato for siste oppdatering, `metadata_last_updated_date`, oppdateres
automatisk til tidspunktet funksjonen kjøres. Dato for opprettelse,
`metadata_created_date`, beholdes uendret.

Datasettets versjonsnummer og periode hentes automatisk fra filnavnet
til målfilen. Et filnavn på formen
`resultatregnskap-klargjort_p2025_v1.parquet` gir dermed
`version = "1"`, `contains_data_from = "2025-01-01"` og
`contains_data_until = "2025-12-31"`.

Både filstier til Parquet-filer og direkte filstier til Datadoc-filer
kan brukes. Dersom en Parquet-fil oppgis, konverteres filstien til
tilhørende Datadoc-fil.

Følgende felt beholdes fra målfilen og kopieres ikke fra kildefilen:

- `short_name`

- `version`

- `id`

- `owner`

- `file_path`

- `metadata_created_date`

- `metadata_created_by`

- `metadata_last_updated_by`

- `contains_data_from`

- `contains_data_until`

Feltet `metadata_last_updated_date` settes til tidspunktet funksjonen
kjøres. Tidspunktet lagres i UTC.

Feltet `version` erstattes med versjonsnummeret som hentes fra filnavnet
til målfilen. Feltene `contains_data_from` og `contains_data_until`
settes tilsvarende ut fra perioden i filnavnet.

Funksjonen forventer et filnavn der periode og versjon følger mønsteret
`_pYYYY_vN`, for eksempel `_p2025_v1`. Dersom periode eller versjon ikke
kan identifiseres, gis en advarsel og eksisterende verdi beholdes.

## Examples

``` r
if (FALSE) { # \dontrun{
copy_metadata_dataset(
  filsti_datadoc_egen =
    "/buckets/produkt/speshelse/klargjorte-data/regnskap/2025/resultatregnskap-klargjort_p2025_v1.parquet",
  filsti_datadoc_original =
    "/buckets/produkt/speshelse/klargjorte-data/regnskap/2024/resultatregnskap-klargjort_p2024_v3.parquet"
)

copy_metadata_dataset(
  filsti_datadoc_egen = "data/resultat_p2025_v2.parquet",
  filsti_datadoc_original = "data/resultat_p2024_v1.parquet",
  overwrite = TRUE
)
} # }
```
