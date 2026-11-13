# Kopier metadata fra Vardef til en Datadoc-fil

Kopierer navn, Vardef-ID og første enhetstype ved treff på kortnavn.
KLASS-ID og øvrige dokumentasjonsfelt endres ikke.

## Usage

``` r
copy_vardef_to_datadoc(
  filsti,
  variabler = NULL,
  overwrite = FALSE,
  date = NULL,
  dato_unntak = NULL,
  language = "nb",
  status = "auto",
  quiet = TRUE,
  data_source = "75",
  temporality_type = NULL
)
```

## Arguments

- filsti:

  Filsti til Datadoc JSON eller tilhørende Parquet-fil.

- variabler:

  Valgfri karaktervektor med kortnavn. NULL velger alle. En navngitt
  vektor kan brukes til mapping mellom Datadoc og Vardef, f.eks.
  c(komm_nr_2019 = "komm_nr").

- overwrite:

  Med FALSE (standard) hoppes hele variabelen over dersom name,
  definition_uri eller unit_type allerede har innhold. TRUE erstatter
  disse feltene når Vardef har en verdi. Manglende kildeverdier sletter
  ikke eksisterende innhold.

- date:

  Valgfri gyldighetsdato for oppslag i Vardef, som Date eller
  YYYY-MM-DD. NULL (standard) inkluderer alle gyldighetsperioder.

- dato_unntak:

  NULL eller en data frame med egne dataperioder for enkeltvariabler. Må
  inneholde short_name, contains_data_from og contains_data_until.
  Kortnavnene viser til Datadoc, ikke Vardef. Datoene brukes bare til å
  oppdatere Datadoc og påvirker ikke oppslag eller filtrering i Vardef.
  Datoene oppdateres bare for variabler som får treff i Vardef og som
  ikke hoppes over.

- language:

  Språk for navnet: nb, nn eller en. Standard nb.

- status:

  auto (standard) søker først eksternt og deretter internt for kortnavn
  uten eksternt treff.

- quiet:

  Skjul statusmeldinger. Standard TRUE. Advarsler og feil vises
  fortsatt.

- data_source:

  Tekstkode for datakilde. Standard er `"75"` (Delte data fra SSB).

- temporality_type:

  Valgfri temporalitetstype (f.eks. `"STATUS"`).

## Value

Usynlig tibble med variabel, vardef_kortnavn, resultat og endrede_felt.

## Details

Første ikke-tomme enhetstype fra Vardef brukes. Navn kopieres bare på
valgt språk, uten fallback. Publiseringsstatus filtreres ikke. Flere
motstridende definisjoner gir advarsel og hoppes over.
