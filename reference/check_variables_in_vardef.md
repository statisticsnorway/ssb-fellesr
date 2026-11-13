# Kontroller om variabler finnes i Vardef

Kontrollerer om variabler finnes i ekstern eller intern Vardef,
uavhengig av gyldighetsdato og publiseringsstatus.

## Usage

``` r
check_variables_in_vardef(
  variabler,
  date = NULL,
  language = "nb",
  status = "auto",
  quiet = TRUE
)
```

## Arguments

- variabler:

  Karaktervektor med variabelnavn.

- date:

  Valgfri gyldighetsdato. NULL (standard) inkluderer alle definisjoner,
  også historiske.

- language:

  Språkkode: "nb", "nn" eller "en".

- status:

  "auto" (standard), "ekstern" eller "intern".

- quiet:

  Skjul statusmeldinger. Standard er TRUE.

## Value

En tibble med én rad per variabel og informasjon om treff i Vardef.
