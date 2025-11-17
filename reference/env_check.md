# Funksjon for aa sjekke hvilket miljoe man er i

`env_check` er en hjelpefunksjon som sjekker hvilket miljø man er i.
DAPLA_ENVIRONMENT: Angir miljøet (DEV, STAGING, TEST, PROD),
DAPLA_REGION: Viser kjøreområdet (ON_PREM, DAPLA_LAB, BIP, CLOUD_RUN) og
DAPLA_SERVICE: Identifiserer tjenesten (JUPYTERLAB, VS_CODE, R_STUDIO,
KILDOMATEN).

## Usage

``` r
env_check()
```

## Value

Karaktervektor: f.eks. PROD-ON_PREM-JUPYTERLAB eller PROD-BIP-JUPYTERLAB

## Examples

``` r
if (FALSE) { # \dontrun{
env_check()
} # }
```
