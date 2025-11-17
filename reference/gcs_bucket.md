# Funksjon for aa koble til Google Cloud Storage bucket med arrow

`gcs_bucket` er en hjelpefunksjon som kobler til en bucket paa Google
Cloud Storage med pakken `arrow`. Autentiseringen skjer via access_token
og expiration som er lagret som miljoevariabler i Jupyter paa DAPLA.

## Usage

``` r
gcs_bucket(bucket)
```

## Arguments

- bucket:

  Full sti til Google Cloud Storage bucket.

## Examples

``` r
if (FALSE) { # \dontrun{
bucket <- gcs_bucket("ssb-prod-dapla-felles-data-delt")
bucket
} # }
```
