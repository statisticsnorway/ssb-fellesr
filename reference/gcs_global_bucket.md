# Funksjon for aa koble til Google Cloud Storage bucket med googleCloudStorageR

`gcs_global_bucket` er en hjelpefunksjon som kobler til en bucket paa
Google Cloud Storage med pakken `googleCloudStorageR`. Autentiseringen
skjer via access_token og expiration som er lagret som miljoevariabler i
Jupyter paa DAPLA.

## Usage

``` r
gcs_global_bucket(bucket)
```

## Arguments

- bucket:

  Full sti til Google Cloud Storage bucket.

## Examples

``` r
if (FALSE) { # \dontrun{
gcs_global_bucket("ssb-prod-dapla-felles-data-delt")
bucket
} # }
```
