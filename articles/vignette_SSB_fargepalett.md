# SSB fargepallet fra KLASS

SSBs fargepalett, hentet fra SSBs Designhåndbok, er tilgjengelig fra
KLASS ([Kodeliste for SSB
fargepalett](https://www.ssb.no/klass/klassifikasjoner/614)). Denne kan
lastes inn i R med funksjonen `klassR`:

``` r
ssb_farger <- klassR::GetKlass(614, output_style = "wide") %>%
  dplyr::rename(farge_nummer = code3, 
                HEX = name3, 
                farge = name2, 
                type = name1) %>%
  dplyr::select(-code1, -code2)

ssb_farger
#>    farge_nummer     HEX farge           type
#> 11    SSB Blå 1 #dcf1fc   Blå Sekundærfarger
#> 12    SSB Blå 2 #83c1e9   Blå Sekundærfarger
#> 13    SSB Blå 3 #3396d2   Blå Sekundærfarger
#> 14    SSB Blå 4 #006cb6   Blå Sekundærfarger
#> 15    SSB Blå 5 #143f90   Blå Sekundærfarger
#> 16  SSB Grønn 1 #e3f1e6 Grønn   Primærfarger
#> 17  SSB Grønn 2 #90cc93 Grønn   Primærfarger
#> 18  SSB Grønn 3 #1a9d49 Grønn   Primærfarger
#> 19  SSB Grønn 4 #00824d Grønn   Primærfarger
#> 20  SSB Grønn 5 #075745 Grønn   Primærfarger
#> 21    SSB Gul 1 #f3ebc6   Gul Sekundærfarger
#> 22    SSB Gul 2 #e8d777   Gul Sekundærfarger
#> 23    SSB Gul 3 #d2bc2a   Gul Sekundærfarger
#> 24    SSB Gul 4 #b59924   Gul Sekundærfarger
#> 25    SSB Gul 5 #9a7b1c   Gul Sekundærfarger
#> 26    SSB Hvit  #ffffff  Hvit   Primærfarger
#> 27  SSB Lilla 1 #eae3f0 Lilla Sekundærfarger
#> 28  SSB Lilla 2 #c0b3d5 Lilla Sekundærfarger
#> 29  SSB Lilla 3 #9582bb Lilla Sekundærfarger
#> 30  SSB Lilla 4 #6d58a4 Lilla Sekundærfarger
#> 31  SSB Lilla 5 #472f91 Lilla Sekundærfarger
#> 32   SSB Mørk 1 #e7ecec  Mørk   Primærfarger
#> 33   SSB Mørk 2 #a2baba  Mørk Sekundærfarger
#> 34   SSB Mørk 3 #6f9090  Mørk Sekundærfarger
#> 35   SSB Mørk 4 #4b7272  Mørk Sekundærfarger
#> 36   SSB Mørk 5 #274247  Mørk   Primærfarger
#> 37    SSB Rød 1 #ffddca   Rød Sekundærfarger
#> 38    SSB Rød 2 #f8a67d   Rød Sekundærfarger
#> 39    SSB Rød 3 #f26539   Rød Sekundærfarger
#> 40    SSB Rød 4 #c4351c   Rød Sekundærfarger
#> 41    SSB Rød 5 #93180a   Rød Sekundærfarger
```
