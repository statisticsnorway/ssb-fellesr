# Fellesfunksjoner i R for SSB (`fellesr`)

Her finner du en rekke funksjoner laget for å løse problemstillinger som er vanlige i SSB. Pakken kan installeres i et renv-miljø i Jupyterlab (på DAPLA og i prodsonen) og i RStudio Server (prodsonen). Følgende kode vil sette opp et `renv`-miljø og installere `fellesr`-pakken. Dette skal kun gjøres én gang per prosjekt. Les mer om hvordan å opprette et virtuelt miljø med `renv` i [Dapla-manualen](https://manual.dapla.ssb.no/jobbe-med-kode.html#r-og-renv)

```
renv::init()
renv::install("statisticsnorway/ssb-fellesr")
```
For å ta i bruk funksjoner i `fellesr` må du kalle biblioteket med

```
library(fellesr)
```

Se under «Articles» på **[fellesr nettsiden](https://statisticsnorway.github.io/ssb-fellesr/)** eller finner du en oppsummering nedenfor med linker for å velge emner.

Pakken `fellesr` inneholder følgende funksjoner:

#### Produksjonssonen
+ [Uttrekk fra Dynarev i R](https://statisticsnorway.github.io/ssb-fellesr/articles/vignette_dynarev_uttrekk.html) (`dynarev_uttrekk`)
+ [Laste opp data til Statistikkbanken](https://statisticsnorway.github.io/ssb-fellesr/articles/vignette_statbank_lasting.html) (`statbank_lasting`)
+ [Kjøre shiny og esquisse](https://statisticsnorway.github.io/ssb-fellesr/articles/vignette_shiny_ssb.html) (`runApp_ssb`, `runExample_ssb`, `esquisser_ssb`)

#### DAPLA
+ [Lese inn og skrive filer på DAPLA](https://statisticsnorway.github.io/ssb-fellesr/articles/vignette__DAPLA_jukseark.html) (`read_SSB`, `write_SSB`, `gcs_bucket`, `list.files`) 
+ [Laste opp data til Statistikkbanken](https://statisticsnorway.github.io/ssb-fellesr/articles/vignette_statbank_lasting.html) (`statbank_lasting`)
+ [Kjøre shiny og esquisse](https://statisticsnorway.github.io/ssb-fellesr/articles/vignette_shiny_ssb.html) (`runApp_ssb`, `runExample_ssb`, `esquisser_ssb`)

#### Visualisering
+ [SSB theme for ggplot](https://statisticsnorway.github.io/ssb-fellesr/articles/vignette_SSB_theme.html)
+ [SSB fargepallet fra KLASS](https://statisticsnorway.github.io/ssb-fellesr/articles/vignette_SSB_fargepalett.html)



### Andre pakker laget for bruk i SSB
I tillegg til `fellesr` har det blitt laget flere pakker for bruk i SSB. Under følger en foreløpig oversikt. Dersom du har laget en funksjon eller en pakke og ønsker å få lagt den til i `fellesr` eller i listen under er det bare å ta kontakt. 

+ [PxWebApiData](https://cran.r-project.org/web/packages/PxWebApiData/vignettes/Introduction.html) - Statistikkbankens API
+ [klassR](https://statisticsnorway.github.io/ssb-klassr/articles/klassR-vignette.html) - KLASS API
+ [SSBtools](https://github.com/statisticsnorway/ssb-ssbtools)
+ [SmallCountRounding](https://cran.r-project.org/web/packages/SmallCountRounding/vignettes/Introduction_to_SmallCountRounding.html)
+ [GaussSuppression](https://cran.r-project.org/web/packages/GaussSuppression/vignettes/define_tables.html)
+ [GISSB](https://statisticsnorway.github.io/GISSB/articles/GISSB_vignette.html) - Nettverksanalyse i R
+ [Kostra](https://github.com/statisticsnorway/ssb-kostra/)
+ [struktuR](https://github.com/statisticsnorway/struktuR)
+ [SSBpris](https://github.com/statisticsnorway/SSBpris)
+ [SdcForetakPerson](https://github.com/statisticsnorway/SdcForetakPerson)
+ [SSBcellKey](https://github.com/statisticsnorway/SSBcellKey)
+ [easySdcTable](https://cran.r-project.org/web/packages/easySdcTable/index.html)
+ [CalibrateSSB](https://github.com/statisticsnorway/sssb-calibrateSSB)
+ [RegSDC](https://cran.r-project.org/web/packages/RegSDC/index.html)


### Kursmateriell, veiledning og tips
+ [kurs-r-nybegynner-jupyter](https://github.com/statisticsnorway/kurs-r-nybegynner-jupyter)
+ [kurs-r-viderekomne](kurs-r-viderekomne)
+ [Metodebiblioteket](https://statisticsnorway.github.io/ssb-metodebiblioteket/catalogue_edit.html)
+ [R grunnkurs](https://github.com/statisticsnorway/kurs-r-grunnkurs-dapla)
+ [R for viderekomne](https://github.com/statisticsnorway/kurs-r-viderekomne)
+ [R i produksjon](https://github.com/statisticsnorway/kurs-r-produksjon/tree/main)
+ [Bruk av renv for pakke installering](https://manual.dapla.ssb.no/statistikkere/jobbe-med-kode.html#r-og-renv)
+ [Hvordan å bygge en tilleggspakke](https://statisticsnorway.github.io/ssb-fellesr/articles/web_only/bygge-en-r-tilleggspakke.html)
+ [Generelle veiledning til R programmering i SSB](https://wiki.ssb.no/display/s880/Veiledning+til+R+programmering+i+SSB)
+ [databehandling med dplyr jukseark](https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf)
+ [Kjøre R fra SAS](https://statisticsnorway.github.io/ssb-fellesr/articles/web_only/kjre-r-p-sl-stata-p3-fra-sas.html)


### Eksempler på produksjonsløp i R
+ [Spesialisthelsetjenesten - Personell](https://github.com/statisticsnorway/spesh-personell/tree/master/Personelltabeller)
+ [Verdsettelsesmodell-fritidsbolig](https://github.com/statisticsnorway/Verdsettelsesmodell-fritidsbolig)
