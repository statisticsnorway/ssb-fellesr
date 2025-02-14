#' Opprett mappestruktur
#'
#' Funksjonen oppretter en standard mappestruktur basert på en filsti til en arbeidsmappe, og returnerer en liste
#' som inneholder stiene til hver undermappe. Det er også mulig å spesifisere en valgfri parameter `periode`
#' for å opprette en undermappe med det navnet i hver av hovedmappene.
#'
#' @param arbeidsmappe En karakterstreng som spesifiserer hovedmappen der de nye undermappene skal opprettes.
#' Hvis denne stien slutter med en skråstrek (`/`), fjernes den automatisk. Dersom `arbeidsmappe` ikke
#' eksisterer, vil funksjonen automatisk opprette den.
#' @param mapper En karaktervektor som inneholder navnene på undermappene som skal opprettes. Standardverdien er
#' `c("inndata", "klargjorte-data", "statistikk", "utdata")`. Det er mulig å legge til flere mapper (f.eks. "oppdrag").
#' @param periode En valgfri karakterstreng som spesifiserer en undermappe som opprettes i hver av hovedmappene.
#' Hvis denne ikke er spesifisert (default er `NULL`), opprettes ingen slike undermapper.
#'
#' @details
#' Denne funksjonen går gjennom en liste av mapper, og for hver mappe:
#' - Fjerner en eventuell avsluttende skråstrek (`/`) fra hovedmappen.
#' - Oppretter `arbeidsmappe` dersom den ikke allerede finnes.
#' - Oppretter undermappen inne i arbeidsmappen dersom den ikke allerede eksisterer.
#' - Hvis `periode` er spesifisert, oppretter funksjonen også en undermappe med navnet spesifisert av `periode`
#'   i hver hovedmappe.
#' - Erstatt bindestreker i undermappenavn med understrek for å lage gyldige variabelnavn.
#' - Samler hver undermappe-sti i en liste og returnerer denne listen.
#'
#' Variabelnavnene i listen har formatet `mappe_navn_mappe`, der eventuelle bindestreker i mappe-navn
#' erstattes med understrek.
#'
#' @return En liste der hver nøkkel er navnet på en undermappe (med understrek i stedet for bindestreker),
#' og verdiene er de fulle stiene til de respektive mappene.
#'
#' @examples
#' # Opprett standard mappestruktur i en mappe "prosjekt"
#' mapper <- opprett_mappestruktur("prosjekt")
#' print(mapper)
#'
#' # Opprett en egendefinert mappestruktur
#' mapper <- opprett_mappestruktur("analyse", mapper = c("data", "output", "figurer"))
#' print(mapper)
#'
#' # Opprett mappestruktur med en spesifikk periode (f.eks. "2024" eller "2024-Q1")
#' mapper <- opprett_mappestruktur("prosjekt", periode = "2024")
#' print(mapper)
#'
#' # Eksempel på hvordan stiene fra listen kan tilordnes til individuelle variabler
#' mappestruktur <- opprett_mappestruktur("prosjekt")
#'
#' inndata_mappe <- mappestruktur$inndata_mappe
#' klargjorte_data_mappe <- mappestruktur$klargjorte_data_mappe
#' statistikk_mappe <- mappestruktur$statistikk_mappe
#' utdata_mappe <- mappestruktur$utdata_mappe
#'
#' @export
opprett_mappestruktur <- function(arbeidsmappe,
                                  mapper = c("inndata", "klargjorte-data", "statistikk", "utdata"),
                                  periode = NULL) {

  # Fjerner eventuell avsluttende "/" fra arbeidsmappe først
  arbeidsmappe <- sub("/$", "", arbeidsmappe)

  # Opprett en tom liste for å lagre stier
  mapper_liste <- list()

  # Itererer over listen av mapper
  for (mappe in mapper) {
    # Lag full sti til mappen
    full_mappe_path <- file.path(arbeidsmappe, mappe)

    # Oppretter mappen om den ikke finnes
    if (!dir.exists(full_mappe_path)) {
      dir.create(full_mappe_path, recursive = TRUE)
    }

    # Hvis 'periode' er spesifisert, opprett undermappe med det navnet og oppdater stien
    if (!is.null(periode)) {
      full_mappe_path <- file.path(full_mappe_path, periode)

      # Oppretter undermappen om den ikke finnes
      if (!dir.exists(full_mappe_path)) {
        dir.create(full_mappe_path, recursive = TRUE)
      }
    }

    # Sørg for at stien slutter med "/"
    full_mappe_path <- paste0(rtrim(full_mappe_path, "/"), "/")

    # Erstatt bindestrek med understrek i variabelnavn
    var_navn <- gsub("-", "_", paste0(mappe, "_mappe"))  # F.eks. "klargjorte_data_mappe"

    # Legg stien inn i listen
    mapper_liste[[var_navn]] <- full_mappe_path
  }

  # Returnerer listen med mapper
  return(mapper_liste)
}

# Hjelpefunksjon for å fjerne avsluttende skråstreker fra en filsti
rtrim <- function(x, char = "/") {
  sub(paste0(char, "$"), "", x)
}


#' Finn versjonsnummer fra et filnavn
#'
#' Funksjonen finner og returnerer versjonsnummeret fra et filnavn som følger en navnestandard
#' der versjonsnummeret er spesifisert etter `_v`, etterfulgt av et eller flere siffer.
#' Dersom filen ikke er versjonert i henhold til denne standarden, returnerer funksjonen `FALSE`.
#'
#' @param fil En karakterstreng som representerer filnavnet hvor funksjonen skal søke etter et
#' versjonsnummer.
#'
#' @details
#' Funksjonen forventer at filnavnet inneholder et versjonsnummer etter mønsteret `_v` fulgt av ett eller flere siffer.
#' For eksempel vil et filnavn som `rapport_v2.csv` returnere `2` som versjonsnummer.
#'
#' Dersom filnavnet ikke inneholder et versjonsnummer i dette formatet, vil funksjonen returnere `FALSE`
#' og gi en advarsel om at filen ikke er versjonert i henhold til navnestandarden. Se
#' [navnestandarden](https://manual.dapla.ssb.no/statistikkere/navnestandard.html) for mer informasjon.
#'
#' Funksjonen gir også en advarsel dersom filnavnet inneholder stor "V" i stedet for liten "v", siden det forventes at
#' versjonsnummeret skal markeres med små bokstaver (`_v`).
#'
#' @return Versjonsnummeret som et heltall (integer) dersom det finnes, ellers `FALSE` dersom filen
#' ikke følger versjonsstandard.
#'
#' @examples
#' # Finn versjonsnummer fra et gyldig filnavn
#' finn_versjon("rapport_v2.csv")  # Returnerer 2
#'
#' # Filnavn uten versjonsnummer (advarsel vil gis)
#' finn_versjon("rapport.csv")  # Returnerer advarsel og FALSE
#'
#' # Filnavn med stor "V" (advarsel vil gis)
#' finn_versjon("rapport_V2.csv")  # Returnerer advarsel og FALSE
#'
#' @export
finn_versjon <- function(fil) {

  # Sjekk om filnavnet inneholder stor "V"
  if (grepl("_V", fil)) {
    stop("Filnavnet inneholder stor 'V' i stedet for liten 'v'. Funksjonen forventer '_v' med små bokstaver.")

      versjon <- FALSE

  } else if (!grepl(".*_v\\d{1,}\\..*$", fil)) {

    # Gi en advarsel om at filen ikke er versjonert riktig, men bare hvis det ikke var en stor 'V'
    stop("Filen er ikke versjonert i henhold til navnestandarden. ",
            "Se https://manual.dapla.ssb.no/statistikkere/navnestandard.html for mer informasjon.")

    versjon <- FALSE

  } else {

    ## Vi henter ut versjonsnummeret ved å erstatte hele filnavnet med
    ## tallene etter "_v"

    versjon <- as.integer(gsub(pattern = ".*_v(\\d{1,})\\..*$",
                               replacement = "\\1",
                               x = fil))
  }

  return(versjon)
}


#' Lag versjonert filsti
#'
#' Funksjonen genererer en filsti for en versjonert fil basert på et gitt filnavn. Den kan enten
#' finne siste versjon av en fil eller lage en ny versjon av filen.
#'
#' @param fil En karakterstreng som representerer filstien til filen som skal versjoneres.
#' @param versjon Angir hvordan versjonsnummeret skal håndteres. Kan være `"siste"` for å hente siste versjon,
#' `"ny"` for å lage en ny versjon, eller et heltall som spesifiserer en bestemt versjon. Standardverdien er `"siste"`.
#'
#' @details
#' Funksjonen tar et filnavn og sjekker om det er versjonert i henhold til et mønster der versjonsnummeret er angitt etter `_v`,
#' etterfulgt av ett eller flere siffer (f.eks. `_v1`, `_v2`). Den kan enten hente siste versjon, lage en ny versjon, eller returnere en
#' spesifikk versjon av filen hvis det er angitt.
#'
#' Hvis filen ikke eksisterer i den angitte mappen, eller hvis den ikke inneholder en gyldig versjon, vil funksjonen stoppe med en feilmelding.
#'
#' Når `versjon = "ny"`, vil funksjonen generere et nytt filnavn med versjonsnummer som er én høyere enn det høyeste eksisterende nummeret.
#' Hvis `versjon = "siste"`, returneres stien til den siste versjonen av filen. Hvis et spesifikt versjonsnummer er oppgitt som et heltall,
#' vil funksjonen returnere stien til denne versjonen hvis den eksisterer.
#'
#' @return En karakterstreng som representerer filstien til den versjonerte filen. Funksjonen stopper med en feilmelding hvis
#' den ikke kan finne eller opprette en versjonert filsti i henhold til de angitte kriteriene.
#'
#' @examples
#' \dontrun{
#' # Lag en ny versjon av filen "data.parquet" i mappen "prosjekt"
#' lag_versjonert_filsti("prosjekt/data_v1.parquet", versjon = "ny")
#'
#' # Hent stien til siste versjon av filen "data.parquet"
#' lag_versjonert_filsti("prosjekt/data_v1.parquet", versjon = "siste")
#'
#' # Hent stien til en spesifikk versjon (versjon 3) av filen "data.parquet"
#' lag_versjonert_filsti("prosjekt/data_v1.parquet", versjon = 3)
#'
#'}
#' @export
lag_versjonert_filsti <- function(fil,
                                  versjon = "siste") {

  mappe <- dirname(fil)

  # Sjekk om mappen eksisterer
  if (!dir.exists(mappe)) stop("Fant ikke angitt mappe: ", mappe)

  # Fjern eventuell versjonsinformasjon fra basenavnet
  basenavn <- gsub(pattern = "_v\\d{1,}$",
                   replacement = "",
                   x = tools::file_path_sans_ext(basename(fil)))

  filendelse <- tools::file_ext(fil)

  # Hent alle filer i mappen som matcher basenavnet
  filer <- list.files(mappe, basenavn)

  # Hvis vi skal lage en ny versjon og det ikke finnes filer fra før
  if (versjon != "ny" & length(filer) == 0) {
    stop("Fant ingen filer som matchet ", basenavn, " i ", mappe, ".")
  }

  versjonerte_filer <- filer[grepl("_v\\d{1,}\\..*$", filer)]

  if (length(versjonerte_filer) == 0) {
    # Ingen versjonerte filer funnet
    if (versjon == "ny") {
      ny_versjon <- 1
    } else if (versjon == "siste" || ((versjon %% 1) == 0)) {
      stop("Fant ingen versjonerte filer i ", mappe, ".")
    }
  } else {
    # Hent alle versjoner fra filene
    versjoner <- vapply(versjonerte_filer, finn_versjon, integer(1))
    if (versjon == "ny") {
      ny_versjon <- max(versjoner) + 1
    } else if (versjon == "siste") {
      ny_versjon <- max(versjoner)
    } else if ((versjon %% 1) == 0) {
      if (!versjon %in% versjoner) {
        stop("Fant ikke versjon ", versjon, ".")
      } else {
        # Versjon er et gyldig heltall, så vi bruker det uendret
        ny_versjon <- versjon
      }
    }
  }

  # Lag den nye filstien basert på versjonen
  ny_filsti <- paste0(mappe, "/", basenavn, "_v", ny_versjon, ".", filendelse)

  # Håndtering for ny versjon
  if (versjon == "ny") {
    if (file.exists(ny_filsti)) {
      stop("Genererte et filnavn som allerede eksisterer i angitt mappe.")
    } else {
      return(ny_filsti)
    }
  } else if (versjon == "siste" || ((versjon %% 1) == 0)) {
    if (!file.exists(ny_filsti)) {
      stop("Klarte ikke å finne siste versjon. ",
           "Pass på at alle filer i mappen slutter med ",
           "_v{versjonsnummer}.{filendelse}")
    } else {
      return(ny_filsti)
    }
  }
}


#' Sjekk for endringer i rader og kolonner mellom siste og nyeste versjon av en fil
#'
#' Funksjonen sammenligner antall rader og kolonner, samt kolonnenavnene, i den siste versjonerte filen
#' med tilsvarende i en ny fil for å se om det har skjedd endringer i struktur.
#'
#' @param filsti En karakterstreng som representerer filstien til den nyeste versjonen av filen.
#' Funksjonen finner selv den siste versjonerte filen ved å bruke \code{\link{lag_versjonert_filsti}}.
#'
#' @details
#' Funksjonen åpner den siste versjonerte filen i en gitt sti ved hjelp av \code{\link{lag_versjonert_filsti}} og sammenligner
#' antall rader og kolonner, samt kolonnene (navn og antall), med de i den nye filen som er angitt av `filsti`.
#' Det kontrolleres først om antallet rader eller kolonner har endret seg. Deretter kontrolleres det om navnene på kolonnene
#' har endret seg. Hvis det er en endring i enten antall rader, antall kolonner eller kolonnenavnene, returnerer funksjonen `TRUE`.
#' Hvis ingen endringer oppdages, returneres `FALSE`.
#'
#' @return Funksjonen returnerer en logisk verdi:
#' \itemize{
#'   \item `TRUE`: Hvis det har skjedd endringer i antall rader, antall kolonner eller kolonnenavn mellom den siste versjonerte filen og den nye filen.
#'   \item `FALSE`: Hvis det ikke er noen endringer i rader eller kolonner.
#' }
#'
#' @examples
#' \dontrun{
#' # Sjekk om det er endringer i rader eller kolonner mellom siste versjon og ny fil
#' sjekk_endring_rader_kolonner("prosjekt/data.parquet")
#'
#'}
#' @export
sjekk_endring_rader_kolonner <- function(filsti) {

  # Hent antall rader og kolonner fra siste versjonerte fil
  data_siste <- arrow::open_dataset(lag_versjonert_filsti(filsti, "siste"))
  dim_siste <- dim(data_siste)

  # Hent antall rader og kolonner fra ny fil
  data_ny <- arrow::open_dataset(filsti)
  dim_ny <- dim(data_ny)

  # Sjekk om antall rader eller kolonner er forskjellige
  if (!identical(dim_siste, dim_ny)) {
    return(TRUE)  # Returnerer TRUE hvis antall rader eller kolonner er forskjellige
  }

  # Hent kolonner fra siste versjonerte fil
  kolonnenavn_siste <- data_siste$schema$names

  # Hent kolonner fra ny fil
  kolonnenavn_ny <- data_ny$schema$names

  # Sjekk om det er endringer i kolonnenavn eller antall kolonner
  if ((length(kolonnenavn_siste) != length(kolonnenavn_ny)) |
      !identical(sort(kolonnenavn_siste), sort(kolonnenavn_ny))) {
    endring_kolonner <- TRUE
  } else {
    endring_kolonner <- FALSE
  }

  return(endring_kolonner)
}


#' Sjekk for endringer i datatyper mellom siste og nyeste versjon av en fil
#'
#' Funksjonen sammenligner datatypene til kolonnene i den siste versjonerte filen med datatypene i en ny fil
#' for å se om det har skjedd endringer i datatypene for kolonnene.
#'
#' @param filsti En karakterstreng som representerer filstien til den nyeste versjonen av filen.
#' Funksjonen finner selv den siste versjonerte filen ved å bruke \code{\link{lag_versjonert_filsti}}.
#'
#' @details
#' Funksjonen åpner den siste versjonerte filen i en gitt sti ved hjelp av \code{\link{lag_versjonert_filsti}} og sammenligner
#' kolonnenavn og datatyper med de i den nye filen som er angitt av `filsti`. Det kontrolleres om datatypene for
#' kolonnene har endret seg mellom den siste versjonen og den nye filen. Hvis det er en endring i datatypene, returnerer funksjonen `TRUE`.
#' Hvis ingen endringer oppdages, returneres `FALSE`.
#'
#' @return Funksjonen returnerer en logisk verdi:
#' \itemize{
#'   \item `TRUE`: Hvis det har skjedd endringer i datatypene til kolonnene mellom den siste versjonerte filen og den nye filen.
#'   \item `FALSE`: Hvis det ikke er noen endringer i datatypene.
#' }
#'
#' @examples
#' \dontrun{
#' # Sjekk om det er endringer i datatypene mellom siste versjon og ny fil
#' sjekk_endring_datatype("prosjekt/data.parquet")
#'
#'}
#' @export
sjekk_endring_datatype <- function(filsti){

  # Hent datatyper fra siste versjonerte fil
  data_siste <- arrow::open_dataset(lag_versjonert_filsti(filsti, "siste"))$schema
  kolonnenavn_siste <- data_siste$names
  kolonne_datatyper_siste <- sapply(data_siste$fields, function(x) x$type$ToString())
  datatyper_siste <- data.frame(Kolonne = kolonnenavn_siste, Datatype = kolonne_datatyper_siste)

  # Hent datatyper fra ny fil
  data_ny <- arrow::open_dataset(filsti)$schema
  kolonnenavn_ny <- data_ny$names
  kolonne_datatyper_ny <- sapply(data_ny$fields, function(x) x$type$ToString())
  datatyper_ny <- data.frame(Kolonne = kolonnenavn_ny, Datatype = kolonne_datatyper_ny)

  # Sammenlign datatyper mellom siste og ny fil
  sammenligning_diff <- merge(datatyper_siste, datatyper_ny, by = "Kolonne", suffixes = c("_siste", "_ny")) %>%
    filter(Datatype_siste != Datatype_ny)

  # Sjekk om det er forskjeller i datatypene
  if (nrow(sammenligning_diff) > 0){
    endring_datatype <- TRUE
  } else {
    endring_datatype <- FALSE
  }

  return(endring_datatype)
}

#' Sjekk for endringer i sum for numeriske kolonner mellom siste og nyeste versjon av en fil
#'
#' Denne funksjonen sjekker om det har skjedd noen endringer i summene av de numeriske kolonnene mellom den siste versjonerte filen med en ny fil for å se om det har skjedd endringer i dataene.
#' Funksjonen sammenligner datasett ved å beregne summen av numeriske kolonner i hvert sett, og deretter sjekke om det finnes forskjeller.
#'
#' @param filsti En tekststreng som spesifiserer filstien til den nye datasetten som skal sammenlignes.
#' Funksjonen forventer at det finnes en tidligere versjonert fil på samme plassering.
#'
#' @return En logisk verdi (\code{TRUE} eller \code{FALSE}). Returnerer \code{TRUE} hvis det er endringer i summen av numeriske kolonner mellom de to datasettene, og \code{FALSE} hvis det ikke er noen endringer.
#'
#' @details Funksjonen benytter seg av Arrow-pakken for å åpne datasettene med lazy loading og arsenal-pakken for å sammenligne dataene.
#' Den håndterer kolonner som er numeriske (int eller float) og summerer disse før sammenligning.
#' Dersom det er noen forskjeller i summene mellom de to datasettene, returnerer funksjonen \code{TRUE}.
#'
#' @import arrow dplyr arsenal
#'
#' @examples
#' \dontrun{
#' # Sjekk om det er endringer i verdiene mellom siste versjon og ny fil
#' sjekk_endring_sum("prosjekt/data.parquet")
#'
#'}
#' @export
sjekk_endring_sum <- function(filsti) {

  # Åpne datasettene med lazy loading
  data_siste <- arrow::open_dataset(lag_versjonert_filsti(filsti, "siste"))
  data_ny <- arrow::open_dataset(filsti)

  # Funksjon for å sjekke om en kolonne er numerisk (integer eller float/double)
  is_numeric_column <- function(dataset, column_name) {
    column_type <- dataset$schema$GetFieldByName(column_name)$type$ToString()  # Konverter til streng
    column_type %in% c("int32", "int64", "float", "double")
  }

  # Få kolonnenavn fra datasettene
  columns_siste <- data_siste$schema$names
  columns_ny <- data_ny$schema$names

  # Filtrer kolonner som er numeriske for data_siste og data_ny
  numeric_columns_siste <- columns_siste[sapply(columns_siste, function(col) is_numeric_column(data_siste, col))]
  numeric_columns_ny <- columns_ny[sapply(columns_ny, function(col) is_numeric_column(data_ny, col))]

  # Dynamisk bygge summarize for data_siste
  summarized_siste <- data_siste %>%
    summarize(across(all_of(numeric_columns_siste), ~ sum(.x, na.rm = TRUE)))

  # Dynamisk bygge summarize for data_ny
  summarized_ny <- data_ny %>%
    summarize(across(all_of(numeric_columns_ny), ~ sum(.x, na.rm = TRUE)))

  # Samle resultatene inn i minnet med collect()
  resultat_siste <- summarized_siste %>% collect()
  resultat_ny <- summarized_ny %>% collect()




  # Sammenlign dataene mellom siste og ny fil
  # comparison <- arsenal::comparedf(resultat_siste, resultat_ny, by = NULL)
  comparison <- all.equal(resultat_siste, resultat_ny, tolerance = .Machine$double.eps^0.5)

  # forskjeller <- summary(comparison)$diffs.table

  # # Sjekk om det er forskjeller i dataene
  # if (nrow(forskjeller) > 0){
  #   endring_sum <- TRUE
  # } else {
  #   endring_sum <- FALSE
  # }

  if (any(comparison == TRUE)){
    endring_sum <- FALSE
  } else {
    endring_sum <- TRUE

  }

  return(endring_sum)
}


#' Sjekk for endringer i verdier mellom siste og nyeste versjon av en fil
#'
#' Funksjonen sammenligner innholdet (verdiene) i den siste versjonerte filen med en ny fil for å se om det har skjedd endringer i dataene.
#'
#' @param filsti En karakterstreng som representerer filstien til den nyeste versjonen av filen.
#' Funksjonen finner selv den siste versjonerte filen ved å bruke \code{\link{lag_versjonert_filsti}}.
#'
#' @details
#' Funksjonen bruker \code{\link{dplyr::anti_join}} for å sammenligne verdiene i den siste versjonerte filen med den nye filen.
#' Den sjekker om det finnes rader som er forskjellige mellom de to datasettene. Hvis det finnes forskjeller, returnerer funksjonen `TRUE`.
#' Hvis ingen forskjeller oppdages, returneres `FALSE`.
#'
#' @return Funksjonen returnerer en logisk verdi:
#' \itemize{
#'   \item `TRUE`: Hvis det har skjedd endringer i verdiene mellom den siste versjonerte filen og den nye filen.
#'   \item `FALSE`: Hvis det ikke er noen endringer i verdiene.
#' }
#'
#' @examples
#' \dontrun{
#' # Sjekk om det er endringer i verdiene mellom siste versjon og ny fil
#' sjekk_endring_verdier("prosjekt/data.parquet")
#'
#'}
#' @export
sjekk_endring_verdier <- function(filsti){

  # Hent data fra siste versjonerte fil og ny fil
  data_siste <- arrow::read_parquet(lag_versjonert_filsti(filsti, "siste"))
  data_ny <- arrow::read_parquet(filsti)

  # Sammenlign dataene mellom siste og ny fil
  # comparison <- arsenal::comparedf(data_siste, data_ny, by = NULL)
  comparison <- all.equal(data_siste, data_ny)

  # forskjeller <- summary(comparison)$diffs.table

  # # Sjekk om det er forskjeller i dataene
  # if (nrow(forskjeller) > 0){
  #   endring_verdier <- TRUE
  # } else {
  #   endring_verdier <- FALSE
  # }

  if (any(comparison == TRUE)){
    endring_verdier <- FALSE
  } else {
    endring_verdier <- TRUE

  }

  return(endring_verdier)
}

#' Sjekk for endringer i kolonner, datatyper eller verdier mellom siste og nyeste versjon av en fil
#'
#' Funksjonen sjekker om det har skjedd noen endringer i kolonner, datatyper eller verdier mellom den siste versjonerte filen og en ny fil.
#' Den kombinerer tre separate sjekker: for kolonner, datatyper og verdier.
#'
#' @param filsti En karakterstreng som representerer filstien til den nyeste versjonen av filen.
#' Funksjonen finner selv den siste versjonerte filen ved å bruke \code{\link{lag_versjonert_filsti}}.
#'
#' @details
#' Funksjonen kombinerer sjekker fra \code{\link{sjekk_endring_rader_kolonner}}, \code{\link{sjekk_endring_datatype}}, og \code{\link{sjekk_endring_verdier}}.
#' Den sjekker først om det har skjedd endringer i kolonnene, deretter i datatypene, og til slutt om det har skjedd endringer i verdiene mellom filene.
#' Hvis det oppdages en endring på noen av disse områdene, returnerer funksjonen `TRUE`. Hvis ingen endringer oppdages, returneres `FALSE`.
#'
#' @return Funksjonen returnerer en logisk verdi:
#' \itemize{
#'   \item `TRUE`: Hvis det har skjedd endringer i kolonnene, datatypene eller verdiene mellom den siste versjonerte filen og den nye filen.
#'   \item `FALSE`: Hvis det ikke er noen endringer i noen av disse områdene.
#' }
#'
#' @examples
#' \dontrun{
#' # Sjekk om det er noen endringer mellom siste versjon og ny fil
#' sjekk_endring("prosjekt/data.parquet")
#'
#'}
#' @export
sjekk_endring <- function(filsti){
  endring_rader_kolonner <- sjekk_endring_rader_kolonner(filsti = filsti)

  if (endring_rader_kolonner == TRUE){
    endring <- TRUE
    print(glue::glue("Endring i rader og/eller kolonner: {filsti}"))
    return(endring)  # Avslutt funksjonen uten feilmelding hvis kolonner har endret seg
  }

  endring_datatype <- sjekk_endring_datatype(filsti = filsti)

  if (endring_datatype == TRUE){
    endring <- TRUE
    print(glue::glue("Endring i datatyper: {filsti}"))
    return(endring)  # Avslutt funksjonen uten feilmelding hvis datatyper har endret seg
  }

  endring_sum <- sjekk_endring_sum(filsti = filsti)

  if (endring_sum == TRUE){
    endring <- TRUE
    print(glue::glue("Endring i summen til numeriske kolonner for: {filsti}"))
    return(endring)  # Avslutt funksjonen uten feilmelding hvis datatyper har endret seg
  }

  endring_verdier <- sjekk_endring_verdier(filsti = filsti)

  if (endring_verdier == TRUE){
    endring <- TRUE
    print(glue::glue("Endring i verdier for: {filsti}"))
    return(endring)  # Avslutt funksjonen uten feilmelding hvis verdier har endret seg
  } else {
    endring <- FALSE
    return(endring)  # Returner FALSE hvis ingen endringer er oppdaget
  }
}



#' Lag logg for versjonering
#'
#' Funksjonen oppdaterer en loggfil med informasjon om en ny kjøring der flere filstier blir versjonert. Den sjekker først om det allerede finnes
#' en logg, og hvis det ikke er noen endringer siden forrige kjøring, vil loggfilen ikke bli oppdatert.
#'
#' @param resultat En liste som inneholder resultatet av kjøringen fra \code{\link{versjoner_filer}}. Hver oppføring representerer filstiens oppdaterte versjon.
#' @param arbeidsmappe En karakterstreng som representerer banen til arbeidsmappen der loggfilen skal lagres.
#' @param logg_fil En karakterstreng som representerer navnet på loggfilen. Standard er `"versjonering_logg.json"`.
#'
#' @details
#' Funksjonen logger hver kjøring av \code{\link{versjoner_filer}} ved å sjekke om loggfilen allerede eksisterer. Hvis det er første gang loggfilen opprettes,
#' vil en ny logg bli laget. Hvis det allerede finnes en logg, vil funksjonen sjekke om resultatet av den nye kjøringen er
#' identisk med den siste kjøringen. Hvis ingen endringer er oppdaget (dvs. ingen oppdaterte versjoner av filene), vil loggen ikke bli oppdatert.
#' Hvis det er en forskjell i resultatene, vil funksjonen legge til en ny kjøring med en unik ID og tidsstempel.
#'
#' Objektet `resultat` fra \code{\link{versjoner_filer}} er en liste der hver filsti er oppdatert til å representere siste versjon.
#' Funksjonen skriver denne informasjonen til loggen hvis en endring oppdages.
#'
#' @return Returnerer den oppdaterte loggen som en liste. Hvis ingen endringer ble oppdaget, returnerer funksjonen uten å oppdatere loggen.
#'
#' @examples
#' \dontrun{
#' # Versjoner filstier og loggfør kjøringen
#' filstier <- c("data1.parquet", "data2.parquet")
#' resultat <- versjoner_filer(filstier)
#' logg <- logg_kjoring(resultat, arbeidsmappe = "prosjekt/logg")
#'
#' # Hvis ingen endringer siden forrige kjøring, blir loggfilen ikke oppdatert
#' logg <- logg_kjoring(resultat, arbeidsmappe = "prosjekt/logg")
#'
#'}
#' @seealso \code{\link{versjoner_filer}} for generering av resultatet.
#'
#' @export
logg_kjoring <- function(resultat,
                         logg_fil = "versjonering_logg.json",
                         arbeidsmappe,
                         periode) {

  logg_filsti <- glue::glue("{arbeidsmappe}/{logg_fil}")

  # Sjekk om loggfilen allerede eksisterer
  if (file.exists(logg_filsti)) {
    # Les inn eksisterende logg
    logg <- jsonlite::fromJSON(logg_filsti)

    # Sjekk om loggen har minst én kjøring
    if (length(logg) > 0) {
      # Hent den siste oppføringen (siste kjøring)
      siste_kjoring <- logg[[length(logg)]]

      # Sjekk om den nye kjøringen er identisk med den siste versjonerte filene
      identisk <- identical(siste_kjoring$versjonerte_filer, resultat)

      if (identisk) {
        cat("Ingenting har endret seg siden forrige kjøring. Loggen oppdateres ikke.\n")
        return(logg)
      }
    }
  } else {
    # Opprett en tom liste hvis loggfilen ikke finnes
    logg <- list()
  }

  # Finn antall kjøringer så langt (lengden på loggen)
  antall_kjoringer <- length(logg) + 1

  # Generer en unik ID for denne kjøringen (f.eks. R1, R2, ...)
  kjoring_id <- paste0("R", antall_kjoringer)

  # Få gjeldende dato og klokkeslett
  dato_tid <- Sys.time()

  # Lag en ny kjøring med 'kjoring_id' og 'dato_tid' som attributter
  ny_kjoring <- list(
    attributes = list(
      kjoring_id = kjoring_id,
      dato_tid = as.character(dato_tid),
      periode = periode
    ),
    versjonerte_filer = resultat
  )

  # Legg til den nye kjøringen i loggen
  logg[[kjoring_id]] <- ny_kjoring

  # Skriv oppdatert logg til JSON-fil
  jsonlite::write_json(logg, logg_filsti, pretty = TRUE, auto_unbox = TRUE)

  cat("Loggen er oppdatert for kjøring", kjoring_id, "\n")

  # Returner oppdatert logg
  return(logg)
}





#' Versjonere filer
#'
#' Funksjonen oppdaterer filstier for flere filer ved å lage en ny versjon av filene dersom det er oppdaget endringer,
#' eller returnerer stien til den siste versjonerte filen hvis ingen endringer er oppdaget.
#' Funksjonen logger også resultatene av kjøringen ved hjelp av \code{\link{logg_kjoring}}, som lagrer informasjon om
#' versjonerte filer og kjøringsattributter som `kjoring_id` og `dato_tid` i en egen loggfil.
#'
#' @param filstier En karaktervektor som inneholder navnene på filstier som skal versjoneres.
#' Funksjonen antar at filene er registrert som variable i det globale miljøet.
#' @param logg_fil Navnet på loggfilen der kjøringsinformasjon og versjonerte filer lagres. Standard er "versjonering_logg.json".
#' @param arbeidsmappe Stien til arbeidsmappen der loggfilen lagres.
#'
#' @details
#' Funksjonen itererer over en liste med filstier og sjekker først om en versjonert fil allerede eksisterer. Hvis det ikke finnes
#' en versjonert fil opprettes versjon 1 (v1) automatisk. Dersom versjonerte filer allerede eksisterer sjekkes det om det har skjedd endringer
#' i kolonner, datatyper eller verdier for hver fil (mot den siste versjonerte filen) ved hjelp av \code{\link{sjekk_endring}}. Hvis det er oppdaget endringer,
#' vil funksjonen opprette en ny versjon av filen. Hvis ingen endringer oppdages, returneres stien til den siste versjonerte filen.
#' Etter hver kjøring, oppdateres loggfilen med informasjon om hvilke versjoner av filene som har blitt brukt i kjøringen samt metadata om kjøringen
#' (`kjoring_id` og `dato_tid`) ved hjelp av \code{\link{logg_kjoring}}.
#'
#' @return En liste som inneholder filstiene til de siste versjonene av de versjonerte filene.
#'
#' @examples
#' \dontrun{
#' # Lag versjonerte filstier for en liste med filnavn
#' filstier <- c("filsti1", "filsti2")
#' versjoner_filer(filstier)
#'
#'}
#' @export
versjoner_filer <- function(filstier,
                            logg_fil = "versjonering_logg.json",
                            arbeidsmappe,
                            periode) {

  # Lager liste med filstier
  filstier_liste <- setNames(mget(filstier, envir = globalenv()), filstier)

   # Itererer over hver filsti
  updated_filstier <- purrr::map(filstier_liste, function(filsti) {

    # Sjekker først om finn_versjon er FALSE eller versjon 1 skal opprettes
    if (finn_versjon(lag_versjonert_filsti(filsti, versjon = "ny")) == 1) {
      data <- arrow::read_parquet(filsti)
      arrow::write_parquet(data, lag_versjonert_filsti(filsti, versjon = "ny"))
      # Hvis versjon 1 opprettes, returner stien til den nye versjonen
      return(lag_versjonert_filsti(filsti, versjon = "siste"))
    }

    # Sjekker om det er en endring for denne filstien
    if (sjekk_endring(filsti = filsti)) {
      data <- arrow::read_parquet(filsti)
      arrow::write_parquet(data, lag_versjonert_filsti(filsti, versjon = "ny"))
      # Hvis det er en endring, oppdater filstien med ny versjon
      return(lag_versjonert_filsti(filsti, versjon = "siste"))
    } else {
      # Hvis det ikke er noen endring, returner stien til siste versjon
      return(lag_versjonert_filsti(filsti, versjon = "siste"))
    }
  })

  # Beholder de originale navnene for filstiene
  names(updated_filstier) <- names(filstier_liste)

  logg_kjoring(resultat = updated_filstier,
             logg_fil = logg_fil,
             arbeidsmappe = arbeidsmappe,
             periode = as.character(periode))

  return(updated_filstier)
}

#' Finn en release fra logg basert på ulike kriterier
#'
#' Funksjonen søker gjennom en loggfil for å finne en spesifikk release, siste release, alle releaser innenfor et datointervall, alle releaser for en gitt periode eller siste release innenfor en spesifisert periode.
#'
#' @param release En karakterstreng som spesifiserer hvilken release som skal hentes. Hvis `release = "siste"`, returneres den nyeste releasen basert på dato. Kombiner med `periode` for å hente den siste releasen innenfor en spesifikk periode.
#' @param dato En karakterstreng som spesifiserer en eksakt dato eller dato med tid (f.eks. "2023", `"2023-01-01"` eller `"2023-01-01 12:00:00"`).
#' Hvis bare dato oppgis, returneres alle releaser fra denne dagen.
#' @param dato_start En karakterstreng som spesifiserer startdato for et datointervall (f.eks. "2023", `"2023-01-01"`). Brukes sammen med `dato_slutt` for å hente releaser innenfor et intervall.
#' @param dato_slutt En karakterstreng som spesifiserer sluttdato for et datointervall (f.eks. "2023", `"2023-01-31"`). Brukes sammen med `dato_start`.
#' @param periode En karakterstreng som spesifiserer en tidsperiode (f.eks. "2023", `"2023-Q1"`). Hvis `release = "siste"` er angitt, returneres den nyeste releasen innenfor den spesifiserte perioden.
#' @param arbeidsmappe En karakterstreng som representerer banen til arbeidsmappen der loggfilen ligger.
#' @param logg_fil En karakterstreng som representerer navnet på loggfilen. Standard er `"versjonering_logg.json"`.
#'
#' @details
#' Funksjonen leser inn en loggfil i JSON-format og søker etter en release basert på de oppgitte kriteriene.
#' Du kan søke etter:
#' \itemize{
#'   \item Spesifikk release: Angi `release` for å hente en bestemt release.
#'   \item Nyeste release: Angi `release = "siste"` for å hente den siste basert på tidsstempelet.
#'   \item Nyeste release innenfor en spesifikk periode: Angi både `release = "siste"` og `periode` for å hente den nyeste releasen innenfor perioden.
#'   \item Spesifikk dato: Angi `dato` for å hente releaser som ble logget på en gitt dato.
#'   \item Dato-intervall: Angi både `dato_start` og `dato_slutt` for å hente releaser innenfor et bestemt tidsrom.
#' }
#'
#' Hvis flere kriterier er oppgitt (f.eks. både `release` og `dato`), vil `release`-kriteriet ha prioritet.
#'
#' @return Returnerer en liste over releaser som samsvarer med kriteriene, eller `NULL` hvis ingen releaser ble funnet eller hvis loggfilen ikke eksisterer.
#'
#' @examples
#' \dontrun{
#' # Finn siste release
#' finn_release(release = "siste", arbeidsmappe = "prosjekt")
#'
#' # Finn siste release i en spesifikk periode
#' finn_release(release = "siste", periode = "2021-Q1", arbeidsmappe = "prosjekt")
#'
#' # Finn en spesifikk release
#' finn_release(release = "R1", arbeidsmappe = "prosjekt")
#'
#' # Finn releaser fra en bestemt dato
#' finn_release(dato = "2023-01-01", arbeidsmappe = "prosjekt")
#'
#' # Finn releaser innenfor et datointervall
#' finn_release(dato_start = "2023-01-01", dato_slutt = "2023-01-31", arbeidsmappe = "prosjekt")
#'
#'}
#' @seealso \code{\link{logg_kjoring}} for å loggføre kjøringer i loggfilen.
#'
#' @export
finn_release <- function(release = NULL,
                         dato = NULL,
                         dato_start = NULL,
                         dato_slutt = NULL,
                         periode = NULL,
                         arbeidsmappe,
                         logg_fil = "versjonering_logg.json") {

  logg_filsti <- glue::glue("{arbeidsmappe}/{logg_fil}")

  # Sjekk om loggfilen eksisterer
  if (!file.exists(logg_filsti)) {
    warning("Loggfilen finnes ikke. Ingen versjonerte filer er registrert.")
    return(NULL)
  }

  logg <- jsonlite::fromJSON(logg_filsti)

  # Sjekk om loggfilen er tom eller ikke inneholder noen versjonerte filer
  if (length(logg) == 0) {
    warning("Ingen versjonerte filer funnet i loggfilen.")
    return(NULL)
  }

  resultat <- NULL

  # Søk etter spesifikk periode hvis oppgitt
  if (!is.null(periode)) {
    # Filtrer elementer der periode i attributes matcher perioden
    logg <- Filter(function(x) x$attributes$periode == periode, logg)

    # Hvis release også er "siste", finn den nyeste innenfor perioden
    if (!is.null(release) && release == "siste") {
      if (length(logg) > 0) {
        datoer <- sapply(logg, function(x) as.POSIXct(x$attributes$dato_tid))  # Hent alle dato_tid som POSIXct
        siste_indeks <- which.max(datoer)  # Finn indeksen til nyeste dato
        resultat <- logg[[siste_indeks]]  # Hent det nyeste elementet
      } else {
        warning("Ingen oppføringer funnet for den spesifiserte perioden.")
        return(NULL)
      }
      return(resultat)
    }
  }

  # Søk etter spesifikk release hvis oppgitt (dersom release != "siste" eller ingen periode)
  if (!is.null(release) && release != "siste") {
    resultat <- logg[[release]]
    return(resultat)
  }

  # Hvis release er "siste" uten periode, finn den nyeste uavhengig av periode
  if (!is.null(release) && release == "siste") {
    datoer <- sapply(logg, function(x) as.POSIXct(x$attributes$dato_tid))  # Hent alle dato_tid som POSIXct
    siste_indeks <- which.max(datoer)  # Finn indeksen til nyeste dato
    resultat <- logg[[siste_indeks]]  # Hent det nyeste elementet
    return(resultat)
  }

  # Søk etter en spesifikk dato (enten full dato_tid eller bare dato)
  if (!is.null(dato)){
    # Hvis bare dato (uten klokkeslett) er oppgitt
    if (nchar(dato) == 10) {  # Antar format "YYYY-MM-DD" er 10 tegn langt
      # Filtrer elementer som har dato_tid i attributes som starter med datoen
      resultat <- Filter(function(x) startsWith(x$attributes$dato_tid, dato), logg)
    } else {
      # Hvis både dato og klokkeslett er oppgitt, gjør eksakt sammenligning
      resultat <- Filter(function(x) x$attributes$dato_tid == dato, logg)
    }
  }

  # Søk etter datoer innenfor et intervall
  if (!is.null(dato_start) & !is.null(dato_slutt)) {
    # Hvis dato_start og dato_slutt er uten klokkeslett, sett default tidspunkter
    if (nchar(dato_start) == 10) {
      dato_start <- as.POSIXct(paste0(dato_start, " 00:00:00"))
    } else {
      dato_start <- as.POSIXct(dato_start)
    }

    if (nchar(dato_slutt) == 10) {
      dato_slutt <- as.POSIXct(paste0(dato_slutt, " 23:59:59"))
    } else {
      dato_slutt <- as.POSIXct(dato_slutt)
    }

    # Filtrer elementer der dato_tid i attributes er innenfor intervallet
    resultat <- Filter(function(x) {
      dato_tid <- as.POSIXct(x$attributes$dato_tid)
      return(dato_tid >= dato_start & dato_tid <= dato_slutt)
    }, logg)
  }

  return(resultat)
}
