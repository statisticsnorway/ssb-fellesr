# Datadoc-funksjoner


#' Konverter filsti mellom Parquet og Datadoc
#'
#' Konverterer en filsti mellom en Parquet-fil og den tilhørende
#' Datadoc JSON-filen.
#'
#' Standardoppførselen er å konvertere fra `.parquet` til `__DOC.json`.
#' Ved å sette `to = "parquet"` konverteres en Datadoc-fil tilbake til
#' den tilhørende Parquet-filstien. Filstier som allerede har ønsket
#' format, returneres uendret. Ved `to = "json"` godtas også andre
#' JSON-filnavn enn `__DOC.json`.
#'
#' @param filsti En tekststreng eller tekstvektor med filstien til én eller
#'   flere Parquet- eller Datadoc-filer.
#' @param to En tekststreng som angir hvilket filformat filstien skal
#'   konverteres til. Gyldige verdier er `"json"` og `"parquet"`.
#'   Standardverdien er `"json"`.
#' @param warn_if_missing En logisk verdi som angir om det skal gis en
#'   advarsel dersom filen den konverterte filstien peker til, ikke finnes.
#'   Standardverdien er `TRUE`.
#'
#' @return En tekststreng eller tekstvektor med den konverterte filstien.
#'
#' @examples
#' datadoc_path("/buckets/data/personell_v1.parquet")
#'
#' datadoc_path(
#'   "/buckets/data/personell_v1__DOC.json",
#'   to = "parquet"
#' )
#'
#' @export
datadoc_path <- function(filsti, to = c("json", "parquet"), warn_if_missing = TRUE) {
  to <- match.arg(to)
  .datadoc_flag(warn_if_missing, "warn_if_missing")
  if (!is.character(filsti) || anyNA(filsti) || any(!nzchar(trimws(filsti)))) {
    stop("`filsti` må inneholde ikke-tomme filstier uten NA.", call. = FALSE)
  }
  er_parquet <- grepl("\\.parquet$", filsti, ignore.case = TRUE)
  er_json <- grepl("\\.json$", filsti, ignore.case = TRUE)
  er_doc <- grepl("__DOC\\.json$", filsti, ignore.case = TRUE)
  output <- filsti
  if (to == "json") {
    if (any(!er_parquet & !er_json)) {
      stop("`filsti` må slutte på `.parquet` eller `.json`.", call. = FALSE)
    }
    output[er_parquet] <- sub("\\.parquet$", "__DOC.json", filsti[er_parquet],
                              ignore.case = TRUE)
  } else {
    if (any(!er_parquet & !er_doc)) {
      stop("Til Parquet kreves `.parquet` eller `__DOC.json`.", call. = FALSE)
    }
    output[er_doc] <- sub("__DOC\\.json$", ".parquet", filsti[er_doc],
                          ignore.case = TRUE)
  }
  mangler <- output[!file.exists(output)]
  if (warn_if_missing && length(mangler)) {
    warning("Følgende filer finnes ikke:\n", paste(mangler, collapse = "\n"),
            call. = FALSE)
  }
  output
}

#' Hent URL til tjenesten for variabeldefinisjoner
#'
#' Returnerer endepunktet til SSBs tjeneste for variabeldefinisjoner.
#'
#' @param status En tekststreng som angir hvilken tjeneste URL-en skal
#'   peke til. Gyldige verdier er `"ekstern"` og `"intern"`.
#'
#' @return En tekststreng med URL-en til den valgte tjenesten.
#'
#' @examples
#' vardef_url()
#'
#' vardef_url(status = "intern")
#'
#' @export
vardef_url <- function(status = "ekstern") {
  
  status <- match.arg(
    status,
    choices = c("ekstern", "intern")
  )
  
  switch(
    status,
    ekstern = paste0(
      "https://metadata.ssb.no",
      "/public/variable-definitions"
    ),
    intern = paste0(
      "https://metadata.intern.ssb.no",
      "/variable-definitions"
    )
  )
}


.vardef_get <- function(
    path = NULL,
    query = list(),
    status = "auto",
    language = "nb",
    token = NULL,
    allow_not_found = FALSE,
    quiet = TRUE
) {
  
  status <- match.arg(
    status,
    c("auto", "ekstern", "intern")
  )
  
  language <- match.arg(
    language,
    c("nb", "nn", "en")
  )
  
  .datadoc_flag(quiet, "quiet")
  
  if (!is.null(path)) {
    .datadoc_text(path, "path")
  }
  
  # Bestem rekkefølgen på oppslagene
  tjenester <- if (status == "auto") {
    c("ekstern", "intern")
  } else {
    status
  }
  
  for (tjeneste in tjenester) {
    
    url <- vardef_url(status = tjeneste)
    
    if (!is.null(path)) {
      url <- paste0(
        url,
        "/",
        utils::URLencode(path, reserved = TRUE)
      )
    }
    
    headers <- c(
      `Accept-Language` = language
    )
    
    # Autentisering bare ved internt oppslag
    if (tjeneste == "intern") {
      
      access_token <- token
      
      if (is.null(access_token)) {
        access_token <- tryCatch(
          .vardef_token(),
          error = function(e) {
            stop(
              "Kunne ikke autentisere mot intern Vardef: ",
              conditionMessage(e),
              call. = FALSE
            )
          }
        )
      }
      
      headers <- c(
        headers,
        Authorization = paste("Bearer", access_token)
      )
    }
    
    response <- httr::GET(
      url = url,
      query = query,
      httr::add_headers(.headers = headers)
    )
    
    http_status <- httr::status_code(response)
    
    # Ikke funnet: prøv neste tjeneste
    if (http_status == 404L) {
      next
    }
    
    response_text <- httr::content(
      response,
      as = "text",
      encoding = "UTF-8"
    )
    
    # Andre HTTP-feil skal ikke tolkes som manglende definisjon
    if (httr::http_error(response)) {
      
      problem <- tryCatch(
        jsonlite::fromJSON(response_text),
        error = function(e) NULL
      )
      
      detail <- if (
        is.list(problem) &&
        !is.null(problem$detail)
      ) {
        as.character(problem$detail)
      } else {
        "Tjenesten returnerte ingen lesbar feilmelding."
      }
      
      stop(
        "Vardef-oppslaget feilet (",
        tjeneste,
        ", HTTP ",
        http_status,
        ").\nURL: ",
        response$url,
        "\nFeilmelding: ",
        detail,
        call. = FALSE
      )
    }
    
    if (!nzchar(response_text)) {
      return(NULL)
    }
    
    return(
      jsonlite::fromJSON(
        response_text,
        simplifyDataFrame = TRUE
      )
    )
  }
  
  # Alle aktuelle tjenester returnerte 404
  beskjed <- if (status == "auto") {
    "Variabeldefinisjonen finnes verken i ekstern eller intern Vardef."
  } else {
    paste0(
      "Variabeldefinisjonen finnes ikke i ",
      status,
      " Vardef."
    )
  }
  
  if (allow_not_found) {
    if (!quiet) message(beskjed)
    return(NULL)
  }
  
  stop(beskjed, call. = FALSE)
}


#' Hent variabeldefinisjoner
#'
#' Henter variabeldefinisjoner fra SSBs tjeneste for
#' variabeldefinisjoner. Dersom en dato oppgis, returneres bare
#' definisjoner som er gyldige på denne datoen.
#'
#' @param date `NULL`, et `Date`-objekt eller en tekststreng med dato på
#'   formatet `"YYYY-MM-DD"`. Dersom verdien er `NULL`, returneres alle
#'   tilgjengelige variabeldefinisjoner. Dersom en dato oppgis, returneres
#'   bare definisjoner der datoen ligger mellom `valid_from` og
#'   `valid_until`. Standardverdien er `NULL`.
#' @param language En tekststreng med språkkoden som skal brukes for
#'   tekstinnholdet i responsen. Gyldige verdier er `"nb"`, `"nn"` og
#'   `"en"`. Standardverdien er `"nb"`.
#' @param status En tekststreng som angir om den eksterne eller interne
#'   tjenesten skal brukes. Gyldige verdier er `"ekstern"` og `"intern"`.
#'   Standardverdien er `"ekstern"`.
#' @param token Et eventuelt bearer-token ved bruk av den interne
#'   tjenesten. Standardverdien er `NULL`.
#'
#' @return En data frame eller liste opprettet fra JSON-responsen.
#'   Dersom `date` er oppgitt, returneres en data frame med
#'   variabeldefinisjonene som er gyldige på den angitte datoen.
#'
#' @details
#' Filtreringen på dato gjøres lokalt etter at variabeldefinisjonene er
#' hentet fra tjenesten. En definisjon regnes som gyldig når
#' `valid_from` er tidligere enn eller lik den oppgitte datoen, og
#' `valid_until` enten mangler eller er senere enn eller lik den
#' oppgitte datoen.
#'
#' Manglende verdi i `valid_from` tolkes som at definisjonen ikke har
#' noen nedre gyldighetsgrense. Manglende verdi i `valid_until` tolkes
#' som at definisjonen fortsatt er gyldig.
#'
#' @examples
#' \dontrun{
#' # Hent alle tilgjengelige variabeldefinisjoner
#' get_all_variable_definitions()
#'
#' # Hent alle tilgjengelige variabeldefinisjoner på engelsk
#' get_all_variable_definitions(
#'   language = "en"
#' )
#'
#' # Hent definisjoner som er gyldige på en bestemt dato
#' get_all_variable_definitions(
#'   date = "2025-01-01"
#' )
#'
#' # Datoen kan også oppgis som et Date-objekt
#' get_all_variable_definitions(
#'   date = as.Date("2025-01-01"),
#'   language = "en"
#' )
#' }
#'
#' @export
get_all_variable_definitions <- function(
    date = NULL,
    language = "nb",
    status = "auto",
    token = NULL
) {
  
  variable_definitions <- .vardef_get(
    status = status,
    language = language,
    token = token
  )
  
  # Returner alle dersom dato ikke er oppgitt
  if (is.null(date)) {
    return(variable_definitions)
  }
  
  # Kontroller og konverter dato
  if (inherits(date, "Date")) {
    
    if (length(date) != 1L || is.na(date)) {
      stop(
        "`date` må inneholde nøyaktig én gyldig dato.",
        call. = FALSE
      )
    }
    
    filter_date <- date
    
  } else if (
    is.character(date) &&
    length(date) == 1L &&
    !is.na(date) &&
    grepl(
      pattern = "^\\d{4}-\\d{2}-\\d{2}$",
      x = date
    )
  ) {
    
    filter_date <- as.Date(
      date,
      format = "%Y-%m-%d"
    )
    
  } else {
    
    stop(
      paste0(
        "`date` må være `NULL`, et Date-objekt eller én ",
        "tekststreng på formatet \"YYYY-MM-DD\"."
      ),
      call. = FALSE
    )
  }
  
  if (is.na(filter_date)) {
    stop(
      "`date` er ikke en gyldig dato.",
      call. = FALSE
    )
  }
  
  required_variables <- c(
    "valid_from",
    "valid_until"
  )
  
  missing_variables <- setdiff(
    required_variables,
    names(variable_definitions)
  )
  
  if (length(missing_variables) > 0L) {
    stop(
      paste0(
        "Responsen mangler følgende variabler: ",
        paste(
          missing_variables,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  variable_definitions |>
    dplyr::mutate(
      valid_from = dplyr::na_if(
        valid_from,
        ""
      ),
      valid_until = dplyr::na_if(
        valid_until,
        ""
      ),
      valid_from = as.Date(valid_from),
      valid_until = as.Date(valid_until)
    ) |>
    dplyr::filter(
      is.na(valid_from) | valid_from <= filter_date,
      is.na(valid_until) | valid_until >= filter_date
    )
}




#' Hent variabeldefinisjon etter kortnavn
#'
#' Henter en variabeldefinisjon fra SSBs tjeneste for
#' variabeldefinisjoner basert på variabelens kortnavn og en
#' gyldighetsdato.
#'
#' @param short_name En tekststreng med kortnavnet til variabeldefinisjonen
#'   som skal hentes.
#' @param date En tekststreng med datoen variabeldefinisjonen skal være
#'   gyldig på, angitt på formatet `"YYYY-MM-DD"`. Standardverdien er
#'   dagens dato.
#' @param language En tekststreng med språkkoden som skal brukes for
#'   tekstinnholdet i responsen. Standardverdien er `"nb"`.
#'
#' @param status `"ekstern"` (standard) eller `"intern"`.
#' @param token Valgfritt Bearer-token til intern tjeneste.
#'
#' @return Et objekt opprettet fra JSON-responsen fra tjenesten.
#'   Returtypen avhenger av strukturen i responsen og vil vanligvis være
#'   en liste eller en data frame.
#'
#' @details
#' Kortnavnet sendes til tjenesten som parameteren `short_name`, mens
#' datoen sendes som parameteren `date_of_validity`. Språket sendes i
#' HTTP-hodet `Accept-Language`.
#'
#' Funksjonen stopper med en feilmelding dersom tjenesten returnerer
#' en HTTP-feil.
#'
#' @examples
#' \dontrun{
#' get_variable_definition_by_shortname(
#'   short_name = "sesongjustering"
#' )
#'
#' get_variable_definition_by_shortname(
#'   short_name = "sesongjustering",
#'   date = "2025-01-01",
#'   language = "en"
#' )
#' }
#'
#' @export

get_variable_definition_by_shortname <- function(
    short_name,
    date = format(Sys.Date(), "%Y-%m-%d"),
    language = "nb",
    status = "auto",
    quiet = TRUE,
    token = NULL
) {
  
  .datadoc_text(short_name, "short_name")
  
  status <- match.arg(
    status,
    c("auto", "ekstern", "intern")
  )
  
  language <- match.arg(
    language,
    c("nb", "nn", "en")
  )
  
  query <- list(
    short_name = short_name,
    date_of_validity = .datadoc_date(date)
  )
  
  # Kontroller om resultatet inneholder riktig definisjon
  gyldig_treff <- function(x) {
    
    if (is.null(x) || NROW(x) == 0L) {
      return(FALSE)
    }
    
    if (!is.list(x) ||
        !"short_name" %in% names(x)) {
      return(FALSE)
    }
    
    any(
      !is.na(x$short_name) &
        x$short_name == short_name
    )
  }
  
  # Bestem hvilke tjenester som skal forsøkes
  tjenester <- if (status == "auto") {
    c("ekstern", "intern")
  } else {
    status
  }
  
  for (tjeneste in tjenester) {
    
    resultat <- .vardef_get(
      query = query,
      status = tjeneste,
      language = language,
      token = token,
      allow_not_found = TRUE,
      quiet = TRUE
    )
    
    if (gyldig_treff(resultat)) {
      return(resultat)
    }
  }
  
  # Ingen treff
  if (!quiet) {
    message(
      "Fant ikke variabeldefinisjonen '",
      short_name,
      "' i ",
      if (status == "auto") {
        "verken ekstern eller intern Vardef."
      } else {
        paste0(status, " Vardef.")
      }
    )
  }
  
  NULL
}



#' Hent variabeldefinisjon etter ID
#'
#' Henter en variabeldefinisjon fra SSBs tjeneste for
#' variabeldefinisjoner basert på variabeldefinisjonens unike ID og en
#' gyldighetsdato.
#'
#' @param id En tekststreng med den unike ID-en til variabeldefinisjonen
#'   som skal hentes.
#' @param date En tekststreng med datoen variabeldefinisjonen skal være
#'   gyldig på, angitt på formatet `"YYYY-MM-DD"`. Standardverdien er
#'   dagens dato.
#' @param language En tekststreng med språkkoden som skal brukes for
#'   tekstinnholdet i responsen. Standardverdien er `"nb"`.
#'
#' @param status `"ekstern"` (standard) eller `"intern"`.
#' @param token Valgfritt Bearer-token til intern tjeneste.
#'
#' @param quiet Med `TRUE` (standard) skjules statusmeldingen ved 404.
#'
#' @return Et objekt opprettet fra JSON-responsen fra tjenesten.
#'   Returtypen avhenger av strukturen i responsen og vil vanligvis være
#'   en liste eller en data frame.
#'
#' @details
#' ID-en legges til URL-en til tjenesten, mens datoen sendes som
#' parameteren `date_of_validity`. Språket sendes i HTTP-hodet
#' `Accept-Language`.
#'
#' Dersom tjenesten returnerer HTTP-status 404 og `quiet = FALSE`, vises en melding om at
#' variabeldefinisjonen ikke ble funnet. Funksjonen returnerer samtidig
#' innholdet i feilresponsen.
#'
#' Andre HTTP-feil fører til at funksjonen stopper med en feilmelding.
#'
#' @examples
#' \dontrun{
#' get_variable_definition_by_id(
#'   id = "91HwKSxr"
#' )
#'
#' get_variable_definition_by_id(
#'   id = "91HwKSxr",
#'   date = "1814-12-31",
#'   language = "en"
#' )
#' }
#'
#' @export
get_variable_definition_by_id <- function(
    id, date = format(Sys.Date(), "%Y-%m-%d"), 
    language = "nb",
    status = "auto", 
    token = NULL, 
    quiet = TRUE) {
  .datadoc_text(id, "id")
  .datadoc_flag(quiet, "quiet")
  .vardef_get(path = id, query = list(date_of_validity = .datadoc_date(date)),
              status = status, language = language, token = token,
              allow_not_found = TRUE, quiet = quiet)
}


#' Hent KLASS-ID fra en variabeldefinisjon
#'
#' Henter den numeriske KLASS-ID-en fra feltet `classification_uri` i en
#' variabeldefinisjon.
#'
#' @param variable_definition En liste eller et listelignende objekt som
#'   inneholder feltet `classification_uri`.
#'
#' @return En tekststreng med KLASS-ID-en. Dersom `classification_uri`
#'   mangler, er tom eller ikke inneholder en numerisk ID på slutten av
#'   adressen, returneres `NA_character_`.
#'
#' @details
#' Funksjonen forventer at `classification_uri` avsluttes med en numerisk
#' KLASS-ID, eventuelt etterfulgt av en skråstrek.
#'
#' Dersom `classification_uri` finnes, men ikke har forventet format, vises
#' en advarsel før `NA_character_` returneres.
#'
#' @examples
#' variable_definition <- list(
#'   classification_uri =
#'     "https://www.ssb.no/klass/klassifikasjoner/1"
#' )
#'
#' vardef_get_klass_id(variable_definition)
#'
#' variable_definition <- list(
#'   classification_uri =
#'     "https://www.ssb.no/klass/klassifikasjoner/1/"
#' )
#'
#' vardef_get_klass_id(variable_definition)
#'
#' variable_definition <- list(
#'   classification_uri = NULL
#' )
#'
#' vardef_get_klass_id(variable_definition)
#'
#' @export
vardef_get_klass_id <- function(variable_definition) {
  uri <- .datadoc_first(variable_definition$classification_uri)
  id <- .datadoc_klass_id(uri)
  if (!is.na(uri) && is.na(id)) {
    warning("Fant ingen KLASS-ID i `classification_uri`: ", uri, call. = FALSE)
  }
  id
}



#' Lag oversikt over variabler i en Datadoc-fil
#'
#' Leser variabelmetadata fra en Datadoc-fil og lager en oversikt
#' over variablene. Dersom en variabel har en referanse til en
#' variabeldefinisjon, hentes supplerende metadata fra Vardef.
#'
#' @param filsti Filsti til en Datadoc-fil eller tilhørende
#'   Parquet-fil.
#' @param language Språkkode: "nb", "nn" eller "en".
#'   Standardverdien er "nb".
#' @param include_vardef Logisk verdi. Med FALSE leses bare
#'   lokale metadata uten oppslag i Vardef.
#'   Standardverdien er TRUE.
#' @param status "auto" (standard), "ekstern" eller "intern".
#'   Med "auto" søkes det først i ekstern Vardef, deretter
#'   intern Vardef for definisjoner som ikke finnes eksternt.
#'
#' @details
#' Vardef-oppslag gjøres etter definition_uri uten filtrering
#' på gyldighetsdato. Dataperioden i Datadoc påvirker dermed
#' ikke hvilken Vardef-definisjon som kan hentes.
#'
#' contains_data_from og contains_data_until beholdes fra
#' Datadoc og kan brukes ved senere oppslag i KLASS.
#'
#' Både classification_uri og classification_reference fra
#' Vardef støttes. KLASS-referanser returneres som KLASS-ID.
#'
#' @return En data.frame med én rad per variabel og følgende
#'   kolonner:
#'   short_name, name, data_type, classification_uri,
#'   vardef_definition_uri, vardef_name, vardef_short_name,
#'   vardef_classification_uri, contains_data_from og
#'   contains_data_until.
#'
#' @export
datadoc_variabeloversikt <- function(
    filsti,
    language = "nb",
    include_vardef = TRUE,
    status = "auto"
) {
  
  language <- match.arg(language, c("nb", "nn", "en"))
  status <- match.arg(status, c("auto", "ekstern", "intern"))
  
  .datadoc_flag(include_vardef, "include_vardef")
  
  # ------------------------------------------------------------
  # 1. Les Datadoc
  # ------------------------------------------------------------
  
  doc <- .datadoc_read(filsti)
  
  variables <- doc$datadoc$variables
  
  if (is.null(variables)) {
    variables <- list()
  }
  
  felt <- function(navn) {
    vapply(
      variables,
      function(x) .datadoc_first(x[[navn]]),
      character(1)
    )
  }
  
  n <- length(variables)
  
  # ------------------------------------------------------------
  # 2. Opprett oversikt fra lokale metadata
  # ------------------------------------------------------------
  
  result <- data.frame(
    short_name = felt("short_name"),
    
    name = vapply(
      variables,
      function(x) {
        .datadoc_language_text(x$name, language)
      },
      character(1)
    ),
    
    data_type = felt("data_type"),
    
    classification_uri = vapply(
      variables,
      function(x) {
        .datadoc_klass_id(x$classification_uri)
      },
      character(1)
    ),
    
    vardef_definition_uri = rep(NA_character_, n),
    vardef_name = rep(NA_character_, n),
    vardef_short_name = rep(NA_character_, n),
    vardef_classification_uri = rep(NA_character_, n),
    
    contains_data_from = felt("contains_data_from"),
    contains_data_until = felt("contains_data_until"),
    
    stringsAsFactors = FALSE
  )
  
  if (!include_vardef || !n) {
    return(result)
  }
  
  # ------------------------------------------------------------
  # 3. Finn Vardef-ID-er fra Datadoc
  # ------------------------------------------------------------
  
  ids <- vapply(
    variables,
    function(x) {
      .datadoc_uri_id(x$definition_uri)
    },
    character(1)
  )
  
  relevante_ids <- unique(
    ids[!is.na(ids) & nzchar(ids)]
  )
  
  if (!length(relevante_ids)) {
    return(result)
  }
  
  # Behold ID fra Datadoc også når oppslaget ikke gir treff.
  result$vardef_definition_uri <- ids
  
  # ------------------------------------------------------------
  # 4. Hjelpefunksjoner for Vardef-responser
  # ------------------------------------------------------------
  
  # Hent ett metadatafelt fra én rad i Vardef-oversikten.
  hent_felt <- function(tab, felt, i) {
    
    if (!felt %in% names(tab)) {
      return(NULL)
    }
    
    kolonne <- tab[[felt]]
    
    # Nøstede data frames: hent riktig rad
    if (is.data.frame(kolonne)) {
      return(kolonne[i, , drop = FALSE])
    }
    
    # Listekolonner: hent riktig element
    if (is.list(kolonne)) {
      return(kolonne[[i]])
    }
    
    # Vanlige vektorer: hent riktig verdi
    kolonne[[i]]
  }
  
  # Konverter et enkelt felt til én tekstverdi.
  tekst <- function(x) {
    
    if (is.null(x)) {
      return(NA_character_)
    }
    
    x <- unlist(x, use.names = FALSE)
    
    if (!length(x)) {
      return(NA_character_)
    }
    
    if (length(x) != 1L) {
      stop(
        "Forventet én verdi i Vardef-feltet.",
        call. = FALSE
      )
    }
    
    .datadoc_first(x)
  }
  
  # Hent navn på ønsket språk.
  vardef_navn <- function(tab, i) {
    
    flat <- paste0("name.", language)
    
    if (flat %in% names(tab)) {
      
      navn <- tekst(hent_felt(tab, flat, i))
      
      if (!is.na(navn) && nzchar(trimws(navn))) {
        return(navn)
      }
      
      # Bruk bokmål som reserve.
      navn_nb <- tekst(hent_felt(tab, "name.nb", i))
      
      if (!is.na(navn_nb) && nzchar(trimws(navn_nb))) {
        return(navn_nb)
      }
      
      return(NA_character_)
    }
    
    # Nøstet språkformat fra Vardef.
    navn <- hent_felt(tab, "name", i)
    
    .vardef_language_text(
      navn,
      language
    )
  }
  
  # Hent KLASS-ID fra Vardef.
  vardef_klass_id <- function(tab, i) {
    
    # Intern Vardef bruker classification_reference.
    ref <- tekst(
      hent_felt(tab, "classification_reference", i)
    )
    
    if (!is.na(ref) && nzchar(trimws(ref))) {
      
      if (grepl("^[0-9]+$", ref)) {
        return(ref)
      }
      
      return(.datadoc_klass_id(ref))
    }
    
    # Ekstern Vardef kan bruke classification_uri.
    uri <- hent_felt(tab, "classification_uri", i)
    
    .datadoc_klass_id(uri)
  }
  
  # ------------------------------------------------------------
  # 5. Hent definisjoner fra Vardef
  # ------------------------------------------------------------
  
  tjenester <- if (status == "auto") {
    c("ekstern", "intern")
  } else {
    status
  }
  
  # Lagre resultater etter Vardef-ID.
  cache <- new.env(parent = emptyenv())
  
  gjenstaar <- relevante_ids
  
  for (tjeneste in tjenester) {
    
    if (!length(gjenstaar)) {
      break
    }
    
    # Viktig:
    # date = NULL gjør at oppslaget ikke filtreres
    # på Datadoc-variabelens dataperiode.
    tab <- get_all_variable_definitions(
      date = NULL,
      language = language,
      status = tjeneste
    )
    
    if (is.null(tab)) {
      next
    }
    
    if (!is.data.frame(tab)) {
      stop(
        "Uventet responsformat fra ",
        tjeneste,
        " Vardef.",
        call. = FALSE
      )
    }
    
    if (!nrow(tab)) {
      next
    }
    
    if (!"id" %in% names(tab)) {
      stop(
        "Responsen fra ",
        tjeneste,
        " Vardef mangler `id`.",
        call. = FALSE
      )
    }
    
    # Undersøk bare Vardef-ID-er som mangler treff.
    treff_indeks <- which(
      !is.na(tab$id) &
        tab$id %in% gjenstaar
    )
    
    if (!length(treff_indeks)) {
      next
    }
    
    funnet <- character()
    
    for (i in treff_indeks) {
      
      id <- as.character(tab$id[[i]])
      
      definisjon <- list(
        id = id,
        
        name = vardef_navn(tab, i),
        
        short_name = tekst(
          hent_felt(tab, "short_name", i)
        ),
        
        classification_uri = vardef_klass_id(tab, i)
      )
      
      # Ikke velg vilkårlig mellom ulike poster med samme ID.
      if (exists(id, envir = cache, inherits = FALSE)) {
        
        eksisterende <- get(
          id,
          envir = cache,
          inherits = FALSE
        )
        
        if (!identical(eksisterende, definisjon)) {
          stop(
            "Fant motstridende Vardef-poster med ID `",
            id,
            "` i ",
            tjeneste,
            " Vardef.",
            call. = FALSE
          )
        }
        
      } else {
        
        assign(
          id,
          definisjon,
          envir = cache
        )
      }
      
      funnet <- c(funnet, id)
    }
    
    # Søk internt bare for ID-er uten eksternt treff.
    gjenstaar <- setdiff(
      gjenstaar,
      unique(funnet)
    )
  }
  
  # ------------------------------------------------------------
  # 6. Koble Vardef-metadata til Datadoc-variablene
  # ------------------------------------------------------------
  
  for (i in which(!is.na(ids) & nzchar(ids))) {
    
    id <- ids[[i]]
    
    if (!exists(id, envir = cache, inherits = FALSE)) {
      next
    }
    
    value <- get(
      id,
      envir = cache,
      inherits = FALSE
    )
    
    result$vardef_definition_uri[[i]] <- value$id
    
    result$vardef_name[[i]] <- value$name
    
    result$vardef_short_name[[i]] <- value$short_name
    
    result$vardef_classification_uri[[i]] <-
      value$classification_uri
  }
  
  # ------------------------------------------------------------
  # 7. Returner oversikten
  # ------------------------------------------------------------
  
  result
}




#' Finn variabler med eller uten kodeliste
#'
#' Henter en oversikt over variablene i en DataDoc-fil og filtrerer
#' variablene etter om de har en tilknyttet kodeliste.
#'
#' En variabel regnes som å ha en kodeliste dersom det finnes en
#' `classification_uri` enten direkte i DataDoc-filen eller i den
#' tilknyttede variabeldefinisjonen.
#'
#' @param filsti Filsti til en DataDoc-fil eller en Parquet-fil. Filstien til
#'   den tilhørende DataDoc-filen utledes ved hjelp av [datadoc_path()].
#' @param with_codelist En logisk verdi som angir hvilke variabler som skal
#'   returneres. Når verdien er `TRUE`, returneres variabler med kodeliste.
#'   Når verdien er `FALSE`, returneres variabler uten kodeliste.
#'   Standardverdien er `TRUE`.
#' @param language En tekststreng med språkkoden som skal brukes ved uthenting
#'   av språkavhengig metadata. Standardverdien er `"nb"`.
#'
#' @return En `data.frame` med én rad per variabel som oppfyller det valgte
#'   kriteriet. Kolonnene er de samme som i resultatet fra
#'   [datadoc_variabeloversikt()].
#'
#' @details
#' Funksjonen undersøker kolonnene `classification_uri` og
#' `vardef_classification_uri` i resultatet fra
#' [datadoc_variabeloversikt()].
#'
#' En variabel regnes som å ha en kodeliste dersom minst én av disse
#' kolonnene inneholder en verdi som ikke er tom eller manglende.
#'
#' `with_codelist` må være én enkelt logisk verdi og kan ikke være `NA`.
#'
#' @examples
#' \dontrun{
#' # Hent variabler med kodeliste
#' variables_with_classification_uri(
#'   filsti = "/buckets/data/personell_v1.parquet"
#' )
#'
#' # Hent variabler uten kodeliste
#' variables_with_classification_uri(
#'   filsti = "/buckets/data/personell_v1.parquet",
#'   with_codelist = FALSE
#' )
#'
#' # Hent engelskspråklig metadata
#' variables_with_classification_uri(
#'   filsti = "/buckets/data/personell_v1.parquet",
#'   language = "en"
#' )
#' }
#'
#' @seealso
#' [datadoc_variabeloversikt()] for oversikten som filtreres.
#'
#' @export
variables_with_classification_uri <- function(
    filsti,
    with_codelist = TRUE,
    language = "nb"
) {
  
  if (
    !is.logical(with_codelist) ||
    length(with_codelist) != 1L ||
    is.na(with_codelist)
  ) {
    stop(
      "`with_codelist` må være enten TRUE eller FALSE.",
      call. = FALSE
    )
  }
  
  variabeloversikt <- datadoc_variabeloversikt(
    filsti = filsti,
    language = language
  ) |>
    dplyr::mutate(
      har_classification_uri_datadoc =
        !is.na(classification_uri) &
        nzchar(trimws(classification_uri)),
      
      har_classification_uri_vardef =
        !is.na(vardef_classification_uri) &
        nzchar(trimws(vardef_classification_uri)),
      
      har_kodeliste =
        har_classification_uri_datadoc |
        har_classification_uri_vardef
    )
  
  variabeloversikt |>
    dplyr::filter(
      har_kodeliste == with_codelist
    ) |>
    dplyr::select(
      -har_classification_uri_datadoc,
      -har_classification_uri_vardef,
      -har_kodeliste
    )
}

#' Legg til verdietiketter fra KLASS
#'
#' Henter kodelister som er referert fra DataDoc eller Vardef. En direkte
#' KLASS-referanse i DataDoc har forrang. Språk og datoperiode brukes ved
#' oppslag; dagens dato brukes hvis sluttdato mangler.
#'
#' @param data En data.frame eller tibble med numeriske eller tekstbaserte koder.
#' @param filsti Filsti til DataDoc-filen eller den tilhørende Parquet-filen.
#' @param language Språkkode: `"nb"` (standard), `"nn"` eller `"en"`.
#' @param quiet Med `TRUE` (standard) skjules statusmeldinger. Advarsler om
#'   manglende etiketter eller mislykkede oppslag og feil vises fortsatt.
#' @param clean_labels Med `TRUE` (standard) fjernes HTML- og
#'   formateringstagger fra etikettene og mellomrom ryddes opp.
#' @details Datatypen, verdiene og kolonneetikettene beholdes. Kodene i
#' kodelisten tilpasses variabelens datatype; uforenlige koder gir feil.
#' Numeriske variabler får numeriske verdietikettkoder. Tekstkoder beholder
#' ledende nuller. Eksisterende verdietiketter erstattes ved gyldig treff.
#' Observerte verdier uten etikett beholdes og gir advarsel, også med
#' `quiet = TRUE`. Vanlige manglende verdier (`NA`) utelates fra kontrollen.
#' @return Datasettet med verdietiketter på variabler med treff.
#' @examples
#' \dontrun{
#' resultat <- add_value_labels(data, "data__DOC.json")
#' }
#' @export
add_value_labels <- function(data, 
                             filsti, 
                             language = "nb", 
                             quiet = TRUE,
                             clean_labels = TRUE) {
  .datadoc_flag(quiet, "quiet")
  .datadoc_flag(clean_labels, "clean_labels")
  language <- match.arg(language, c("nb", "nn", "en"))
  if (!is.data.frame(data)) stop("`data` må være en data.frame eller tibble.", call. = FALSE)
  kjor <- function() {
    meta <- variables_with_classification_uri(filsti, language = language)
    for (i in seq_len(nrow(meta))) {
      variabel <- meta$short_name[[i]]
      if (!variabel %in% names(data)) {
        warning("Variabelen `", variabel, "` finnes ikke i datasettet.", call. = FALSE)
        next
      }
      id <- meta$classification_uri[[i]]
      if (is.na(id) || !nzchar(id)) id <- meta$vardef_classification_uri[[i]]
      if (is.na(id) || !grepl("^[0-9]+$", id)) {
        warning("Ugyldig KLASS-ID for `", variabel, "`.", call. = FALSE)
        next
      }
      dato <- meta$contains_data_until[[i]]
      if (is.na(dato) || !nzchar(dato)) dato <- as.character(Sys.Date())
      koder <- tryCatch(
        klassR::get_klass(id, date = dato, language = language),
        error = function(e) {
          warning("Kunne ikke hente kodeliste for `", variabel, "`: ",
                  conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      if (is.null(koder)) next
      if (!all(c("code", "name") %in% names(koder))) {
        stop("Kodelisten mangler `code` eller `name` for `", variabel, "`.", call. = FALSE)
      }
      data[[variabel]] <- .datadoc_set_value_labels(
        data[[variabel]], koder$code, koder$name, variabel, clean_labels
      )
    }
    data
  }
  if (quiet) suppressMessages(kjor()) else kjor()
}


#' Vis verdietiketter i et datasett
#'
#' Erstatter verdiene i merkede variabler med tilhørende verdietiketter.
#' Variabler uten verdietiketter beholdes uendret.
#'
#' @param data Et datasett som kan inneholde variabler med verdietiketter.
#' @param labels En logisk verdi som angir om verdietikettene skal vises.
#'   Når verdien er `TRUE`, konverteres merkede variabler til faktorer med
#'   verdietikettene som faktorverdier. Når verdien er `FALSE`, returneres
#'   datasettet uendret. Standardverdien er `TRUE`.
#'
#' @return Datasettet som ble oppgitt i `data`. Dersom `labels = TRUE`,
#'   er merkede variabler konvertert til faktorer med verdietikettene som
#'   faktorverdier.
#'
#' @details
#' Funksjonen identifiserer merkede variabler ved hjelp av
#' [haven::is.labelled()] og konverterer dem med [haven::as_factor()].
#'
#' Ved konverteringen brukes `levels = "default"`. Verdier med etikett
#' vises med etiketten; øvrige verdier vises med koden og gir en advarsel. Dette fungerer
#' både for numeriske og tekstbaserte merkede variabler, inkludert
#' tekstkoder med ledende nuller, som `"01"` og `"02"`.
#'
#' `labels` må være én enkelt logisk verdi og kan ikke være `NA`.
#'
#' @examples
#' data <- data.frame(
#'   kjoenn = labelled::labelled(
#'     c(1, 2, 1),
#'     labels = c(
#'       Mann = 1,
#'       Kvinne = 2
#'     )
#'   )
#' )
#'
#' show_labels_df(
#'   data = data
#' )
#'
#' show_labels_df(
#'   data = data,
#'   labels = FALSE
#' )
#'
#' @export
show_labels_df <- function(data, labels = TRUE) {
  .datadoc_flag(labels, "labels")
  if (!labels) return(data)
  original <- data
  for (variabel in names(data)) {
    if (!haven::is.labelled(data[[variabel]])) next
    .datadoc_warn_unlabelled(data[[variabel]], variabel)
    data[[variabel]] <- .datadoc_as_factor(data[[variabel]])
  }
  .datadoc_restore_groups(data, original)
}


#' Vis kolonneetiketter ved utskrift av et datasett
#'
#' Legger variabelens kolonneetikett til som en `pillar`-etikett, slik at
#' etiketten vises under kolonnenavnet når datasettet skrives ut som en
#' tibble.
#'
#' Variabler som ikke har en kolonneetikett, beholdes uendret.
#'
#' @param data Et datasett som kan inneholde variabler med
#'   kolonneetiketter.
#'
#' @return Datasettet som ble oppgitt i `data`, med kolonneetikettene
#'   registrert i variabelens `pillar`-attributt. Verdiene, kolonnenavnene
#'   og de opprinnelige kolonneetikettene beholdes uendret.
#'
#' @details
#' Kolonneetiketten for hver variabel hentes med
#' [labelled::var_label()]. Dersom variabelen har en ikke-tom etikett,
#' lagres denne som elementet `label` i variabelens `pillar`-attributt.
#'
#' `pillar`-attributtet brukes ved utskrift av tibble-objekter og påvirker
#' ikke verdiene i datasettet. Funksjonen endrer heller ikke variabelens
#' opprinnelige `label`-attributt.
#'
#' Eksisterende elementer i `pillar`-attributtet beholdes. Dersom
#' attributtet allerede inneholder et element med navnet `label`,
#' erstattes dette med variabelens gjeldende kolonneetikett.
#'
#' @examples
#' data <- tibble::tibble(
#'   kjoenn = c(1, 2, 1),
#'   alder = c(35, 42, 28)
#' )
#'
#' labelled::var_label(data$kjoenn) <- "Kjønn"
#' labelled::var_label(data$alder) <- "Alder i år"
#'
#' show_column_labels(data)
#'
#' @seealso
#' [labelled::var_label()] for å hente og angi kolonneetiketter.
#'
#' @export
show_column_labels <- function(data) {
  
  vis_kolonnelabel <- function(x) {
    
    kolonnelabel <- labelled::var_label(x)
    
    if (
      is.null(kolonnelabel) ||
      length(kolonnelabel) == 0L ||
      is.na(kolonnelabel[[1L]]) ||
      !nzchar(trimws(as.character(kolonnelabel[[1L]])))
    ) {
      pillar_attributt <- attr(x, "pillar", exact = TRUE)
      if (is.list(pillar_attributt)) {
        pillar_attributt$label <- NULL
        attr(x, "pillar") <- if (length(pillar_attributt)) pillar_attributt else NULL
      }
      return(x)
    }
    
    kolonnelabel <- trimws(
      as.character(kolonnelabel[[1L]])
    )
    
    pillar_attributt <- attr(x, "pillar")
    
    if (is.null(pillar_attributt)) {
      pillar_attributt <- list()
    }
    
    pillar_attributt$label <- kolonnelabel
    
    attr(x, "pillar") <- pillar_attributt
    
    x
  }
  
  original <- data
  for (variabel in names(data)) data[[variabel]] <- vis_kolonnelabel(data[[variabel]])
  .datadoc_restore_groups(data, original)
}



#' Legg til kolonneetiketter fra Datadoc
#'
#' Legger til kolonneetiketter på variablene i et datasett basert på
#' variabelnavnene i den tilhørende DataDoc-filen.
#'
#' @param data Et datasett som skal få lagt til kolonneetiketter.
#' @param filsti En tekststreng med filstien til Parquet-filen som datasettet
#'   er lest fra. Filstien brukes til å finne den tilhørende DataDoc-filen.
#' @param show_labels En logisk verdi som angir om kolonneetikettene også skal
#'   vises ved utskrift av datasettet. Når verdien er `TRUE`, behandles
#'   datasettet med [show_column_labels()]. Standardverdien er `TRUE`.
#'
#' @param language Språkkode: `"nb"` (standard), `"nn"` eller `"en"`.
#'
#' @return Datasettet som ble oppgitt i `data`, med kolonneetiketter lagt til
#'   for variabler som finnes både i datasettet og i DataDoc-filen.
#'
#' @details
#' Variabeloversikten hentes lokalt med [datadoc_variabeloversikt()]
#' og `include_vardef = FALSE`, uten nettverkskall. Verdiene i
#' kolonnen `name` brukes som kolonneetiketter, mens `short_name` brukes til
#' å koble etikettene til variablene i `data`.
#'
#' Metadata for variabler som ikke finnes i `data`, ignoreres.
#'
#' Kolonneetikettene legges til med [labelled::var_label()]. Dersom
#' `show_labels = TRUE`, registreres etikettene i tillegg som
#' `pillar`-etiketter med [show_column_labels()], slik at de vises under
#' kolonnenavnene når datasettet skrives ut som en tibble.
#'
#' Meldinger som oppstår når variabeloversikten hentes, undertrykkes.
#' Advarsler og feil undertrykkes ikke.
#'
#' `show_labels` må være én enkelt logisk verdi og kan ikke være `NA`.
#'
#' @examples
#' \dontrun{
#' data_med_labels <- add_labels(
#'   data = personell,
#'   filsti = "data/personell.parquet"
#' )
#'
#' data_med_labels <- add_labels(
#'   data = personell,
#'   filsti = "data/personell.parquet",
#'   show_labels = FALSE
#' )
#' }
#'
#' @seealso
#' [datadoc_variabeloversikt()] for å hente variabelmetadata,
#' [labelled::var_label()] for å legge til kolonneetiketter og
#' [show_column_labels()] for å vise etikettene ved utskrift.
#'
#' @export
add_labels <- function(
    data,
    filsti,
    show_labels = TRUE,
    language = "nb"
) {
  
  if (
    !is.logical(show_labels) ||
    length(show_labels) != 1L ||
    is.na(show_labels)
  ) {
    stop(
      "`show_labels` må være enten TRUE eller FALSE.",
      call. = FALSE
    )
  }
  
  datadoc_variabeloversikt_df <- suppressMessages(
    datadoc_variabeloversikt(
      filsti = filsti, language = language, include_vardef = FALSE
    )
  )
  
  labels <- stats::setNames(
    datadoc_variabeloversikt_df$name,
    datadoc_variabeloversikt_df$short_name
  )
  
  labels <- labels[
    names(labels) %in% names(data) & !is.na(labels) & nzchar(trimws(labels))
  ]
  
  labelled::var_label(data) <- as.list(labels)
  
  if (show_labels) {
    data <- data |>
      show_column_labels()
  }
  
  data
}


#' Finn variabler uten kolonneetikett
#'
#' Finner variabler i et datasett som mangler en kolonneetikett.
#'
#' @param data Et datasett som skal undersøkes for kolonneetiketter.
#'
#' @return En tekstvektor med navnene på variablene som mangler
#'   kolonneetikett. Dersom alle variablene har en kolonneetikett,
#'   returneres en tom tekstvektor.
#'
#' @details
#' Kolonneetikettene hentes med [labelled::var_label()].
#'
#' En variabel regnes som å mangle kolonneetikett dersom etiketten:
#'
#' \itemize{
#'   \item er `NULL`;
#'   \item har lengde null;
#'   \item bare inneholder manglende verdier; eller
#'   \item bare inneholder tom tekst eller mellomrom.
#' }
#'
#' @examples
#' data <- data.frame(
#'   kjoenn = c(1, 2, 1),
#'   alder = c(35, 42, 28),
#'   inntekt = c(450000, 520000, 390000)
#' )
#'
#' labelled::var_label(data$kjoenn) <- "Kjønn"
#' labelled::var_label(data$alder) <- "Alder i år"
#'
#' vars_without_labels(data)
#'
#' @seealso
#' [labelled::var_label()] for å hente eller angi kolonneetiketter.
#'
#' @export
vars_without_labels <- function(data) {
  
  labels <- labelled::var_label(
    data,
    unlist = FALSE
  )
  
  mangler_label <- vapply(
    labels,
    function(x) {
      
      if (is.null(x) || length(x) == 0L) {
        return(TRUE)
      }
      
      x <- trimws(as.character(x))
      
      all(
        is.na(x) |
          !nzchar(x)
      )
    },
    FUN.VALUE = logical(1)
  )
  
  names(data)[mangler_label]
}


#' Finn variabler med verdietiketter
#'
#' Finner variabler i et datasett som har én eller flere
#' verdietiketter.
#'
#' @param data Et datasett som skal undersøkes for verdietiketter.
#'
#' @return En tekstvektor med navnene på variablene som har minst én
#'   verdietikett. Dersom ingen variabler har verdietiketter, returneres
#'   en tom tekstvektor.
#'
#' @details
#' Verdietikettene for hver variabel hentes med
#' [labelled::val_labels()]. En variabel inkluderes i resultatet dersom
#' den har minst én registrert verdietikett.
#'
#' Kolonneetiketter, som hentes med [labelled::var_label()], tas ikke med
#' i vurderingen.
#'
#' @examples
#' data <- data.frame(
#'   kjoenn = labelled::labelled(
#'     c(1, 2, 1),
#'     labels = c(
#'       Mann = 1,
#'       Kvinne = 2
#'     )
#'   ),
#'   alder = c(35, 42, 28)
#' )
#'
#' vars_with_value_labels(data)
#'
#' @seealso
#' [labelled::val_labels()] for å hente verdietiketter og
#' [vars_without_labels()] for å finne variabler uten kolonneetikett.
#'
#' @export
vars_with_value_labels <- function(data) {
  purrr::keep(
    names(data),
    ~ length(labelled::val_labels(data[[.x]])) > 0L
  )
}



#' Finn observerte verdier uten verdietikett
#'
#' Finner observerte verdier i merkede variabler som ikke har en
#' tilhørende verdietikett.
#'
#' @param data Et datasett som skal undersøkes for manglende
#'   verdietiketter.
#'
#' @details Kolonnen `value_without_label` er tekst slik at numeriske og
#' tekstbaserte koder kan samles i samme oversikt. Inndata endres ikke.
#'
#' @return En tibble med én rad per observert verdi som mangler
#'   verdietikett, og følgende kolonner:
#'
#' \describe{
#'   \item{`variable`}{Navnet på variabelen.}
#'   \item{`value_without_label`}{Den observerte verdien som mangler
#'     verdietikett.}
#' }
#'
#' Dersom alle observerte verdier har verdietiketter, returneres en tom
#' tibble.
#'
#' @details
#' Funksjonen undersøker variablene som returneres av
#' [vars_with_value_labels()]. Det betyr at bare variabler som allerede har
#' minst én verdietikett, blir kontrollert.
#'
#' Manglende verdier fjernes før kontrollen. De gjenværende observerte
#' verdiene sammenlignes med verdiene som er registrert i
#' [labelled::val_labels()].
#'
#' Variabler som ikke har noen verdietiketter, tas ikke med i resultatet.
#' Funksjonen er derfor beregnet på å finne umerkede verdier i variabler
#' med en delvis definert verdietikettliste.
#'
#' @examples
#' data <- data.frame(
#'   kjoenn = labelled::labelled(
#'     c(1, 2, 3, NA),
#'     labels = c(
#'       Mann = 1,
#'       Kvinne = 2
#'     )
#'   ),
#'   alder = c(35, 42, 28, 51)
#' )
#'
#' values_without_labels(data)
#'
#' @seealso
#' [vars_with_value_labels()] for å finne variabler med verdietiketter og
#' [labelled::val_labels()] for å hente verdietikettene til en variabel.
#'
#' @export
values_without_labels <- function(data) {
  result <- lapply(vars_with_value_labels(data), function(var) {
    missing <- .datadoc_unlabelled_values(data[[var]])
    data.frame(variable = rep(var, length(missing)),
               value_without_label = as.character(missing), stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(tibble::tibble(variable = character(), value_without_label = character()), result)
}

#' Fjern alle etiketter fra et datasett
#'
#' Fjerner verdietiketter og kolonneetiketter fra alle variabler i et
#' datasett.
#'
#' @param data Et lokalt eller lazy datasett som etikettene skal fjernes fra.
#'
#' @return En `data.frame` uten verdietiketter eller kolonneetiketter.
#'
#' @details
#' Dataene hentes først inn i minnet med [dplyr::collect()]. Funksjonen kan
#' derfor brukes på både lokale datasett og lazy tabeller som støttes av
#' `dplyr`.
#'
#' Verdietiketter fjernes med [haven::zap_labels()], mens kolonneetiketter
#' fjernes med [haven::zap_label()].
#'
#' For variabler av typen `haven_labelled_spss` vil brukerdefinerte
#' manglende verdier som standard omgjøres til vanlige `NA`-verdier når
#' verdietikettene fjernes.
#'
#' Visningsetiketten i `pillar` fjernes også. Andre metadataattributter,
#' som formater og kolonnebredder, beholdes.
#'
#' @examples
#' data <- data.frame(
#'   kjoenn = labelled::labelled(
#'     c(1, 2, 1),
#'     labels = c(
#'       Mann = 1,
#'       Kvinne = 2
#'     )
#'   ),
#'   alder = c(35, 42, 28)
#' )
#'
#' labelled::var_label(data$kjoenn) <- "Kjønn"
#' labelled::var_label(data$alder) <- "Alder i år"
#'
#' labelled::var_label(
#'   data,
#'   unlist = FALSE
#' )
#'
#' data_uten_labels <- remove_all_labels(data)
#'
#' labelled::var_label(
#'   data_uten_labels,
#'   unlist = FALSE
#' )
#'
#' @seealso
#' [haven::zap_labels()] for å fjerne verdietiketter og
#' [haven::zap_label()] for å fjerne kolonneetiketter.
#'
#' @export
remove_all_labels <- function(data) {
  data <- data |>
    dplyr::collect() |>
    haven::zap_labels() |>
    haven::zap_label() |>
    as.data.frame()
  for (navn in names(data)) {
    pillar <- attr(data[[navn]], "pillar", exact = TRUE)
    if (is.list(pillar)) {
      pillar$label <- NULL
      attr(data[[navn]], "pillar") <- if (length(pillar)) pillar else NULL
    }
  }
  data
}


#' Hent metadata for én variabel fra Datadoc
#'
#' Leser en DataDoc-fil og henter det fullstendige metadataobjektet for
#' en bestemt variabel.
#'
#' @param filsti Filsti til en DataDoc-fil eller en Parquet-fil. Filstien
#'   til den tilhørende DataDoc-filen utledes med [datadoc_path()].
#' @param variabel En tekststreng med kortnavnet til variabelen som
#'   metadata skal hentes for.
#'
#' @return En liste med metadataene som er registrert for variabelen i
#'   DataDoc-filen.
#'
#' @details
#' Funksjonen leser DataDoc-filen med
#' [jsonlite::fromJSON()] og søker etter en variabel der feltet
#' `short_name` er identisk med verdien i `variabel`.
#'
#' Funksjonen stopper med en feilmelding dersom variabelen ikke finnes,
#' eller dersom flere variabler har samme kortnavn.
#'
#' @examples
#' \dontrun{
#' metadata <- metadata_variable(
#'   filsti = "data/personell.parquet",
#'   variabel = "kjoenn"
#' )
#'
#' metadata$name
#' metadata$classification_uri
#' }
#'
#' @seealso
#' [datadoc_path()] for å utlede filstien til DataDoc-filen og
#' [datadoc_variabeloversikt()] for å lage en tabellarisk oversikt over
#' alle variablene.
#'
#' @export
metadata_variable <- function(
    filsti,
    variabel
) {
  
  if (
    !is.character(variabel) ||
    length(variabel) != 1L ||
    is.na(variabel) ||
    !nzchar(trimws(variabel))
  ) {
    stop(
      "`variabel` må være én ikke-tom tekststreng.",
      call. = FALSE
    )
  }
  
  variabel <- trimws(variabel)
  
  datadoc_ref <- .datadoc_read(filsti)
  
  variables <- datadoc_ref$datadoc$variables
  
  variabel_indeks <- which(
    vapply(
      variables,
      function(x) {
        identical(x$short_name, variabel)
      },
      FUN.VALUE = logical(1)
    )
  )
  
  if (length(variabel_indeks) == 0L) {
    stop(
      "Fant ikke variabelen `",
      variabel,
      "` i DataDoc-filen.",
      call. = FALSE
    )
  }
  
  if (length(variabel_indeks) > 1L) {
    stop(
      "Fant flere variabler med kortnavnet `",
      variabel,
      "` i DataDoc-filen.",
      call. = FALSE
    )
  }
  
  variables[[variabel_indeks]]
}


#' Kopier variabelmetadata mellom DataDoc-filer
#'
#' Kopierer metadata for én eller flere variabler fra en original
#' DataDoc-fil til en annen DataDoc-fil.
#'
#' Filstiene kan oppgis enten som filstier til DataDoc-filer med
#' filendelsen `.json`, eller som filstier til Parquet-filer med
#' filendelsen `.parquet`. Parquet-filstier konverteres automatisk til
#' tilhørende DataDoc-filstier med [datadoc_path()].
#'
#' Dokumentasjonsfeltene for hver valgt variabel kopieres. Mottakerens
#' `short_name`, ID, datatype, dataelementsti og pseudonymiseringsmetadata
#' beholdes. Ukjente felt beholdes fra mottakeren.
#'
#' @param filsti_datadoc_egen En tekststreng med filstien til DataDoc-filen
#'   som skal oppdateres, eller til den tilhørende Parquet-filen.
#'   Filstien må slutte på `.json` eller `.parquet`.
#' @param filsti_datadoc_original En tekststreng med filstien til DataDoc-filen
#'   som metadataene skal kopieres fra, eller til den tilhørende
#'   Parquet-filen. Filstien må slutte på `.json` eller `.parquet`.
#' @param variabler En tekstvektor som angir hvilke variabler metadata skal
#'   kopieres for. En unavngitt verdi tolkes som at variabelen har samme
#'   `short_name` i begge filer. I en navngitt vektor angir navnet
#'   `short_name` i filen som skal oppdateres, mens verdien angir
#'   `short_name` i originalfilen.
#'
#' @param overwrite Med `TRUE` (standard) erstattes dokumentasjonsfeltene.
#'   Med `FALSE` hoppes hele variabelen over når `name` ikke er `NULL`.
#' @param quiet Med `TRUE` (standard) skjules statusmeldinger.
#'
#' @return Den oppdaterte DataDoc-strukturen som en liste. Den oppdaterte
#'   strukturen skrives samtidig til DataDoc-filen som svarer til
#'   `filsti_datadoc_egen`.
#'
#' @details
#' Dersom en filsti slutter på `.parquet`, erstattes filendelsen med
#' `__DOC.json` ved hjelp av [datadoc_path()]. Filstier som allerede
#' slutter på `.json`, brukes uendret.
#'
#' Med `quiet = FALSE` skrives JSON-filstiene ut før filene leses. Funksjonen
#' stopper med en feilmelding dersom én eller begge DataDoc-filene ikke
#' finnes. Alle manglende filer oppgis i samme feilmelding.
#'
#' Begge DataDoc-filene leses med [jsonlite::fromJSON()] med
#' `simplifyVector = FALSE`.
#'
#' Kjente dokumentasjonsfelt kopieres samlet for hver valgt variabel.
#' `overwrite = FALSE` gjør at hele variabelen hoppes over når mottakerens
#' `name` ikke er `NULL`. Andre felt brukes ikke til å avgjøre dette.
#' Med `overwrite = TRUE` erstattes dokumentasjonsfeltene som finnes i
#' kilden, også når kildeverdien er `NULL`. Felt som ikke finnes i kilden,
#' beholdes fra mottakeren. Tekniske felt overskrives aldri.
#'
#' Følgende kontroller utføres før filen endres:
#'
#' \itemize{
#'   \item filstiene må være ikke-tomme tekststrenger som slutter på
#'     `.json` eller `.parquet`;
#'   \item begge DataDoc-filene må finnes;
#'   \item `variabler` må være en ikke-tom tekstvektor;
#'   \item samme variabel i mottakerfilen kan ikke oppgis flere ganger;
#'   \item `short_name` må være unik i begge DataDoc-filene; og
#'   \item alle oppgitte variabler må finnes i de respektive filene.
#' }
#'
#' Den oppdaterte DataDoc-strukturen skrives tilbake med
#' [jsonlite::write_json()]. Den eksisterende DataDoc-filen som svarer
#' til `filsti_datadoc_egen`, overskrives.
#'
#' @examples
#' \dontrun{
#' # Oppgi Parquet-filstier
#' copy_metadata_variable(
#'   filsti_datadoc_egen = "/buckets/data/egen_v1.parquet",
#'   filsti_datadoc_original = "/buckets/data/original_v1.parquet",
#'   variabler = c("kjoenn", "alder")
#' )
#'
#' # Oppgi DataDoc-filstier
#' copy_metadata_variable(
#'   filsti_datadoc_egen = "/buckets/data/egen__DOC.json",
#'   filsti_datadoc_original = "/buckets/data/original__DOC.json",
#'   variabler = c(kjoenn = "sex")
#' )
#'
#' # Kombiner Parquet- og DataDoc-filsti
#' copy_metadata_variable(
#'   filsti_datadoc_egen = "/buckets/data/egen.parquet",
#'   filsti_datadoc_original = "/buckets/data/original__DOC.json",
#'   variabler = c(
#'     kjoenn = "sex",
#'     "alder"
#'   )
#' )
#' }
#'
#' @seealso
#' [datadoc_path()] for å opprette en DataDoc-filsti fra en
#' Parquet-filsti, [jsonlite::fromJSON()] for å lese DataDoc-filene og
#' [jsonlite::write_json()] for å skrive den oppdaterte filen.
#'
#' @export
copy_metadata_variable <- function(
    filsti_datadoc_egen,
    filsti_datadoc_original,
    variabler,
    overwrite = TRUE,
    quiet = TRUE
) {
  
  .datadoc_flag(overwrite, "overwrite")
  .datadoc_flag(quiet, "quiet")
  resolve_datadoc_path <- .datadoc_resolve_path
  
  filsti_datadoc_egen <- resolve_datadoc_path(
    filsti = filsti_datadoc_egen,
    argument = "filsti_datadoc_egen"
  )
  
  filsti_datadoc_original <- resolve_datadoc_path(
    filsti = filsti_datadoc_original,
    argument = "filsti_datadoc_original"
  )
  
  datadoc_filstier <- c(
    filsti_datadoc_egen = filsti_datadoc_egen,
    filsti_datadoc_original = filsti_datadoc_original
  )
  
  if (!quiet) message(
    "Forsøker å lese følgende DataDoc-filer:\n",
    paste0(
      "- ",
      names(datadoc_filstier),
      ": ",
      unname(datadoc_filstier),
      collapse = "\n"
    )
  )
  
  filer_finnes <- file.exists(
    datadoc_filstier
  )
  
  if (any(!filer_finnes)) {
    
    manglende_filer <- datadoc_filstier[
      !filer_finnes
    ]
    
    stop(
      "Følgende DataDoc-fil",
      if (length(manglende_filer) == 1L) {
        " finnes ikke:\n"
      } else {
        "er finnes ikke:\n"
      },
      paste0(
        "- ",
        names(manglende_filer),
        ": ",
        unname(manglende_filer),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }
  
  datadoc_egen <- .datadoc_read(filsti_datadoc_egen)
  
  datadoc_original <- .datadoc_read(filsti_datadoc_original)
  
  if (
    !is.character(variabler) ||
    length(variabler) == 0L ||
    anyNA(variabler) ||
    any(!nzchar(trimws(variabler)))
  ) {
    stop(
      "`variabler` må være en tekstvektor med minst én ikke-tom verdi.",
      call. = FALSE
    )
  }
  
  variabler <- trimws(variabler)
  
  variabelnavn <- names(variabler)
  
  if (is.null(variabelnavn)) {
    variabelnavn <- rep(
      "",
      length(variabler)
    )
  } else {
    variabelnavn <- trimws(
      variabelnavn
    )
  }
  
  # Elementer uten navn tolkes som samme variabelnavn i begge filer.
  navn_egen <- ifelse(
    variabelnavn == "",
    unname(variabler),
    variabelnavn
  )
  
  navn_original <- unname(variabler)
  
  if (anyDuplicated(navn_egen)) {
    
    duplikater <- unique(
      navn_egen[duplicated(navn_egen)]
    )
    
    stop(
      "Følgende variabler i egen fil er oppgitt flere ganger: ",
      paste(
        duplikater,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  short_name_egen <- vapply(
    datadoc_egen$datadoc$variables,
    function(x) x$short_name,
    FUN.VALUE = character(1)
  )
  
  short_name_original <- vapply(
    datadoc_original$datadoc$variables,
    function(x) x$short_name,
    FUN.VALUE = character(1)
  )
  
  if (anyDuplicated(short_name_egen)) {
    stop(
      "Det finnes duplikate `short_name` i ",
      "`filsti_datadoc_egen`.",
      call. = FALSE
    )
  }
  
  if (anyDuplicated(short_name_original)) {
    stop(
      "Det finnes duplikate `short_name` i ",
      "`filsti_datadoc_original`.",
      call. = FALSE
    )
  }
  
  mangler_i_egen <- setdiff(
    navn_egen,
    short_name_egen
  )
  
  mangler_i_original <- setdiff(
    navn_original,
    short_name_original
  )
  
  if (length(mangler_i_egen) > 0L) {
    stop(
      "Følgende variabler finnes ikke i egen metadatafil: ",
      paste(
        mangler_i_egen,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  if (length(mangler_i_original) > 0L) {
    stop(
      "Følgende variabler finnes ikke i original metadatafil: ",
      paste(
        mangler_i_original,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  antall_kopiert <- 0L
  for (i in seq_along(navn_egen)) {
    
    indeks_egen <- match(
      navn_egen[[i]],
      short_name_egen
    )
    
    indeks_original <- match(
      navn_original[[i]],
      short_name_original
    )
    
    metadata_original <-
      datadoc_original$datadoc$variables[[indeks_original]]
    
    metadata_egen <- datadoc_egen$datadoc$variables[[indeks_egen]]
    if (!overwrite && !is.null(metadata_egen$name)) next
    datadoc_egen$datadoc$variables[[indeks_egen]] <-
      .datadoc_copy_variable(metadata_egen, metadata_original)
    antall_kopiert <- antall_kopiert + 1L
  }
  
  .datadoc_write(
    datadoc_egen,
    path = filsti_datadoc_egen,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )
  
  if (!quiet) message(
    "Kopierte metadata for ",
    antall_kopiert,
    " variabel",
    if (antall_kopiert == 1L) "." else "er.",
    "\nOppdatert fil: ",
    filsti_datadoc_egen
  )
  
  datadoc_egen
}


#' Legg til kolonner med verdietiketter
#'
#' Oppretter nye kolonner med verdietiketter for merkede variabler,
#' uten å endre de opprinnelige variablene.
#'
#' @param data Et datasett som inneholder én eller flere merkede variabler
#'   med verdietiketter.
#' @param variables En karaktervektor med navn på variablene det skal
#'   opprettes nye etikettkolonner for. Alle variablene må finnes i `data`
#'   og være merkede variabler som gjenkjennes av [haven::is.labelled()].
#'   Dersom `NULL`, opprettes nye kolonner for alle merkede variabler i
#'   `data`. Standardverdien er `NULL`.
#' @param postfix En tekststreng som legges til på slutten av navnet til
#'   de nye variablene. Standardverdien er `"_labelled"`.
#'
#' @return Datasettet som ble oppgitt i `data`, med én ny kolonne for hver
#'   valgt merket variabel. De opprinnelige variablene beholdes uendret.
#'   De nye variablene er faktorer der verdietikettene brukes som
#'   faktorverdier.
#'
#' @details
#' Funksjonen konverterer de valgte variablene med [haven::as_factor()] og
#' `levels = "default"`. Dette innebærer at de nye kolonnene inneholder
#' verdietikettene der de finnes, ellers kodene. De opprinnelige variablene
#' beholdes uendret.
#'
#' Dersom `variables = NULL`, identifiseres alle merkede variabler i
#' datasettet ved hjelp av [haven::is.labelled()]. Dersom ingen merkede
#' variabler finnes, returneres datasettet uendret og det gis en advarsel.
#'
#' Navnet på hver ny variabel består av det opprinnelige variabelnavnet
#' etterfulgt av verdien i `postfix`. Dersom for eksempel variabelen heter
#' `kjoenn` og `postfix = "_labelled"`, får den nye variabelen navnet
#' `kjoenn_labelled`.
#'
#' Funksjonen gir en feil dersom:
#'
#' * `variables` ikke er `NULL` eller en karaktervektor med minst ett
#'   variabelnavn.
#' * én eller flere av variablene i `variables` ikke finnes i `data`.
#' * én eller flere av variablene i `variables` ikke er merkede variabler.
#' * én eller flere av de nye variablene allerede finnes i `data`.
#' * `postfix` ikke er én enkelt tekststreng.
#'
#' Observerte verdier uten etikett gir en advarsel med variabelnavn og
#' koder. Disse vises som koder i faktorkolonnen og blir ikke gjort om til
#' `NA`. Vanlige manglende verdier utløser ikke advarselen. Gruppering
#' beholdes, og grupperingsvariabler kan også velges.
#'
#' @examples
#' data <- data.frame(
#'   kjoenn = labelled::labelled(
#'     c(1, 2, 1),
#'     labels = c(
#'       Mann = 1,
#'       Kvinne = 2
#'     )
#'   ),
#'   region = labelled::labelled(
#'     c("01", "02", "01"),
#'     labels = c(
#'       Ost = "01",
#'       Vest = "02"
#'     )
#'   ),
#'   alder = c(30, 45, 52)
#' )
#'
#' # Opprett etikettkolonner for alle merkede variabler
#' add_labelled_columns(
#'   data = data
#' )
#'
#' # Opprett etikettkolonne for én bestemt variabel
#' add_labelled_columns(
#'   data = data,
#'   variables = "kjoenn"
#' )
#'
#' # Opprett etikettkolonner for flere bestemte variabler
#' add_labelled_columns(
#'   data = data,
#'   variables = c(
#'     "kjoenn",
#'     "region"
#'   )
#' )
#'
#' # Bruk et annet postfiks
#' add_labelled_columns(
#'   data = data,
#'   variables = "kjoenn",
#'   postfix = "_navn"
#' )
#'
#' @export
add_labelled_columns <- function(
    data,
    variables = NULL,
    postfix = "_labelled"
) {
  
  if (!is.null(variables) && (
    !is.character(variables) ||
    length(variables) == 0L ||
    anyNA(variables)
  )) {
    stop(
      "`variables` må være NULL eller en karaktervektor med minst ett variabelnavn.",
      call. = FALSE
    )
  }
  
  if (
    !is.character(postfix) ||
    length(postfix) != 1L ||
    is.na(postfix)
  ) {
    stop(
      "`postfix` må være én enkelt tekststreng.",
      call. = FALSE
    )
  }
  
  if (is.null(variables)) {
    variables <- names(data)[
      vapply(
        data,
        haven::is.labelled,
        logical(1)
      )
    ]
    
    if (length(variables) == 0L) {
      warning(
        "Ingen merkede variabler med verdietiketter ble funnet i `data`.",
        call. = FALSE
      )
      return(data)
    }
  }
  
  variables_missing <- setdiff(
    variables,
    names(data)
  )
  
  if (length(variables_missing) > 0L) {
    stop(
      "Følgende variabler finnes ikke i `data`: ",
      paste(variables_missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  variables_not_labelled <- variables[
    !vapply(
      data[variables],
      haven::is.labelled,
      logical(1)
    )
  ]
  
  if (length(variables_not_labelled) > 0L) {
    stop(
      "Følgende variabler er ikke labelled: ",
      paste(variables_not_labelled, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  variables <- unique(variables)
  
  new_variables <- paste0(
    variables,
    postfix
  )
  
  variables_existing <- intersect(
    new_variables,
    names(data)
  )
  
  if (length(variables_existing) > 0L) {
    stop(
      "Følgende variabler finnes allerede i `data`: ",
      paste(variables_existing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  original <- data
  for (i in seq_along(variables)) {
    x <- data[[variables[[i]]]]
    .datadoc_warn_unlabelled(x, variables[[i]])
    data[[new_variables[[i]]]] <- .datadoc_as_factor(x)
  }
  .datadoc_restore_groups(data, original)
}


#' Kopier metadata på datasett-nivå mellom Datadoc-filer
#'
#' Kopierer metadata på datasett-nivå fra én Datadoc-fil til en annen.
#' Metadata for variabler blir ikke endret.
#'
#' Tekniske felt som identifiserer målfilen beholdes fra målfilen og
#' overskrives ikke med verdier fra kildefilen. Dette gjelder blant annet
#' datasettets kortnavn, ID, filsti, eier og metadata om opprettelse.
#'
#' Dato for siste oppdatering, \code{metadata_last_updated_date}, oppdateres
#' automatisk til tidspunktet funksjonen kjøres. Dato for opprettelse,
#' \code{metadata_created_date}, beholdes uendret.
#'
#' Datasettets versjonsnummer og periode hentes automatisk fra filnavnet
#' til målfilen. Et filnavn på formen
#' \code{resultatregnskap-klargjort_p2025_v1.parquet} gir dermed
#' \code{version = "1"}, \code{contains_data_from = "2025-01-01"} og
#' \code{contains_data_until = "2025-12-31"}.
#'
#' Både filstier til Parquet-filer og direkte filstier til Datadoc-filer
#' kan brukes. Dersom en Parquet-fil oppgis, konverteres filstien til
#' tilhørende Datadoc-fil.
#'
#' @param filsti_datadoc_egen En tekststreng med filsti til Datadoc-filen
#'   som skal oppdateres. Kan også være filstien til den tilhørende
#'   Parquet-filen.
#' @param filsti_datadoc_original En tekststreng med filsti til Datadoc-filen
#'   metadata skal kopieres fra. Kan også være filstien til den tilhørende
#'   Parquet-filen.
#' @param overwrite Logisk verdi som angir om eksisterende metadata på
#'   datasett-nivå i målfilen skal overskrives. Standard er \code{FALSE},
#'   slik at kun tomme eller manglende felt fylles ut. Tekniske felt som
#'   identifiserer målfilen overskrives ikke uavhengig av verdien til
#'   \code{overwrite}.
#'
#' @return Returnerer den oppdaterte Datadoc-strukturen usynlig som en liste.
#'   Datadoc-filen som er angitt i \code{filsti_datadoc_egen} oppdateres
#'   samtidig på disk.
#'
#' @details
#' Følgende felt beholdes fra målfilen og kopieres ikke fra kildefilen:
#' \itemize{
#'   \item \code{short_name}
#'   \item \code{version}
#'   \item \code{id}
#'   \item \code{owner}
#'   \item \code{file_path}
#'   \item \code{metadata_created_date}
#'   \item \code{metadata_created_by}
#'   \item \code{metadata_last_updated_by}
#'   \item \code{contains_data_from}
#'   \item \code{contains_data_until}
#' }
#'
#' Feltet \code{metadata_last_updated_date} settes til tidspunktet funksjonen
#' kjøres. Tidspunktet lagres i UTC.
#'
#' Feltet \code{version} erstattes med versjonsnummeret som hentes fra
#' filnavnet til målfilen. Feltene \code{contains_data_from} og
#' \code{contains_data_until} settes tilsvarende ut fra perioden i
#' filnavnet.
#'
#' Funksjonen forventer et filnavn der periode og versjon følger mønsteret
#' \code{_pYYYY_vN}, for eksempel \code{_p2025_v1}. Dersom periode eller
#' versjon ikke kan identifiseres, gis en advarsel og eksisterende verdi
#' beholdes.
#'
#' @examples
#' \dontrun{
#' copy_metadata_dataset(
#'   filsti_datadoc_egen =
#'     "/buckets/produkt/speshelse/klargjorte-data/regnskap/2025/resultatregnskap-klargjort_p2025_v1.parquet",
#'   filsti_datadoc_original =
#'     "/buckets/produkt/speshelse/klargjorte-data/regnskap/2024/resultatregnskap-klargjort_p2024_v3.parquet"
#' )
#'
#' copy_metadata_dataset(
#'   filsti_datadoc_egen = "data/resultat_p2025_v2.parquet",
#'   filsti_datadoc_original = "data/resultat_p2024_v1.parquet",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
copy_metadata_dataset <- function(filsti_datadoc_egen,
                                  filsti_datadoc_original,
                                  overwrite = FALSE) {
  
  .datadoc_flag(overwrite, "overwrite")
  
  # Behold original sti for å hente versjon og periode
  filsti_data_egen <- filsti_datadoc_egen
  
  # Hent versjonsnummer fra filnavn
  hent_versjon <- function(path) {
    filnavn <- basename(path)
    
    versjon <- stringr::str_match(
      filnavn,
      "_v([0-9]+)(?:\\.parquet|__DOC\\.json)$"
    )[, 2]
    
    if (is.na(versjon)) {
      warning(
        "Fant ikke versjonsnummer i filstien: ",
        path
      )
      return(NULL)
    }
    
    versjon
  }
  
  # Hent periode fra filnavn
  hent_periode <- function(path) {
    filnavn <- basename(path)
    
    aar <- stringr::str_match(
      filnavn,
      "_p([0-9]{4})_v[0-9]+(?:\\.parquet|__DOC\\.json)$"
    )[, 2]
    
    if (is.na(aar)) {
      warning(
        "Fant ikke år/periode i filstien: ",
        path
      )
      
      return(
        list(
          contains_data_from = NULL,
          contains_data_until = NULL
        )
      )
    }
    
    list(
      contains_data_from = paste0(aar, "-01-01"),
      contains_data_until = paste0(aar, "-12-31")
    )
  }
  
  versjon <- hent_versjon(filsti_data_egen) # OBS: erstatt med fellesr::finn_versjon()
  periode <- hent_periode(filsti_data_egen)
  
  filsti_datadoc_egen <- if (
    tolower(tools::file_ext(filsti_datadoc_egen)) == "parquet"
  ) {
    datadoc_path(filsti_datadoc_egen, to = "json")
  } else {
    filsti_datadoc_egen
  }
  
  filsti_datadoc_original <- if (
    tolower(tools::file_ext(filsti_datadoc_original)) == "parquet"
  ) {
    datadoc_path(filsti_datadoc_original, to = "json")
  } else {
    filsti_datadoc_original
  }
  
  # Les Datadoc-filene
  egen <- .datadoc_read(filsti_datadoc_egen)
  
  original <- .datadoc_read(filsti_datadoc_original)
  
  egen_dataset <- egen$datadoc$dataset
  original_dataset <- original$datadoc$dataset
  
  # Felt som tilhører den konkrete målfilen
  # og derfor ikke skal kopieres fra originalen
  behold_fra_egen <- c(
    "short_name",
    "version",
    "id",
    "owner",
    "file_path",
    "metadata_created_date",
    "metadata_created_by",
    "metadata_last_updated_date",
    "metadata_last_updated_by",
    "contains_data_from",
    "contains_data_until"
  )
  
  felter_som_kopieres <- setdiff(
    names(original_dataset),
    behold_fra_egen
  )
  
  # Kopier datasettmetadata
  for (felt in felter_som_kopieres) {
    if (
      overwrite ||
      .datadoc_is_empty(egen_dataset[[felt]])
    ) {
      egen_dataset[[felt]] <- original_dataset[[felt]]
    }
  }
  
  # Sett versjon fra filnavn
  if (!is.null(versjon)) {
    egen_dataset$version <- versjon
  }
  
  # Sett gyldighetsperiode fra filnavn
  if (!is.null(periode$contains_data_from)) {
    egen_dataset$contains_data_from <- periode$contains_data_from
  }
  
  if (!is.null(periode$contains_data_until)) {
    egen_dataset$contains_data_until <- periode$contains_data_until
  }
  
  # Oppdater tidspunkt for siste endring
  egen_dataset$metadata_last_updated_date <- format(
    Sys.time(),
    format = "%Y-%m-%dT%H:%M:%OS6Z",
    tz = "UTC"
  )
  
  egen$datadoc$dataset <- egen_dataset
  
  # Skriv tilbake
  .datadoc_write(
    egen,
    filsti_datadoc_egen,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )
  
  invisible(egen)
}

#' Kopier metadata mellom Datadoc-filer
#'
#' Kopierer variabelmetadata fra en original Datadoc-fil til en annen
#' Datadoc-fil. Metadata kopieres automatisk for variabler som har samme
#' `short_name` i begge filer. Det kan i tillegg angis eksplisitte koblinger
#' mellom variabler med ulike kortnavn.
#'
#' Variabler kan ekskluderes fullstendig fra oppdateringen ved hjelp av
#' `ekskluder_variabler`. For disse variablene kopieres verken metadata eller
#' gyldighetsperiode.
#'
#' Variabler som hoppes over med `overwrite = FALSE`, beholdes helt uendret,
#' også når det gjelder datoer. Gyldighetsperioden for øvrige variabler settes
#' ut fra perioden i filnavnet. For eksempel gir `_p2025_v1` perioden
#' `2025-01-01` til `2025-12-31`. Det kan angis egne gyldighetsperioder for
#' enkeltvariabler ved hjelp av `dato_unntak`.
#'
#' @param filsti_datadoc_egen En tekststreng med filstien til Datadoc-filen
#'   som skal oppdateres. Filnavnet må inneholde periode og versjon på formen
#'   `_pYYYY_vN`, for eksempel `_p2025_v1__DOC.json`.
#' @param filsti_datadoc_original En tekststreng med filstien til Datadoc-filen
#'   som metadata skal kopieres fra.
#' @param variabler `NULL` eller en navngitt tekstvektor med eksplisitte
#'   koblinger mellom variabler. Navnet på hvert element angir `short_name`
#'   i filen som skal oppdateres, mens verdien angir `short_name` i
#'   originalfilen. Standardverdien er `NULL`.
#' @param ekskluder_variabler `NULL` eller en tekstvektor med `short_name`
#'   for variabler i mottakerfilen som ikke skal endres. For disse variablene
#'   kopieres verken metadata eller gyldighetsperiode. En variabel kan ikke
#'   samtidig være oppgitt i `dato_unntak`. Standardverdien er `NULL`.
#' @param overwrite En logisk verdi som angir om eksisterende metadata skal
#'   erstattes. Når verdien er `TRUE`, kopieres metadata for alle aktuelle
#'   variabler. Når verdien er `FALSE`, hoppes variabler over dersom feltet
#'   `name` i mottakerfilen ikke er `NULL`. Standardverdien er `TRUE`.
#' @param quiet Med `TRUE` (standard) skjules statusmeldinger.
#' @param dato_unntak `NULL` eller en data frame med egne gyldighetsperioder
#'   for enkeltvariabler. Metadata kopieres på vanlig måte for disse
#'   variablene, men feltene `contains_data_from` og `contains_data_until`
#'   overskrives med periodene angitt i `dato_unntak`. Dataframen må inneholde
#'   kolonnene `short_name`, `contains_data_from` og `contains_data_until`.
#'   Datoene skal være på formatet `"YYYY-MM-DD"`. En variabel kan ikke
#'   samtidig være oppgitt i `ekskluder_variabler`. Standardverdien er
#'   `NULL`.
#'
#' @return Den oppdaterte Datadoc-strukturen som en liste. Strukturen skrives
#'   samtidig tilbake til filen angitt i `filsti_datadoc_egen`.
#'
#' @details
#' Funksjonen finner først alle variabler som har samme `short_name` i de to
#' Datadoc-filene. Metadata for disse variablene kopieres automatisk.
#'
#' Argumentet `variabler` kan brukes til å koble variabler som har ulike
#' kortnavn i de to filene. En kobling som er angitt eksplisitt i
#' `variabler`, får forrang dersom mottakervariabelen også inngår blant
#' variablene med identiske kortnavn.
#'
#' Variabler som er oppgitt i `ekskluder_variabler`, fjernes fra listen over
#' variabler som skal få kopiert metadata. De hoppes også over når
#' gyldighetsperioden oppdateres. Disse variablene beholdes derfor uendret i
#' mottakerfilen.
#'
#' Kjente dokumentasjonsfelt kopieres fra originalfilen. Felt som finnes i
#' kilden, erstattes samlet, også når kildeverdien er `NULL`. Mottakerens
#' kortnavn, ID, datatype, dataelementsti og pseudonymiseringsmetadata
#' beholdes. Ukjente felt og felt som ikke finnes i kilden, beholdes.
#'
#' Når `overwrite = FALSE`, regnes en variabel som å ha eksisterende metadata
#' dersom feltet `name` ikke er `NULL`. Andre metadatafelt tas ikke med i
#' denne vurderingen.
#'
#' Når `overwrite = FALSE`, beholdes dokumenterte variabler helt uendret,
#' inkludert `contains_data_from` og `contains_data_until`. Dette gjelder
#' også dersom variabelen er oppgitt i `dato_unntak`. Øvrige variabler får
#' oppdatert perioden som beskrevet nedenfor.
#'
#' Variabler i `ekskluder_variabler` er unntatt fra denne oppdateringen og
#' beholdes fullstendig uendret.
#'
#' Standardperioden hentes fra filnavnet til `filsti_datadoc_egen`.
#' Et filnavn som inneholder `_p2025_v1` gir
#' `contains_data_from = "2025-01-01"` og
#' `contains_data_until = "2025-12-31"`.
#'
#' Dersom enkelte variabler har en annen gyldighetsperiode, kan disse oppgis
#' i `dato_unntak`. Metadata kopieres på vanlig måte, men perioden som er
#' angitt i `dato_unntak`, erstatter standardperioden for de aktuelle
#' variablene.
#'
#' Samme variabel kan ikke være oppgitt både i `ekskluder_variabler` og
#' `dato_unntak`.
#'
#' Følgende kontroller utføres før filen endres:
#'
#' \itemize{
#'   \item `overwrite` må være én enkelt logisk verdi;
#'   \item `short_name` må være unik i begge Datadoc-filene;
#'   \item `variabler` må være en navngitt tekstvektor dersom argumentet
#'     ikke er `NULL`;
#'   \item samme mottakervariabel kan ikke oppgis flere ganger i
#'     `variabler`;
#'   \item alle eksplisitt oppgitte variabler må finnes i de respektive
#'     Datadoc-filene;
#'   \item `ekskluder_variabler` må være en tekstvektor dersom argumentet
#'     ikke er `NULL`;
#'   \item alle variabler i `ekskluder_variabler` må finnes i
#'     mottakerfilen;
#'   \item filnavnet til mottakerfilen må inneholde en periode på formen
#'     `_pYYYY_vN`;
#'   \item `dato_unntak` må inneholde de nødvendige kolonnene;
#'   \item hver variabel kan bare forekomme én gang i `dato_unntak`;
#'   \item alle variabler i `dato_unntak` må finnes i mottakerfilen;
#'   \item samme variabel kan ikke forekomme både i `ekskluder_variabler`
#'     og `dato_unntak`; og
#'   \item datoene i `dato_unntak` må være gyldige datoer på formatet
#'     `"YYYY-MM-DD"`.
#' }
#'
#' Den oppdaterte strukturen skrives til `filsti_datadoc_egen` med
#' [jsonlite::write_json()]. Den eksisterende filen overskrives.
#'
#' @examples
#' \dontrun{
#' # Kopier metadata og sett perioden til 2025 for alle aktuelle variabler
#' copy_metadata(
#'   filsti_datadoc_egen =
#'     "/buckets/data/resultat_p2025_v1__DOC.json",
#'   filsti_datadoc_original =
#'     "/buckets/data/resultat_p2024_v1__DOC.json"
#' )
#'
#' # Kopier bare til variabler som ikke allerede har metadata
#' copy_metadata(
#'   filsti_datadoc_egen =
#'     "/buckets/data/resultat_p2025_v1__DOC.json",
#'   filsti_datadoc_original =
#'     "/buckets/data/resultat_p2024_v1__DOC.json",
#'   overwrite = FALSE
#' )
#'
#' # Legg til eksplisitte koblinger mellom ulike kortnavn
#' copy_metadata(
#'   filsti_datadoc_egen =
#'     "/buckets/data/resultat_p2025_v1__DOC.json",
#'   filsti_datadoc_original =
#'     "/buckets/data/resultat_p2024_v1__DOC.json",
#'   variabler = c(
#'     kjoenn = "sex",
#'     bostedskommune = "kommune"
#'   )
#' )
#'
#' # Ikke gjør noen endringer i enkelte variabler
#' copy_metadata(
#'   filsti_datadoc_egen =
#'     "/buckets/data/resultat_p2025_v1__DOC.json",
#'   filsti_datadoc_original =
#'     "/buckets/data/resultat_p2024_v1__DOC.json",
#'   ekskluder_variabler = c(
#'     "orgnr_frtk",
#'     "navn_frtk"
#'   )
#' )
#'
#' # Angi egne gyldighetsperioder for enkelte variabler
#' dato_unntak <- data.frame(
#'   short_name = c(
#'     "orgnr_frtk",
#'     "navn_frtk"
#'   ),
#'   contains_data_from = c(
#'     "2024-01-01",
#'     "2020-01-01"
#'   ),
#'   contains_data_until = c(
#'     "2025-12-31",
#'     "2025-12-31"
#'   )
#' )
#'
#' copy_metadata(
#'   filsti_datadoc_egen =
#'     "/buckets/data/resultat_p2025_v1__DOC.json",
#'   filsti_datadoc_original =
#'     "/buckets/data/resultat_p2024_v1__DOC.json",
#'   dato_unntak = dato_unntak
#' )
#' }
#'
#' @seealso
#' [copy_metadata_variable()] for å kopiere metadata bare for eksplisitt
#' angitte variabler.
#'
#' @export
copy_metadata <- function(
    filsti_datadoc_egen,
    filsti_datadoc_original,
    variabler = NULL,
    ekskluder_variabler = NULL,
    overwrite = TRUE,
    dato_unntak = NULL,
    quiet = TRUE
) {
  
  .datadoc_flag(quiet, "quiet")
  filsti_datadoc_egen <- .datadoc_resolve_path(filsti_datadoc_egen)
  filsti_datadoc_original <- .datadoc_resolve_path(filsti_datadoc_original)
  
  # Kontroller overwrite
  if (
    !is.logical(overwrite) ||
    length(overwrite) != 1L ||
    is.na(overwrite)
  ) {
    stop(
      "`overwrite` må være enten TRUE eller FALSE.",
      call. = FALSE
    )
  }
  
  # Hent årgang fra filnavnet til mottakerfilen
  filnavn_egen <- basename(
    filsti_datadoc_egen
  )
  
  aar <- stringr::str_match(
    filnavn_egen,
    "_p([0-9]{4})_v[0-9]+"
  )[, 2]
  
  if (is.na(aar)) {
    stop(
      paste0(
        "Fant ikke periode i `filsti_datadoc_egen`. ",
        "Filnavnet må inneholde periode og versjon på formen ",
        "`_pYYYY_vN`, for eksempel `_p2025_v1__DOC.json`."
      ),
      call. = FALSE
    )
  }
  
  contains_data_from <- paste0(
    aar,
    "-01-01"
  )
  
  contains_data_until <- paste0(
    aar,
    "-12-31"
  )
  
  # Les Datadoc-filene
  datadoc_egen <- .datadoc_read(filsti_datadoc_egen)
  
  datadoc_original <- .datadoc_read(filsti_datadoc_original)
  
  # Hent short_name
  short_name_egen <- vapply(
    datadoc_egen$datadoc$variables,
    function(x) x$short_name,
    FUN.VALUE = character(1)
  )
  
  short_name_original <- vapply(
    datadoc_original$datadoc$variables,
    function(x) x$short_name,
    FUN.VALUE = character(1)
  )
  
  # Kontroller duplikater i mottakerfilen
  if (anyDuplicated(short_name_egen)) {
    
    duplikater <- unique(
      short_name_egen[
        duplicated(short_name_egen)
      ]
    )
    
    stop(
      "Følgende `short_name` forekommer flere ganger i ",
      "`filsti_datadoc_egen`: ",
      paste(
        duplikater,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # Kontroller duplikater i originalfilen
  if (anyDuplicated(short_name_original)) {
    
    duplikater <- unique(
      short_name_original[
        duplicated(short_name_original)
      ]
    )
    
    stop(
      "Følgende `short_name` forekommer flere ganger i ",
      "`filsti_datadoc_original`: ",
      paste(
        duplikater,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # Kontroller ekskluder_variabler
  if (is.null(ekskluder_variabler)) {
    
    ekskluder_variabler <- character(0)
    
  } else {
    
    if (
      !is.character(ekskluder_variabler) ||
      length(ekskluder_variabler) == 0L ||
      anyNA(ekskluder_variabler) ||
      any(!nzchar(trimws(ekskluder_variabler)))
    ) {
      stop(
        paste0(
          "`ekskluder_variabler` må være en tekstvektor med ",
          "`short_name`, for eksempel c(\"orgnr_frtk\", \"navn_frtk\")."
        ),
        call. = FALSE
      )
    }
    
    ekskluder_variabler <- trimws(
      ekskluder_variabler
    )
    
    if (anyDuplicated(ekskluder_variabler)) {
      
      duplikater <- unique(
        ekskluder_variabler[
          duplicated(ekskluder_variabler)
        ]
      )
      
      stop(
        "Følgende variabler forekommer flere ganger i ",
        "`ekskluder_variabler`: ",
        paste(
          duplikater,
          collapse = ", "
        ),
        ".",
        call. = FALSE
      )
    }
    
    mangler_i_egen <- setdiff(
      ekskluder_variabler,
      short_name_egen
    )
    
    if (length(mangler_i_egen) > 0L) {
      stop(
        "Følgende variabler fra `ekskluder_variabler` finnes ikke i ",
        "`filsti_datadoc_egen`: ",
        paste(
          mangler_i_egen,
          collapse = ", "
        ),
        ".",
        call. = FALSE
      )
    }
  }
  
  # Kontroller dato_unntak
  if (!is.null(dato_unntak)) {
    
    if (!is.data.frame(dato_unntak)) {
      stop(
        "`dato_unntak` må være en data frame.",
        call. = FALSE
      )
    }
    
    nødvendige_kolonner <- c(
      "short_name",
      "contains_data_from",
      "contains_data_until"
    )
    
    manglende_kolonner <- setdiff(
      nødvendige_kolonner,
      names(dato_unntak)
    )
    
    if (length(manglende_kolonner) > 0L) {
      stop(
        "`dato_unntak` mangler følgende kolonner: ",
        paste(
          manglende_kolonner,
          collapse = ", "
        ),
        ".",
        call. = FALSE
      )
    }
    
    # Behold bare nødvendige kolonner
    dato_unntak <- dato_unntak[
      nødvendige_kolonner
    ]
    
    # Konverter til tekst
    dato_unntak$short_name <- as.character(
      dato_unntak$short_name
    )
    
    dato_unntak$contains_data_from <- as.character(
      dato_unntak$contains_data_from
    )
    
    dato_unntak$contains_data_until <- as.character(
      dato_unntak$contains_data_until
    )
    
    # Kontroller short_name
    if (
      anyNA(dato_unntak$short_name) ||
      any(!nzchar(trimws(dato_unntak$short_name)))
    ) {
      stop(
        "`short_name` i `dato_unntak` kan ikke være manglende eller tom.",
        call. = FALSE
      )
    }
    
    dato_unntak$short_name <- trimws(
      dato_unntak$short_name
    )
    
    if (anyDuplicated(dato_unntak$short_name)) {
      
      duplikater <- unique(
        dato_unntak$short_name[
          duplicated(dato_unntak$short_name)
        ]
      )
      
      stop(
        "Følgende variabler forekommer flere ganger i `dato_unntak`: ",
        paste(
          duplikater,
          collapse = ", "
        ),
        ".",
        call. = FALSE
      )
    }
    
    mangler_i_egen <- setdiff(
      dato_unntak$short_name,
      short_name_egen
    )
    
    if (length(mangler_i_egen) > 0L) {
      stop(
        "Følgende variabler fra `dato_unntak` finnes ikke i ",
        "`filsti_datadoc_egen`: ",
        paste(
          mangler_i_egen,
          collapse = ", "
        ),
        ".",
        call. = FALSE
      )
    }
    
    # Kontroller at datoene ikke mangler
    if (
      anyNA(dato_unntak$contains_data_from) ||
      anyNA(dato_unntak$contains_data_until)
    ) {
      stop(
        "Datoene i `dato_unntak` kan ikke være manglende.",
        call. = FALSE
      )
    }
    
    # Kontroller datoformat
    gyldig_format_fra <- grepl(
      "^\\d{4}-\\d{2}-\\d{2}$",
      dato_unntak$contains_data_from
    )
    
    gyldig_format_til <- grepl(
      "^\\d{4}-\\d{2}-\\d{2}$",
      dato_unntak$contains_data_until
    )
    
    if (
      any(!gyldig_format_fra) ||
      any(!gyldig_format_til)
    ) {
      stop(
        paste0(
          "Datoene i `dato_unntak` må være på formatet ",
          "`YYYY-MM-DD`."
        ),
        call. = FALSE
      )
    }
    
    dato_fra <- as.Date(
      dato_unntak$contains_data_from,
      format = "%Y-%m-%d"
    )
    
    dato_til <- as.Date(
      dato_unntak$contains_data_until,
      format = "%Y-%m-%d"
    )
    
    if (
      anyNA(dato_fra) ||
      anyNA(dato_til)
    ) {
      stop(
        "`dato_unntak` inneholder én eller flere ugyldige datoer.",
        call. = FALSE
      )
    }
    
    if (any(dato_fra > dato_til)) {
      stop(
        paste0(
          "`contains_data_from` kan ikke være senere enn ",
          "`contains_data_until` i `dato_unntak`."
        ),
        call. = FALSE
      )
    }
  }
  
  # Kontroller at samme variabel ikke finnes både i
  # ekskluder_variabler og dato_unntak
  if (
    length(ekskluder_variabler) > 0L &&
    !is.null(dato_unntak)
  ) {
    
    overlapp <- intersect(
      ekskluder_variabler,
      dato_unntak$short_name
    )
    
    if (length(overlapp) > 0L) {
      stop(
        "Følgende variabler er oppgitt både i `ekskluder_variabler` ",
        "og `dato_unntak`: ",
        paste(
          overlapp,
          collapse = ", "
        ),
        ". En variabel kan ikke være oppgitt i begge argumentene.",
        call. = FALSE
      )
    }
  }
  
  # Alle variabler med samme navn i begge filer
  variabler_felles <- intersect(
    short_name_egen,
    short_name_original
  )
  
  variabler_felles_mapping <- stats::setNames(
    variabler_felles,
    variabler_felles
  )
  
  # Kontroller eksplisitt oppgitte variabler
  if (is.null(variabler)) {
    
    variabler <- character(0)
    
  } else {
    
    if (
      !is.character(variabler) ||
      length(variabler) == 0L ||
      is.null(names(variabler)) ||
      anyNA(variabler) ||
      anyNA(names(variabler)) ||
      any(!nzchar(trimws(variabler))) ||
      any(!nzchar(trimws(names(variabler))))
    ) {
      stop(
        "`variabler` må være en navngitt tekstvektor, for eksempel ",
        "c(\"navn_i_egen\" = \"navn_i_original\").",
        call. = FALSE
      )
    }
    
    variabler <- trimws(
      variabler
    )
    
    names(variabler) <- trimws(
      names(variabler)
    )
    
    if (anyDuplicated(names(variabler))) {
      
      duplikater <- unique(
        names(variabler)[
          duplicated(names(variabler))
        ]
      )
      
      stop(
        "Følgende variabler i egen fil er oppgitt flere ganger ",
        "i `variabler`: ",
        paste(
          duplikater,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    mangler_i_egen <- setdiff(
      names(variabler),
      short_name_egen
    )
    
    mangler_i_original <- setdiff(
      unname(variabler),
      short_name_original
    )
    
    if (length(mangler_i_egen) > 0L) {
      stop(
        "Følgende variabler fra `variabler` finnes ikke i ",
        "`filsti_datadoc_egen`: ",
        paste(
          mangler_i_egen,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    if (length(mangler_i_original) > 0L) {
      stop(
        "Følgende variabler fra `variabler` finnes ikke i ",
        "`filsti_datadoc_original`: ",
        paste(
          mangler_i_original,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
  }
  
  # Felles variabler kopieres først.
  # Eksplisitt oppgitte variabler får forrang ved overlapp.
  variabler_samlet <- c(
    variabler_felles_mapping,
    variabler
  )
  
  variabler_samlet <- variabler_samlet[
    !duplicated(
      names(variabler_samlet),
      fromLast = TRUE
    )
  ]
  
  # Ekskluderte variabler skal ikke kopieres
  variabler_samlet <- variabler_samlet[
    !names(variabler_samlet) %in% ekskluder_variabler
  ]
  
  variabler_kopiert <- character(0)
  variabler_hoppet_over <- character(0)
  
  # Kopier metadata
  for (i in seq_along(variabler_samlet)) {
    
    navn_egen <- names(
      variabler_samlet
    )[[i]]
    
    navn_original <- unname(
      variabler_samlet[[i]]
    )
    
    indeks_egen <- match(
      navn_egen,
      short_name_egen
    )
    
    indeks_original <- match(
      navn_original,
      short_name_original
    )
    
    metadata_egen <-
      datadoc_egen$datadoc$variables[[indeks_egen]]
    
    har_metadata <- !is.null(
      metadata_egen$name
    )
    
    # Hopp over dersom variabelen allerede har metadata
    # og overwrite = FALSE
    if (!overwrite && har_metadata) {
      
      variabler_hoppet_over <- c(
        variabler_hoppet_over,
        navn_egen
      )
      
      next
    }
    
    metadata_original <-
      datadoc_original$datadoc$variables[[indeks_original]]
    
    datadoc_egen$datadoc$variables[[indeks_egen]] <-
      .datadoc_copy_variable(metadata_egen, metadata_original)
    
    variabler_kopiert <- c(
      variabler_kopiert,
      navn_egen
    )
  }
  
  # Ikke endre perioder på ekskluderte eller dokumenterte, beskyttede variabler.
  dato_oppdatert <- character()
  for (i in seq_along(datadoc_egen$datadoc$variables)) {
    
    navn <- datadoc_egen$datadoc$variables[[i]]$short_name
    
    # Ekskluderte variabler skal ikke endres
    if (navn %in% ekskluder_variabler ||
        (!overwrite && !is.null(datadoc_egen$datadoc$variables[[i]]$name) &&
         !navn %in% variabler_kopiert)) {
      next
    }
    
    fra <- contains_data_from
    til <- contains_data_until
    
    # dato_unntak overskriver standardperioden
    if (
      !is.null(dato_unntak) &&
      navn %in% dato_unntak$short_name
    ) {
      
      indeks_unntak <- match(
        navn,
        dato_unntak$short_name
      )
      
      fra <- dato_unntak$contains_data_from[[indeks_unntak]]
      til <- dato_unntak$contains_data_until[[indeks_unntak]]
    }
    
    datadoc_egen$datadoc$variables[[i]]$contains_data_from <- fra
    
    datadoc_egen$datadoc$variables[[i]]$contains_data_until <- til
    dato_oppdatert <- c(dato_oppdatert, navn)
  }
  
  # Skriv oppdatert Datadoc-fil
  .datadoc_write(
    datadoc_egen,
    path = filsti_datadoc_egen,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )
  
  if (!quiet) message(
    "Kopierte metadata for ",
    length(variabler_kopiert),
    " variabel",
    if (length(variabler_kopiert) == 1L) "" else "er",
    "."
  )
  
  if (length(variabler_hoppet_over) > 0L) {
    if (!quiet) message(
      "Hoppet over ",
      length(variabler_hoppet_over),
      " variabel",
      if (length(variabler_hoppet_over) == 1L) "" else "er",
      " som allerede hadde metadata: ",
      paste(
        variabler_hoppet_over,
        collapse = ", "
      ),
      "."
    )
  }
  
  if (length(ekskluder_variabler) > 0L) {
    if (!quiet) message(
      "Lot ",
      length(ekskluder_variabler),
      " ekskludert",
      if (length(ekskluder_variabler) == 1L) " variabel" else "e variabler",
      " være uendret: ",
      paste(
        ekskluder_variabler,
        collapse = ", "
      ),
      "."
    )
  }
  
  antall_dato_oppdatert <- length(dato_oppdatert)
  
  if (!quiet) message(
    "Oppdaterte gyldighetsperiode for ",
    antall_dato_oppdatert,
    " variabel",
    if (antall_dato_oppdatert == 1L) "" else "er",
    " til ",
    contains_data_from,
    "–",
    contains_data_until,
    if (is.null(dato_unntak)) {
      "."
    } else {
      paste0(
        ", med ",
        sum(dato_unntak$short_name %in% dato_oppdatert),
        " unntak."
      )
    }
  )
  
  datadoc_egen
}

#' Erstatt filsti i Datadoc-metadata
#'
#' Oppdaterer filstien som er lagret i datasettmetadataene i en Datadoc JSON-fil.
#'
#' Funksjonen leser inn en eksisterende Datadoc JSON-fil, erstatter verdien i
#' `datadoc$dataset$file_path` og skriver de oppdaterte metadataene tilbake til
#' den samme JSON-filen.
#'
#' @param filsti_datadoc Tekststreng. Filsti til Datadoc JSON-filen som skal
#'   oppdateres.
#' @param file_path Tekststreng. Ny filsti som skal lagres i
#'   `datadoc$dataset$file_path`.
#'
#' @return Det oppdaterte Datadoc-objektet som en liste. Objektet returneres
#'   usynlig.
#'
#' @details
#' Datadoc JSON-filen angitt i `filsti_datadoc` overskrives med den oppdaterte
#' versjonen. Den eksisterende verdien i `datadoc$dataset$file_path` erstattes
#' med verdien angitt i `file_path`.
#'
#' @examples
#' \dontrun{
#' replace_file_path(
#'   filsti_datadoc = "data/eksempel__DOC.json",
#'   file_path = "data/eksempel.parquet"
#' )
#' }
#'
#' @export
replace_file_path <- function(filsti_datadoc,
                              file_path) {
  
  .datadoc_text(file_path, "file_path")
  datadoc <- .datadoc_read(filsti_datadoc)
  
  datadoc$datadoc$dataset$file_path <- file_path
  
  .datadoc_write(
    datadoc,
    filsti_datadoc,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )
  
  invisible(datadoc)
}

#' Beregn dekning av verdietiketter
#'
#' Beregner hvor stor andel av de observerte verdiene i hver variabel som har
#' en tilhørende verdietikett.
#'
#' Funksjonen beregner både dekning blant alle observerte verdier og blant
#' unike observerte verdier.
#'
#' @param data Et datasett som skal undersøkes for verdietiketter.
#' @param labelled_only En logisk verdi som angir om bare variabler som har
#'   minst én verdietikett skal inkluderes. Når verdien er `FALSE`, inkluderes
#'   alle variabler i datasettet. Standardverdien er `FALSE`.
#'
#' @return En tibble med én rad per variabel og følgende kolonner:
#'
#' \describe{
#'   \item{`variable`}{Navnet på variabelen.}
#'   \item{`has_value_labels`}{Logisk verdi som angir om variabelen har minst
#'     én registrert verdietikett.}
#'   \item{`n_values`}{Antall observerte, ikke-manglende verdier i variabelen.}
#'   \item{`n_values_with_label`}{Antall observerte verdier som har en
#'     tilhørende verdietikett.}
#'   \item{`pct_values_with_label`}{Andel observerte verdier som har en
#'     tilhørende verdietikett, angitt i prosent og avrundet til to desimaler.}
#'   \item{`n_unique_values`}{Antall unike observerte, ikke-manglende verdier.}
#'   \item{`n_unique_values_with_label`}{Antall unike observerte verdier som
#'     har en tilhørende verdietikett.}
#'   \item{`pct_unique_values_with_label`}{Andel unike observerte verdier som
#'     har en tilhørende verdietikett, angitt i prosent og avrundet til to
#'     desimaler.}
#' }
#'
#' @details
#' Verdietikettene hentes med [labelled::val_labels()]. Manglende verdier
#' tas ikke med i beregningene.
#'
#' En observert verdi regnes som merket dersom verdien finnes blant verdiene
#' som er registrert i variabelens verdietiketter.
#'
#' `pct_values_with_label` beregnes på grunnlag av alle observerte verdier.
#' Dersom samme verdi forekommer flere ganger, teller hver forekomst separat.
#'
#' `pct_unique_values_with_label` beregnes derimot på grunnlag av de unike
#' observerte verdiene. Hver forskjellig verdi teller derfor bare én gang,
#' uavhengig av hvor ofte den forekommer i datasettet.
#'
#' Dersom en variabel ikke inneholder noen observerte verdier, settes den
#' aktuelle prosentandelen til `NA`.
#'
#' Når `labelled_only = TRUE`, inkluderes bare variabler som returneres av
#' [vars_with_value_labels()].
#'
#' @examples
#' data <- tibble::tibble(
#'   kjoenn = labelled::labelled(
#'     c(1, 2, 1, 3, NA),
#'     labels = c(
#'       Mann = 1,
#'       Kvinne = 2
#'     )
#'   ),
#'   alder = c(35, 42, 28, 51, 37)
#' )
#'
#' value_label_coverage(data)
#'
#' value_label_coverage(
#'   data,
#'   labelled_only = TRUE
#' )
#'
#' @seealso
#' [vars_with_value_labels()] for å finne variabler med verdietiketter,
#' [values_without_labels()] for å finne observerte verdier uten
#' verdietikett og [labelled::val_labels()] for å hente verdietikettene
#' til en variabel.
#'
#' @export
value_label_coverage <- function(
    data,
    labelled_only = FALSE
) {
  
  if (
    !is.logical(labelled_only) ||
    length(labelled_only) != 1L ||
    is.na(labelled_only)
  ) {
    stop(
      "`labelled_only` må være enten TRUE eller FALSE.",
      call. = FALSE
    )
  }
  
  variables_with_labels <- vars_with_value_labels(data)
  
  variables <- names(data)
  
  if (labelled_only) {
    variables <- variables[
      variables %in% variables_with_labels
    ]
  }
  
  purrr::map_dfr(
    variables,
    function(variable) {
      
      x <- data[[variable]]
      
      observed_values <- x[
        !is.na(x)
      ]
      
      labelled_values <- unname(
        labelled::val_labels(x)
      )
      
      values_have_label <-
        observed_values %in% labelled_values
      
      unique_values <- unique(
        observed_values
      )
      
      unique_values_have_label <-
        unique_values %in% labelled_values
      
      n_values <- length(
        observed_values
      )
      
      n_unique_values <- length(
        unique_values
      )
      
      tibble::tibble(
        variable = variable,
        has_value_labels =
          variable %in% variables_with_labels,
        
        n_values = n_values,
        n_values_with_label =
          sum(values_have_label),
        
        pct_values_with_label =
          if (n_values > 0L) {
            round(
              sum(values_have_label) /
                n_values * 100,
              digits = 2
            )
          } else {
            NA_real_
          },
        
        n_unique_values =
          n_unique_values,
        
        n_unique_values_with_label =
          sum(unique_values_have_label),
        
        pct_unique_values_with_label =
          if (n_unique_values > 0L) {
            round(
              sum(unique_values_have_label) /
                n_unique_values * 100,
              digits = 2
            )
          } else {
            NA_real_
          }
      )
    }
  )
}

#' Kontroller forventet dekning av verdietiketter
#'
#' Lager en oversikt over hvilke variabler i et datasett som forventes å ha
#' verdietiketter, hvilke som faktisk har verdietiketter, og hvilke variabler
#' som mangler forventede verdietiketter.
#'
#' Forventningen kan baseres på variabeltype og på en eksplisitt angitt
#' tekstvektor med variabelnavn. Dersom en DataDoc-fil oppgis, hentes i tillegg
#' ID-er for variabeldefinisjoner og KLASS-klassifikasjoner.
#'
#' @param data Et datasett som skal undersøkes for verdietiketter.
#' @param filsti `NULL` eller en tekststreng med filstien til en DataDoc
#'   JSON-fil. Dersom en filsti oppgis, hentes `vardef_id` og `klass_id`
#'   fra variabelmetadataene i DataDoc-filen. Standardverdien er `NULL`.
#' @param variables `NULL` eller en tekstvektor med navn på variabler som
#'   forventes å ha verdietiketter. Variabler som oppgis her, markeres som
#'   forventet å ha verdietiketter uavhengig av datatype. Standardverdien
#'   er `NULL`.
#' @param character_variables En logisk verdi som angir om alle
#'   tekstvariabler skal forventes å ha verdietiketter. Når verdien er
#'   `TRUE`, markeres alle variabler der [is.character()] er `TRUE`.
#'   Standardverdien er `TRUE`.
#' @param missing_only En logisk verdi som angir om resultatet bare skal
#'   inneholde variabler som forventes å ha verdietiketter, men som mangler
#'   slike etiketter. Når verdien er `FALSE`, returneres alle variabler.
#'   Standardverdien er `FALSE`.
#'
#' @return En tibble med én rad per variabel og følgende kolonner:
#'
#' \describe{
#'   \item{`variable`}{Navnet på variabelen.}
#'   \item{`type`}{Den første klassen til variabelen, hentet fra
#'     [class()].}
#'   \item{`should_have_value_labels`}{Logisk verdi som angir om variabelen
#'     forventes å ha verdietiketter.}
#'   \item{`has_value_labels`}{Logisk verdi som angir om variabelen faktisk
#'     har minst én registrert verdietikett.}
#'   \item{`missing_expected_value_labels`}{Logisk verdi som er `TRUE`
#'     dersom variabelen forventes å ha verdietiketter, men ikke har noen
#'     registrerte verdietiketter.}
#'   \item{`vardef_id`}{ID-en til variabeldefinisjonen hentet fra
#'     `definition_uri` i DataDoc-filen. Kolonnen inkluderes bare når
#'     `filsti` er oppgitt.}
#'   \item{`klass_id`}{KLASS-ID-en hentet fra `classification_uri` i
#'     DataDoc-filen. Kolonnen inkluderes bare når `filsti` er oppgitt.}
#' }
#'
#' @details
#' En variabel regnes som å ha verdietiketter dersom
#' [labelled::val_labels()] returnerer minst én etikett.
#'
#' Når `character_variables = TRUE`, forventes alle tekstvariabler å ha
#' verdietiketter. Variabler som oppgis eksplisitt i `variables`, forventes
#' også å ha verdietiketter, uavhengig av datatype.
#'
#' Dersom både `character_variables = FALSE` og `variables = NULL`, forventes
#' ingen variabler å ha verdietiketter.
#'
#' Når `filsti` er oppgitt, leses DataDoc-filen med
#' [jsonlite::fromJSON()]. Funksjonen bruker `short_name` til å koble
#' metadataene i DataDoc-filen til variablene i `data`.
#'
#' ID-en i `definition_uri` og `classification_uri` hentes fra teksten etter
#' siste skråstrek (`/`) eller kolon (`:`). En eventuell avsluttende skråstrek
#' fjernes før ID-en hentes.
#'
#' Dersom et av URI-feltene mangler i DataDoc-filen, settes den tilhørende
#' ID-en til `NA`.
#'
#' Når `missing_only = TRUE`, filtreres resultatet slik at bare variabler der
#' `missing_expected_value_labels` er `TRUE`, returneres.
#'
#' @examples
#' data <- tibble::tibble(
#'   kjoenn = labelled::labelled(
#'     c("1", "2", "1"),
#'     labels = c(
#'       Mann = "1",
#'       Kvinne = "2"
#'     )
#'   ),
#'   bosted = c("01", "02", "03"),
#'   alder = c(35, 42, 28)
#' )
#'
#' value_label_expectations(data)
#'
#' value_label_expectations(
#'   data,
#'   variables = "alder"
#' )
#'
#' value_label_expectations(
#'   data,
#'   missing_only = TRUE
#' )
#'
#' \dontrun{
#' value_label_expectations(
#'   data,
#'   filsti = "data/personell__DOC.json"
#' )
#' }
#'
#' @seealso
#' [vars_with_value_labels()] for å finne variabler som har verdietiketter,
#' [values_without_labels()] for å finne observerte verdier uten
#' verdietikett og [value_label_coverage()] for å beregne dekningen av
#' verdietiketter.
#'
#' @export
value_label_expectations <- function(
    data,
    filsti = NULL,
    variables = NULL,
    character_variables = TRUE,
    missing_only = FALSE
) {
  
  if (
    !is.logical(character_variables) ||
    length(character_variables) != 1L ||
    is.na(character_variables)
  ) {
    stop(
      "`character_variables` må være enten TRUE eller FALSE.",
      call. = FALSE
    )
  }
  
  if (
    !is.logical(missing_only) ||
    length(missing_only) != 1L ||
    is.na(missing_only)
  ) {
    stop(
      "`missing_only` må være enten TRUE eller FALSE.",
      call. = FALSE
    )
  }
  
  should_have_labels <- rep(
    FALSE,
    ncol(data)
  )
  
  names(should_have_labels) <- names(data)
  
  if (character_variables) {
    should_have_labels <- should_have_labels |
      vapply(
        data,
        is.character,
        logical(1)
      )
  }
  
  if (!is.null(variables)) {
    should_have_labels[
      names(should_have_labels) %in% variables
    ] <- TRUE
  }
  
  result <- tibble::tibble(
    variable = names(data),
    type = vapply(
      data,
      function(x) class(x)[1],
      character(1)
    ),
    should_have_value_labels = should_have_labels,
    has_value_labels = vapply(
      data,
      function(x) {
        length(labelled::val_labels(x)) > 0L
      },
      logical(1)
    )
  ) |>
    dplyr::mutate(
      missing_expected_value_labels =
        should_have_value_labels & !has_value_labels
    )
  
  # Hent ID-er direkte fra DataDoc ---------------------------------
  
  if (!is.null(filsti)) {
    
    datadoc <- jsonlite::fromJSON(
      filsti
    )
    
    datadoc_variables <- datadoc$datadoc$variables
    
    if (is.null(datadoc_variables)) {
      stop(
        "Fant ikke `datadoc$variables` i DataDoc-filen.",
        call. = FALSE
      )
    }
    
    if (!"short_name" %in% names(datadoc_variables)) {
      stop(
        "Fant ikke `short_name` i DataDoc-filen.",
        call. = FALSE
      )
    }
    
    # Hjelpefunksjon: hent første ikke-tomme verdi
    first_nonempty <- function(x) {
      
      if (is.null(x) || length(x) == 0L) {
        return(NA_character_)
      }
      
      x <- as.character(x)
      x <- trimws(x)
      
      x <- x[
        !is.na(x) &
          nzchar(x)
      ]
      
      if (length(x) == 0L) {
        return(NA_character_)
      }
      
      x[[1L]]
    }
    
    # Hjelpefunksjon: hent ID fra URI
    extract_id <- function(x) {
      
      x <- first_nonempty(x)
      
      if (is.na(x)) {
        return(NA_character_)
      }
      
      # Fjern eventuell avsluttende skråstrek
      x <- sub(
        pattern = "/+$",
        replacement = "",
        x = x
      )
      
      # Hent delen etter siste "/" eller ":"
      sub(
        pattern = "^.*[:/]",
        replacement = "",
        x = x
      )
    }
    
    n_variables <- NROW(
      datadoc_variables
    )
    
    # Vardef-ID
    if ("definition_uri" %in% names(datadoc_variables)) {
      
      vardef_id <- vapply(
        seq_len(n_variables),
        function(i) {
          extract_id(
            datadoc_variables$definition_uri[[i]]
          )
        },
        character(1)
      )
      
    } else {
      
      vardef_id <- rep(
        NA_character_,
        n_variables
      )
    }
    
    # KLASS-ID
    if ("classification_uri" %in% names(datadoc_variables)) {
      
      klass_id <- vapply(
        seq_len(n_variables),
        function(i) {
          extract_id(
            datadoc_variables$classification_uri[[i]]
          )
        },
        character(1)
      )
      
    } else {
      
      klass_id <- rep(
        NA_character_,
        n_variables
      )
    }
    
    metadata <- tibble::tibble(
      variable = datadoc_variables$short_name,
      vardef_id = vardef_id,
      klass_id = klass_id
    )
    
    result <- result |>
      dplyr::left_join(
        metadata,
        by = "variable"
      )
  }
  
  if (missing_only) {
    result <- result |>
      dplyr::filter(
        missing_expected_value_labels
      )
  }
  
  result
}



#' Sett versjonsbeskrivelse i en DataDoc-fil
#'
#' Legger til eller oppdaterer versjonsbeskrivelsen for et datasett i en
#' DataDoc-fil. Versjonsbeskrivelsen lagres med tilhørende språkkode.
#'
#' @param filsti_datadoc Tekststreng. Filsti til DataDoc-filen som skal
#'   oppdateres.
#' @param version_description Tekststreng. Versjonsbeskrivelsen som skal
#'   lagres.
#' @param language_code Tekststreng. Språkkode for versjonsbeskrivelsen.
#'   Standard er `"nb"`.
#' @param overwrite Logisk verdi. Angir om en eksisterende
#'   versjonsbeskrivelse skal overskrives. Standard er `FALSE`.
#'
#' @details
#' Funksjonen leser DataDoc-filen, oppdaterer
#' `datadoc.dataset.version_description` og skriver den oppdaterte
#' dokumentasjonen tilbake til samme fil.
#'
#' Versjonsbeskrivelsen lagres som en liste med `languageCode` og
#' `languageText`.
#'
#' Bare teksten på valgt språk vurderes og oppdateres; andre språk beholdes.
#' Dersom teksten på valgt språk allerede er utfylt og `overwrite = FALSE`,
#' blir filen ikke endret, og funksjonen gir en advarsel. Sett
#' `overwrite = TRUE` for å erstatte en eksisterende versjonsbeskrivelse.
#'
#' @return
#' Returnerer det oppdaterte DataDoc-objektet usynlig. Dersom en eksisterende
#' versjonsbeskrivelse ikke overskrives, returneres det uendrede
#' DataDoc-objektet usynlig.
#'
#' @examples
#' \dontrun{
#' set_version_description(
#'   filsti_datadoc = "data/datasett__DOC.json",
#'   version_description = "Oppdaterte tall for 2026."
#' )
#'
#' set_version_description(
#'   filsti_datadoc = "data/datasett__DOC.json",
#'   version_description = "Reviderte tall for 2026.",
#'   overwrite = TRUE
#' )
#'
#' set_version_description(
#'   filsti_datadoc = "data/dataset__DOC.json",
#'   version_description = "Updated figures for 2026.",
#'   language_code = "en"
#' )
#' }
#'
#' @export
set_version_description <- function(filsti_datadoc, version_description,
                                    language_code = "nb", overwrite = FALSE) {
  .datadoc_flag(overwrite, "overwrite")
  .datadoc_text(version_description, "version_description")
  language_code <- match.arg(language_code, c("nb", "nn", "en"))
  datadoc <- .datadoc_read(filsti_datadoc)
  felt <- datadoc$datadoc$dataset$version_description
  existing <- .datadoc_language_text(felt, language_code, fallback = FALSE)
  if (!overwrite && !is.na(existing)) {
    warning("Versjonsbeskrivelsen er allerede utfylt på ", language_code,
            ". Bruk overwrite = TRUE for å erstatte teksten.", call. = FALSE)
    return(invisible(datadoc))
  }
  datadoc$datadoc$dataset$version_description <- .datadoc_set_language(
    felt, version_description, language_code, overwrite = overwrite
  )
  .datadoc_write(datadoc, filsti_datadoc)
  invisible(datadoc)
}


#' Sammenlign variabelmetadata mellom to DataDoc-filer
#'
#' Sammenligner utvalgt variabelmetadata mellom to DataDoc-filer og viser
#' hvilke variabler som har ulik KLASS-referanse eller ulik tidsperiode for
#' dataene.
#'
#' @param filsti_1 Tekststreng. Filsti til den første DataDoc-filen eller
#'   Parquet-filen.
#' @param filsti_2 Tekststreng. Filsti til den andre DataDoc-filen eller
#'   Parquet-filen.
#' @param only_differences Logisk verdi. Dersom `TRUE`, returneres kun
#'   variabler med ulik metadata. Dersom `FALSE`, returneres alle variabler
#'   som finnes i begge DataDoc-filene. Standard er `TRUE`.
#'
#' @details
#' Dersom en filsti peker til en Parquet-fil, brukes `datadoc_path()` til å
#' finne tilhørende DataDoc-fil.
#'
#' Funksjonen sammenligner følgende metadata for variabler som finnes i
#' begge DataDoc-filene:
#'
#' * KLASS-id hentet fra `classification_uri`
#' * `contains_data_from`
#' * `contains_data_until`
#'
#' Variabler som kun finnes i én av DataDoc-filene, tas ikke med i
#' sammenligningen.
#'
#' Forskjeller i `contains_data_from` eller `contains_data_until` oppsummeres
#' i kolonnen `ulik_tidsperiode`. Kolonnen `ulik_metadata` er `TRUE` dersom
#' enten KLASS-id eller tidsperiode er forskjellig mellom filene.
#'
#' Manglende verdier behandles som en forskjell dersom metadata er utfylt
#' i den ene DataDoc-filen, men mangler i den andre.
#'
#' @return
#' En tibble med én rad per variabel som finnes i begge DataDoc-filene.
#' Resultatet inneholder følgende kolonner:
#'
#' * `variable`: Variabelens kortnavn.
#' * `klass_id_1`: KLASS-id i den første DataDoc-filen.
#' * `klass_id_2`: KLASS-id i den andre DataDoc-filen.
#' * `ulik_klass_id`: Om KLASS-id er forskjellig.
#' * `contains_data_from_1`: Startdato for data i den første DataDoc-filen.
#' * `contains_data_from_2`: Startdato for data i den andre DataDoc-filen.
#' * `contains_data_until_1`: Sluttdato for data i den første DataDoc-filen.
#' * `contains_data_until_2`: Sluttdato for data i den andre DataDoc-filen.
#' * `ulik_tidsperiode`: Om start- eller sluttdato er forskjellig.
#' * `ulik_metadata`: Om KLASS-id eller tidsperiode er forskjellig.
#'
#' Dersom `only_differences = TRUE`, inneholder resultatet kun rader der
#' `ulik_metadata` er `TRUE`.
#'
#' @examples
#' \dontrun{
#' compare_variable_metadata(
#'   filsti_1 = "data/datasett_2025__DOC.json",
#'   filsti_2 = "data/datasett_2026__DOC.json"
#' )
#'
#' compare_variable_metadata(
#'   filsti_1 = "data/datasett_2025.parquet",
#'   filsti_2 = "data/datasett_2026.parquet",
#'   only_differences = FALSE
#' )
#' }
#'
#' @export
compare_variable_metadata <- function(
    filsti_1,
    filsti_2,
    only_differences = TRUE
) {
  
  .datadoc_flag(only_differences, "only_differences")
  
  # Gjør om parquet-sti til datadoc-sti ved behov
  filsti_1 <- if (
    tolower(tools::file_ext(filsti_1)) == "parquet"
  ) {
    datadoc_path(filsti_1, to = "json")
  } else {
    filsti_1
  }
  
  filsti_2 <- if (
    tolower(tools::file_ext(filsti_2)) == "parquet"
  ) {
    datadoc_path(filsti_2, to = "json")
  } else {
    filsti_2
  }
  
  # Les datadoc
  datadoc_1 <- .datadoc_read(filsti_1)
  
  datadoc_2 <- .datadoc_read(filsti_2)
  
  # Hjelpefunksjon for å hente KLASS-id fra classification_uri
  get_klass_id <- .datadoc_klass_id
  
  # Hjelpefunksjon for å hente relevant variabelmetadata
  get_variable_metadata <- function(datadoc) {
    if (!length(datadoc$datadoc$variables)) {
      return(tibble::tibble(variable = character(), classification_uri = character(),
                            klass_id = character(), contains_data_from = character(),
                            contains_data_until = character()))
    }
    purrr::map_dfr(
      datadoc$datadoc$variables,
      function(x) {
        
        tibble::tibble(
          variable = x$short_name,
          classification_uri = if (is.null(x$classification_uri)) {
            NA_character_
          } else {
            x$classification_uri
          },
          klass_id = get_klass_id(x$classification_uri),
          contains_data_from = if (is.null(x$contains_data_from)) {
            NA_character_
          } else {
            x$contains_data_from
          },
          contains_data_until = if (is.null(x$contains_data_until)) {
            NA_character_
          } else {
            x$contains_data_until
          }
        )
      }
    )
  }
  
  metadata_1 <- get_variable_metadata(datadoc_1)
  metadata_2 <- get_variable_metadata(datadoc_2)
  
  # Behold kun variabler som finnes i begge
  result <- metadata_1 |>
    dplyr::inner_join(
      metadata_2,
      by = "variable",
      suffix = c("_1", "_2")
    ) |>
    dplyr::mutate(
      ulik_klass_id = dplyr::coalesce(
        klass_id_1 != klass_id_2,
        xor(
          is.na(klass_id_1),
          is.na(klass_id_2)
        )
      ),
      ulik_fra = dplyr::coalesce(
        contains_data_from_1 != contains_data_from_2,
        xor(
          is.na(contains_data_from_1),
          is.na(contains_data_from_2)
        )
      ),
      ulik_til = dplyr::coalesce(
        contains_data_until_1 != contains_data_until_2,
        xor(
          is.na(contains_data_until_1),
          is.na(contains_data_until_2)
        )
      ),
      ulik_tidsperiode = ulik_fra | ulik_til,
      ulik_metadata = ulik_klass_id | ulik_tidsperiode
    ) |>
    dplyr::select(
      variable,
      klass_id_1,
      klass_id_2,
      ulik_klass_id,
      contains_data_from_1,
      contains_data_from_2,
      contains_data_until_1,
      contains_data_until_2,
      ulik_tidsperiode,
      ulik_metadata
    )
  
  if (only_differences) {
    result <- result |>
      dplyr::filter(ulik_metadata)
  }
  
  result
}


#' Finn variabler som mangler obligatorisk metadata
#'
#' Lager en oversikt over hvilke variabler i en Datadoc-fil som mangler
#' ett eller flere obligatoriske metadatafelt.
#'
#' @param filsti En tekststreng med filstien til en Datadoc-fil eller den
#'   tilhørende Parquet-filen.
#' @param only_incomplete En logisk verdi som angir om bare variabler som
#'   mangler ett eller flere obligatoriske metadatafelt skal returneres.
#'   Når verdien er `TRUE`, returneres bare ufullstendig dokumenterte
#'   variabler. Når verdien er `FALSE`, returneres alle variabler.
#'   Standardverdien er `TRUE`.
#'
#' @return En `data.frame` med én rad per variabel og følgende kolonner:
#'
#' \describe{
#'   \item{`variable`}{Variabelens kortnavn (`short_name`).}
#'   \item{`n_missing`}{Antall obligatoriske metadatafelt som mangler.}
#'   \item{`missing_required_fields`}{En kommaseparert tekststreng med navnene
#'     på de obligatoriske metadatafeltene som mangler. Verdien er `NA`
#'     dersom ingen obligatoriske felt mangler.}
#'   \item{`complete`}{En logisk verdi som angir om alle obligatoriske
#'     metadatafelt er utfylt.}
#' }
#'
#' @details
#' Funksjonen kontrollerer følgende obligatoriske, brukerutfylte
#' metadatafelt for hver variabel:
#'
#' \itemize{
#'   \item `name`
#'   \item `is_personal_data`
#'   \item `unit_type`
#'   \item `variable_role`
#'   \item `data_source`
#'   \item `temporality_type`
#' }
#'
#' Dette er en utfyllingskontroll av de seks feltene ovenfor, ikke full
#' validering mot DataDoc-modellen. Betingede krav kontrolleres ikke.
#' For språkposter kontrolleres tekstinnholdet, ikke bare at språk finnes.
#'
#' Et metadatafelt regnes som manglende dersom verdien er `NULL`, har
#' lengde null, er `NA`, eller består av tom tekst eller bare mellomrom.
#'
#' Logiske verdier behandles som gyldige verdier. Dette innebærer blant
#' annet at `FALSE` i `is_personal_data` ikke regnes som manglende metadata.
#'
#' Dersom `only_incomplete = TRUE`, filtreres variabler som har alle de
#' obligatoriske metadatafeltene utfylt bort fra resultatet.
#'
#' @examples
#' \dontrun{
#' # Vis bare variabler som mangler obligatorisk metadata
#' summarise_missing_required_metadata(
#'   filsti = "data/personell__DOC.json"
#' )
#'
#' # Vis alle variabler
#' summarise_missing_required_metadata(
#'   filsti = "data/personell__DOC.json",
#'   only_incomplete = FALSE
#' )
#' }
#'
#' @seealso
#' [datadoc_path()] for å konvertere mellom filstier til Parquet- og
#' Datadoc-filer.
#'
#' @export
summarise_missing_required_metadata <- function(
    filsti,
    only_incomplete = TRUE
) {
  
  .datadoc_flag(only_incomplete, "only_incomplete")
  
  # Gjør om parquet-sti til datadoc-sti ved behov
  filsti <- if (
    tolower(tools::file_ext(filsti)) == "parquet"
  ) {
    datadoc_path(filsti, to = "json")
  } else {
    filsti
  }
  
  # Les Datadoc
  datadoc <- .datadoc_read(filsti)
  
  variables <- datadoc$datadoc$variables
  
  # Obligatoriske, brukerutfylte metadatafelt
  required_fields <- c(
    "name",
    "is_personal_data",
    "unit_type",
    "variable_role",
    "data_source",
    "temporality_type"
  )
  
  # Hjelpefunksjon for å avgjøre om et felt mangler
  is_missing_metadata <- .datadoc_is_empty
  
  # Lag oversikt per variabel
  result <- lapply(
    variables,
    function(variable) {
      
      missing_fields <- required_fields[
        vapply(
          required_fields,
          function(field) {
            is_missing_metadata(variable[[field]])
          },
          logical(1)
        )
      ]
      
      data.frame(
        variable = if (
          !is.null(variable$short_name) &&
          length(variable$short_name) > 0
        ) {
          variable$short_name
        } else {
          NA_character_
        },
        n_missing = length(missing_fields),
        missing_required_fields = if (length(missing_fields) == 0) {
          NA_character_
        } else {
          paste(missing_fields, collapse = ", ")
        },
        complete = length(missing_fields) == 0,
        stringsAsFactors = FALSE
      )
    }
  )
  
  result <- dplyr::bind_rows(
    tibble::tibble(variable = character(), n_missing = integer(),
                   missing_required_fields = character(), complete = logical()),
    result
  )
  
  if (only_incomplete) {
    result <- dplyr::filter(
      result,
      !complete
    )
  }
  
  result
}


# Felles innlesing og utvalg av kolonner som finnes i datasettet.


#' Oppdater variabelnavn og intervjumetadata i DataDoc
#'
#' Henter variabelnavn fra en metadatatabell eller fra kolonneetiketter i
#' et datasett. Koblingen skjer på eksakt likhet med `short_name`.
#' Standardverdier for intervjuspørsmål fyller tomme felt for matchende
#' variabler. Utvalget kan avgrenses med `intervjuvariabler`.
#'
#' @param filsti_datadoc Filsti til DataDoc-filen (.json) eller til den
#'   tilhørende Parquet-filen. JSON-filen må allerede finnes.
#' @param metadata En metadatatabell med kolonnene angitt i
#'   `variabel_kolonne` og `betydning_kolonne`,
#'   filstien til en slik tabell i Parquet-format, eller et data.frame med
#'   kolonneetiketter i attributtet `label`. Gjentatte, identiske tekster
#'   for samme variabel er tillatt. Tomme etiketter brukes ikke som navn.
#' @param variabler Tekstvektor med kortnavn som skal oppdateres.
#'   NULL velger alle kortnavn som finnes i både kilden og DataDoc.
#' @param overwrite Logisk verdi. FALSE fyller bare manglende navn på
#'   valgt språk. TRUE tillater også å erstatte navnet på dette språket.
#'   Andre språk og utfylte fagfelt beholdes ved begge innstillinger.
#' @param intervjuvariabler Kortnavn på direkte intervjuspørsmål som skal få
#'   standardverdier. Standard er `variabler`. NULL velger alle variablene
#'   som matcher innenfor utvalget. Bruk `character()` for bare navn.
#'   Alle intervjuvariabler må inngå i utvalget og finnes i begge kilder.
#' @param unit_type Enhetstype som tekstkode. Standard er "20" (person).
#' @param temporality_type Standard er "STATUS" for status på
#'   intervjutidspunktet. Andre muligheter er "ACCUMULATED", "FIXED" og "EVENT".
#' @param data_source Tekstkode fra KLASS 712. Standard er "67".
#'   Koden brukes direkte. NULL hopper over feltet. Verdien "SSB"
#'   støttes også og slår opp SSB i KLASS for datasettets sluttdato.
#' @param variable_role Standard er "MEASURE" for selve intervjusvaret.
#' @param population_description Eventuell dokumentert populasjonstekst
#'   for alle valgte intervjuspørsmål, på `language_code`. Standard NULL
#'   hopper over feltet. Funksjonen utleder ikke hvem som fikk spørsmålet.
#'   Manglende populasjon vil fortsatt kunne utløse valideringsadvarsler.
#' @param language_code Språk for navn og eventuell populasjonstekst.
#'   Standard er "nb". Tillatte verdier er "nb", "nn" og "en".
#' @details Bruk NULL for et fagfelt som skal hoppes over. Send spørsmål
#'   med forskjellige referanseperioder i separate kall. Funksjonen
#'   oppdaterer variabelmetadata i JSON-filen. Øvrige deler av dokumentet
#'   beholdes, også eventuell tidligere beregnet fullføringsprosent.
#' @param variabel_kolonne Kolonnen med kortnavn i en metadatatabell.
#'   Standard er `"variabel"` for bakoverkompatibilitet.
#' @param betydning_kolonne Kolonnen med spørsmålstekst. Standard er
#'   `"variable_text"`. Bruk `"sporsmaal_text"` for EHIS.
#' @param clean_labels Med `TRUE` (standard) renses etiketttekst for tagger.
#' @param quiet Med `TRUE` (standard) skjules statusmeldinger.
#'
#' @return Det oppdaterte DataDoc-dokumentet, usynlig. Antall endrede
#'   variabler meldes i konsollen. Ved null endringer skrives ikke filen.
#' @examples
#' \dontrun{
#' # Fyll navn og tomme fagfelt for alle matchende variabler.
#' set_variable_names_from_labels(filsti_datadoc, kodelister)
#' # Fyll bare navn.
#' set_variable_names_from_labels(
#'   filsti_datadoc, kodelister, intervjuvariabler = character()
#' )
#' set_variable_names_from_labels(
#'   filsti_datadoc, royk_med_labels,
#'   variabler = c("tob1", "tob2")
#' )
#' }
#' @export
set_variable_names_from_labels <- function(
    filsti_datadoc, metadata, variabler = NULL, overwrite = FALSE,
    intervjuvariabler = variabler, unit_type = "20", temporality_type = "STATUS",
    data_source = "67", variable_role = "MEASURE",
    population_description = NULL, language_code = "nb",
    variabel_kolonne = "variabel", betydning_kolonne = "variable_text",
    clean_labels = TRUE, quiet = TRUE) {
  .datadoc_flag(quiet, "quiet")
  .datadoc_flag(clean_labels, "clean_labels")
  .datadoc_columns(list(variabel_kolonne = variabel_kolonne,
                        betydning_kolonne = betydning_kolonne))
  
  tekst <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  if (!tekst(filsti_datadoc)) stop("Oppgi én filsti til DataDoc.")
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` må være TRUE eller FALSE.")
  }
  if (!tekst(language_code) || !language_code %in% c("nb", "nn", "en")) {
    stop("`language_code` må være nb, nn eller en.")
  }
  for (utvalg in list(variabler, intervjuvariabler)) {
    if (!is.null(utvalg) && (!is.character(utvalg) || anyNA(utvalg) ||
                             any(!nzchar(trimws(utvalg))))) {
      stop("Variabelutvalg må bestå av ikke-tomme kortnavn.")
    }
  }
  standard <- list(unit_type = unit_type, temporality_type = temporality_type,
                   variable_role = variable_role, data_source = data_source)
  for (verdi in c(standard, list(population_description))) {
    if (!is.null(verdi) && !tekst(verdi)) stop("Bruk én tekstverdi eller NULL per fagfelt.")
  }
  if (!is.null(temporality_type) &&
      !temporality_type %in% c("STATUS", "FIXED", "EVENT", "ACCUMULATED")) {
    stop("Ukjent temporalitetstype: ", temporality_type)
  }
  if (!is.null(variable_role) &&
      !variable_role %in% c("MEASURE", "IDENTIFIER", "ATTRIBUTE", "START_TIME", "STOP_TIME")) {
    stop("Ukjent variabelrolle: ", variable_role)
  }
  
  if (tolower(tools::file_ext(filsti_datadoc)) == "parquet") {
    filsti_datadoc <- sub("\\.parquet$", "__DOC.json", filsti_datadoc, ignore.case = TRUE)
  }
  if (tolower(tools::file_ext(filsti_datadoc)) != "json" || !file.exists(filsti_datadoc)) {
    stop("Finner ingen DataDoc-fil på oppgitt sti: ", filsti_datadoc)
  }
  signatur <- unname(tools::md5sum(filsti_datadoc))
  doc <- .datadoc_read(filsti_datadoc)
  poster <- doc$datadoc$variables
  if (!is.list(poster) || is.null(poster)) stop("DataDoc mangler listen datadoc.variables.")
  kortnavn <- vapply(poster, function(x) x[["short_name"]], character(1))
  if (anyNA(kortnavn) || any(!nzchar(kortnavn)) || anyDuplicated(kortnavn)) {
    stop("DataDoc må ha ett unikt kortnavn per variabel.")
  }
  
  if (tekst(metadata)) {
    if (tolower(tools::file_ext(metadata)) != "parquet") stop("Metadatafilen må være Parquet.")
    metadata <- arrow::read_parquet(metadata)
    if (!all(c(variabel_kolonne, betydning_kolonne) %in% names(metadata))) {
      stop("Metadatafilen mangler angitte kolonner.", call. = FALSE)
    }
  }
  if (!is.data.frame(metadata)) stop("Oppgi en metadatatabell, en Parquet-fil eller et datasett.")
  if (all(c(variabel_kolonne, betydning_kolonne) %in% names(metadata))) {
    kilde <- data.frame(kortnavn = trimws(as.character(metadata[[variabel_kolonne]])),
                        navn = as.character(metadata[[betydning_kolonne]]))
  } else {
    etiketter <- vapply(metadata, function(x) {
      verdi <- attr(x, "label", exact = TRUE)
      if (tekst(verdi)) trimws(verdi) else NA_character_
    }, character(1))
    kilde <- data.frame(kortnavn = names(metadata), navn = unname(etiketter))
  }
  
  kilde$navn <- .datadoc_clean_labels(kilde$navn, clean_labels)
  felles <- intersect(kortnavn, kilde$kortnavn)
  if (!is.null(variabler)) {
    mangler <- setdiff(variabler, felles)
    if (length(mangler)) stop("Valgte variabler mangler i DataDoc eller kilden: ",
                              paste(mangler, collapse = ", "))
    felles <- intersect(felles, variabler)
  }
  # NULL følger det ferdige utvalget, også når variabler er utelatt.
  if (is.null(intervjuvariabler)) {
    intervjuvariabler <- felles
  }
  if (length(setdiff(intervjuvariabler, felles))) {
    stop("Intervjuvariablene må finnes både i kilden og i DataDoc og være med i utvalget.")
  }
  kilde <- unique(kilde[kilde$kortnavn %in% felles & !is.na(kilde$navn) &
                          nzchar(kilde$navn), , drop = FALSE])
  duplikat <- anyDuplicated(kilde$kortnavn)
  if (duplikat) stop("Motstridende kolonneetiketter for ", kilde$kortnavn[[duplikat]], ".")
  navn <- stats::setNames(kilde$navn, kilde$kortnavn)
  
  # DataDoc representerer flerspråklig tekst som en liste med språkposter.
  sett_spraktekst <- function(felt, verdi, erstatt = FALSE) {
    .datadoc_set_language(felt, verdi, language_code, overwrite = erstatt)
  }
  
  antall <- 0L
  for (i in which(kortnavn %in% felles)) {
    var <- poster[[i]]
    id <- kortnavn[[i]]
    if (id %in% names(navn)) {
      var$name <- sett_spraktekst(var$name, navn[[id]], overwrite)
    }
    if (id %in% intervjuvariabler) {
      for (felt in names(standard)) {
        verdi <- standard[[felt]]
        eksisterende <- var[[felt]]
        tomt <- is.null(eksisterende) || length(eksisterende) == 0L ||
          (is.character(eksisterende) && length(eksisterende) == 1L &&
             (is.na(eksisterende) || !nzchar(trimws(eksisterende))))
        if (is.null(verdi) || !tomt) next
        if (felt == "data_source" && verdi == "SSB") {
          dato <- doc$datadoc$dataset$contains_data_until
          if (!tekst(dato)) dato <- as.character(Sys.Date())
          kilder <- klassR::get_klass(712, date = dato, language = "nb")
          kilde_navn <- tolower(trimws(kilder$name))
          ssb <- unique(as.character(kilder$code[kilde_navn %in%
                                                   c("statistisk sentralbyrå", "statistisk sentralbyrå (ssb)", "ssb")]))
          if (length(ssb) != 1L || !tekst(ssb)) {
            stop("Fant ikke én entydig kode for SSB i KLASS 712. ",
                 "Oppgi virksomhetens avtalte kode i data_source, eller NULL for å hoppe over.")
          }
          verdi <- ssb[[1L]]
          standard$data_source <- verdi
        }
        var[[felt]] <- verdi
      }
      if (!is.null(population_description)) {
        var$population_description <- sett_spraktekst(var$population_description,
                                                      population_description)
      }
    }
    if (!identical(var, poster[[i]])) {
      poster[[i]] <- var
      antall <- antall + 1L
    }
  }
  
  if (antall > 0L) {
    doc$datadoc$variables <- poster
    .datadoc_write(doc, filsti_datadoc, expected_signature = signatur)
  }
  if (!quiet) message("Oppdaterte ", antall, " av ", length(felles), " matchende variabler.")
  invisible(doc)
}

#' Opprett et tomt Datadoc-dokument
#'
#' Oppretter et nytt Datadoc-metadatadokument for en Parquet-fil.
#' Dokumentet opprettes på tilsvarende måte som når en udokumentert
#' Parquet-fil åpnes og lagres i Datadoc Editor uten at metadata fylles
#' ut manuelt. Valgfrie argumenter fyller ut tekstfelter ved opprettelsen.
#'
#' @param filsti En tekststreng med full filsti til en Parquet-fil.
#'   Filen må ligge under en produktsti på formen
#'   `"/buckets/produkt/<statistikk>/..."`, der `<statistikk>` er
#'   statistikkens kortnavn.
#' @param population_description En tekststreng som beskriver populasjonen.
#'   Samme tekst legges i `population_description` på datasettnivå og for
#'   alle variabler. Standard er `NULL`, som lar feltene være uendret.
#' @param name En tekststreng med datasettets navn. Gjelder bare
#'   datasettnivå. Standard er `NULL`.
#' @param description En tekststreng med beskrivelse av datasettet.
#'   Gjelder bare datasettnivå. Standard er `NULL`.
#' @param version_description En tekststreng med beskrivelse av denne
#'   versjonen av datasettet. Gjelder bare datasettnivå. Standard er `NULL`.
#' @param language_code En tekststreng med språkkoden for tekstfeltene,
#'   for eksempel `"nb"`, `"nn"` eller `"en"`. Standard er `"nb"`.
#'
#' @param quiet Logisk verdi. Standard er `TRUE`. Med `TRUE` skjules
#'   meldinger, advarsler og konsollutskrifter fra R og Python under
#'   kjøringen. Feil som stopper kjøringen, vises fortsatt.
#'
#' @details
#' Funksjonen bruker Python-pakken `dapla-toolbelt-metadata` via
#' `reticulate` til å opprette Datadoc-dokumentet. Metadata som kan
#' utledes automatisk fra Parquet-filen og filstien, som variabelnavn,
#' datatyper, filnavn og datatilstand, fylles ut automatisk.
#'
#' Statistikkens kortnavn hentes fra filstien. For eksempel gir
#' `"/buckets/produkt/speshelse/..."` kortnavnet `"speshelse"`.
#' Statistikkområdet (`subject_field`) hentes fra SSBs emnestruktur
#' basert på statistikkens primære plassering.
#'
#' Feltene `metadata_created_by` og `metadata_last_updated_by` settes
#' fra miljøvariabelen `GIT_USER_MAIL`.
#'
#' Metadatadokumentet lagres i samme mappe som Parquet-filen og får
#' samme filnavn med suffikset `"__DOC.json"`. For eksempel vil
#' `"data_p2025_v1.parquet"` gi `"data_p2025_v1__DOC.json"`.
#'
#' Funksjonen stopper dersom inputfilen ikke er en Parquet-fil,
#' dersom statistikkens kortnavn ikke kan utledes fra filstien, eller
#' dersom Datadoc-filen allerede finnes.
#'
#' Det gis en advarsel dersom statistikkområdet ikke kan bestemmes,
#' eller dersom miljøvariabelen `GIT_USER_MAIL` ikke er tilgjengelig.
#'
#' Tekstfeltene lagres som språkposter med `languageCode` og
#' `languageText`. En eksisterende tekst på valgt språk erstattes når
#' argumentet er angitt; eventuelle andre språk beholdes. `NULL` betyr
#' at feltet beholdes slik Python-pakken opprettet det. Angitte tekster
#' må være én ikke-tom tekststreng og kan ikke være `NA`.
#'
#' Populasjonsteksten kopieres til alle variabler, også administrative
#' og avledede variabler. Den beskriver en felles populasjon; eventuell
#' spørsmålsruting eller andre avgrensninger må dokumenteres separat.
#'
#' Tekstfeltene fylles ut etter at Python-pakken har skrevet dokumentet.
#' Pakken kan derfor varsle om manglende obligatoriske metadata før
#' disse feltene fylles ut. Andre obligatoriske felter kan fortsatt
#' mangle etter opprettelsen.
#'
#' @return Usynlig (`invisible`) tekststreng med filstien til det
#'   opprettede Datadoc-dokumentet.
#'
#' @examples
#' \dontrun{
#' create_empty_datadoc(
#'   "/buckets/produkt/speshelse/klargjorte-data/data_p2025_v1.parquet"
#' )
#'
#' create_empty_datadoc(
#'   filsti = "/buckets/produkt/royk/klargjorte-data/royk_p2026.parquet",
#'   population_description = "Beskriv undersøkelsens populasjon her.",
#'   name = "Røyk, alkohol og andre rusmidler 2026",
#'   description = "Klargjorte data fra undersøkelsen i 2026.",
#'   version_description = "Første klargjorte versjon.",
#'   language_code = "nb",
#'   quiet = TRUE
#' )
#' }
#'
#' @export
create_empty_datadoc <- function(
    filsti,
    population_description = NULL,
    name = NULL,
    description = NULL,
    version_description = NULL,
    language_code = "nb",
    quiet = TRUE) {
  
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("Argumentet 'quiet' må være TRUE eller FALSE.", call. = FALSE)
  }
  
  kjor <- function() {
    
    kontroller_tekst <- function(x, argument, tillat_null = TRUE) {
      if (is.null(x) && tillat_null) {
        return(invisible(NULL))
      }
      if (!is.character(x) || length(x) != 1L ||
          is.na(x) || !nzchar(trimws(x))) {
        stop(
          "Argumentet '", argument,
          "' må være én ikke-tom tekststreng uten NA",
          if (tillat_null) " eller NULL." else ".",
          call. = FALSE
        )
      }
      invisible(NULL)
    }
    
    kontroller_tekst(filsti, "filsti", tillat_null = FALSE)
    kontroller_tekst(language_code, "language_code", tillat_null = FALSE)
    language_code <- match.arg(trimws(language_code), c("nb", "nn", "en"))
    
    tekstfelter <- list(
      population_description = population_description,
      name = name,
      description = description,
      version_description = version_description
    )
    for (felt in names(tekstfelter)) {
      kontroller_tekst(tekstfelter[[felt]], felt)
    }
    
    # ------------------------------------------------------------------
    # 1. Kontroller fil
    # ------------------------------------------------------------------
    
    if (tolower(tools::file_ext(filsti)) != "parquet") {
      stop(
        "Filen må være en parquet-fil: ",
        filsti
      )
    }
    
    
    # ------------------------------------------------------------------
    # 2. Finn sti til Datadoc-fil
    # ------------------------------------------------------------------
    
    datadoc_filsti <- sub(
      "\\.parquet$",
      "__DOC.json",
      filsti,
      ignore.case = TRUE
    )
    
    if (file.exists(datadoc_filsti)) {
      stop(
        "Datadoc-filen finnes allerede: ",
        datadoc_filsti
      )
    }
    
    
    # ------------------------------------------------------------------
    # 3. Finn statistikk-kortnavn fra filstien
    # ------------------------------------------------------------------
    
    statistikk <- stringr::str_match(
      filsti,
      "/buckets/produkt/([^/]+)/"
    )[, 2]
    
    if (is.na(statistikk)) {
      stop(
        paste0(
          "Kunne ikke finne statistikk-kortnavn fra filstien.\n",
          "Forventet en sti på formen:\n",
          "/buckets/produkt/<statistikk>/..."
        )
      )
    }
    
    
    # ------------------------------------------------------------------
    # 4. Finn statistikkområde / subject_field
    # ------------------------------------------------------------------
    
    subject_url <- paste0(
      "https://www.ssb.no/xp/_/service/mimir/",
      "subjectStructurStatistics"
    )
    
    subject_field <- tryCatch(
      {
        emnestruktur <- xml2::read_xml(
          subject_url
        )
        
        delemner <- xml2::xml_find_all(
          emnestruktur,
          ".//delemne"
        )
        
        treff <- vapply(
          delemner,
          function(delemne) {
            
            statistikker <- xml2::xml_find_all(
              delemne,
              ".//Statistikk"
            )
            
            if (length(statistikker) == 0) {
              return(FALSE)
            }
            
            kortnavn <- xml2::xml_attr(
              statistikker,
              "kortnavn"
            )
            
            primaer <- xml2::xml_attr(
              statistikker,
              "isPrimaerPlassering"
            )
            
            any(
              kortnavn == statistikk &
                primaer == "true",
              na.rm = TRUE
            )
          },
          logical(1)
        )
        
        if (!any(treff)) {
          NA_character_
        } else {
          xml2::xml_attr(
            delemner[treff][[1]],
            "emnekode"
          )
        }
      },
      error = function(e) {
        warning(
          "Kunne ikke hente statistikkområde for '",
          statistikk,
          "': ",
          conditionMessage(e)
        )
        
        NA_character_
      }
    )
    
    
    # ------------------------------------------------------------------
    # 5. Python-avhengighet
    # ------------------------------------------------------------------
    
    reticulate::py_require(
      "dapla-toolbelt-metadata"
    )
    
    opprett_metadata <- function() {
      if (quiet) {
        # Stans også loggere som allerede har en egen konsollhandler.
        logging <- reticulate::import("logging")
        tidligere_loggnivaa <- logging$root$manager$disable
        on.exit(logging$disable(tidligere_loggnivaa), add = TRUE)
        logging$disable(.Machine$integer.max)
      }
      
      dapla_datasets <- reticulate::import(
        "dapla_metadata.datasets"
      )
      
      
      # ------------------------------------------------------------------
      # 6. Opprett Datadoc-objekt fra parquet-filen
      # ------------------------------------------------------------------
      
      metadata <- dapla_datasets$Datadoc(
        dataset_path = filsti
      )
      
      
      # ------------------------------------------------------------------
      # 7. Sett statistikkområde eksplisitt
      # ------------------------------------------------------------------
      
      if (!is.na(subject_field) && nzchar(subject_field)) {
        metadata$dataset$subject_field <- subject_field
      } else {
        warning(
          "Fant ikke statistikkområde for statistikken '",
          statistikk,
          "'."
        )
      }
      
      
      # ------------------------------------------------------------------
      # 8. Skriv Datadoc-dokument
      # ------------------------------------------------------------------
      
      metadata$write_metadata_document()
      invisible(NULL)
    }
    
    if (quiet) {
      invisible(reticulate::py_capture_output(
        opprett_metadata(),
        type = c("stdout", "stderr")
      ))
    } else {
      opprett_metadata()
    }
    
    # ------------------------------------------------------------------
    # 9. Fyll ut tekstfelter og Opprettet av / Oppdatert av
    # ------------------------------------------------------------------
    
    datadoc <- .datadoc_read(datadoc_filsti)
    
    if (!is.list(datadoc$datadoc$dataset)) {
      stop("Datadoc-filen mangler forventet struktur: datadoc.dataset.")
    }
    
    sett_spraktekst <- function(eksisterende, tekst) {
      .datadoc_set_language(eksisterende, tekst, language_code, overwrite = TRUE)
    }
    
    for (felt in names(tekstfelter)) {
      if (!is.null(tekstfelter[[felt]])) {
        datadoc$datadoc$dataset[[felt]] <- sett_spraktekst(
          datadoc$datadoc$dataset[[felt]],
          tekstfelter[[felt]]
        )
      }
    }
    
    if (!is.null(population_description)) {
      for (i in seq_along(datadoc$datadoc$variables)) {
        datadoc$datadoc$variables[[i]]$population_description <- sett_spraktekst(
          datadoc$datadoc$variables[[i]]$population_description,
          population_description
        )
      }
    }
    
    bruker <- Sys.getenv(
      "GIT_USER_MAIL",
      unset = NA_character_
    )
    
    if (!is.na(bruker) && nzchar(bruker)) {
      
      datadoc$datadoc$dataset$metadata_created_by <- bruker
      datadoc$datadoc$dataset$metadata_last_updated_by <- bruker
      
    } else {
      warning(
        "Miljøvariabelen GIT_USER_MAIL er ikke satt. ",
        "Feltene 'Opprettet av' og 'Oppdatert av' ble derfor ikke fylt ut."
      )
    }
    
    .datadoc_write(
      datadoc,
      path = datadoc_filsti,
      pretty = TRUE,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )
    
    # ------------------------------------------------------------------
    # 10. Ferdig
    # ------------------------------------------------------------------
    
    message(
      "Datadoc-fil opprettet: ",
      datadoc_filsti
    )
    
    if (!is.na(subject_field) && nzchar(subject_field)) {
      message(
        "Statistikkområde: ",
        subject_field,
        " (",
        statistikk,
        ")"
      )
    }
    
    invisible(datadoc_filsti)
  }
  
  if (!quiet) {
    return(invisible(kjor()))
  }
  
  # Gjenopprett konsollen også dersom kjøringen stopper med en feil.
  stille <- file(if (.Platform$OS.type == "windows") "NUL" else "/dev/null",
                 open = "wt")
  tidligere_output <- sink.number(type = "output")
  tidligere_message <- sink.number(type = "message")
  on.exit({
    sink(tidligere_message, type = "message")
    while (sink.number(type = "output") > tidligere_output) {
      sink(type = "output")
    }
    close(stille)
  }, add = TRUE)
  sink(stille, type = "output")
  sink(stille, type = "message")
  
  invisible(suppressMessages(suppressWarnings(kjor())))
}

#' Legg til verdietiketter fra en metadatafil eller metadatatabell
#'
#' Kobler variabelnavn og svarkoder i metadataene mot datasettet og legger
#' til verdietiketter med `labelled::set_value_labels()`.
#'
#' @param data En data.frame eller tibble som skal få verdietiketter.
#' @param kodelister En data.frame, tibble eller filsti til en Parquet-fil
#'   med variabelnavn, svarkoder og tilhørende etiketter.
#' @param quiet Med `TRUE` (standard) skjules statusmeldinger. Advarsler om
#'   observerte verdier uten etikett og feil vises fortsatt.
#' @param variabel_kolonne Kolonnen med variabelnavn. Standard er `"varnavn"`.
#' @param kode_kolonne Kolonnen med svarkoder. Standard er `"kode"`.
#' @param betydning_kolonne Kolonnen med svarkodetekst. Standard er `"tekst"`.
#' @param clean_labels Med `TRUE` (standard) fjernes HTML- og
#'   formateringstagger, og mellomrom ryddes opp. Med `FALSE` beholdes
#'   taggene, også `<rt>`. Innledende og avsluttende mellomrom trimmes alltid.
#'
#' @details Datatypen og verdiene i data beholdes. Numeriske variabler får
#' numeriske etikettkoder; tekstvariabler beholder tekstkoder og ledende
#' nuller. Koder som ikke kan tilpasses variabelens datatype, gir feil.
#' Faktorer, datoer, logiske variabler og andre spesialklasser støttes ikke
#' av denne funksjonen og må håndteres eksplisitt før etiketter legges på.
#'
#' Bare variabler med treff behandles. Rader uten kode eller etikett
#' ignoreres. Identiske oppslag slås sammen; motstridende etiketter for
#' samme kode gir feil. Eksisterende verdietiketter erstattes ved gyldig
#' treff. Eksisterende kolonneetiketter beholdes. Observerte verdier uten
#' etikett beholdes og gir advarsel, også med `quiet = TRUE`. Vanlige `NA`
#' utelates fra kontrollen.
#'
#' Rensingen beholder tekst mellom tagger, `^BedrNavn` og `<.>`.
#' HTML-entiteter som `&amp;` og `&nbsp;` dekodes ikke. Ingen filer skrives.
#' @return Datasettet med verdietiketter på variablene som har treff.
#' @examples
#' data <- data.frame(nrinnled = c(1L, 2L))
#' kodelister <- data.frame(
#'   varnavn = c("nrinnled", "nrinnled"), kode = c("1", "2"),
#'   tekst = c("Intervju", "<rt>Frafall</rt>")
#' )
#' resultat <- add_value_labels_from_file(data, kodelister)
#' labelled::val_labels(resultat$nrinnled)
#' is.integer(resultat$nrinnled)
#'
#' # Det tidligere formatet angis eksplisitt:
#' tidligere <- data.frame(variabel = "nrinnled", varnavn = "1",
#'                         sporsmaal_text = "Intervju")
#' resultat <- add_value_labels_from_file(
#'   data[1, , drop = FALSE], tidligere,
#'   variabel_kolonne = "variabel", kode_kolonne = "varnavn",
#'   betydning_kolonne = "sporsmaal_text"
#' )
#' @export
add_value_labels_from_file <- function(
    data, kodelister, quiet = TRUE, variabel_kolonne = "varnavn",
    kode_kolonne = "kode", betydning_kolonne = "tekst", clean_labels = TRUE) {
  .datadoc_flag(quiet, "quiet")
  .datadoc_flag(clean_labels, "clean_labels")
  .datadoc_columns(list(variabel_kolonne = variabel_kolonne,
                        kode_kolonne = kode_kolonne,
                        betydning_kolonne = betydning_kolonne))
  kjor <- function() {
    oppslag <- .les_etikettoppslag(
      data, kodelister, felt = c(kode = kode_kolonne, betydning = betydning_kolonne),
      variabel_kolonne = variabel_kolonne
    )
    for (variabel in unique(oppslag$variabel)) {
      rader <- oppslag[oppslag$variabel == variabel, , drop = FALSE]
      data[[variabel]] <- .datadoc_set_value_labels(
        data[[variabel]], rader$kode, rader$betydning, variabel, clean_labels
      )
    }
    data
  }
  if (quiet) suppressMessages(kjor()) else kjor()
}

#' Legg til kolonneetiketter fra en metadatafil eller metadatatabell
#'
#' Kobler variabelnavn i metadataene mot kolonnenavn i datasettet og legger
#' til beskrivende kolonneetiketter med `labelled::var_label()`.
#' Som standard brukes `varnavn` til koblingen og `sporsmaal_text`
#' som kolonneetikett.
#'
#' @param data En data.frame eller tibble som skal få kolonneetiketter.
#' @param kodelister En data.frame eller tibble med variabelnavn og
#'   kolonneetiketter, eller en tekststreng med filstien til en
#'   Parquet-fil med disse opplysningene.
#' @param variabel_kolonne En tekststreng med navnet på kolonnen i
#'   `kodelister` som inneholder variabelnavnene. Disse kobles mot
#'   kolonnenavnene i `data`. Standard er `"varnavn"`.
#' @param betydning_kolonne En tekststreng med navnet på kolonnen i
#'   `kodelister` som inneholder kolonneetikettene, for eksempel
#'   spørsmålsteksten. Standard er `"sporsmaal_text"`.
#' @param clean_labels Logisk verdi. Standard er `TRUE`.
#'   Med `TRUE` fjernes HTML- og formateringstagger fra kolonneetikettene,
#'   og mellomrom ryddes opp. Teksten mellom taggene beholdes.
#'
#' @details
#' Bare de to angitte kolonnene i `kodelister` kreves. Funksjonen kan
#' derfor også brukes for variabler uten svarkoder.
#'
#' Variabelnavnene i metadataene trimmes for innledende og avsluttende
#' mellomrom før kobling. Koblingen skiller mellom store og små bokstaver.
#' Variabler i metadataene som ikke finnes i `data`, ignoreres.
#' Kolonner uten treff eller med manglende eller tom etikett beholder
#' eventuell eksisterende kolonneetikett.
#'
#' Innledende og avsluttende mellomrom fjernes fra etikettene uavhengig
#' av `clean_labels`. Ved treff med en ikke-tom etikett erstattes en
#' eventuell eksisterende kolonneetikett.
#'
#' Med `clean_labels = TRUE` erstattes HTML- og formateringstagger som
#' `<br>`, `<gt>`, `<nt>` og `<rt>` med mellomrom. Gjentatte mellomrom,
#' tabulatorer og linjeskift samles til ett mellomrom. Mellomrom foran
#' tegnsetting fjernes, men mellomrom foran tre prikker beholdes.
#' Intervjuinstruksjoner og annen tekst mellom taggene beholdes.
#' Plassholdere som `^BedrNavn` og teksten `<.>` beholdes også.
#' HTML-entiteter som `&amp;` og `&nbsp;` dekodes ikke.
#'
#' Gjentatte, identiske etiketter for samme variabel er tillatt.
#' Ulike, ikke-tomme etiketter for samme variabel gir en feil.
#' Kontroll av duplikater skjer etter trimming og eventuell rensing.
#'
#' Funksjonen endrer bare kolonneetikettene. Kolonnenavn, verdier,
#' datatyper og verdietiketter beholdes. Ingen filer skrives.
#'
#' @return Datasettet med kolonneetiketter på variablene som har treff.
#'
#' @examples
#' data <- data.frame(nrinnled = c("1", "2"))
#' kodelister <- data.frame(
#'   varnavn = "nrinnled",
#'   sporsmaal_text = "Status på IO"
#' )
#'
#' resultat <- add_column_labels_from_file(data, kodelister)
#' labelled::var_label(resultat$nrinnled)
#'
#' # Fjern HTML fra kolonneetikettene:
#' kodelister$sporsmaal_text <- "Status<br><gt>på IO</gt>"
#' resultat <- add_column_labels_from_file(
#'   data,
#'   kodelister,
#'   clean_labels = TRUE
#' )
#' labelled::var_label(resultat$nrinnled) # "Status på IO"
#'
#' # Bruk andre kolonnenavn i metadataene:
#' kodelister <- data.frame(
#'   variabel = "nrinnled",
#'   variable_text = "Status på IO"
#' )
#' resultat <- add_column_labels_from_file(
#'   data,
#'   kodelister,
#'   variabel_kolonne = "variabel",
#'   betydning_kolonne = "variable_text"
#' )
#'
#' @export
add_column_labels_from_file <- function(
    data,
    kodelister,
    variabel_kolonne = "varnavn",
    betydning_kolonne = "sporsmaal_text",
    clean_labels = TRUE) {
  if (!is.logical(clean_labels) || length(clean_labels) != 1L ||
      is.na(clean_labels)) {
    stop("`clean_labels` må være TRUE eller FALSE.", call. = FALSE)
  }
  
  kolonneargumenter <- list(
    variabel_kolonne = variabel_kolonne,
    betydning_kolonne = betydning_kolonne
  )
  for (argument in names(kolonneargumenter)) {
    x <- kolonneargumenter[[argument]]
    if (!is.character(x) || length(x) != 1L || is.na(x) ||
        !nzchar(trimws(x))) {
      stop("`", argument, "` må være ett ikke-tomt kolonnenavn.",
           call. = FALSE)
    }
  }
  if (identical(variabel_kolonne, betydning_kolonne)) {
    stop("Variabel og betydning må angis med to ulike kolonnenavn.",
         call. = FALSE)
  }
  
  oppslag <- .les_etikettoppslag(
    data,
    kodelister,
    felt = c(variable_text = betydning_kolonne),
    variabel_kolonne = variabel_kolonne
  )
  oppslag$variable_text <- .datadoc_clean_labels(oppslag$variable_text, clean_labels)
  gyldig <- !is.na(oppslag$variable_text) & nzchar(oppslag$variable_text)
  oppslag <- unique(oppslag[gyldig, , drop = FALSE])
  
  duplikat <- anyDuplicated(oppslag$variabel)
  if (duplikat > 0L) {
    stop("Flere kolonneetiketter for variabelen `",
         oppslag$variabel[[duplikat]], "`.", call. = FALSE)
  }
  
  for (i in seq_len(nrow(oppslag))) {
    variabel <- oppslag$variabel[[i]]
    labelled::var_label(data[[variabel]]) <- oppslag$variable_text[[i]]
  }
  data
}

# Felles innlesing og utvalg av kolonner som finnes i datasettet.
.les_etikettoppslag <- function(
    data, kodelister, felt, variabel_kolonne = "variabel") {
  if (!is.data.frame(data)) {
    stop("`data` må være en data.frame eller tibble.", call. = FALSE)
  }
  
  if (is.character(kodelister) && length(kodelister) == 1L &&
      !is.na(kodelister)) {
    if (tolower(tools::file_ext(kodelister)) != "parquet") {
      stop("Oppgi en Parquet-fil eller les kodelisten inn som en tabell.",
           call. = FALSE)
    }
    kodelister <- arrow::read_parquet(kodelister)
  }
  
  if (!is.data.frame(kodelister)) {
    stop("`kodelister` må være en tabell eller en filsti.", call. = FALSE)
  }
  kolonner <- c(variabel_kolonne, unname(felt))
  mangler <- setdiff(kolonner, names(kodelister))
  if (length(mangler)) {
    stop("Kodelisten mangler ", paste(mangler, collapse = ", "), ".",
         call. = FALSE)
  }
  
  oppslag <- as.data.frame(kodelister)[kolonner]
  # Standardiser navn først etter utvalg, slik at kildekolonner ikke overskrives.
  names(oppslag) <- c("variabel", if (is.null(names(felt))) felt else names(felt))
  oppslag$variabel <- trimws(as.character(oppslag$variabel))
  oppslag[oppslag$variabel %in% names(data), , drop = FALSE]
}

# Interne hjelpefunksjoner -------------------------------------------------

.datadoc_flag <- function(x, argument) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", argument, "` må være TRUE eller FALSE.", call. = FALSE)
  }
  invisible(NULL)
}

.datadoc_text <- function(x, argument) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("`", argument, "` må være én ikke-tom tekststreng.", call. = FALSE)
  }
  invisible(NULL)
}

.datadoc_columns <- function(kolonner) {
  for (navn in names(kolonner)) .datadoc_text(kolonner[[navn]], navn)
  if (anyDuplicated(unlist(kolonner, use.names = FALSE))) {
    stop("Kolonneargumentene må angi ulike kolonnenavn.", call. = FALSE)
  }
  invisible(NULL)
}

.datadoc_first <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x)) x[[1L]] else NA_character_
}

.datadoc_date <- function(x) {
  if (inherits(x, "Date")) x <- as.character(x)
  .datadoc_text(x, "date")
  dato <- tryCatch(as.Date(x, format = "%Y-%m-%d"), error = function(e) NA)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x) || is.na(dato) ||
      format(dato, "%Y-%m-%d") != x) {
    stop("`date` må være en gyldig dato på formen YYYY-MM-DD.", call. = FALSE)
  }
  x
}

.datadoc_resolve_path <- function(filsti, argument = "filsti") {
  .datadoc_text(filsti, argument)
  datadoc_path(filsti, to = "json", warn_if_missing = FALSE)
}

.datadoc_read <- function(filsti) {
  filsti <- .datadoc_resolve_path(filsti)
  if (!file.exists(filsti)) stop("Finner ikke DataDoc-filen: ", filsti, call. = FALSE)
  signatur <- unname(tools::md5sum(filsti))
  if (is.na(signatur)) stop("Kunne ikke lese filsignatur: ", filsti, call. = FALSE)
  doc <- jsonlite::read_json(filsti, simplifyVector = FALSE)
  if (!is.list(doc$datadoc)) stop("Filen mangler objektet `datadoc`.", call. = FALSE)
  if (!identical(signatur, unname(tools::md5sum(filsti)))) {
    stop("DataDoc-filen ble endret under innlesing. Les filen på nytt.", call. = FALSE)
  }
  attr(doc, "fellesr_datadoc_source") <- list(
    path = normalizePath(filsti, mustWork = TRUE), signature = signatur
  )
  doc
}

# Skriv først en komplett, validerbar JSON-fil i samme mappe. Filen erstattes
# bare dersom innholdet ikke er endret siden innlesing. Dette er en kontroll
# av samtidige endringer, ikke en distribuert fillås for eksterne filsystemer.
.datadoc_write <- function(x, path, pretty = TRUE, auto_unbox = TRUE,
                           null = "null", digits = NA, expected_signature = NULL) {
  path <- .datadoc_resolve_path(path)
  source <- attr(x, "fellesr_datadoc_source", exact = TRUE)
  attr(x, "fellesr_datadoc_source") <- NULL
  if (is.null(expected_signature) && !is.null(source) &&
      identical(normalizePath(path, mustWork = FALSE), source$path)) {
    expected_signature <- source$signature
  }
  ny_fil <- tempfile(".datadoc-", tmpdir = dirname(path), fileext = ".json")
  on.exit(unlink(ny_fil), add = TRUE)
  jsonlite::write_json(x, ny_fil, pretty = pretty, auto_unbox = auto_unbox,
                       null = null, digits = digits)
  if (!jsonlite::validate(paste(readLines(ny_fil, warn = FALSE, encoding = "UTF-8"),
                                collapse = "\n"))) {
    stop("Den nye JSON-filen kunne ikke valideres; originalen er uendret.", call. = FALSE)
  }
  if (!is.null(expected_signature) &&
      (!file.exists(path) || !identical(expected_signature, unname(tools::md5sum(path))))) {
    stop("DataDoc-filen ble endret etter innlesing. Les filen på nytt.", call. = FALSE)
  }
  if (file.exists(path)) Sys.chmod(ny_fil, mode = file.info(path)$mode)
  if (!file.rename(ny_fil, path)) {
    stop("Kunne ikke erstatte DataDoc-filen: ", path,
         ". Originalfilen ble ikke slettet.", call. = FALSE)
  }
  invisible(path)
}

.datadoc_language_records <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  }
  if (is.list(x) && all(c("languageCode", "languageText") %in% names(x))) {
    codes <- unlist(x$languageCode, use.names = FALSE)
    texts <- unlist(x$languageText, use.names = FALSE)
    if (length(codes) == 1L && length(texts) <= 1L) return(list(x))
    if (length(codes) != length(texts)) stop("Ugyldige språkposter.", call. = FALSE)
    return(lapply(seq_along(codes), function(i) {
      list(languageCode = codes[[i]], languageText = texts[[i]])
    }))
  }
  if (is.list(x) && all(vapply(x, is.list, logical(1)))) return(x)
  stop("Uventet format på flerspråklig DataDoc-felt.", call. = FALSE)
}

.datadoc_language_text <- function(x, language = "nb", fallback = TRUE) {
  if (is.character(x)) return(.datadoc_first(x))
  records <- .datadoc_language_records(x)
  codes <- vapply(records, function(y) .datadoc_first(y$languageCode), character(1))
  texts <- vapply(records, function(y) .datadoc_first(y$languageText), character(1))
  treff <- which(!is.na(codes) & codes == language & !is.na(texts))
  if (length(treff)) return(texts[[treff[[1L]]]])
  if (fallback) .datadoc_first(texts) else NA_character_
}

.datadoc_set_language <- function(felt, verdi, language = "nb", overwrite = FALSE) {
  if (is.null(verdi)) return(felt)
  .datadoc_text(verdi, "verdi")
  language <- match.arg(language, c("nb", "nn", "en"))
  .datadoc_flag(overwrite, "overwrite")
  records <- .datadoc_language_records(felt)
  codes <- vapply(records, function(x) .datadoc_first(x$languageCode), character(1))
  i <- which(!is.na(codes) & codes == language)
  if (length(i) > 1L) stop("Flere språkposter for ", language, ".", call. = FALSE)
  if (!length(i)) {
    records[[length(records) + 1L]] <- list(languageCode = language, languageText = verdi)
  } else if (overwrite || .datadoc_is_empty(records[[i]]$languageText)) {
    records[[i]]$languageText <- verdi
  }
  records
}

.datadoc_is_empty <- function(x) {
  if (is.null(x) || !length(x)) return(TRUE)
  if (is.list(x)) {
    if ("languageText" %in% names(x)) return(.datadoc_is_empty(x$languageText))
    return(all(vapply(x, .datadoc_is_empty, logical(1))))
  }
  if (is.character(x)) return(all(is.na(x) | !nzchar(trimws(x))))
  all(is.na(x))
}

.datadoc_uri_id <- function(uri) {
  uri <- .datadoc_first(uri)
  if (is.na(uri)) return(NA_character_)
  uri <- sub("[?#].*$", "", uri)
  sub("^.*[:/]", "", sub("/+$", "", uri))
}

.datadoc_klass_id <- function(uri) {
  uri <- .datadoc_first(uri)
  if (is.na(uri)) return(NA_character_)
  uri <- sub("[?#].*$", "", uri)
  # En variantsti er ikke samme ID som klassifikasjonen som står foran.
  # Variantoppslag krever egne KLASS-endepunkter og støttes ikke her.
  if (grepl("/(varianter|variants)/", uri)) {
    warning("KLASS-variant krever eget oppslag og hoppes over: ", uri, call. = FALSE)
    return(NA_character_)
  }
  id <- .datadoc_uri_id(uri)
  if (!is.na(id) && grepl("^[0-9]+$", id)) id else NA_character_
}

.datadoc_copy_variable <- function(egen, original) {
  # Bare kjente dokumentasjonsfelt kopieres. ID, datatype, dataelementsti,
  # pseudonymiseringsmetadata og eventuelle ukjente tekniske felt beholdes.
  felter <- c("name", "definition_uri", "is_personal_data", "unit_type",
              "measurement_unit", "multiplication_factor",
              "variable_role", "classification_uri", "comment",
              "data_source", "temporality_type", "population_description",
              "format", "contains_data_from", "contains_data_until",
              "invalid_value_description")
  for (felt in intersect(felter, names(original))) egen[felt] <- original[felt]
  egen
}

.datadoc_clean_labels <- function(tekst, clean_labels = TRUE) {
  .datadoc_flag(clean_labels, "clean_labels")
  tekst <- as.character(tekst)
  if (clean_labels) {
    tekst <- gsub("<\\s*/?\\s*[A-Za-z][^>]*>", " ", tekst, perl = TRUE)
    tekst <- gsub("[[:space:]\u00a0]+", " ", tekst, perl = TRUE)
    tekst <- gsub(" +([,;:!?]|\\.(?!\\.))", "\\1", tekst, perl = TRUE)
  }
  trimws(tekst)
}

.datadoc_raw_values <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- unclass(x)
    attributes(x) <- NULL
  }
  x
}

.datadoc_unlabelled_values <- function(x) {
  raw <- .datadoc_raw_values(x)
  observed <- unique(raw[!is.na(raw)])
  labs <- labelled::val_labels(x)
  # Etiketter med tom tekst regnes også som manglende.
  valid <- !is.na(names(labs)) & nzchar(trimws(names(labs)))
  observed[!observed %in% unname(labs[valid])]
}

.datadoc_warn_unlabelled <- function(x, variabel) {
  missing <- .datadoc_unlabelled_values(x)
  if (length(missing)) {
    vist <- utils::head(as.character(missing), 20L)
    warning("Variabelen `", variabel, "` har ", length(missing),
            " observerte koder uten verdietikett: ", paste(vist, collapse = ", "),
            if (length(missing) > 20L) ", ..." else "",
            ". Verdiene beholdes.", call. = FALSE)
  }
  invisible(NULL)
}

.datadoc_set_value_labels <- function(x, kode, betydning, variabel, clean_labels = TRUE) {
  kode <- trimws(as.character(kode))
  betydning <- .datadoc_clean_labels(betydning, clean_labels)
  valid <- !is.na(kode) & nzchar(kode) & !is.na(betydning) & nzchar(betydning)
  kode <- kode[valid]
  betydning <- betydning[valid]
  if (!length(kode)) {
    .datadoc_warn_unlabelled(x, variabel)
    return(x)
  }
  raw <- .datadoc_raw_values(x)
  if (is.object(raw) || !(typeof(raw) %in% c("integer", "double", "character"))) {
    stop("Variabelen `", variabel, "` har en datatype som ikke støttes: ",
         paste(class(x), collapse = "/"), ". Bruk numeriske eller tekstbaserte koder.", call. = FALSE)
  }
  if (typeof(raw) != "character") {
    numeric_codes <- suppressWarnings(as.numeric(kode))
    invalid <- is.na(numeric_codes) | !is.finite(numeric_codes)
    # Avvis grenseområdet ved 2^53 der ulike heltall kan avrundes til samme double.
    invalid <- invalid | abs(numeric_codes) >= 2^53
    if (typeof(raw) == "integer") {
      invalid <- invalid | numeric_codes != trunc(numeric_codes) |
        numeric_codes > .Machine$integer.max | numeric_codes < -.Machine$integer.max
    }
    invalid[is.na(invalid)] <- TRUE
    if (any(invalid)) {
      stop("Koder kan ikke tilpasses datatypen til `", variabel, "`: ",
           paste(unique(kode[invalid]), collapse = ", "), call. = FALSE)
    }
    kode <- if (typeof(raw) == "integer") as.integer(numeric_codes) else numeric_codes
  }
  rader <- unique(data.frame(kode = kode, betydning = betydning, stringsAsFactors = FALSE))
  duplikat <- anyDuplicated(rader$kode)
  if (duplikat) {
    stop("Flere betydninger for kode `", rader$kode[[duplikat]],
         "` i variabelen `", variabel, "`.", call. = FALSE)
  }
  kolonneetikett <- labelled::var_label(x)
  x <- labelled::set_value_labels(x, .labels = stats::setNames(rader$kode, rader$betydning))
  labelled::var_label(x) <- kolonneetikett
  .datadoc_warn_unlabelled(x, variabel)
  x
}

.datadoc_restore_groups <- function(data, original) {
  if (inherits(original, "rowwise_df")) {
    return(dplyr::rowwise(dplyr::ungroup(data),
                          tidyselect::all_of(dplyr::group_vars(original))))
  }
  if (inherits(original, "grouped_df")) {
    return(dplyr::group_by(dplyr::ungroup(data),
                           dplyr::across(tidyselect::all_of(dplyr::group_vars(original))),
                           .drop = dplyr::group_by_drop_default(original)))
  }
  data
}

# Tomme etiketttekster skal ikke skjule en observert kode ved faktorvisning.
.datadoc_as_factor <- function(x) {
  labs <- labelled::val_labels(x)
  valid <- !is.na(names(labs)) & nzchar(trimws(names(labs)))
  if (length(labs) && any(!valid)) {
    attr(x, "labels") <- labs[valid]
  }
  haven::as_factor(x, levels = "default")
}



#' Hent Access Token for intern Vardef
#'
#' @return Et Access Token som tekststreng.
#' @keywords internal

.vardef_token <- function() {
  
  reticulate::py_require("dapla-toolbelt-metadata")
  
  auth <- reticulate::import(
    "dapla_auth_client",
    convert = FALSE
  )
  
  token <- reticulate::py_to_r(
    auth$AuthClient$fetch_personal_token(
      scopes = list("all_groups", "current_group"),
      audiences = list("vardef")
    )
  )
  
  if (!is.character(token) ||
      length(token) != 1L ||
      is.na(token) ||
      !nzchar(token)) {
    stop("Kunne ikke hente Access Token for Vardef.", call. = FALSE)
  }
  
  token
}

.vardef_language_text <- function(x, language = "nb") {
  
  language <- match.arg(language, c("nb", "nn", "en"))
  
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  
  # Vardef-format: list(nb = "...", nn = NULL, en = "...")
  if (
    is.list(x) &&
    !is.null(names(x)) &&
    all(names(x) %in% c("nb", "nn", "en"))
  ) {
    
    hent_tekst <- function(verdi) {
      if (
        is.character(verdi) &&
        length(verdi) == 1L &&
        !is.na(verdi) &&
        nzchar(trimws(verdi))
      ) {
        return(trimws(verdi))
      }
      
      NA_character_
    }
    
    # Forsøk først ønsket språk
    tekst <- hent_tekst(x[[language]])
    
    if (!is.na(tekst)) {
      return(tekst)
    }
    
    # Bruk første tilgjengelige språk som reserve
    for (sprak in c("nb", "nn", "en")) {
      tekst <- hent_tekst(x[[sprak]])
      
      if (!is.na(tekst)) {
        return(tekst)
      }
    }
    
    return(NA_character_)
  }
  
  # Støtt også eksisterende Datadoc-format
  .datadoc_language_text(x, language)
}



#' Kontroller om variabler finnes i Vardef
#'
#' Kontrollerer om variabler finnes i ekstern eller intern Vardef,
#' uavhengig av gyldighetsdato og publiseringsstatus.
#'
#' @param variabler Karaktervektor med variabelnavn.
#' @param date Valgfri gyldighetsdato. NULL (standard) inkluderer
#'   alle definisjoner, også historiske.
#' @param language Språkkode: "nb", "nn" eller "en".
#' @param status "auto" (standard), "ekstern" eller "intern".
#' @param quiet Skjul statusmeldinger. Standard er TRUE.
#'
#' @return En tibble med én rad per variabel og informasjon
#'   om treff i Vardef.
#'
#' @export
check_variables_in_vardef <- function(
    variabler,
    date = NULL,
    language = "nb",
    status = "auto",
    quiet = TRUE
) {
  
  language <- match.arg(language, c("nb", "nn", "en"))
  status <- match.arg(status, c("auto", "ekstern", "intern"))
  .datadoc_flag(quiet, "quiet")
  
  if (!is.character(variabler) ||
      anyNA(variabler) ||
      any(!nzchar(trimws(variabler)))) {
    stop(
      "`variabler` må være en karaktervektor uten NA eller tomme navn.",
      call. = FALSE
    )
  }
  
  variabler <- unique(trimws(variabler))
  
  if (!quiet) {
    message(
      "Kontrollerer ",
      length(variabler),
      " variabler mot Vardef."
    )
  }
  
  # ------------------------------------------------------------
  # 1. Bestem hvilke tjenester som skal brukes
  # ------------------------------------------------------------
  
  tjenester <- if (status == "auto") {
    c("ekstern", "intern")
  } else {
    status
  }
  
  # ------------------------------------------------------------
  # 2. Hent definisjoner fra begge tjenester
  # ------------------------------------------------------------
  
  alle_definisjoner <- lapply(tjenester, function(tjeneste) {
    
    definisjoner <- get_all_variable_definitions(
      date = date,
      language = language,
      status = tjeneste
    )
    
    if (is.null(definisjoner)) {
      return(tibble::tibble())
    }
    
    if (!is.data.frame(definisjoner)) {
      stop(
        "Uventet responsformat fra ",
        tjeneste,
        " Vardef.",
        call. = FALSE
      )
    }
    
    if (!"short_name" %in% names(definisjoner)) {
      stop(
        "Responsen fra ",
        tjeneste,
        " Vardef mangler `short_name`.",
        call. = FALSE
      )
    }
    
    # Behold bare relevante variabler
    definisjoner <- definisjoner |>
      dplyr::filter(
        .data$short_name %in% variabler
      )
    
    if (nrow(definisjoner) == 0L) {
      return(tibble::tibble())
    }
    
    # Felles kolonner på tvers av API-ene
    hent_kolonne <- function(navn) {
      if (navn %in% names(definisjoner)) {
        as.character(definisjoner[[navn]])
      } else {
        rep(NA_character_, nrow(definisjoner))
      }
    }
    
    # Navn fra valgt språk, med fallback til bokmål
    navn <- hent_kolonne(paste0("name.", language))
    navn_nb <- hent_kolonne("name.nb")
    
    mangler_navn <- is.na(navn) | !nzchar(navn)
    navn[mangler_navn] <- navn_nb[mangler_navn]
    
    # KLASS-ID: internt brukes classification_reference,
    # eksternt kan classification_uri brukes.
    klass_id <- hent_kolonne("classification_reference")
    klass_uri <- hent_kolonne("classification_uri")
    
    mangler_klass <- is.na(klass_id) | !nzchar(klass_id)
    
    klass_id[mangler_klass] <- vapply(
      klass_uri[mangler_klass],
      .datadoc_klass_id,
      character(1)
    )
    
    tibble::tibble(
      variabel = hent_kolonne("short_name"),
      vardef_id = hent_kolonne("id"),
      vardef_navn = navn,
      vardef_klass_id = klass_id,
      variable_status = hent_kolonne("variable_status"),
      valid_from = hent_kolonne("valid_from"),
      valid_until = hent_kolonne("valid_until"),
      kilde = tjeneste
    )
  })
  
  definisjoner <- dplyr::bind_rows(alle_definisjoner)
  
  # ------------------------------------------------------------
  # 3. Velg definisjon for hver variabel
  # ------------------------------------------------------------
  
  if (nrow(definisjoner) > 0L) {
    
    definisjoner <- definisjoner |>
      dplyr::mutate(
        .valid_from = as.Date(valid_from),
        .oppdatert = dplyr::coalesce(
          .valid_from,
          as.Date("1900-01-01")
        )
      ) |>
      dplyr::group_by(variabel) |>
      dplyr::arrange(
        dplyr::desc(.oppdatert),
        kilde,
        .by_group = TRUE
      ) |>
      dplyr::mutate(
        antall_definisjoner = dplyr::n(),
        finnes_ekstern = any(kilde == "ekstern"),
        finnes_intern = any(kilde == "intern")
      ) |>
      dplyr::slice(1L) |>
      dplyr::ungroup() |>
      dplyr::select(
        -.valid_from,
        -.oppdatert
      )
  }
  
  # ------------------------------------------------------------
  # 4. Koble resultatet mot variabelnavnene
  # ------------------------------------------------------------
  
  resultat <- tibble::tibble(
    variabel = variabler
  ) |>
    dplyr::left_join(
      definisjoner,
      by = "variabel"
    ) |>
    dplyr::mutate(
      finnes_i_vardef = !is.na(vardef_id),
      finnes_ekstern = dplyr::coalesce(
        finnes_ekstern, FALSE
      ),
      finnes_intern = dplyr::coalesce(
        finnes_intern, FALSE
      ),
      antall_definisjoner = dplyr::coalesce(
        antall_definisjoner, 0L
      )
    ) |>
    dplyr::select(
      variabel,
      finnes_i_vardef,
      finnes_ekstern,
      finnes_intern,
      vardef_id,
      vardef_navn,
      vardef_klass_id,
      variable_status,
      valid_from,
      valid_until,
      kilde,
      antall_definisjoner
    )
  
  # ------------------------------------------------------------
  # 5. Oppsummering
  # ------------------------------------------------------------
  
  if (!quiet) {
    message(
      "Fant ",
      sum(resultat$finnes_i_vardef),
      " av ",
      nrow(resultat),
      " variabler i Vardef."
    )
  }
  
  resultat
}



#' Kopier metadata fra Vardef til DataDoc
#'
#' Kopierer navn, Vardef-ID og første enhetstype ved treff på
#' kortnavn. KLASS-ID og øvrige dokumentasjonsfelt endres ikke.
#'
#' @param filsti_datadoc Filsti til DataDoc JSON eller tilhørende
#'   Parquet-fil.
#' @param variabler Valgfri tekstvektor med kortnavn. NULL velger alle.
#'   En navngitt vektor kan brukes til mapping mellom DataDoc og Vardef,
#'   f.eks. c(komm_nr_2019 = "komm_nr").
#' @param overwrite Med FALSE (standard) hoppes hele variabelen over
#'   dersom name, definition_uri eller unit_type allerede har innhold.
#'   TRUE erstatter disse feltene når Vardef har en verdi.
#'   Manglende kildeverdier sletter ikke eksisterende innhold.
#' @param date Valgfri gyldighetsdato for oppslag i Vardef,
#'   som Date eller YYYY-MM-DD. NULL (standard) inkluderer
#'   alle gyldighetsperioder.
#' @param dato_unntak NULL eller en data frame med egne
#'   dataperioder for enkeltvariabler. Må inneholde
#'   short_name, contains_data_from og contains_data_until.
#'   Kortnavnene viser til DataDoc, ikke Vardef.
#'   Datoene brukes bare til å oppdatere DataDoc og påvirker
#'   ikke oppslag eller filtrering i Vardef.
#'   Datoene oppdateres bare for variabler som får treff i
#'   Vardef og som ikke hoppes over.
#' @param language Språk for navnet: nb, nn eller en.
#'   Standard nb.
#' @param status auto (standard) søker først eksternt og
#'   deretter internt for kortnavn uten eksternt treff.
#' @param quiet Skjul statusmeldinger. Standard TRUE.
#'   Advarsler og feil vises fortsatt.
#'
#' @details
#' Første ikke-tomme enhetstype fra Vardef brukes.
#' Navn kopieres bare på valgt språk, uten fallback.
#' Publiseringsstatus filtreres ikke.
#' Flere motstridende definisjoner gir advarsel og hoppes over.
#'
#' @return Usynlig tibble med variabel, vardef_kortnavn,
#'   resultat og endrede_felt.
#'
#' @export
copy_vardef_to_datadoc <- function(
    filsti_datadoc,
    variabler = NULL,
    overwrite = FALSE,
    date = NULL,
    dato_unntak = NULL,
    language = "nb",
    status = "auto",
    quiet = TRUE
) {
  
  .datadoc_flag(overwrite, "overwrite")
  .datadoc_flag(quiet, "quiet")
  
  language <- match.arg(language, c("nb", "nn", "en"))
  status <- match.arg(status, c("auto", "ekstern", "intern"))
  
  # ------------------------------------------------------------
  # 1. Kontroller gyldighetsdato for Vardef
  # ------------------------------------------------------------
  
  if (!is.null(date)) {
    date <- as.Date(.datadoc_date(date))
    
    if (length(date) != 1L || is.na(date)) {
      stop(
        "`date` må være én gyldig dato.",
        call. = FALSE
      )
    }
  }
  
  # ------------------------------------------------------------
  # 2. Kontroller dato_unntak
  # ------------------------------------------------------------
  
  if (!is.null(dato_unntak)) {
    
    if (!is.data.frame(dato_unntak)) {
      stop(
        "`dato_unntak` må være en data frame.",
        call. = FALSE
      )
    }
    
    nodvendige <- c(
      "short_name",
      "contains_data_from",
      "contains_data_until"
    )
    
    mangler <- setdiff(
      nodvendige,
      names(dato_unntak)
    )
    
    if (length(mangler)) {
      stop(
        "`dato_unntak` mangler kolonnene: ",
        paste(mangler, collapse = ", "),
        call. = FALSE
      )
    }
    
    dato_unntak <- dato_unntak[nodvendige]
    
    dato_unntak[] <- lapply(
      dato_unntak,
      as.character
    )
    
    dato_unntak$short_name <- trimws(
      dato_unntak$short_name
    )
    
    if (
      anyNA(dato_unntak$short_name) ||
      any(!nzchar(dato_unntak$short_name)) ||
      anyDuplicated(dato_unntak$short_name)
    ) {
      stop(
        "`short_name` i `dato_unntak` må være ",
        "unikt og ikke-tomt.",
        call. = FALSE
      )
    }
    
    for (felt in c(
      "contains_data_from",
      "contains_data_until"
    )) {
      
      verdi <- dato_unntak[[felt]]
      
      if (
        anyNA(verdi) ||
        any(!grepl(
          "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
          verdi
        ))
      ) {
        stop(
          "Ugyldig datoformat i `",
          felt,
          "`. Bruk YYYY-MM-DD.",
          call. = FALSE
        )
      }
      
      dato <- as.Date(
        verdi,
        format = "%Y-%m-%d"
      )
      
      if (
        anyNA(dato) ||
        any(as.character(dato) != verdi)
      ) {
        stop(
          "Ugyldige datoer i `",
          felt,
          "`.",
          call. = FALSE
        )
      }
    }
    
    if (any(
      as.Date(dato_unntak$contains_data_from) >
      as.Date(dato_unntak$contains_data_until)
    )) {
      stop(
        "`contains_data_from` kan ikke være senere ",
        "enn `contains_data_until`.",
        call. = FALSE
      )
    }
  }
  
  # ------------------------------------------------------------
  # 3. Les DataDoc
  # ------------------------------------------------------------
  
  path <- .datadoc_resolve_path(filsti_datadoc)
  
  doc <- .datadoc_read(path)
  
  vars <- doc$datadoc$variables
  
  navn <- vapply(
    vars,
    function(x) .datadoc_first(x$short_name),
    character(1)
  )
  
  if (
    anyNA(navn) ||
    any(!nzchar(trimws(navn))) ||
    anyDuplicated(navn)
  ) {
    stop(
      "DataDoc må ha unike, ikke-tomme kortnavn.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 4. Opprett kobling mellom DataDoc og Vardef
  # ------------------------------------------------------------
  
  if (is.null(variabler)) {
    
    kobling <- stats::setNames(
      navn,
      navn
    )
    
  } else {
    
    if (
      !is.character(variabler) ||
      anyNA(variabler) ||
      any(!nzchar(trimws(variabler)))
    ) {
      stop(
        "`variabler` må være en tekstvektor ",
        "uten NA eller tomme verdier.",
        call. = FALSE
      )
    }
    
    vardef_navn <- trimws(
      unname(variabler)
    )
    
    datadoc_navn <- names(variabler)
    
    if (is.null(datadoc_navn)) {
      
      datadoc_navn <- vardef_navn
      
    } else {
      
      if (
        anyNA(datadoc_navn) ||
        any(!nzchar(trimws(datadoc_navn)))
      ) {
        stop(
          "Ved mapping må alle elementene ",
          "ha et DataDoc-navn.",
          call. = FALSE
        )
      }
      
      datadoc_navn <- trimws(datadoc_navn)
    }
    
    if (anyDuplicated(datadoc_navn)) {
      stop(
        "Hver DataDoc-variabel kan bare angis ",
        "én gang i `variabler`.",
        call. = FALSE
      )
    }
    
    mangler <- setdiff(
      datadoc_navn,
      navn
    )
    
    if (length(mangler)) {
      stop(
        "Variabler mangler i DataDoc: ",
        paste(mangler, collapse = ", "),
        call. = FALSE
      )
    }
    
    kobling <- stats::setNames(
      vardef_navn,
      datadoc_navn
    )
  }
  
  indeks <- match(
    names(kobling),
    navn
  )
  
  # ------------------------------------------------------------
  # 5. Kontroller at dato_unntak gjelder valgte variabler
  # ------------------------------------------------------------
  
  if (!is.null(dato_unntak)) {
    
    ukjente <- setdiff(
      dato_unntak$short_name,
      names(kobling)
    )
    
    if (length(ukjente)) {
      stop(
        "Dato er angitt for variabler som ikke er valgt: ",
        paste(ukjente, collapse = ", "),
        call. = FALSE
      )
    }
  }
  
  # ------------------------------------------------------------
  # 6. Opprett resultatoversikt
  # ------------------------------------------------------------
  
  resultat <- tibble::tibble(
    variabel = navn[indeks],
    vardef_kortnavn = unname(kobling),
    resultat = rep(
      "ingen_treff",
      length(indeks)
    ),
    endrede_felt = rep(
      "",
      length(indeks)
    )
  )
  
  if (!length(indeks)) {
    
    if (!quiet) {
      message("Ingen variabler å behandle.")
    }
    
    return(invisible(resultat))
  }
  
  # ------------------------------------------------------------
  # 7. Kontroller eksisterende metadata
  # ------------------------------------------------------------
  
  dokumentasjonsfelt <- c(
    "name",
    "definition_uri",
    "unit_type"
  )
  
  har_innhold <- vapply(
    indeks,
    function(i) {
      
      any(vapply(
        dokumentasjonsfelt,
        function(felt) {
          !.datadoc_is_empty(vars[[i]][[felt]])
        },
        logical(1)
      ))
    },
    logical(1)
  )
  
  beskyttet <- !overwrite & har_innhold
  
  resultat$resultat[beskyttet] <- "beholdt"
  
  # ------------------------------------------------------------
  # 8. Hjelpefunksjoner for Vardef-respons
  # ------------------------------------------------------------
  
  rad <- function(tab, i) {
    
    lapply(tab, function(x) {
      
      if (is.data.frame(x)) {
        
        rad(x, i)
        
      } else if (is.list(x)) {
        
        x[[i]]
        
      } else {
        
        x[i]
      }
    })
  }
  
  tekst <- function(x) {
    
    x <- unlist(
      x,
      use.names = FALSE
    )
    
    if (!length(x)) {
      return(NA_character_)
    }
    
    if (length(x) != 1L) {
      stop(
        "Forventet én verdi i Vardef-feltet.",
        call. = FALSE
      )
    }
    
    .datadoc_first(x)
  }
  
  navn_paa_sprak <- function(x) {
    
    flat <- paste0(
      "name.",
      language
    )
    
    if (flat %in% names(x)) {
      return(tekst(x[[flat]]))
    }
    
    if (
      is.list(x$name) &&
      any(c("nb", "nn", "en") %in% names(x$name))
    ) {
      return(tekst(x$name[[language]]))
    }
    
    .datadoc_language_text(
      x$name,
      language,
      fallback = FALSE
    )
  }
  
  # Kontroller om Vardef-definisjonen er gyldig på date.
  gyldig_paa_dato <- function(post) {
    
    fra <- tekst(post$valid_from)
    til <- tekst(post$valid_until)
    
    fra <- if (is.na(fra) || !nzchar(fra)) {
      as.Date(NA)
    } else {
      as.Date(.datadoc_date(fra))
    }
    
    til <- if (is.na(til) || !nzchar(til)) {
      as.Date(NA)
    } else {
      as.Date(.datadoc_date(til))
    }
    
    if (
      !is.na(fra) &&
      !is.na(til) &&
      fra > til
    ) {
      stop(
        "Ugyldig gyldighetsperiode i Vardef: ",
        tekst(post$short_name),
        call. = FALSE
      )
    }
    
    # NULL inkluderer alle gyldighetsperioder.
    if (is.null(date)) {
      return(TRUE)
    }
    
    (is.na(fra) || date >= fra) &&
      (is.na(til) || date <= til)
  }
  
  # ------------------------------------------------------------
  # 9. Hent definisjoner fra Vardef
  # ------------------------------------------------------------
  
  poster <- vector(
    "list",
    length(indeks)
  )
  
  tjenester <- if (status == "auto") {
    c("ekstern", "intern")
  } else {
    status
  }
  
  gjenstaar <- which(!beskyttet)
  
  for (tjeneste in tjenester) {
    
    if (!length(gjenstaar)) {
      break
    }
    
    maal <- unique(
      resultat$vardef_kortnavn[gjenstaar]
    )
    
    if (!quiet) {
      message(
        "Søker i ",
        tjeneste,
        " Vardef etter ",
        length(maal),
        " kortnavn."
      )
    }
    
    hent <- function() {
      get_all_variable_definitions(
        date = NULL,
        language = language,
        status = tjeneste
      )
    }
    
    tab <- if (quiet) {
      suppressMessages(hent())
    } else {
      hent()
    }
    
    if (is.null(tab)) {
      next
    }
    
    if (!is.data.frame(tab)) {
      stop(
        "Uventet respons fra ",
        tjeneste,
        " Vardef.",
        call. = FALSE
      )
    }
    
    if (!nrow(tab)) {
      next
    }
    
    if (!"short_name" %in% names(tab)) {
      stop(
        "Responsen fra ",
        tjeneste,
        " Vardef mangler `short_name`.",
        call. = FALSE
      )
    }
    
    funnet <- integer()
    
    for (i in which(
      tab$short_name %in% maal
    )) {
      
      post <- rad(tab, i)
      
      if (!gyldig_paa_dato(post)) {
        next
      }
      
      kortnavn <- tekst(
        post$short_name
      )
      
      aktuelle <- gjenstaar[
        resultat$vardef_kortnavn[gjenstaar] ==
          kortnavn
      ]
      
      for (j in aktuelle) {
        
        poster[[j]][[
          length(poster[[j]]) + 1L
        ]] <- post
        
        funnet <- c(
          funnet,
          j
        )
      }
    }
    
    # Søk bare internt for variabler uten eksternt treff.
    gjenstaar <- setdiff(
      gjenstaar,
      unique(funnet)
    )
  }
  
  # ------------------------------------------------------------
  # 10. Kopier metadata til DataDoc
  # ------------------------------------------------------------
  
  problemer <- character()
  
  for (j in which(!beskyttet)) {
    
    i <- indeks[[j]]
    
    treff <- poster[[j]]
    
    if (!length(treff)) {
      next
    }
    
    forslag <- lapply(treff, function(x) {
      
      units <- x$unit_types
      
      if (.datadoc_is_empty(units)) {
        units <- x$unit_type
      }
      
      units <- trimws(as.character(
        unlist(units, use.names = FALSE)
      ))
      
      units <- unique(
        units[!is.na(units) & nzchar(units)]
      )
      
      list(
        name = navn_paa_sprak(x),
        definition_uri = tekst(x$id),
        unit_type = if (length(units)) {
          units[[1L]]
        } else {
          NA_character_
        }
      )
    })
    
    forslag <- unique(forslag)
    
    problem <- if (length(forslag) > 1L) {
      
      "flere_treff"
      
    } else if (
      is.na(forslag[[1L]]$definition_uri)
    ) {
      
      "mangler_vardef_id"
      
    } else {
      
      ""
    }
    
    if (nzchar(problem)) {
      
      resultat$resultat[[j]] <- problem
      
      problemer <- c(
        problemer,
        paste0(
          navn[[i]],
          " (",
          problem,
          ")"
        )
      )
      
      next
    }
    
    kilde <- forslag[[1L]]
    
    ny <- vars[[i]]
    
    endret <- character()
    
    # ----------------------------------------------------------
    # 11. Oppdater metadata fra Vardef
    # ----------------------------------------------------------
    
    for (felt in dokumentasjonsfelt) {
      
      verdi <- kilde[[felt]]
      
      # Manglende kildeverdi sletter ikke eksisterende verdi.
      if (is.na(verdi)) {
        next
      }
      
      gammel <- if (felt == "name") {
        
        .datadoc_language_text(
          ny$name,
          language,
          fallback = FALSE
        )
        
      } else {
        
        tekst(ny[[felt]])
      }
      
      if (identical(gammel, verdi)) {
        next
      }
      
      ny[[felt]] <- if (felt == "name") {
        
        .datadoc_set_language(
          ny$name,
          verdi,
          language,
          overwrite = TRUE
        )
        
      } else {
        
        verdi
      }
      
      endret <- c(
        endret,
        felt
      )
    }
    
    # ----------------------------------------------------------
    # 12. Oppdater datoperiode fra dato_unntak
    # ----------------------------------------------------------
    
    # Datoene brukes bare til DataDoc.
    # De påvirker ikke Vardef-oppslaget.
    
    if (!is.null(dato_unntak)) {
      
      indeks_unntak <- match(
        navn[[i]],
        dato_unntak$short_name
      )
      
      if (!is.na(indeks_unntak)) {
        
        for (felt in c(
          "contains_data_from",
          "contains_data_until"
        )) {
          
          ny_verdi <- dato_unntak[[felt]][[
            indeks_unntak
          ]]
          
          gammel_verdi <- tekst(
            ny[[felt]]
          )
          
          if (identical(gammel_verdi, ny_verdi)) {
            next
          }
          
          ny[[felt]] <- ny_verdi
          
          endret <- c(
            endret,
            felt
          )
        }
      }
    }
    
    # ----------------------------------------------------------
    # 13. Registrer endringer
    # ----------------------------------------------------------
    
    if (length(endret)) {
      
      doc$datadoc$variables[[i]] <- ny
      
      resultat$resultat[[j]] <- "oppdatert"
      
      resultat$endrede_felt[[j]] <- paste(
        endret,
        collapse = ", "
      )
      
    } else {
      
      resultat$resultat[[j]] <- "uendret"
    }
  }
  
  # ------------------------------------------------------------
  # 14. Advar om problemer
  # ------------------------------------------------------------
  
  if (length(problemer)) {
    
    warning(
      "Hoppet over: ",
      paste(problemer, collapse = "; "),
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 15. Lagre DataDoc dersom noe er endret
  # ------------------------------------------------------------
  
  antall <- sum(
    resultat$resultat == "oppdatert"
  )
  
  if (antall > 0L) {
    .datadoc_write(
      doc,
      path
    )
  }
  
  # ------------------------------------------------------------
  # 16. Oppsummer
  # ------------------------------------------------------------
  
  if (!quiet) {
    
    message(
      "Oppdaterte ",
      antall,
      " av ",
      nrow(resultat),
      " valgte variabler. ",
      sum(resultat$resultat == "beholdt"),
      " ble beholdt på grunn av eksisterende innhold; ",
      sum(resultat$resultat == "ingen_treff"),
      " manglet treff."
    )
  }
  
  invisible(resultat)
}
