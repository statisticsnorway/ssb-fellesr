
# Datadoc-funksjoner


#' Konverter filsti mellom Parquet og Datadoc
#'
#' Konverterer en filsti mellom en Parquet-fil og den tilhørende
#' Datadoc JSON-filen.
#'
#' Standardoppførselen er å konvertere fra `.parquet` til `__DOC.json`.
#' Ved å sette `to = "parquet"` konverteres en Datadoc-fil tilbake til
#' den tilhørende Parquet-filstien.
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
datadoc_path <- function(
    filsti,
    to = c("json", "parquet"),
    warn_if_missing = TRUE
) {

  to <- match.arg(to)

  if (
    !is.logical(warn_if_missing) ||
    length(warn_if_missing) != 1L ||
    is.na(warn_if_missing)
  ) {
    stop(
      "`warn_if_missing` må være enten TRUE eller FALSE.",
      call. = FALSE
    )
  }

  if (to == "json") {

    if (any(!grepl("\\.parquet$", filsti, ignore.case = TRUE))) {
      stop(
        "`filsti` må slutte på `.parquet` når `to = \"json\"`.",
        call. = FALSE
      )
    }

    output <- sub(
      pattern = "\\.parquet$",
      replacement = "__DOC.json",
      x = filsti,
      ignore.case = TRUE
    )

  } else {

    if (any(!grepl("__DOC\\.json$", filsti, ignore.case = TRUE))) {
      stop(
        "`filsti` må slutte på `__DOC.json` når `to = \"parquet\"`.",
        call. = FALSE
      )
    }

    output <- sub(
      pattern = "__DOC\\.json$",
      replacement = ".parquet",
      x = filsti,
      ignore.case = TRUE
    )
  }

  if (warn_if_missing) {

    missing_files <- output[!file.exists(output)]

    if (length(missing_files) > 0L) {
      warning(
        "Følgende fil",
        if (length(missing_files) > 1L) "er" else "",
        " finnes ikke:\n",
        paste0("- ", missing_files, collapse = "\n"),
        call. = FALSE
      )
    }
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
    status = "ekstern",
    language = "nb",
    token = NULL
) {

  status <- match.arg(
    status,
    choices = c("ekstern", "intern")
  )

  language <- match.arg(
    language,
    choices = c("nb", "nn", "en")
  )

  url <- vardef_url(
    status = status
  )

  if (!is.null(path)) {

    if (
      !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      path == ""
    ) {
      stop(
        "`path` må være én ikke-tom tekststreng.",
        call. = FALSE
      )
    }

    url <- paste0(
      url,
      "/",
      utils::URLencode(
        path,
        reserved = TRUE
      )
    )
  }

  headers <- c(
    `Accept-Language` = language
  )

  if (!is.null(token)) {
    headers <- c(
      headers,
      Authorization = paste(
        "Bearer",
        token
      )
    )
  }

  response <- httr::GET(
    url = url,
    query = query,
    httr::add_headers(
      .headers = headers
    )
  )

  response_text <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )

  if (httr::http_error(response)) {

    content_type <- httr::http_type(response)

    problem <- if (
      content_type %in% c(
        "application/json",
        "application/problem+json"
      )
    ) {
      tryCatch(
        jsonlite::fromJSON(
          response_text,
          simplifyVector = TRUE
        ),
        error = function(e) NULL
      )
    } else {
      NULL
    }

    detail <- if (
      is.list(problem) &&
      !is.null(problem$detail) &&
      length(problem$detail) == 1L
    ) {
      problem$detail
    } else if (identical(content_type, "text/html")) {
      paste0(
        "Tjenesten returnerte en HTML-side i stedet for et API-svar. ",
        "Kontroller endepunktet."
      )
    } else {
      "Tjenesten returnerte ingen lesbar feilmelding."
    }

    stop(
      paste0(
        "Forespørselen til Vardef feilet med HTTP-status ",
        httr::status_code(response),
        ".\n",
        "URL: ",
        response$url,
        "\n",
        "Feilmelding: ",
        detail
      ),
      call. = FALSE
    )
  }

  if (!nzchar(response_text)) {
    return(NULL)
  }

  jsonlite::fromJSON(
    response_text,
    simplifyDataFrame = TRUE
  )
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
    status = "ekstern",
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
    language = "nb"
) {

  response <- httr::GET(
    url = vardef_url(),
    query = list(
      short_name = short_name,
      date_of_validity = date
    ),
    httr::add_headers(
      `Accept-Language` = language
    )
  )

  httr::stop_for_status(response)

  variable_definition <- jsonlite::fromJSON(
    httr::content(
      response,
      as = "text",
      encoding = "UTF-8"
    )
  )

  return(variable_definition)
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
#' @return Et objekt opprettet fra JSON-responsen fra tjenesten.
#'   Returtypen avhenger av strukturen i responsen og vil vanligvis være
#'   en liste eller en data frame.
#'
#' @details
#' ID-en legges til URL-en til tjenesten, mens datoen sendes som
#' parameteren `date_of_validity`. Språket sendes i HTTP-hodet
#' `Accept-Language`.
#'
#' Dersom tjenesten returnerer HTTP-status 404, vises en melding om at
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
    id,
    date = format(Sys.Date(), "%Y-%m-%d"),
    language = "nb"
) {

  response <- httr::GET(
    url = glue::glue("{vardef_url()}/{id}"),
    query = list(
      date_of_validity = date
    ),
    httr::add_headers(
      `Accept-Language` = language
    )
  )

  status <- httr::status_code(response)

  if (status != 404L) {
    httr::stop_for_status(response)
  }

  variable_definition <- jsonlite::fromJSON(
    httr::content(
      response,
      as = "text",
      encoding = "UTF-8"
    )
  )

  if (status == 404L) {

    detail <- variable_definition$detail

    if (
      is.null(detail) ||
      length(detail) == 0L ||
      is.na(detail[[1L]])
    ) {
      detail <- "Ingen nærmere beskrivelse."
    }

    message(
      "Status ", status,
      ": Fant ikke variabeldefinisjonen med ID `",
      id,
      "`. ",
      detail
    )
  }

  return(variable_definition)
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

  classification_uri <- variable_definition$classification_uri

  if (
    is.null(classification_uri) ||
    length(classification_uri) == 0L ||
    is.na(classification_uri[[1L]]) ||
    !nzchar(trimws(classification_uri[[1L]]))
  ) {
    return(NA_character_)
  }

  classification_uri <- trimws(
    as.character(classification_uri[[1L]])
  )

  if (!grepl("[0-9]+/?$", classification_uri)) {
    warning(
      "Fant ingen KLASS-ID på slutten av `classification_uri`: ",
      classification_uri,
      call. = FALSE
    )

    return(NA_character_)
  }

  sub(
    pattern = "^.*?([0-9]+)/?$",
    replacement = "\\1",
    x = classification_uri
  )
}


#' Lag oversikt over variabler i en Datadoc-fil
#'
#' Leser variabelmetadata fra en Datadoc-fil og lager en oversikt
#' over variablene. Dersom en variabel har en referanse til en
#' variabeldefinisjon, hentes supplerende metadata fra SSBs tjeneste for
#' variabeldefinisjoner.
#'
#' @param filsti En tekststreng med filstien til en Parquet-fil. Filstien til
#'   den tilhørende Datadoc-filen utledes ved hjelp av [datadoc_path()].
#' @param language En tekststreng med språkkoden som skal brukes ved uthenting
#'   av navn og annen språkavhengig metadata. Standardverdien er `"nb"`.
#'
#' @return En `data.frame` med én rad per variabel og følgende kolonner:
#'
#' \describe{
#'   \item{`short_name`}{Variabelens kortnavn fra Datadoc-filen.}
#'   \item{`name`}{Variabelens navn på det valgte språket.}
#'   \item{`data_type`}{Variabelens datatype.}
#'   \item{`classification_uri`}{KLASS-ID hentet fra variabelens
#'     `classification_uri` i DataDoc-filen.}
#'   \item{`vardef_definition_uri`}{ID-en til variabeldefinisjonen som ble
#'     hentet fra tjenesten for variabeldefinisjoner.}
#'   \item{`vardef_name`}{Navnet fra variabeldefinisjonen på det valgte
#'     språket.}
#'   \item{`vardef_short_name`}{Kortnavnet fra variabeldefinisjonen.}
#'   \item{`vardef_classification_uri`}{KLASS-ID hentet fra
#'     `classification_uri` i variabeldefinisjonen.}
#'   \item{`contains_data_from`}{Startdatoen for perioden variabelen
#'     inneholder data for.}
#'   \item{`contains_data_until`}{Sluttdatoen for perioden variabelen
#'     inneholder data for.}
#' }
#'
#' @details
#' Funksjonen forventer at Datadoc-filen har samme filsti som Parquet-filen,
#' men med filendelsen `.parquet` erstattet med `__DOC.json`.
#'
#' Språkavhengige felt kan være lagret som tekstvektorer, data frames eller
#' lister. Funksjonen forsøker først å hente tekst for språket angitt i
#' `language`. Dersom dette språket ikke finnes, brukes den første
#' tilgjengelige ikke-tomme teksten.
#'
#' Verdiene i `classification_uri` omgjøres til KLASS-ID-er. Dersom URI-en
#' mangler eller ikke avsluttes med en numerisk ID, returneres
#' `NA_character_`.
#'
#' Dersom en variabel inneholder en `definition_uri`, brukes sluttdatoen i
#' `contains_data_until` som gyldighetsdato ved oppslag i tjenesten for
#' variabeldefinisjoner. Dersom sluttdatoen mangler, brukes dagens dato.
#'
#' Hver kombinasjon av variabeldefinisjons-ID og gyldighetsdato hentes bare
#' én gang. Dersom oppslaget feiler, vises en advarsel og de tilhørende
#' Vardef-feltene fylles med manglende verdier.
#'
#' @seealso
#' [datadoc_path()] for å opprette filstien til DataDoc-filen og
#' [get_variable_definition_by_id()] for å hente én variabeldefinisjon.
#'
#' @examples
#' \dontrun{
#' datadoc_variabeloversikt(
#'   filsti = "/buckets/data/personell_v1.parquet"
#' )
#'
#' datadoc_variabeloversikt(
#'   filsti = "/buckets/data/personell_v1.parquet",
#'   language = "en"
#' )
#' }
#'
#' @export
datadoc_variabeloversikt <- function(
    filsti,
    language = "nb"
) {

  first_nonempty_character <- function(x) {

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

  extract_language_text <- function(
    x,
    language = "nb"
  ) {

    if (is.null(x) || length(x) == 0L) {
      return(NA_character_)
    }

    # Dersom feltet allerede er en tekstvektor
    if (is.character(x)) {
      return(first_nonempty_character(x))
    }

    # Data frame med languageCode og languageText
    if (
      is.data.frame(x) &&
      all(c("languageCode", "languageText") %in% names(x))
    ) {

      language_text <- x$languageText[
        !is.na(x$languageCode) &
          x$languageCode == language &
          !is.na(x$languageText) &
          nzchar(trimws(x$languageText))
      ]

      if (length(language_text) > 0L) {
        return(trimws(language_text[[1L]]))
      }

      # Bruk første tilgjengelige språk dersom ønsket språk mangler
      return(first_nonempty_character(x$languageText))
    }

    # Navngitt liste med languageCode og languageText
    if (
      is.list(x) &&
      all(c("languageCode", "languageText") %in% names(x))
    ) {

      language_code <- unlist(
        x$languageCode,
        use.names = FALSE
      )

      language_text <- unlist(
        x$languageText,
        use.names = FALSE
      )

      index <- which(
        !is.na(language_code) &
          language_code == language &
          !is.na(language_text) &
          nzchar(trimws(language_text))
      )

      if (length(index) > 0L) {
        return(trimws(language_text[[index[[1L]]]]))
      }

      return(first_nonempty_character(language_text))
    }

    # Liste med ett element per språk
    if (is.list(x)) {

      language_text <- vapply(
        x,
        extract_language_text,
        FUN.VALUE = character(1),
        language = language
      )

      return(first_nonempty_character(language_text))
    }

    NA_character_
  }

  extract_id_from_uri <- function(uri) {

    uri <- first_nonempty_character(uri)

    if (is.na(uri)) {
      return(NA_character_)
    }

    # Fjern eventuell avsluttende skråstrek
    uri <- sub(
      pattern = "/+$",
      replacement = "",
      x = uri
    )

    # Hent teksten etter siste kolon eller skråstrek
    id <- sub(
      pattern = "^.*[:/]",
      replacement = "",
      x = uri
    )

    first_nonempty_character(id)
  }

  extract_klass_id <- function(classification_uri) {

    classification_uri <- first_nonempty_character(
      classification_uri
    )

    if (is.na(classification_uri)) {
      return(NA_character_)
    }

    classification_uri <- sub(
      pattern = "/+$",
      replacement = "",
      x = classification_uri
    )

    klass_id <- sub(
      pattern = "^.*[:/]",
      replacement = "",
      x = classification_uri
    )

    if (!grepl("^[0-9]+$", klass_id)) {
      return(NA_character_)
    }

    klass_id
  }

  datadoc_filsti <- datadoc_path(filsti)

  datadoc_ref <- jsonlite::fromJSON(
    datadoc_filsti
  )

  variables <- datadoc_ref$datadoc$variables
  n_variables <- NROW(variables)

  short_names <- variables$short_name

  names_nb <- vapply(
    variables$name,
    extract_language_text,
    FUN.VALUE = character(1),
    language = language
  )

  classification_uri <- vapply(
    seq_len(n_variables),
    function(i) {
      extract_klass_id(
        variables$classification_uri[[i]]
      )
    },
    FUN.VALUE = character(1)
  )

  definition_uri <- vapply(
    seq_len(n_variables),
    function(i) {
      first_nonempty_character(
        variables$definition_uri[[i]]
      )
    },
    FUN.VALUE = character(1)
  )

  vardef_id <- vapply(
    definition_uri,
    extract_id_from_uri,
    FUN.VALUE = character(1)
  )

  contains_data_from <- variables$contains_data_from
  contains_data_until <- variables$contains_data_until

  # Bruk sluttdatoen for datainnholdet som gyldighetsdato.
  # Dersom denne mangler, brukes dagens dato.
  vardef_date <- as.character(contains_data_until)

  vardef_date[
    is.na(vardef_date) |
      !nzchar(trimws(vardef_date))
  ] <- format(
    Sys.Date(),
    "%Y-%m-%d"
  )

  variabeloversikt <- data.frame(
    short_name = short_names,
    name = names_nb,
    data_type = variables$data_type,
    classification_uri = classification_uri,
    contains_data_from = contains_data_from,
    contains_data_until = contains_data_until,
    .vardef_id = vardef_id,
    .vardef_date = vardef_date,
    stringsAsFactors = FALSE
  )

  vardef_oppslag <- variabeloversikt |>
    dplyr::filter(
      !is.na(.vardef_id),
      nzchar(trimws(.vardef_id))
    ) |>
    dplyr::distinct(
      .vardef_id,
      .vardef_date
    )

  if (nrow(vardef_oppslag) == 0L) {

    variabeloversikt <- variabeloversikt |>
      dplyr::mutate(
        vardef_definition_uri = NA_character_,
        vardef_name = NA_character_,
        vardef_short_name = NA_character_,
        vardef_classification_uri = NA_character_
      ) |>
      dplyr::select(
        -.vardef_id,
        -.vardef_date
      )

    return(
      as.data.frame(variabeloversikt)
    )
  }

  vardef_metadata <- purrr::map2_dfr(
    vardef_oppslag$.vardef_id,
    vardef_oppslag$.vardef_date,
    function(id, date) {

      variable_definition <- tryCatch(
        get_variable_definition_by_id(
          id = id,
          date = date,
          language = language
        ),
        error = function(e) {

          warning(
            "Kunne ikke hente Vardef-metadata for ID `",
            id,
            "` med dato ",
            date,
            ". Feilmelding: ",
            conditionMessage(e),
            call. = FALSE
          )

          NULL
        }
      )

      if (is.null(variable_definition)) {

        return(
          tibble::tibble(
            .vardef_id = id,
            .vardef_date = date,
            vardef_definition_uri = id,
            vardef_name = NA_character_,
            vardef_short_name = NA_character_,
            vardef_classification_uri = NA_character_
          )
        )
      }

      vardef_definition_uri <- first_nonempty_character(
        variable_definition$id
      )

      if (is.na(vardef_definition_uri)) {
        vardef_definition_uri <- id
      }

      tibble::tibble(
        .vardef_id = id,
        .vardef_date = date,
        vardef_definition_uri = vardef_definition_uri,
        vardef_name = extract_language_text(
          variable_definition$name,
          language = language
        ),
        vardef_short_name = first_nonempty_character(
          variable_definition$short_name
        ),
        vardef_classification_uri = extract_klass_id(
          variable_definition$classification_uri
        )
      )
    }
  )

  variabeloversikt <- variabeloversikt |>
    dplyr::left_join(
      vardef_metadata,
      by = c(
        ".vardef_id",
        ".vardef_date"
      )
    ) |>
    dplyr::select(
      short_name,
      name,
      data_type,
      classification_uri,
      vardef_definition_uri,
      vardef_name,
      vardef_short_name,
      vardef_classification_uri,
      contains_data_from,
      contains_data_until
    )

  as.data.frame(variabeloversikt)
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
#' @param filsti En tekststreng med filstien til en Parquet-fil. Filstien til
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
#' Legger til verdietiketter på variabler i et datasett basert på
#' klassifikasjoner registrert i DataDoc eller Vardef.
#'
#' For hver variabel hentes KLASS-ID fra DataDoc dersom denne finnes.
#' Dersom KLASS-ID ikke er registrert i DataDoc, brukes eventuell
#' klassifikasjon fra Vardef. KLASS-ID fra DataDoc har dermed forrang.
#'
#' Kodelisten hentes fra KLASS ved hjelp av
#' [klassR::get_klass()]. Dersom `contains_data_until` er registrert
#' i DataDoc, brukes denne datoen ved henting av kodelisten.
#'
#' Variabler som ikke finnes i `data`, som mangler en gyldig KLASS-ID,
#' eller der kodelisten ikke kan hentes, hoppes over med en advarsel.
#'
#' Kodene fra KLASS behandles som tekst. Dersom en variabel i `data`
#' ikke er av typen `character`, konverteres den til `character` før
#' verdietikettene legges til. Dette gir som standard en advarsel.
#'
#' Ved å sette `quiet = TRUE` undertrykkes alle advarsler som oppstår
#' under kjøringen av funksjonen. Feil (`error`) undertrykkes ikke.
#'
#' @param data Et datasett, for eksempel en `data.frame` eller tibble,
#'   som skal få lagt til verdietiketter.
#' @param filsti En tekststreng med filsti til DataDoc-filen eller den
#'   tilhørende datafilen.
#' @param language En tekststreng som angir språk som skal brukes ved
#'   henting av metadata. Standard er `"nb"`.
#' @param quiet Logisk verdi. Dersom `TRUE`, undertrykkes alle advarsler
#'   som oppstår under kjøringen. Standard er `FALSE`.
#'
#' @return Datasettet `data` med verdietiketter lagt til for variabler
#'   der en gyldig klassifikasjon ble funnet. Variabler kan bli
#'   konvertert til `character` dersom de opprinnelig har en annen
#'   datatype enn kodene fra KLASS.
#'
#' @details
#' Funksjonen bruker [variables_with_classification_uri()] til å finne
#' variabler med tilknyttede klassifikasjoner.
#'
#' Dersom både DataDoc og Vardef inneholder en klassifikasjon for samme
#' variabel, brukes klassifikasjonen fra DataDoc.
#'
#' Verdietikettene legges til med [labelled::set_value_labels()], der
#' navnene fra KLASS brukes som etiketter og kodene som underliggende
#' verdier.
#'
#' @examples
#' \dontrun{
#' data_med_labels <- add_value_labels(
#'   data = data,
#'   filsti = "data__DOC.json"
#' )
#'
#' # Undertrykk advarsler
#' data_med_labels <- add_value_labels(
#'   data = data,
#'   filsti = "data__DOC.json",
#'   quiet = TRUE
#' )
#' }
#'
#' @export
add_value_labels <- function(
    data,
    filsti,
    language = "nb",
    quiet = TRUE
) {

  if (
    !is.logical(quiet) ||
    length(quiet) != 1 ||
    is.na(quiet)
  ) {
    stop(
      "`quiet` må være enten TRUE eller FALSE.",
      call. = FALSE
    )
  }

  add_labels <- function() {

    variabler_med_kodelister <- variables_with_classification_uri(
      filsti = filsti,
      language = language
    ) |>
      dplyr::mutate(
        # KLASS-ID-en fra DataDoc får forrang dersom begge finnes.
        classification_uri_effective = dplyr::coalesce(
          dplyr::na_if(
            trimws(as.character(classification_uri)),
            ""
          ),
          dplyr::na_if(
            trimws(as.character(vardef_classification_uri)),
            ""
          )
        )
      )

    for (i in seq_len(nrow(variabler_med_kodelister))) {

      variabel <- variabler_med_kodelister$short_name[[i]]

      classification_uri <-
        variabler_med_kodelister$classification_uri_effective[[i]]

      contains_data_until <-
        variabler_med_kodelister$contains_data_until[[i]]

      # Hopp over dersom variabelen ikke finnes i datasettet
      if (!variabel %in% names(data)) {
        warning(
          "Variabelen `", variabel,
          "` finnes ikke i datasettet og ble hoppet over.",
          call. = FALSE
        )

        next
      }

      # Hopp over dersom ingen KLASS-ID ble funnet
      if (
        is.na(classification_uri) ||
        !nzchar(trimws(classification_uri))
      ) {
        warning(
          "Fant ingen KLASS-ID for variabelen `",
          variabel,
          "`. Variabelen ble hoppet over.",
          call. = FALSE
        )

        next
      }

      # Kontroller at bare selve KLASS-ID-en brukes videre
      if (!grepl("^[0-9]+$", classification_uri)) {
        warning(
          "Ugyldig KLASS-ID for variabelen `",
          variabel,
          "`: ",
          classification_uri,
          ". Forventet kun sifre. Variabelen ble hoppet over.",
          call. = FALSE
        )

        next
      }

      kodeliste_klass <- tryCatch(
        klassR::get_klass(
          classification_uri,
          date = contains_data_until
        ) |>
          dplyr::mutate(
            code = trimws(as.character(code)),
            name = trimws(as.character(name))
          ),
        error = function(e) {
          warning(
            "Kunne ikke hente kodeliste for `",
            variabel,
            "` med KLASS-ID ",
            classification_uri,
            ". Variabelen ble hoppet over. Feilmelding: ",
            conditionMessage(e),
            call. = FALSE
          )

          NULL
        }
      )

      if (is.null(kodeliste_klass)) {
        next
      }

      labs <- stats::setNames(
        kodeliste_klass$code,
        kodeliste_klass$name
      )

      # Sørg for at variabelen har samme type som kodene i kodelisten
      if (!is.character(data[[variabel]])) {

        warning(
          "Variabelen `",
          variabel,
          "` har type `",
          typeof(data[[variabel]]),
          "`, mens kodene i KLASS er tekst. ",
          "Variabelen konverteres til `character` før verdietikettene legges til.",
          call. = FALSE
        )

        data[[variabel]] <- as.character(
          data[[variabel]]
        )
      }

      data[[variabel]] <- labelled::set_value_labels(
        data[[variabel]],
        .labels = labs
      )
    }

    data
  }

  if (quiet) {
    suppressMessages(
      suppressWarnings(
        add_labels()
      )
    )
  } else {
    add_labels()
  }
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
#' Ved konverteringen brukes `levels = "labels"`, slik at faktorverdiene
#' består av verdietikettene og ikke de underliggende kodene. Dette fungerer
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
show_labels_df <- function(
    data,
    labels = TRUE
) {

  if (
    !is.logical(labels) ||
    length(labels) != 1L ||
    is.na(labels)
  ) {
    stop(
      "`labels` må være enten TRUE eller FALSE.",
      call. = FALSE
    )
  }

  if (labels) {
    data <- dplyr::mutate(
      data,
      dplyr::across(
        tidyselect::where(haven::is.labelled),
        ~ haven::as_factor(
          .x,
          levels = "labels"
        )
      )
    )
  }

  data
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

  dplyr::mutate(
    data,
    dplyr::across(
      tidyselect::everything(),
      vis_kolonnelabel
    )
  )
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
#' @return Datasettet som ble oppgitt i `data`, med kolonneetiketter lagt til
#'   for variabler som finnes både i datasettet og i DataDoc-filen.
#'
#' @details
#' Variabeloversikten hentes med [datadoc_variabeloversikt()]. Verdiene i
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
    show_labels = TRUE
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
      filsti = filsti
    )
  )

  labels <- stats::setNames(
    datadoc_variabeloversikt_df$name,
    datadoc_variabeloversikt_df$short_name
  )

  labels <- labels[
    names(labels) %in% names(data)
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
  purrr::map_dfr(
    vars_with_value_labels(data),
    function(var) {

      x <- data[[var]]

      # Observerte verdier i data
      observed_values <- unique(x)
      observed_values <- observed_values[
        !is.na(observed_values)
      ]

      # Verdier med verdietiketter
      labelled_values <- unname(
        labelled::val_labels(x)
      )

      # Observerte verdier uten verdietikett
      missing_labels <- setdiff(
        observed_values,
        labelled_values
      )

      if (length(missing_labels) == 0L) {
        return(NULL)
      }

      tibble::tibble(
        variable = var,
        value_without_label = missing_labels
      ) |>
        remove_all_labels()
    }
  )
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
#' Andre metadataattributter, som formater og kolonnebredder, fjernes ikke.
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
  data |>
    dplyr::collect() |>
    haven::zap_labels() |>
    haven::zap_label() |>
    as.data.frame()
}


#' Hent metadata for én variabel fra Datadoc
#'
#' Leser en DataDoc-fil og henter det fullstendige metadataobjektet for
#' en bestemt variabel.
#'
#' @param filsti En tekststreng med filstien til Parquet-filen. Filstien
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

  datadoc_ref <- jsonlite::fromJSON(
    datadoc_path(filsti),
    simplifyVector = FALSE
  )

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
#' Hele metadataobjektet for hver valgt variabel erstattes. Variabelens
#' `short_name` beholdes imidlertid slik det er angitt i DataDoc-filen
#' som metadataene kopieres til.
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
#' @return Den oppdaterte DataDoc-strukturen som en liste. Den oppdaterte
#'   strukturen skrives samtidig til DataDoc-filen som svarer til
#'   `filsti_datadoc_egen`.
#'
#' @details
#' Dersom en filsti slutter på `.parquet`, erstattes filendelsen med
#' `__DOC.json` ved hjelp av [datadoc_path()]. Filstier som allerede
#' slutter på `.json`, brukes uendret.
#'
#' De endelige JSON-filstiene skrives ut før filene leses. Funksjonen
#' stopper med en feilmelding dersom én eller begge DataDoc-filene ikke
#' finnes. Alle manglende filer oppgis i samme feilmelding.
#'
#' Begge DataDoc-filene leses med [jsonlite::fromJSON()] med
#' `simplifyVector = FALSE`.
#'
#' For hver valgt variabel kopieres hele elementet fra
#' `datadoc$variables` i originalfilen til den tilsvarende variabelen i
#' filen som skal oppdateres. Feltet `short_name` erstattes deretter med
#' variabelnavnet som brukes i mottakerfilen.
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
    variabler
) {

  resolve_datadoc_path <- function(filsti, argument) {

    if (
      !is.character(filsti) ||
      length(filsti) != 1L ||
      is.na(filsti) ||
      !nzchar(trimws(filsti))
    ) {
      stop(
        "`",
        argument,
        "` må være én ikke-tom tekststreng.",
        call. = FALSE
      )
    }

    filsti <- trimws(filsti)

    if (
      grepl(
        pattern = "\\.parquet$",
        x = filsti,
        ignore.case = TRUE
      )
    ) {
      return(
        datadoc_path(filsti)
      )
    }

    if (
      grepl(
        pattern = "\\.json$",
        x = filsti,
        ignore.case = TRUE
      )
    ) {
      return(filsti)
    }

    stop(
      "`",
      argument,
      "` må slutte på `.json` eller `.parquet`.",
      call. = FALSE
    )
  }

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

  message(
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

  datadoc_egen <- jsonlite::fromJSON(
    filsti_datadoc_egen,
    simplifyVector = FALSE
  )

  datadoc_original <- jsonlite::fromJSON(
    filsti_datadoc_original,
    simplifyVector = FALSE
  )

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

  navn_original <- unname(
    variabler
  )

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

    # Behold variabelnavnet som brukes i egen metadatafil.
    metadata_original$short_name <- navn_egen[[i]]

    datadoc_egen$datadoc$variables[[indeks_egen]] <-
      metadata_original
  }

  jsonlite::write_json(
    datadoc_egen,
    path = filsti_datadoc_egen,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )

  message(
    "Kopierte metadata for ",
    length(navn_egen),
    " variabel",
    if (length(navn_egen) == 1L) "." else "er.",
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
#' `levels = "labels"`. Dette innebærer at de nye kolonnene inneholder
#' verdietikettene, mens de opprinnelige kodene beholdes i de opprinnelige
#' variablene.
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
#' Det gis en advarsel dersom konverteringen introduserer nye
#' missing-verdier. Missing-verdier som allerede finnes i de opprinnelige
#' variablene utløser ikke advarselen. Nye missing-verdier kan blant annet
#' oppstå dersom enkelte verdier ikke har en tilhørende verdietikett.
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

  original_data <- data

  data <- dplyr::mutate(
    data,
    dplyr::across(
      dplyr::all_of(variables),
      ~ haven::as_factor(
        .x,
        levels = "labels"
      ),
      .names = paste0(
        "{.col}",
        postfix
      )
    )
  )

  variables_with_new_missing <- variables[
    vapply(
      seq_along(variables),
      function(i) {
        original <- original_data[[variables[i]]]
        labelled <- data[[new_variables[i]]]

        any(
          !is.na(original) & is.na(labelled)
        )
      },
      logical(1)
    )
  ]

  if (length(variables_with_new_missing) > 0L) {
    warning(
      "Følgende variabler fikk nye missing-verdier ved konvertering til verdietiketter: ",
      paste(variables_with_new_missing, collapse = ", "),
      ". Dette kan skyldes at enkelte verdier mangler verdietikett.",
      call. = FALSE
    )
  }

  data
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
    datadoc_path(filsti_datadoc_egen, to = "parquet")
  } else {
    filsti_datadoc_egen
  }

  filsti_datadoc_original <- if (
    tolower(tools::file_ext(filsti_datadoc_original)) == "parquet"
  ) {
    datadoc_path(filsti_datadoc_original, to = "parquet")
  } else {
    filsti_datadoc_original
  }

  # Les Datadoc-filene
  egen <- jsonlite::read_json(
    filsti_datadoc_egen,
    simplifyVector = FALSE
  )

  original <- jsonlite::read_json(
    filsti_datadoc_original,
    simplifyVector = FALSE
  )

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
      is.null(egen_dataset[[felt]]) ||
      length(egen_dataset[[felt]]) == 0
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
  jsonlite::write_json(
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
#' Gyldighetsperioden for øvrige variabler i mottakerfilen settes automatisk
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
#' For hver øvrige variabel kopieres hele metadataobjektet fra originalfilen.
#' Feltet `short_name` erstattes deretter med kortnavnet som brukes i filen
#' som oppdateres.
#'
#' Når `overwrite = FALSE`, regnes en variabel som å ha eksisterende metadata
#' dersom feltet `name` ikke er `NULL`. Andre metadatafelt tas ikke med i
#' denne vurderingen.
#'
#' Gyldighetsperioden oppdateres uavhengig av `overwrite`. Dette innebærer at
#' også variabler som ikke får kopiert metadata fordi de allerede har
#' metadata, får oppdatert feltene `contains_data_from` og
#' `contains_data_until`.
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
    dato_unntak = NULL
) {

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
  datadoc_egen <- jsonlite::fromJSON(
    filsti_datadoc_egen,
    simplifyVector = FALSE
  )

  datadoc_original <- jsonlite::fromJSON(
    filsti_datadoc_original,
    simplifyVector = FALSE
  )

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

    # Behold short_name fra egen metadatafil
    metadata_original$short_name <- navn_egen

    datadoc_egen$datadoc$variables[[indeks_egen]] <-
      metadata_original

    variabler_kopiert <- c(
      variabler_kopiert,
      navn_egen
    )
  }

  # Oppdater gyldighetsperiode for alle variabler,
  # bortsett fra ekskluderte variabler
  for (i in seq_along(datadoc_egen$datadoc$variables)) {

    navn <- datadoc_egen$datadoc$variables[[i]]$short_name

    # Ekskluderte variabler skal ikke endres
    if (navn %in% ekskluder_variabler) {
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
  }

  # Skriv oppdatert Datadoc-fil
  jsonlite::write_json(
    datadoc_egen,
    path = filsti_datadoc_egen,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )

  message(
    "Kopierte metadata for ",
    length(variabler_kopiert),
    " variabel",
    if (length(variabler_kopiert) == 1L) "" else "er",
    "."
  )

  if (length(variabler_hoppet_over) > 0L) {
    message(
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
    message(
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

  antall_dato_oppdatert <- length(
    setdiff(
      short_name_egen,
      ekskluder_variabler
    )
  )

  message(
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
        nrow(dato_unntak),
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

  datadoc <- jsonlite::read_json(
    filsti_datadoc,
    simplifyVector = FALSE
  )

  datadoc$datadoc$dataset$file_path <- file_path

  jsonlite::write_json(
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
#' Dersom `version_description` allerede er utfylt og `overwrite = FALSE`,
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
set_version_description <- function(filsti_datadoc,
                                    version_description,
                                    language_code = "nb",
                                    overwrite = FALSE) {

  datadoc <- jsonlite::read_json(
    filsti_datadoc,
    simplifyVector = FALSE
  )

  existing_description <- datadoc$datadoc$dataset$version_description

  if (!is.null(existing_description) && !overwrite) {
    warning(
      "version_description er allerede utfylt. ",
      "Bruk overwrite = TRUE for å overskrive eksisterende verdi."
    )

    return(invisible(datadoc))
  }

  datadoc$datadoc$dataset$version_description <- list(
    list(
      languageCode = language_code,
      languageText = version_description
    )
  )

  jsonlite::write_json(
    datadoc,
    filsti_datadoc,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )

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

  # Gjør om parquet-sti til datadoc-sti ved behov
  filsti_1 <- if (
    tolower(tools::file_ext(filsti_1)) == "parquet"
  ) {
    datadoc_path(filsti_1, to = "parquet")
  } else {
    filsti_1
  }

  filsti_2 <- if (
    tolower(tools::file_ext(filsti_2)) == "parquet"
  ) {
    datadoc_path(filsti_2, to = "parquet")
  } else {
    filsti_2
  }

  # Les datadoc
  datadoc_1 <- jsonlite::read_json(
    filsti_1,
    simplifyVector = FALSE
  )

  datadoc_2 <- jsonlite::read_json(
    filsti_2,
    simplifyVector = FALSE
  )

  # Hjelpefunksjon for å hente KLASS-id fra classification_uri
  get_klass_id <- function(classification_uri) {

    if (
      is.null(classification_uri) ||
      length(classification_uri) == 0 ||
      is.na(classification_uri)
    ) {
      return(NA_character_)
    }

    id <- stringr::str_match(
      classification_uri,
      "(?:klassifikasjoner|classifications)/(\\d+)"
    )[, 2]

    if (is.na(id)) {
      id <- stringr::str_extract(
        classification_uri,
        "\\d+(?=/?$)"
      )
    }

    id
  }

  # Hjelpefunksjon for å hente relevant variabelmetadata
  get_variable_metadata <- function(datadoc) {

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

  # Gjør om parquet-sti til datadoc-sti ved behov
  filsti <- if (
    tolower(tools::file_ext(filsti)) == "parquet"
  ) {
    datadoc_path(filsti, to = "parquet")
  } else {
    filsti
  }

  # Les Datadoc
  datadoc <- jsonlite::read_json(
    filsti,
    simplifyVector = FALSE
  )

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
  is_missing_metadata <- function(x) {

    if (is.null(x) || length(x) == 0) {
      return(TRUE)
    }

    if (length(x) == 1 && is.na(x)) {
      return(TRUE)
    }

    if (
      is.character(x) &&
      all(is.na(x) | trimws(x) == "")
    ) {
      return(TRUE)
    }

    FALSE
  }

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

  result <- dplyr::bind_rows(result)

  if (only_incomplete) {
    result <- dplyr::filter(
      result,
      !complete
    )
  }

  result
}
