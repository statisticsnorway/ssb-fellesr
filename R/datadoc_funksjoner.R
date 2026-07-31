
# Datadoc-funksjoner


#' Lag filsti til Datadoc-fil
#'
#' Oppretter filstien til en Datadoc-fil basert på filstien til en
#' Parquet-fil. Filendelsen `.parquet` erstattes med `__DOC.json`.
#'
#' @param filsti En tekststreng eller tegnvektor med filstien til én eller
#'   flere Parquet-filer.
#'
#' @return En tekststreng eller tegnvektor med filstien til den tilhørende
#'   DataDoc-filen.
#'
#' @examples
#' datadoc_path("/buckets/data/personell_v1.parquet")
#'
#' datadoc_path(
#'   c(
#'     "/buckets/data/personell_v1.parquet",
#'     "/buckets/data/regnskap_v1.parquet"
#'   )
#' )
#'
#' @export
datadoc_path <- function(filsti) {
  sub(
    pattern = "\\.parquet$",
    replacement = "__DOC.json",
    x = filsti
  )
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
#' kodelister fra SSBs klassifikasjonssystem KLASS.
#'
#' Funksjonen finner variabler med en tilknyttet kodeliste gjennom
#' Datadoc-metadata eller en tilknyttet variabeldefinisjon. Kodelistene
#' hentes fra KLASS, og navnene i kodelisten legges til som verdietiketter
#' ved hjelp av [labelled::set_value_labels()].
#'
#' @param data Et datasett som skal få lagt til verdietiketter.
#' @param filsti En tekststreng med filstien til Parquet-filen som datasettet
#'   er lest fra. Filstien brukes til å finne den tilhørende DataDoc-filen.
#' @param language En tekststreng med språkkoden som skal brukes ved uthenting
#'   av språkavhengig metadata. Standardverdien er `"nb"`.
#'
#' @return Datasettet som ble oppgitt i `data`, med verdietiketter lagt til
#'   på variabler med en gyldig kodeliste i KLASS. Variabler som ikke kan
#'   behandles, returneres uendret.
#'
#' @details
#' Funksjonen bruker [variables_with_classification_uri()] til å finne
#' variabler med en tilknyttet kodeliste.
#'
#' Dersom en KLASS-ID finnes både direkte i Datadoc-filen og i den
#' tilknyttede variabeldefinisjonen, brukes ID-en fra Datadoc-filen.
#' ID-en fra variabeldefinisjonen brukes dersom Datadoc-filen ikke
#' inneholder en KLASS-ID.
#'
#' Kodelisten hentes med [klassR::get_klass()]. Verdien i
#' `contains_data_until` brukes som dato for oppslaget i KLASS.
#'
#' En variabel hoppes over med en advarsel dersom:
#'
#' \itemize{
#'   \item variabelen ikke finnes i `data`;
#'   \item det ikke finnes en KLASS-ID;
#'   \item KLASS-ID-en inneholder andre tegn enn sifre; eller
#'   \item kodelisten ikke kan hentes fra KLASS.
#' }
#'
#' Verdiene i kodelistens kolonne `code` brukes som variabelverdier, mens
#' verdiene i kolonnen `name` brukes som verdietiketter.
#'
#' @examples
#' \dontrun{
#' data_med_labels <- add_value_labels(
#'   data = personell,
#'   filsti = "/buckets/data/personell_v1.parquet"
#' )
#'
#' data_med_labels <- add_value_labels(
#'   data = personell,
#'   filsti = "/buckets/data/personell_v1.parquet",
#'   language = "en"
#' )
#' }
#'
#' @seealso
#' [variables_with_classification_uri()] for å finne variabler med
#' kodelister og [labelled::set_value_labels()] for å legge til
#' verdietiketter.
#'
#' @export
add_value_labels <- function(
    data,
    filsti,
    language = "nb"
) {

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

    data[[variabel]] <- labelled::set_value_labels(
      data[[variabel]],
      .labels = labs
    )
  }

  data
}



#' Vis verdietiketter i et datasett
#'
#' Erstatter verdiene i merkede variabler med tilhørende verdietiketter.
#' Variabler uten verdietiketter beholdes uendret.
#'
#' @param data Et datasett som kan inneholde variabler med verdietiketter.
#' @param labels En logisk verdi som angir om verdietikettene skal vises.
#'   Når verdien er `TRUE`, konverteres merkede variabler til etikettene
#'   deres. Når verdien er `FALSE`, returneres datasettet uendret.
#'   Standardverdien er `TRUE`.
#' @param keep_codes En logisk verdi som angir om de opprinnelige kodene
#'   skal inkluderes sammen med verdietikettene. Argumentet sendes videre
#'   til `keep.labels` i [sjlabelled::as_label()]. Standardverdien er
#'   `TRUE`.
#'
#' @return Datasettet som ble oppgitt i `data`. Dersom `labels = TRUE`,
#'   er variabler med verdietiketter konvertert til faktorer med etikettene
#'   som verdier.
#'
#' @details
#' Funksjonen identifiserer merkede variabler ved hjelp av
#' [haven::is.labelled()] og konverterer dem med
#' [sjlabelled::as_label()].
#'
#' Når `keep_codes = TRUE`, beholdes de opprinnelige kodene i
#' faktorverdiene sammen med etikettene. Når `keep_codes = FALSE`, vises
#' bare etikettene.
#'
#' `labels` og `keep_codes` må være én enkelt logisk verdi og kan ikke
#' være `NA`.
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
#'   keep_codes = FALSE
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
    labels = TRUE,
    keep_codes = TRUE
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

  if (
    !is.logical(keep_codes) ||
    length(keep_codes) != 1L ||
    is.na(keep_codes)
  ) {
    stop(
      "`keep_codes` må være enten TRUE eller FALSE.",
      call. = FALSE
    )
  }

  if (labels) {
    data <- dplyr::mutate(
      data,
      dplyr::across(
        tidyselect::where(haven::is.labelled),
        ~ sjlabelled::as_label(
          .x,
          keep.labels = keep_codes
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
      )
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


#' Kopier metadata mellom Datadoc-filer
#'
#' Kopierer variabelmetadata fra en original Datadoc-fil til en annen
#' Datadoc-fil. Metadata kopieres automatisk for variabler som har samme
#' `short_name` i begge filer. Det kan i tillegg angis eksplisitte koblinger
#' mellom variabler med ulike kortnavn.
#'
#' @param filsti_datadoc_egen En tekststreng med filstien til DataDoc-filen
#'   som skal oppdateres.
#' @param filsti_datadoc_original En tekststreng med filstien til DataDoc-filen
#'   som metadata skal kopieres fra.
#' @param variabler `NULL` eller en navngitt tekstvektor med eksplisitte
#'   koblinger mellom variabler. Navnet på hvert element angir `short_name`
#'   i filen som skal oppdateres, mens verdien angir `short_name` i
#'   originalfilen. Standardverdien er `NULL`.
#' @param overwrite En logisk verdi som angir om eksisterende metadata skal
#'   erstattes. Når verdien er `TRUE`, kopieres metadata for alle aktuelle
#'   variabler. Når verdien er `FALSE`, hoppes variabler over dersom feltet
#'   `name` i mottakerfilen ikke er `NULL`. Standardverdien er `TRUE`.
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
#' For hver variabel kopieres hele metadataobjektet fra originalfilen.
#' Feltet `short_name` erstattes deretter med kortnavnet som brukes i filen
#' som oppdateres.
#'
#' Når `overwrite = FALSE`, regnes en variabel som å ha eksisterende metadata
#' dersom feltet `name` ikke er `NULL`. Andre metadatafelt tas ikke med i
#' denne vurderingen.
#'
#' Følgende kontroller utføres før filen endres:
#'
#' \itemize{
#'   \item `overwrite` må være én enkelt logisk verdi;
#'   \item `short_name` må være unik i begge Datadoc-filene;
#'   \item `variabler` må være en navngitt tekstvektor dersom argumentet
#'     ikke er `NULL`;
#'   \item samme mottakervariabel kan ikke oppgis flere ganger; og
#'   \item alle eksplisitt oppgitte variabler må finnes i de respektive
#'     DataDoc-filene.
#' }
#'
#' Den oppdaterte strukturen skrives til `filsti_datadoc_egen` med
#' [jsonlite::write_json()]. Den eksisterende filen overskrives.
#'
#' @examples
#' \dontrun{
#' # Kopier metadata for alle variabler med samme short_name
#' copy_metadata(
#'   filsti_datadoc_egen = "/buckets/data/egen__DOC.json",
#'   filsti_datadoc_original = "/buckets/data/original__DOC.json"
#' )
#'
#' # Legg også til en eksplisitt kobling mellom ulike kortnavn
#' copy_metadata(
#'   filsti_datadoc_egen = "/buckets/data/egen__DOC.json",
#'   filsti_datadoc_original = "/buckets/data/original__DOC.json",
#'   variabler = c(
#'     kjoenn = "sex",
#'     bostedskommune = "kommune"
#'   )
#' )
#'
#' # Kopier bare til variabler som ikke allerede har metadata
#' copy_metadata(
#'   filsti_datadoc_egen = "/buckets/data/egen__DOC.json",
#'   filsti_datadoc_original = "/buckets/data/original__DOC.json",
#'   overwrite = FALSE
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
    overwrite = TRUE
) {

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

  datadoc_egen <- jsonlite::fromJSON(
    filsti_datadoc_egen,
    simplifyVector = FALSE
  )

  datadoc_original <- jsonlite::fromJSON(
    filsti_datadoc_original,
    simplifyVector = FALSE
  )

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
    duplikater <- unique(
      short_name_egen[duplicated(short_name_egen)]
    )

    stop(
      "Følgende `short_name` forekommer flere ganger i ",
      "`filsti_datadoc_egen`: ",
      paste(duplikater, collapse = ", "),
      call. = FALSE
    )
  }

  if (anyDuplicated(short_name_original)) {
    duplikater <- unique(
      short_name_original[duplicated(short_name_original)]
    )

    stop(
      "Følgende `short_name` forekommer flere ganger i ",
      "`filsti_datadoc_original`: ",
      paste(duplikater, collapse = ", "),
      call. = FALSE
    )
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

    variabler <- trimws(variabler)
    names(variabler) <- trimws(names(variabler))

    if (anyDuplicated(names(variabler))) {
      duplikater <- unique(
        names(variabler)[duplicated(names(variabler))]
      )

      stop(
        "Følgende variabler i egen fil er oppgitt flere ganger ",
        "i `variabler`: ",
        paste(duplikater, collapse = ", "),
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
        paste(mangler_i_egen, collapse = ", "),
        call. = FALSE
      )
    }

    if (length(mangler_i_original) > 0L) {
      stop(
        "Følgende variabler fra `variabler` finnes ikke i ",
        "`filsti_datadoc_original`: ",
        paste(mangler_i_original, collapse = ", "),
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

  variabler_kopiert <- character(0)
  variabler_hoppet_over <- character(0)

  for (i in seq_along(variabler_samlet)) {

    navn_egen <- names(variabler_samlet)[[i]]
    navn_original <- unname(variabler_samlet[[i]])

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

    har_metadata <- !is.null(metadata_egen$name)

    # Hopp over dersom variabelen allerede har metadata
    # og overwrite = FALSE.
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
      paste(variabler_hoppet_over, collapse = ", "),
      "."
    )
  }

  datadoc_egen
}






