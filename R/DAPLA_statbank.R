

#' statbank_encrypt_request
#'
#' `statbank_encrypt_request`
#'
#' @param ...
#'
#' @examples
#' \dontrun{
#' }
#' @encoding UTF-8


statbank_encrypt_request <- function(laste_bruker) {
  if (Sys.getenv('CLUSTER_ID')=="staging-bip-app") {
    db <- "TEST"
  }

  if (Sys.getenv('CLUSTER_ID')=="prod-bip-app") {
    db <- "PROD"
  }

  encrypt_request <- httr::POST(
    Sys.getenv('STATBANK_ENCRYPT_URL'),
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste0("Bearer ", httr::content(httr::GET(Sys.getenv('LOCAL_USER_PATH'), httr::add_headers('Authorization' = paste0('token ', Sys.getenv("JUPYTERHUB_API_TOKEN")))))$access_token)),
    body = list(message = getPass::getPass(paste0("Lastepassord (", db, "):"))),
    encode = "json"
  )
  username_encryptedpassword <- openssl::base64_encode(paste0(laste_bruker, ":", httr::content(encrypt_request)$message))
  return(username_encryptedpassword)
}


#' statbank_uttaksbeskrivelse
#'
#' `statbank_uttaksbeskrivelse`
#'
#' @param ...
#'
#' @examples
#' \dontrun{
#'}
#'@encoding UTF-8

statbank_uttaksbeskrivelse <- function(tabell_id,
                                       laste_bruker,
                                       ask = TRUE,
                                       boundary = 12345) {
  if (ask == TRUE){
    username_encryptedpassword <- statbank_encrypt_request(laste_bruker = laste_bruker)
  }

  URL <- paste0(Sys.getenv('STATBANK_BASE_URL'), 'statbank/sos/v1/uttaksbeskrivelse?', "tableId=", tabell_id)

  uttaksbeksrivelse <- httr::GET(URL,
                                 httr::add_headers(
                                   'Authorization' = paste0('Basic ', username_encryptedpassword),
                                   'Content-Type' = paste0('multipart/form-data; boundary=', boundary),
                                   'Connection' = 'keep-alive',
                                   'Accept' = '*/*'
                                 ))

  uttaksbeksrivelse <- jsonlite::fromJSON(httr::content(uttaksbeksrivelse))
  return(uttaksbeksrivelse)
}

#' statbank_body
#'
#' `statbank_body`
#'
#' @param ...
#'
#' @examples
#' \dontrun{
#'}
#'@encoding UTF-8

statbank_body <- function(data, tabell_id, ask = TRUE, boundary = 12345) {

  data_all <- ""

  if (class(data)[1] %in% c("data.frame", "tibble", "tbl_df", "tbl")) {
    data <- list(data)
  }

  for (i in 1:length(data)) {

    filename <- statbank_uttaksbeskrivelse(tabell_id = tabell_id, ask = ask)$DeltabellTitler$Filnavn[i]
    start <- paste0("--", boundary, "\r\nContent-Disposition:form-data; filename=", filename, "\r\nContent-type:text/plain\r\n\r\n")

    data_1 <- data.frame(data[i])
    data_1 <- data_1 %>%
      dplyr::mutate_all(~format(., decimal.mark = ',')) %>% # endrer desimaltegn til komma
      dplyr::mutate_all(., str_trim) # fjerner whitespace
    data_1 <- do.call(paste, c(data_1[colnames(data_1)], sep = ";", collapse = "\r\n"))

    data_1 <- paste0(start, data_1)

    data_all <- paste0(data_all, data_1, sep = paste0("\r\n"))
  }

  data_all <- paste0(data_all, "--12345--\r\n")
  return(data_all)
}


#' statbank_validering
#'
#' `statbank_validering`
#'
#' @param ...
#'
#' @examples
#' \dontrun{
#'}
#'@encoding UTF-8

statbank_validering <- function(data,
                                tabell_id,
                                laste_bruker,
                                ask = FALSE) {

  uttaksbeskrivelse <- statbank_uttaksbeskrivelse(tabell_id = tabell_id, laste_bruker = laste_bruker, ask = ask)

  problemer_alle <- data.frame()

  if (class(data)[1] %in% c("data.frame", "tibble", "tbl_df", "tbl")) {
    data <- list(data)
  }

  for (i in 1:length(data)) {

    data_1 <- data.frame(data[i])

    variabler <- data.frame(uttaksbeskrivelse$deltabller$variabler[i]) # OBS: kan det finnes flere i listen?
    statistikkvariabler <- data.frame(uttaksbeskrivelse$deltabller$statistikkvariabler[i])

    # Vektor med kolonnenavn #
    Klassifikasjonsvariabel <- variabler$Klassifikasjonsvariabel
    statistikkvariabler <- statistikkvariabler$Text
    kolonnenavn <- c(Klassifikasjonsvariabel, statistikkvariabler)

    # Fjerner uten kodeliste #
    variabler_med_kodeliste <- variabler %>%
      dplyr::filter(Kodeliste_id != "-") # OBS

    # Legger til kolonnenavn #
    colnames(data_1) <- kolonnenavn # OBS

    data_1$filnavn <- uttaksbeskrivelse$DeltabellTitler$Filnavn[i]
    data_1$problemer <- ""

    for (j in variabler_med_kodeliste$Klassifikasjonsvariabel) {

      variabler_1 <- variabler %>%
        dplyr::filter(Klassifikasjonsvariabel == j)

      kodeliste_plassering <- which(uttaksbeskrivelse$kodelister$kodeliste == variabler_1$Kodeliste_id)
      kodeliste_navn <- uttaksbeskrivelse$kodelister$kodeliste[kodeliste_plassering]

      koder <- data.frame(uttaksbeskrivelse$kodelister$koder[kodeliste_plassering])

      data_1 <- data_1 %>%
        dplyr::mutate(problemer = case_when(
          !!sym(j) %in% unique(as.character(koder$kode)) ~ "",
          TRUE ~ j
        ))
    }

    problemer <- data_1 %>%
      dplyr::filter(problemer != "")

    problemer_alle <- rbind(problemer_alle, problemer)
  }

  if (nrow(problemer_alle)>0) {
    problemer_alle <- problemer_alle %>%
      dplyr::relocate(filnavn, problemer)
    return(problemer_alle)
  } else {
    print("Ingen ugyldige verdier i kodeliste")
  }
}






