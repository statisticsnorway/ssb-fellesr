# -*- coding: utf-8 -*-

# user_agent
user_agent <- function() {

  onyxia <- stringr::str_detect(Sys.getenv('OIDC_TOKEN_EXCHANGE_URL'), "sso")
  dapla <- stringr::str_detect(Sys.getenv('STATBANK_ENCRYPT_URL'), "^http://dapla")
  bakke <- any(base::list.files("/ssb/") %in% "stamme01")
  prod <- stringr::str_detect(Sys.getenv('STATBANK_BASE_URL'), "i.ssb")

  if ((dapla == TRUE & prod == TRUE)) {
    user_agent <- paste0("DaplaProd-R-", httr:::default_ua())
  }
  if ((dapla == TRUE & prod == FALSE)) {
    user_agent <- paste0("DaplaTest-R-", httr:::default_ua())
  }
  if ((bakke == TRUE & prod == TRUE)) {
    user_agent <- paste0("BakkeProd-R-", httr:::default_ua())
  }
  if ((bakke == TRUE & prod == FALSE)) {
    user_agent <- paste0("BakkeTest-R-", httr:::default_ua())
  }
  if (onyxia == TRUE){
    user_agent <- paste0("OnyxiaTest-R-", httr:::default_ua())
  }
  if (!exists("user_agent")) {
    warning("Ukjent miljoe. Denne funksjonene fungerer kun paa Dapla og i produksjonssonen")
  }
  return(user_agent)
}

user_agent()

# statbank_encrypt_request

statbank_encrypt_request <- function(laste_bruker) {
  prod <- stringr::str_detect(Sys.getenv('STATBANK_BASE_URL'), "i.ssb")

  if (prod == TRUE | Sys.getenv("RSTUDIO") == 1) {
    db <- "PROD"
  }

  if (prod == FALSE) {
    db <- "TEST"
  }


  # Prodsonen
  if (Sys.getenv('LOCAL_USER_PATH') == "") {
    encrypt_request <- httr::POST(
      Sys.getenv('STATBANK_ENCRYPT_URL'),
      httr::add_headers(
        "Content-Type" = "application/json"),
      body = list(message = getPass::getPass(paste0("Lastepassord for ",  laste_bruker, " (", db, "):"))),
      encode = "json"
    )

    # DAPLA
  } else {
    encrypt_request <- httr::POST(
      Sys.getenv('STATBANK_ENCRYPT_URL'),
      httr::add_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste0("Bearer ", httr::content(httr::GET(Sys.getenv('LOCAL_USER_PATH'), httr::add_headers('Authorization' = paste0('token ', Sys.getenv("JUPYTERHUB_API_TOKEN")))))$access_token)),
      body = list(message = getPass::getPass(paste0("Lastepassord for ",  laste_bruker, " (", db, "):"))),
      encode = "json"
    )
  }

  username_encryptedpassword <- openssl::base64_encode(paste0(laste_bruker, ":", httr::content(encrypt_request)$message))
  return(username_encryptedpassword)
}


#' Funksjon for aa hente uttaksbeskrivelsen til en statistikkbanktabell
#'
#' Funksjonen `statbank_uttaksbeskrivelse` henter uttaksbeskrivelsen til en statistikkbanktabell.
#'
#' @param tabell_id Karaktervektor med tabell ID til tabellen som det skal lastes opp data til.
#' @param laste_bruker Karaktervektor med seksjonens lastebruker.
#' @param ask Boolsk. Hvis `TRUE` blir man spurt om passord til lastebrukeren. Hvis `FALSE` maa `username_encryptedpassword` foerst vaere laget med funksjonen `statbank_encrypt_request`.
#' @param username_encryptedpassword Passord som er encryptert.
#' @param boundary Numerisk vektor med tallverdien som skiller de ulike filene i opplastningen. Trenger ikke aa endres.
#' @export
#'
#' @examples
#' \dontrun{
#' uttaksbeskrivelse <- statbank_uttaksbeskrivelse("13772", "LAST330")
#'
#' uttaksbeskrivelse$Huvudtabell
#' uttaksbeskrivelse$TabellId
#' uttaksbeskrivelse$DeltabellTitler$Filnavn
#' uttaksbeskrivelse$DeltabellTitler$Filtext

#' # Variabler
#' uttaksbeskrivelse$deltabller$deltabell
#' uttaksbeskrivelse$deltabller$variabler
#' uttaksbeskrivelse$deltabller$statistikkvariabler
#' uttaksbeskrivelse$deltabller$null_prikk_missing
#' uttaksbeskrivelse$deltabller$eksempel_linje
#'
#' # Kodelister
#' uttaksbeskrivelse$kodelister$kodeliste
#' uttaksbeskrivelse$kodelister$SumIALtTotalKode
#' uttaksbeskrivelse$kodelister$koder
#' }
#' @encoding UTF-8

statbank_uttaksbeskrivelse <- function(tabell_id,
                                       laste_bruker,
                                       ask = TRUE,
                                       username_encryptedpassword = "",
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
                                   'Accept' = '*/*',
                                   'User-Agent' = user_agent()
                                 ))

  uttaksbeksrivelse <- jsonlite::fromJSON(httr::content(uttaksbeksrivelse))
  return(uttaksbeksrivelse)
}

# statbank_body

statbank_body <- function(data,
                          tabell_id,
                          ask = TRUE,
                          username_encryptedpassword = "",
                          boundary = 12345) {

  data_all <- ""

  if (any(class(data) %in% c("data.frame", "tibble", "tbl_df", "tbl", "spec_tbl_df"))) {
    data <- list(data)
  }

  for (i in 1:length(data)) {

    filename <- statbank_uttaksbeskrivelse(tabell_id = tabell_id, ask = ask, username_encryptedpassword = username_encryptedpassword)$DeltabellTitler$Filnavn[i]
    start <- paste0("--", boundary, "\r\nContent-Disposition:form-data; filename=", filename, "\r\nContent-type:text/plain\r\n\r\n")

    data_1 <- data.frame(data[i])
    data_1 <- data_1 %>%
      dplyr::mutate_all(~format(., decimal.mark = ',')) %>% # endrer desimaltegn til komma
      dplyr::mutate_all(., stringr::str_trim) # fjerner whitespace
    data_1 <- do.call(paste, c(data_1[colnames(data_1)], sep = ";", collapse = "\r\n"))

    data_1 <- paste0(start, data_1)

    data_all <- paste0(data_all, data_1, sep = paste0("\r\n"))
  }

  data_all <- paste0(data_all, "--12345--\r\n")
  return(data_all)
}


# statbank_validering

statbank_validering <- function(data,
                                tabell_id,
                                laste_bruker,
                                username_encryptedpassword = "",
                                ask = FALSE) {

  uttaksbeskrivelse <- statbank_uttaksbeskrivelse(tabell_id = tabell_id, laste_bruker = laste_bruker, ask = ask, username_encryptedpassword = username_encryptedpassword)

  problemer_alle <- data.frame()
  dubletter_alle <- data.frame()

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

    if (!is.null(uttaksbeskrivelse$deltabller$null_prikk_missing)) {
      null_prikk_missing <-  data.frame(uttaksbeskrivelse$deltabller$null_prikk_missing[1])
      null_prikk_missing_kolonner <- paste0("kolonnenummer_", null_prikk_missing$kolonnenummer)
      kolonnenavn <- c(Klassifikasjonsvariabel, statistikkvariabler, null_prikk_missing_kolonner)
    }


    # Fjerner uten kodeliste #
    variabler_med_kodeliste <- variabler %>%
      dplyr::filter(Kodeliste_id != "-") # OBS

    # Legger til kolonnenavn #
    if (length(colnames(data_1)) != length(kolonnenavn)){
      print(paste0("OBS: antall kolonner i filen er ikke det samme som i uttaksbeskrivelsen: ", paste0(kolonnenavn, collapse = ", ")))
    }
    colnames(data_1) <- kolonnenavn # OBS

    # Dublettsjekk
    grupperingsvariabler <- c()
    for (j in 1:length(Klassifikasjonsvariabel)) {
      grupperingsvariabler <- c(grupperingsvariabler, colnames(data_1)[j])
    }

    dubletter <- data_1 %>%
      dplyr::group_by_at(grupperingsvariabler) %>%
      dplyr::filter(n()>1) %>%
      dplyr::mutate(across(everything(), as.character),
                    filnavn = uttaksbeskrivelse$DeltabellTitler$Filnavn[i],
                    problemer = "Dublett")

    data_1$filnavn <- uttaksbeskrivelse$DeltabellTitler$Filnavn[i]
    data_1$problemer <- ""

    for (k in variabler_med_kodeliste$Klassifikasjonsvariabel) {

      variabler_1 <- variabler %>%
        dplyr::filter(Klassifikasjonsvariabel == k)

      kodeliste_plassering <- which(uttaksbeskrivelse$kodelister$kodeliste == variabler_1$Kodeliste_id)
      kodeliste_navn <- uttaksbeskrivelse$kodelister$kodeliste[kodeliste_plassering]

      koder <- data.frame(uttaksbeskrivelse$kodelister$koder[kodeliste_plassering])

      data_1 <- data_1 %>%
        dplyr::mutate(problemer_2 = case_when(
          !!sym(k) %in% unique(as.character(koder$kode)) ~ "",
          TRUE ~ k
        ),
        problemer = paste0(problemer, problemer_2)) %>%
        dplyr::select(-problemer_2)
    }

    problemer <- data_1 %>%
      dplyr::filter(problemer != "")

    problemer_alle <- rbind(problemer_alle, problemer)
    dubletter_alle <- rbind(dubletter_alle, dubletter)

  }

  problemer_alle <- rbind(problemer_alle, dubletter_alle)

  if (nrow(problemer_alle)>0) {
    problemer_alle <- problemer_alle %>%
      dplyr::relocate(filnavn, problemer) %>%
      dplyr::distinct()
    return(problemer_alle)
  } else {
    print(paste0("Ingen ugyldige verdier eller dubletter for tabell ", tabell_id))
  }
}

#' Funksjon for aa hente initialene til paalogget bruker
#'
#' Funksjonen `initialer_funk` henter initialene til brukeren som er paalogget Jupyterlab/RStudio.
#'
#' @param lastefil Navn av fil til å laste opp
#' @returns Karaktervektor med initialer (vanligvis tre siffer)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' initialer_funk()
#'}
#' @encoding UTF-8

initialer_funk <- function(lastefil) {
  if (grepl("Bakke", user_agent())) {
    initialer <- Sys.getenv('USER')
  }
  if (grepl("Dapla", user_agent())) {
    initialer <- gsub("@ssb.no", "", Sys.getenv('JUPYTERHUB_USER'))
  }
  if (grepl("Onyxia", user_agent())) {
    initialer <- gsub("user-ssb-", "", Sys.getenv("KUBERNETES_NAMESPACE"))
  }
  if (!exists("initialer")) {
    warning("Finner ikke initialer")
  }
  return(initialer)
}



#' Funksjon for aa laste opp data fra Jupyterlab til Statistikkbanken
#'
#' Funksjonen `statbank_lasting` laster opp data fra Jupyterlab til Statistikkbanken. Det er mulig aa laste opp objekter direkte eller ved aa oppgi filsti til en eller flere .parquet-filer som er lagret i en Google Cloud Storage bucket (DAPLA) eller paa Linux (produksjonssonen).
#'
#' @param lastefil Objekt eller en liste med objekter som inneholder dataene som skal lastes opp. Det er ogsaa mulig aa oppgi en karaktervektor med filnavn til .parquet-filer lagret i en Google Cloud Storage bucket (DAPLA) eller paa Linux (produksjonssonen). Filstien oppgis under `lastefilsti`.
#' @param lastefilsti Karaktervektor med filsti til en Google Cloud Storage bucket hvor lastefilene (.parquet) er plassert. Dersom man laster opp objekter direkte kan denne staa blank.
#' @param tabell_id Karaktervektor med tabell ID til tabellen som det skal lastes opp data til.
#' @param laste_bruker Karaktervektor med seksjonens lastebruker.
#' @param publiseringsdato Karaktervektor med publiseringsdato. Formatet skal vaere "YYYY-MM-DD".
#' @param initialer Karaktervektor med initialer til personen som laster opp dataene (og som mottar e-post med lastelogg). Initialer hentes fra miljoevariabel i Jupyter saa dersom det ikke er en annen person enn brukeren som kjoerer programmet som skal motta e-post kan denne staa blank.
#' @param autooverskriv Numerisk vektor. Standardverdi er satt til 1.
#' @param autogodkjenn Numerisk vektor. 0: manuell, 1: automatisk (umiddelbart), 2: JIT (just-in-time). Standardverdi er satt til 2.
#' @param boundary Numerisk vektor med tallverdien som skiller de ulike filene i opplastningen. Trenger ikke aa endres.
#' @param ask Boolsk. Hvis `TRUE` blir man spurt om passord til lastebrukeren. Hvis `FALSE` maa `username_encryptedpassword` foerst vaere laget med funksjonen `statbank_encrypt_request`.
#' @param username_encryptedpassword Passord som er encryptert.
#' @param validering Boolsk. Valideringssjekk for aa oppdage vanlige lastefeil foer tabellen lastes opp.
#' @export
#'
#' @examples
#' \dontrun{
#' transfer_log <- statbank_lasting(lastefil = roykalderkj1,
#'                                  tabell_id = "05307",
#'                                  laste_bruker = "LAST330",
#'                                  publiseringsdato = "2022-12-31")
#' transfer_log
#'
#' transfer_log <- statbank_lasting(lastefil = "roykalderkj1.parquet",
#'                                  lastefilsti = "ssb-prod-spesh-personell-data-kilde",
#'                                  tabell_id = "05307",
#'                                  laste_bruker = "LAST330",
#'                                  publiseringsdato = "2022-12-31")
#' transfer_log
#' }
#' @encoding UTF-8
statbank_lasting <- function(lastefil,
                             lastefilsti = "",
                             tabell_id,
                             laste_bruker,
                             publiseringsdato,
                             initialer = initialer_funk(),
                             autooverskriv = 1,
                             autogodkjenn = 2,
                             boundary = 12345,
                             ask = TRUE,
                             username_encryptedpassword = "",
                             validering = TRUE) {

  if (class(tabell_id) == "numeric"){
    print("OBS: tabell-ID er numerisk, denne maa ha en karakterverdi (legg til fnutter)")
  }

  if (weekdays(as.POSIXlt(publiseringsdato)) %in% c("Saturday", "Sunday")) {
    print("OBS: publiseringsdato er satt til en helg. Er dette med vilje?")
  }

  if (as.POSIXlt(publiseringsdato)-as.POSIXlt(Sys.Date()) > 120) {
    print("OBS: publiseringsdato kan ikke settes mer enn 120 dager frem i tid")
  }


  if (any(class(lastefil) %in% c("data.frame", "tibble", "tbl_df", "tbl", "spec_tbl_df"))) {
    lastefil <- as.data.frame(lastefil)
  }

  if (class(lastefil) == "character" & length(lastefil)==1){
    lastefil <- read_SSB(paste0(lastefilsti, "/", lastefil))
    lastefil <- list(lastefil)
  }

  if (class(lastefil) == "character" & length(lastefil)>1){

    lastefil_alle <- list()
    for (i in lastefil){

      lastefil_1 <- read_SSB(paste0(lastefilsti, "/", i))
      lastefil_alle[[i]] <- lastefil_1
    }
    lastefil <- lastefil_alle
  }

  if (ask == TRUE){
    username_encryptedpassword <- statbank_encrypt_request(laste_bruker = laste_bruker)
  }


  if (validering == TRUE) {
    validering <- statbank_validering(data = lastefil,
                                      tabell_id = tabell_id,
                                      laste_bruker = laste_bruker,
                                      username_encryptedpassword = username_encryptedpassword,
                                      ask = FALSE)

    if (length(validering)>1) {
      print(paste0("Ugyldige verdier eller dubletter finnes for tabell ", tabell_id, ". Lasteoppdrag ikke startet."))
      return(validering)
      stop()
    }

  }


  uttaksbeskrivelse <- statbank_uttaksbeskrivelse(tabell_id = tabell_id, ask = FALSE, username_encryptedpassword = username_encryptedpassword)

  if ((any(class(lastefil) %in% c("data.frame", "tibble", "tbl_df", "tbl", "spec_tbl_df")) & length(uttaksbeskrivelse$DeltabellTitler$Filnavn) != 1) |
      (any(class(lastefil) %in% c("list") & length(uttaksbeskrivelse$DeltabellTitler$Filnavn) != length(lastefil)))){
    print(paste0("Antall lastefiler er ikke korrekt. I tabell ", tabell_id, " maa foelgende lastefiler spesifiseres: ",
                 paste0(uttaksbeskrivelse$DeltabellTitler$Filnavn, collapse = ", ")))
    stop()
  }

  body <- statbank_body(data = lastefil, tabell_id = tabell_id, ask = FALSE, username_encryptedpassword = username_encryptedpassword)

  url_transfer <- paste0(paste0(Sys.getenv('STATBANK_BASE_URL'), 'statbank/sos/v1/DataLoader?'),
                         "initialier=", initialer,
                         # "&hovedtabell=", tabell_id, # Skal ogsaa fungere med tabell_id (men gjoer ikke det i PROD)
                         "&hovedtabell=", uttaksbeskrivelse$Huvudtabell,
                         "&publiseringsdato=", publiseringsdato,
                         "&fagansvarlig1=", initialer,
                         "&fagansvarlig2=", initialer,
                         "&auto_overskriv_data=", autooverskriv,
                         "&auto_godkjenn_data=", autogodkjenn)


  transfer_log <- httr::POST(url_transfer,
                             httr::add_headers(
                               'Authorization' = paste0('Basic ', username_encryptedpassword),
                               'Content-Type' = paste0('multipart/form-data; boundary=', boundary),
                               'Connection' = 'keep-alive',
                               'Accept-Encoding' = 'gzip, deflate, br',
                               'Accept' = '*/*'),
                             'User-Agent' = user_agent(),
                             body = list(raw = body))


  if (httr::content(transfer_log)$TotalResult$Status == "Success") {
    print("Lasting vellykket!")
    return(transfer_log)
    stop()
  }

  if (httr::content(transfer_log)$TotalResult$Status == "Failure" & try(grepl("brudd paa unik skranke", httr::content(transfer_log)$ItemResults[[1]]$Exception$Message))) {
    print("Lasting mislyktes. Kan skyldes at forrige opplasting ikke er ferdig. Vent noen minutter og proev igjen.")
    return(transfer_log)
  }

  if (httr::content(transfer_log)$TotalResult$Status == "Failure" & try(!grepl("brudd paa unik skranke", httr::content(transfer_log)$ItemResults[[1]]$Exception$Message))) {
    print("Lasting mislyktes")
    return(transfer_log)
  }
}
