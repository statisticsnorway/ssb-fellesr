

#' Funksjon for å laste opp data fra DAPLA til Statistikkbanken
#'
#' Funksjonen `statbank_transfer` laster opp data fra DAPLA til Statistikkbanken. Det er mulig å laste opp objekter direkte eller ved å oppgi filsti til .dat-filer som er lagret i en Google Cloud Storage bucket.
#'
#' @param lastefil Objekt eller en liste med objekter som inneholder dataene som skal lastes opp. Det er også mulig å oppgi en karaktervektor med filnavn til .dat-filer lagret i en Google Cloud Storage bucket. Filstien oppgis under `lastefilsti`.
#' @param lastefilsti Karaktervektor med filsti til en Google Cloud Storage bucket hvor lastefilene (.dat) er plassert. Dersom man laster opp objekter direkte kan denne stå blank.
#' @param tabell_id Karaktervektor med tabell ID til tabellen som det skal lastes opp data til.
#' @param laste_bruker Karaktervektor med seksjonens lastebruker.
#' @param publiseringsdato Karaktervektor med publiseringsdato. Formatet skal være "ÅÅÅÅ-MM-DD".
#' @param initialer Karaktervektor med initialer til personen som laster opp dataene (og som mottar e-post med lastelogg). Initialer hentes fra miljøvariabel i Jupyter så dersom det ikke er en annen person enn brukeren som kjører programmet som skal motta e-post kan denne stå blank.
#' @param autooverskriv Numerisk vektor. Standardverdi er satt til 1.
#' @param autogodkjenn Numerisk vektor. 0: manuell, 1: automatisk (umiddelbart), 2: JIT (just-in-time). Standardverdi er satt til 2.
#' @param boundary Numerisk vektor med tallverdien som skiller de ulike filene i opplastningen. Trenger ikke å endres.
#' @param ask Boolsk. Hvis `TRUE` blir man spurt om passord til lastebrukeren. Hvis `FALSE` må `username_encryptedpassword` først være laget med funksjonen `statbank_encrypt_request`.
#' @param validering
#'
#' @examples
#' \dontrun{
#' transfer_log <- statbank_lasting(lastefil = roykalderkj1,
#'                                  tabell_id = "05307",
#'                                  laste_bruker = "LAST330",
#'                                  publiseringsdato = "2022-12-31")
#' transfer_log
#'
#' transfer_log <- statbank_lasting(lastefil = "roykalderkj1.dat",
#'                                  lastefilsti = "ssb-prod-spesh-personell-data-kilde",
#'                                  tabell_id = "05307",
#'                                  laste_bruker = "LAST330",
#'                                  publiseringsdato = "2022-12-31")
#' transfer_log
#'}
#'@encoding UTF-8

statbank_transfer <- function(lastefil,
                              lastefilsti = "",
                              tabell_id,
                              laste_bruker,
                              publiseringsdato,
                              initialer = gsub("@ssb.no", "", Sys.getenv('JUPYTERHUB_USER')),
                              autooverskriv = 1,
                              autogodkjenn = 2,
                              boundary = 12345,
                              ask = TRUE,
                              validering = TRUE) {


  if (ask == TRUE){
    username_encryptedpassword <- statbank_encrypt_request(laste_bruker = laste_bruker)
  }

  if (weekdays(as.POSIXlt(publiseringsdato)) %in% c("Saturday", "Sunday")) {
    print("OBS: publiseringsdato er satt til en helg")
  }

  if (class(lastefil)[1] %in% c("data.frame", "tibble", "tbl_df", "tbl")) {
    lastefil <- as.data.frame(lastefil)
  }

  if (class(lastefil) == "character" & length(lastefil)==1){
    lastefil <- read_SSB(paste0(lastefilsti, "/", lastefil), delim = ";",  col_names = FALSE)
    lastefil <- list(lastefil)
  }

  if (class(lastefil) == "character" & length(lastefil)>1){

    lastefil_alle <- list()
    for (i in lastefil){

      lastefil_1 <- read_SSB(paste0(lastefilsti, "/", i), delim = ";",  col_names = FALSE)
      lastefil_alle[[i]] <- lastefil_1
    }
    lastefil <- lastefil_alle
  }

  if (validering == TRUE) {
    validering <- statbank_validering(data = lastefil,
                                      tabell_id = tabell_id,
                                      laste_bruker = laste_bruker,
                                      ask = FALSE)

    if (length(validering)>1) {
      print("Ugyldige verdier finnes i kodeliste. Lasteoppdrag ikke startet.")
      return(validering)
      stop()
    }

  }


  uttaksbeskrivelse <- statbank_uttaksbeskrivelse(tabell_id = tabell_id, ask = FALSE)
  body <- statbank_body(data = lastefil, tabell_id = tabell_id, ask = FALSE)

  url_transfer <- paste0(paste0(Sys.getenv('STATBANK_BASE_URL'), 'statbank/sos/v1/DataLoader?'),
                         "initialier=", initialer,
                         # "&hovedtabell=", tabell_id, # Skal også fungere med tabell_id (men gjør ikke det i PROD)
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
                             body = list(raw = body))


  if (httr::content(transfer_log)$TotalResult$Status == "Success") {
    print("Lasting vellykket!")
    return(transfer_log)
    stop()
  }

  if (httr::content(transfer_log)$TotalResult$Status == "Failure" & try(grepl("brudd på unik skranke", httr::content(transfer_log)$ItemResults[[1]]$Exception$Message))) {
    print("Lasting mislyktes. Kan skyldes at forrige opplasting ikke er ferdig. Vent noen minutter og prøv igjen.")
    return(transfer_log)
  }

  if (httr::content(transfer_log)$TotalResult$Status == "Failure" & try(!grepl("brudd på unik skranke", httr::content(transfer_log)$ItemResults[[1]]$Exception$Message))) {
    print("Lasting mislyktes")
    return(transfer_log)
  }

  # return(transfer_log)

}
