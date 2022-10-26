# statbank_encrypt_request #
statbank_encrypt_request <- function(lastebruker) {
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
username_encryptedpassword <- openssl::base64_encode(paste0(lastebruker, ":", httr::content(encrypt_request)$message))
return(username_encryptedpassword)
}



# statbank_uttaksbeskrivelse #
statbank_uttaksbeskrivelse <- function(tabell_id,
                                       lastebruker,
                                      ask = TRUE, 
                                      boundary = 12345) {
  if (ask == TRUE){
        username_encryptedpassword <- statbank_encrypt_request(lastebruker = lastebruker)
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

# statbank_body #
statbank_body <- function(data, tabell_id, ask = TRUE, boundary = 12345) {

data_all <- ""

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
    
 # statbank_transfer #   
 statbank_transfer <- function(data, 
                             tabell_id,
                             lastebruker,
                             publiseringsdato, 
                             initialer = gsub("@ssb.no", "", Sys.getenv('JUPYTERHUB_USER')), 
                             auto_overskriv_data = 1, 
                             auto_godkjenn_data = 2, 
                             boundary = 12345) {

username_encryptedpassword <- statbank_encrypt_request(lastebruker = lastebruker)
uttaksbeskrivelse <- statbank_uttaksbeskrivelse(tabell_id = tabell_id, ask = FALSE)
body <- statbank_body(data = data, tabell_id = tabell_id, ask = FALSE)

url_transfer <- paste0(paste0(Sys.getenv('STATBANK_BASE_URL'), 'statbank/sos/v1/DataLoader?'), 
                       "initialier=", initialer,
                       "&hovedtabell=", uttaksbeskrivelse$Huvudtabell, 
                       "&publiseringsdato=", publiseringsdato, 
                       "&fagansvarlig1=", initialer, 
                      "&fagansvarlig2=", initialer, 
                      "&auto_overskriv_data=", auto_overskriv_data,
                      "&auto_godkjenn_data=", auto_godkjenn_data) 


transfer_log <- httr::POST(url_transfer,
                            httr::add_headers(
                              'Authorization' = paste0('Basic ', username_encryptedpassword), 
                              'Content-Type' = paste0('multipart/form-data; boundary=', boundary),
                              'Connection' = 'keep-alive',
                             'Accept-Encoding' = 'gzip, deflate, br',
                              'Accept' = '*/*'),
                             body = list(raw = body))
    
    return(transfer_log)
    }