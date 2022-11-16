

#' Uploading to Statbank
#' Function for uploading a file in .dat format to the Norwegian Statistics Bank
#'
#' @param laste_dbase Database for loading up file. Choose between "TEST", "QA" or "PROD"
#' @param laste_bruker Oracle use ID for loading to StatBank
#' @param lastefilsti Path to where the .dat file is located
#' @param lastefil Name of the .dat file for loading (case sensitive)
#' @param hovedtabell Name of the tabell for publication
#' @param publiseringsdato Date for publication. This should be in the format 'yyyy-mm-dd'. For example "2021-02-28".
#' @param mailto Initials (three letters) for the person to send confirmation email to. SSB initials. Default is user initials.
#' @param autooverskriv If copies should be overwritten. 0 = no, 1 = yes (default)
#' @param autogodkjenn How data should be approved. 0 = Manual approval, 1 = Automatic (default), 2 = Just-in-time
#' @param timeout Length of time to wait for script to run before stopping. Default 10 (seconds)
#' @export
statbank_lasting <- function(lastefil,
                             tabell_id = "",
                             laste_dbase = "", # OBS?
                             laste_bruker,
                             lastefilsti = "",
                             hovedtabell = "",
                             publiseringsdato,
                             mailto = "",
                             autooverskriv = 1,
                             autogodkjenn = 1, # OBS: sett denne til 2 (JIT)?
                             timeout = 10,
                             validering = TRUE,
                             ask = TRUE){

  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {

    if (missing(mailto)){
      mailto <- gsub("@ssb.no", "", Sys.getenv('JUPYTERHUB_USER'))
    }
    transfer_log <- statbank_transfer(lastefil = lastefil,
                                      lastefilsti = lastefilsti,
                                      tabell_id = tabell_id,
                                      laste_bruker = laste_bruker,
                                      publiseringsdato = publiseringsdato,
                                      initialer = mailto,
                                      autooverskriv = autooverskriv,
                                      autogodkjenn = autogodkjenn,
                                      ask = ask)

    return(transfer_log)


  }

  # check laste_db
  if (!laste_dbase %in% c("PROD", "TEST", "QA")){
    stop("laste_db should be one of 'TEST', 'PROD' og 'QA")
  }

  # check server
  if (grepl("p3", Sys.info()['nodename'])){
    stop("Tables can't be uploaded from p3 server because of the firewall.
         Try uploading from python-03 or one of the sas servers.")
  }

  if (missing(mailto)){
    mailto <- Sys.info()["user"]
  }

  # check if log file exists and remove
  tmp <- list.files("/tmp/")
  logname <- paste0(laste_bruker, "overforing_R.log")
  if (logname %in% tmp){
    system(paste0("rm /tmp/", logname))
    Sys.sleep(1)
  }

  # Check for environment STATBAS path
  if (Sys.getenv('STATBAS') == ""){
    Sys.setenv('STATBAS' = "/ssb/stamme04/statbas")
  }

  # get passord
  passord <- getPass::getPass(paste0("Passord for laste_bruker (", laste_bruker, "):") )

  # call perl file
  call_perl(laste_dbase = laste_dbase,
            laste_bruker = laste_bruker,
            lastefilsti = lastefilsti,
            lastefil = lastefil,
            passord = passord)

  # Wait for process and log file to be generated
  Sys.sleep(2)
  count <- 1
  while(!paste0(laste_bruker,"overforing_R.log") %in% list.files("/tmp")){
    Sys.sleep(1)
    count <- count + 1
    if (count > (timeout - 2)) break()
  }

  # Check perl file ran correctly
  if(paste0(laste_bruker,"overforing_R.log") %in% list.files("/tmp")){
    infile <- read.delim(paste0("/tmp/",laste_bruker, "overforing_R.log"), stringsAsFactors = F)
    if(!grepl("Programmet er ferdig", infile)){
      system(paste0("rm /tmp/", laste_bruker, "overforing_R.log"))
      stop(paste("The perl file was not able to be run because of the following error:\n", infile))
    } else {
      cat("Perl script has been run successfully! \n")
    }
  } else {
    system(paste0("rm /tmp/", laste_bruker, "overforing_R.log"))
    stop("Perl script has timed out")
  }

  # Call bash file
  call_shell(laste_dbase = laste_dbase,
             laste_bruker = laste_bruker,
             hovedtabell = hovedtabell,
             publiseringsdato = publiseringsdato,
             mailto = mailto,
             passord = passord,
             autooverskriv = autooverskriv,
             autogodkjenn = autogodkjenn)

  # Remove temporary files
  system(paste0("rm /tmp/", laste_bruker, "overforing_R.log"))

}


#' Call system perl file
#' Create and run call for system perl file
#'
#' @param laste_dbase Database for loading up file. Choose between "TEST", "QA" or "PROD"
#' @param laste_bruker Oracle use ID for loading to StatBank
#' @param lastefilsti Path to where the .dat file is located
#' @param lastefil Name of the .dat file for loading (case sensitive)
#' @param passord String variable with the password for the laste_bruker
#' @keywords internal
call_perl <- function(laste_dbase,
                      laste_bruker,
                      lastefilsti,
                      lastefil,
                      passord){
  # path to perl file
  path <- file.path("/ssb/stamme04/statbas/system/prog/filoverforing", laste_dbase, "overforing.pl")

  # where temporary files are saved
  temp1 <- "-o /tmp -a"

  # file and path to .dat file for uploading
  filpath <- file.path(lastefilsti, lastefil)
  if (length(lastefil) > 1){
    filpath <- paste(filpath, collapse=" ")
  }

  # name and path to log file
  logfil <- paste0("/tmp/", laste_bruker,"overforing_R.log")

  # create and run call
  system(paste(path, temp1, laste_bruker, passord, filpath, ">", logfil, "2>&1"))
}

#' System shell call
#' Create and run call to shell file
#' @param laste_dbase Database for loading up file. Choose between "TEST", "QA" or "PROD"
#' @param laste_bruker Oracle use ID for loading to StatBank
#' @param hovedtabell Name of the tabell for publication
#' @param publiseringsdato Dato for publisering
#' @param mailto Initialer av bruker for å sende epost til
#' @param autooverskriv If dubbletter should be overwritten. 0 = no, 1 = yes (default)
#' @param autogodkjenn How data should be approved. 0 = Manual approval, 1 = Automatic (default), 2 = Just-in-time
#' @keywords internal
call_shell <- function(laste_dbase,
                       laste_bruker,
                       hovedtabell,
                       publiseringsdato,
                       mailto,
                       passord,
                       autooverskriv = 1,
                       autogodkjenn = 1){

  # file path to shell file
  filsti <- file.path("/ssb/stamme04/statbas/system/prog/bestillLasting",laste_dbase,"bestillLasting.sh")

  # format date
  publiseringsdato <- as.Date(publiseringsdato)
  if (publiseringsdato < Sys.Date()){
    stop("Publishing date is set before current date")
  }
  publiseringsdato <- format(publiseringsdato, "%Y.%m.%d")

  # paste call
  shell_call <- system(paste(filsti,
                             laste_bruker,
                             passord,
                             hovedtabell,
                             publiseringsdato, "08:00",
                             mailto,
                             autooverskriv,
                             autogodkjenn
  ), intern = TRUE
  )
  cat("Shell script has been run \n")
}
