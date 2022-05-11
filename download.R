library(rgbif)

download_gbif <- function(specie_name, decade=NULL, n_try=30, sleep_duration=30) {
  ref = ""
  specie_key <- name_backbone(name = specie_name, rank = "species")$speciesKey

  start_time <- Sys.time()
  if (is.null(decade)){
    ref <- paste0(gsub(" ", "_", specie_name))
    
    download_key <- occ_download(
      pred("taxonKey", specie_key),
      pred("country", "FR"),
      pred("hasCoordinate", TRUE)
    )
  } else {
    ref <- paste0(gsub(" ", "_", specie_name), "_", as.character(decade))
    
    download_key <- occ_download(
      pred("taxonKey", specie_key),
      pred("country", "FR"),
      pred("hasCoordinate", TRUE),
      pred_and(
        pred_gte("year", decade),
        pred_lt("year", decade + 10)
      )
    )
  }
  

  download_url <- paste0("http://api.gbif.org/v1/occurrence/download/request/",
                        download_key[1], ".zip")

  try_download <- try(download.file(url = download_url,
                                    destfile = paste0("./data/", ref, ".zip"),
                                    mode = "wb",
                                    quiet = TRUE),
                      silent = TRUE)

  n <- 1
  while (inherits(try_download, "try-error") & n < n_try) {
    print(paste("Le lien de telechargement n'est pas encore pret.",
                "Temps ecoule (min) :",
                round(as.numeric(paste(difftime(Sys.time(),
                                                start_time,
                                                units = "mins"))), 2)))
    Sys.sleep(sleep_duration)

    try_download <- try(download.file(url = download_url,
                                      destfile = paste0("./data/", ref, ".zip"),
                                      mode = "wb",
                                      quiet = TRUE),
                        silent = TRUE)
    n <- n + 1
    if (n == n_try) {
      stop("Le jeu de donnee n'a pas pu etre telecharge, verifiez les arguments et reassayez plus tard")
    }
  }
  unzip(paste0("./data/", ref, ".zip"),
        exdir = paste0("./data/", ref),
        overwrite = TRUE)
  unlink(paste0("./data/", ref, ".zip"))
}

download_gbif("Hirundo rustica", 1970)
download_gbif("Hirundo rustica", 2010)

download_gbif("Picus viridis", 1970)
download_gbif("Picus viridis", 2010)


download_gbif("Margaritifera auricularia")
