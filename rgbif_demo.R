library(rgbif)
library(raster)

Helianthus <- name_backbone(name="Helianthus",
                            rank="genus",
                            kingdom="plants")

out <- name_lookup(query="mammalia")

occ_search(scientificName = "Ursus americanus", limit = 20)

key <- name_suggest(q="Ursus americanus", rank = "species")$data$key

occ_search(taxonKey = key, limit = 20)

occ_get(key = key)

res <- occ_search(geometry = "POLYGON((30.1 10.1, 10 20, 20 40, 40 40, 30.1 10.1))", limit = 50)
res %>% occ_issues(cdround)
res %>% occ_issues(-mdatunl)

denmark_code <- isocodes[grep("Denmark", isocodes$name), "code"]
occ_count(country = denmark_code)


mapped <- map_fetch(taxonKey = 2480498, year = 2000:2017)
plot(mapped)


occ_download(pred("taxonKey", 2436775))
d <- occ_download_get('') %>%
  occ_download_import()



