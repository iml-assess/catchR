##' read ZIFF files
##' @param sp Numeric species code (ex: 100 for Atlantic cod). Can also be a vector of multiple numeric species codes.
##' @param path Directory where files to read are located. Ex: "//dcqcimlna01a/BD_Peches/Ziff/Format CSV/Fichiers de données/".
##' @param year Vector of years to read (ex: 2015:2020). NULL by default, meaning the function will keep data from all available years.
##' @param language Language to use for column names, either "fr" (default) or "en".
##' @import readr lubridate
##' @importFrom  data.table rbindlist
##' @details
#' This function reads Zonal Interchange File format (ZIFF) data from .csv files "Version_totale", available from: //dcqcimlna01a/BD_Peches/Ziff/Format CSV/Fichiers de données/.
#' 
#' Notes:
#'  \enumerate{
#'    \item{NAFO Divisions and Subareas all capitalised to ensure consistency with other databases}
#'    \item{year, trimester and month values are from catch dates. If no catch date is provided, the landing date is used instead.}
#'    \item{Data can be read in both languages (column names, province names) but catch-at-age functions require the English version.}
#' }
##' @rdname read.ziff
##' @export
read.ziff <- function(sp, path, year = NULL, language = c("fr", "en")){

   language <- match.arg(language)

   # 1) Files to read
   files <- list.files(pattern = "^Version_totale_", full.names = TRUE, ignore.case = TRUE, path = path)
   if(all(is.na(files))) stop('No files with string "Version_totale" in directory.')
   # if only some years are requested, the function won't read all files
   if(!is.null(year)){
      if(!is.numeric(year)) stop('Argument "year" needs to be a numeric vector.')
      
      ys <- sapply(files, function(x){y <- gsub(".*totale_(.+).csv", "\\1", x)})
      ys <- cbind(start = as.numeric(substring(ys, 1, 4)),
                  end   =  as.numeric(substring(ys, 5, 8)))
      id <- apply(ys, 1, function(z) any(sapply(year, function(x) x %in% z[1]:z[2])))
      files <- files[id]
   }
   # 2) Files reading
   ziff <- lapply(1:length(files), function(x){
            print(files[x])
            z <- read.csv(files[x])
            z <- z[z$cod_esp %in% sp, ] # keep only the lines of each file for the species of interest.
            return(z)
            })
   ziff <- rbindlist(ziff, fill = TRUE) # faster than rbind.fill or do.call
   ziff <- as.data.frame(ziff)

   # 3) data clean up
   # opano / div
   ziff[ziff$opano %in% c("","XXX"), 'opano'] <- NA
   ziff[ziff$div %in% c("","XXX"), 'div'] <- NA
   ziff$opano <- toupper(ziff$opano) # too messy to begin with. nafo also uses upper case in data
   ziff$div <- toupper(ziff$div) # idem

   # dates
   ziff$date_cap <- ymd(ziff$date_cap)
   ziff$date_deb <- ymd(ziff$date_deb)
   ziff$annee <- with(ziff, ifelse(is.na(date_cap), year(date_deb), year(date_cap)))
   if(!is.null(year)) ziff <- ziff[ziff$annee %in% year, ]
   ziff$annee_gestion <- ifelse(month(ziff$date_deb) <= 4 & day(ziff$date_deb) <= 15 & ziff$annee > 1999, ziff$annee - 1, ziff$annee) # starts on May 15th May since 2000
   ziff$mois_cap <- month(ziff$date_cap)
   ziff$mois_deb <- month(ziff$date_deb)
   ziff$mois     <- with(ziff, ifelse(is.na(mois_cap), mois_deb, mois_cap))
   ziff$trim_cap <- quarter(ziff$date_cap)
   ziff$trim_deb <- quarter(ziff$date_deb)
   ziff$trim <- with(ziff, ifelse(is.na(trim_cap), trim_deb, trim_cap))

   # province
   provs <- data.frame(fr = c('Inconnu', 'N-É', 'N-B', 'IPE', 'QC', 'T-N'), en = c('Unknown', 'NS', 'NB', 'PEI', 'QC', 'NL'))
   ziff$prov_att <- provs[, language][floor(ziff$port_att / 10000) + 1]
   ziff$prov_att[is.na(ziff$prov_att)] <- provs[1, language]
   ziff$prov_deb <- provs[, language][floor(ziff$port_deb / 10000) + 1]
   ziff$prov_deb[is.na(ziff$prov_deb)] <- provs[1, language]

   # unit
   ziff[ziff$un_mes == 'P'& !is.na(ziff$un_mes), 'pd_deb'] <- ziff[ziff$un_mes == 'P' & !is.na(ziff$un_mes), 'pd_deb'] * 0.453592
   levels(ziff$un_mes)[levels(ziff$un_mes) == 'P'] <- 'KfromP' # Converted langings in pounds to landings in kilograms

   # general stuff
   ziff[ziff == 0] <- NA # all 0s are NA
   ziff[] <- lapply(ziff, function(x) if(is.factor(x)) factor(x) else x) # drop unused factor levels

   # translate dataframe columns
   if(language == 'en') colnames(ziff) <- c(ziff_meta_csv$en,'year', 'year.management', 'catch.month', 'land.month', 'month', 
                                            'catch.trim', 'land.trim', 'trim', 'prov.home', 'prov.land')

   # add info for species, gear types, tonnage class
   ziff <- merge(ziff, ziff_species, by.x = ziff_meta_csv[ziff_meta_csv$fr == 'cod_esp', language], by.y = 'cod_esp', all.x = T)
   ziff <- merge(ziff, ziff_gear, by.x = ziff_meta_csv[ziff_meta_csv$fr == 'engin', language], by.y = 'engin', all.x = T)
   ziff <- merge(ziff, ziff_tonnage, by.x = ziff_meta_csv[ziff_meta_csv$fr == 'cl_ton', language], by.y = 'cl_ton', all.x = T)

   return(ziff)
}
