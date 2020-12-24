##' read ziff files
##' @param speciescode numeric species code
##' @param year vector of years to read
##' @param language en or fr
##' @param ... arguments to list.files (e.g., path)
##' @import readr lubridate 
##' @importFrom  data.table rbindlist
##' @details 
#' Reads ziff files in csv format. The original names of the files are presumed (Version_totale_...).
#' 
#' Available from \\ dcqcimlna01a \ BD_Peches \ Ziff \ Format CSV \ Fichiers de donnees
#' 
#' See find.species to get the species names/codes used in the database.
#' 
#' Notes:
#'  \enumerate{
#'    \item sub opanos all capitalised to ensure consistency with other databases
#'    \item{year / doy / month are from catch, unless info only available for landings }
#' }
##' @rdname read.ziff
##' @export
read.ziff <- function(speciescode,species,year=NULL,language=c('en','fr'),...){

   language <- match.arg(language)

   files <- list.files(pattern = "^Version_totale_", full.names = TRUE, ignore.case = TRUE, ...)
   if(all(is.na(files))) stop('no files with string Version_totale in directory ')
   ### if only some years; don't read in all files
   if(!is.null(year)){
       if(!is.numeric(year)) stop('year needs to be a numeric vector')

       ys <- sapply(files,function(x){y <- gsub(".*totale_(.+).csv", "\\1", x)})
       ys <- cbind(start = as.numeric(substring(ys,1,4)),
                        end   =  as.numeric(substring(ys,5,8)))  
       id <- apply(ys,1, function(z) any(sapply(year, function(x) x %in% z[1]:z[2])))
       files <- files[id]
   }
   ### read files
   ziff <- lapply(1:length(files),function(x){
       print(files[x])
       z <- read.csv(files[x])
       z <- z[z$cod_esp %in% speciescode,]
       return(z)
       })
   ziff <- rbindlist(ziff,fill=TRUE)                # faster than rbind.fill or do.call
   ziff <- as.data.frame(ziff)
   
   ### clean up 
   # 1) opano / div
   ziff[ziff$opano %in% c("","XXX"),'opano'] <- NA
   ziff[ziff$div %in% c("","XXX"),'div'] <- NA
   ziff$opano <- toupper(ziff$opano)                   # too messy to begin with. nafo also uses upper case in data
   ziff$div <- toupper(ziff$div)                       # idem
   # 2)  dates
   ziff$date_cap <- ymd(ziff$date_cap)
   ziff$date_deb <- ymd(ziff$date_deb)
   ziff$annee <- with(ziff, ifelse(is.na(date_cap), year(date_deb), year(date_cap)))
   if(!is.null(year)) ziff <- ziff[ziff$annee %in% year,]
   ziff$anneeGestion <- ifelse(month(ziff$date_deb)<=4 & day(ziff$date_deb)<=15 & ziff$annee>1999,ziff$annee-1,ziff$annee)    # starts on May 15th May since 2000
   ziff$doy_cap <- yday(ziff$date_cap)
   ziff$doy_deb <- yday(ziff$date_deb)
   ziff$doy     <-  with(ziff, ifelse(is.na(doy_cap), doy_deb, doy_cap))
   ziff$mois_cap <- month(ziff$date_cap)
   ziff$mois_deb <- month(ziff$date_deb)
   ziff$mois     <- with(ziff, ifelse(is.na(mois_cap), mois_deb, mois_cap))
   ziff$trim_cap <- quarter(ziff$date_cap)
   ziff$trim_deb <- quarter(ziff$date_deb)
   # 3) regions
   #?? not sure about this
   
   # 4) province
   provs <- data.frame(fr=c('inconnu','N-E','N-B','IPE','QC','T-N'),en=c('unknown','NS','NB','PEI','QC','NL'))
   ziff$prov_att <- provs[,language][floor(ziff$port_att/10000)+1]
   ziff$prov_att[is.na(ziff$prov_att)] <- provs[1,language]
   ziff$prov_deb <- provs[,language][floor(ziff$port_deb/10000)+1]
   ziff$prov_deb[is.na(ziff$prov_deb)] <- provs[1,language]
   
   # 5) unit
   ziff[ziff$un_mes=='P'&!is.na(ziff$un_mes),'pd_deb'] <- ziff[ziff$un_mes=='P'&!is.na(ziff$un_mes),'pd_deb']* 0.453592
   levels(ziff$un_mes)[levels(ziff$un_mes)=='P'] <- 'KfromP'
   
   ### general stuff
   ziff[ziff==0] <- NA                                                   # all 0s are NA 
   ziff[] <- lapply(ziff, function(x) if(is.factor(x)) factor(x) else x) # drop unused factor levels
   
   ### translate cols
   if(language=='en') colnames(ziff) <- c(ziff_meta_csv$en,'year','year.management','catch.doy','land.doy','doy','catch.month','land.month','month','catch.trim','land.trim','prov.home','prov.land')
   
   ### add info for species, gear types, tonnage class
   ziff <- merge(ziff, ziff_species, by.x = ziff_meta_csv[ziff_meta_csv$fr=='cod_esp',language], by.y='cod_esp',all.x = T)
   ziff <- merge(ziff, ziff_gear, by.x = ziff_meta_csv[ziff_meta_csv$fr=='engin',language], by.y='engin', all.x = T)
   ziff <- merge(ziff, ziff_tonnage, by.x = ziff_meta_csv[ziff_meta_csv$fr=='cl_ton',language], by.y='cl_ton', all.x = T)
 
    return(ziff)
}


