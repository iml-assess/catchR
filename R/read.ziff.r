##' read ziff files
##' @param speciescode species code (numeric, for total csv file)
##' @param species species name (character string, for species specific dat files)
##' @param year vector of years to read (all by default)
##' @param language en or fr (english by default)
##' @param format data format (old dat or new csv)
##' @param ... arguments to list.files (e.g., path)
##' @import readr lubridate 
##' @importFrom  data.table rbindlist
##' @details Two ziff formats are supported; \cr
##' old dat format: z_180_....espece.dat -> S:/Ziff/POISSON/DONNEES/ \cr
##' new csv format: Version_totale_....csv -> \\ dcqcimlna01a \ BD_Peches \ Ziff \ Format CSV \ Fichiers de données
##' @rdname read.ziff
##' @export
read.ziff <- function(speciescode,species,year=NULL,language=c('en','fr'),format=c('csv','dat'),...){

    language <- match.arg(language)
    format <- match.arg(format)
    
    switch(format,
           "csv" = {
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
               
               ziff2 <<-ziff # security check (to remove!!!!)
               
               ### clean up 
               # 1) opano / div
               ziff[ziff$opano %in% c("","XXX"),'opano'] <- NA
               ziff[ziff$div %in% c("","XXX"),'div'] <- NA
               ziff$opano <- toupper(ziff$opano)                   # too messy to begin with. nafo also uses upper case in data
               ziff$div <- toupper(ziff$div)                       # idem
               # 2)  dates
               ziff$date_cap <- ymd(ziff$date_cap)
               ziff$date_deb <- ymd(ziff$date_deb)
               ziff$annee <- with(ziff, ifelse(!is.na(date_cap), year(date_cap), year(date_deb)))
               if(!is.null(year)) ziff <- ziff[ziff$annee %in% year,]
               ziff$anneeGestion <- ifelse(month(ziff$date_deb)<=4 & day(ziff$date_deb)<=15 & ziff$annee>1999,ziff$annee-1,ziff$annee)    #commencant le 15 mai depuis 2000 (utilise debarquement pour pas avoir ? g?r?r les NA)
               ziff$doy_cap <- yday(ziff$date_cap)
               ziff$doy_deb <- yday(ziff$date_deb)
               ziff$mois_cap <- month(ziff$date_cap)
               ziff$mois_deb <- month(ziff$date_deb)
               ziff$mois     <- ifelse(is.na(ziff$mois_deb),ziff$mois_cap,ziff$mois_cap)
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
               if(language=='en') colnames(ziff) <- c(ziff_meta_csv$en,'year','year.management','catch.doy','land.doy','catch.month','land.month','month','catch.trim','land.trim','prov.home','prov.land')
               
               ### add info for species, gear types, tonnage class
               ziff <- merge(ziff, ziff_species, by.x = ziff_meta_csv[ziff_meta_csv$fr=='cod_esp',language], by.y='cod_esp',all.x = T)
               ziff <- merge(ziff, ziff_gear, by.x = ziff_meta_csv[ziff_meta_csv$fr=='engin',language], by.y='engin', all.x = T)
               ziff <- merge(ziff, ziff_tonnage, by.x = ziff_meta_csv[ziff_meta_csv$fr=='cl_ton',language], by.y='cl_ton', all.x = T)
           },
           "dat" = {
               files <- list.files(pattern=".dat",ignore.case = TRUE,...)
               files_sp <-  unique(tolower(gsub(".*_(.+).dat", "\\1", files,ignore.case = TRUE)))
               
               ### species
               species <- tolower(iconv(species, to='ASCII//TRANSLIT'))
               if(missing(species)|!species %in% files_sp) 
                   stop(paste('species available:',paste(files_sp,collapse=', ')))
               files_my <- grep(species,files,ignore.case = TRUE,value=TRUE)
               
               ### year
               if(!is.null(year)){
                   if(!is.numeric(year)) stop('year needs to be a numeric vector')
                   year <- as.character(year)
                   year <- substr(year,3,4)
                   ys <- substr(gsub("z180_(.+).*", "\\1", files_my,ignore.case = TRUE),1,2)
                   files_my <- files_my[which(ys %in% year)]
               }
               
               ### ziff_meta
               nam <- ziff_meta_dat[,language]
               wstart <- as.numeric(gsub("(.+)-.*", "\\1", ziff_meta_dat[,'col']))
               wend <- as.numeric(gsub(".*-(.+)", "\\1", ziff_meta_dat[,'col']))
               
               ### read files
               ziff <- lapply(1:length(files_my),function(x){
                   suppressWarnings(
                       suppressMessages(
                           read_fwf(paste0(dir,files_my[x]), fwf_positions(wstart,wend,col_names=nam))
                       )
                   )
               })
               ziff <- do.call('rbind',ziff)
               ziff <- as.data.frame(ziff)
               
               ### clean up
               # nafo zones and provinces
               ziff[,8] <- as.numeric(ziff[,8])
               provs <- data.frame(fr=c('inconnu','N-E','N-B','IPE','QC','T-N'),en=c('unknown','NS','NB','PEI','QC','NL'))
               ziff$province <- provs[,language][floor(ziff[,8][[1]]/10000)+1] #utilise 'NA' au lieu de NA pour fonction aggregate
               ziff$province[is.na(ziff$province)] <- provs[1,language]
               
               ## dates
               ziff$debDate <- ymd(do.call(paste, c(ziff[,14:16], sep="-")))
               ziff$captDate <- ymd(do.call(paste, c(ziff[,43:45], sep="-")))
               ziff$debDOY <- as.numeric(format(ziff$debDate, "%j"))
               ziff$captDOY <- as.numeric(format(ziff$captDate, "%j"))
               ziff$annee <- ifelse(is.na(ziff$debDate),year(ziff$captDate),year(ziff$debDate))
               ziff$anneeGestion <- ifelse(month(ziff$debDate)<=4 & day(ziff$debDate)<=15 & ziff$annee>1999,ziff$annee-1,ziff$annee)    #commencant le 15 mai depuis 2000 (utilise debarquement pour pas avoir ? g?r?r les NA)
               
               # #ajustements pour format dans fichier d'origine
               ziff[,31] <- ziff[,31]/100 
               ziff[,38] <- ziff[,38]/10
               ziff[,39] <- ziff[,39]/10 
               ziff[,40] <- ziff[,40]/100 
               ziff[,42] <- ziff[,42]/10
               
               ## uniformiser mesure de quantit? d?barqu? a Kilogramme (dans la forme d?barqu?e)
               ziff$qteDebKg <- ziff[,30]
               ziff$qteDebKg[ziff[,29]=='P'&!is.na(ziff[,29])] <- ziff[ziff[,29]=='P'&!is.na(ziff[,29]),30] * 0.453592 #attention, ily a un 'N'???
               
               if(language=='en') colnames(ziff)[59:65] <- c('land.date','catch.date','land.doy','catch.doy','year','year.management','land.quantity.kg')
               
               ## mise en forme des donn?es de position
               ziff[,c(48:49,55:56)] <- type.convert(ziff[,c(48:49,55:56)])
               ziff$latitude <- floor(ziff[,55]/10000)+ziff[,55]%%10000/6000
               ziff$latitude[is.na(ziff$latitude)] <- (floor(ziff[,48]/100)+ziff[,48]%%100/60)[is.na(ziff$latitude)]
               ziff$longitude <- -floor(ziff[,56]/10000)-ziff[,56]%%10000/6000
               ziff$longitude[is.na(ziff$longitude)] <- (-floor(ziff[,49]/100)-ziff[,49]%%100/60)[is.na(ziff$longitude)] #si pas long6, utiliser long4
               
               ziff$quadY <- sapply(ziff[,51], function(x){toupper(substr(x, start=1, stop=2))})
               ziff$quadY[ziff$quadY < 'A'] <- NA
               ziff$quadX <- sapply(ziff[,51], function(x){as.numeric(substr(x, start=3, stop=5))})
               ziff$quadX[ziff$quadX==0] <- NA
               
           },
           stop("format can only be dat or csv")
           )
   
    return(ziff)
}


