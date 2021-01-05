##' Read NAFO 21B data
##' @param path Path to which to download raw files.
##' @param year Vector of years to read (all by default).
##' @param species species name(s). Ex: 101 for Atlantic cod.
##' @param overwrite If overwrite = FALSE (default) and downloaded data is already at specified path, the function will just read the local data. If overwrite = TRUE, the function will over-write data.
##' @importFrom rvest html_nodes html_attr
##' @importFrom readr read_lines
##' @importFrom tidyr gather
##' @importFrom xml2 read_html
##' @importFrom data.table rbindlist fread
##' @details 
##' Download data from https://www.nafo.int/Data/Catch-Statistics.
##' 
##' See find.species to get the species names used in the database.
##' @rdname read.nafoB
##' @export
read.nafoB <- function(path, year = NULL, species = NULL, overwrite = FALSE){
    site <- 'https://www.nafo.int'
    page <- read_html(paste0(site, '/Data/Catch-Statistics'))
    
    links <- html_nodes(page, 'a') # find all links
    urls <- html_attr(links, 'href') # get the urls
    zips <- urls[grep('.zip', urls)] # get all zip files
    
    # year ranges
    ys <- sapply(zips, function(x){y <- gsub(".*b-(.+).zip", "\\1", x)})
    ys <- cbind(start = sub("(.*)-.*", "\\1", ys), end = sub(".*-(.*)", "\\1", ys))
    n <- nchar(ys)
    ys[n == 2] <- format(as.Date(ys[n == 2], format = "%y"), "%Y")
    ys <- type.convert(ys)
    ys[ys > 2050] <- ys[ys > 2050] - 100
    
    if(!is.null(year)){
        id <- apply(ys, 1, function(y) any(sapply(year, function(x) x %in% y[1]:y[2])))
        zips <- zips[id]
    }
    
    # prep
    if(missing(path)) stop('The function needs a path to which to download raw files.')
    dir.create(path, showWarnings = F) 
    mergeif <- function(x, y) if(any(colnames(x) %in% colnames(y))) merge(x, y, all.x = TRUE) else x

    # get the data
    nafo <- lapply(1:length(zips), function(z){
        zi <- sub(".*/(.*)", "\\1", zips[z])
        un <- paste0(path, gsub('.zip','', zi))
        sh <- gsub('.zip', '', zi)
        if(!dir.exists(un)|overwrite == TRUE){
            di <- paste0(path, zi)
            download.file(url = paste0(site, zips[z]), destfile = di)
            unzip(zipfile = di, exdir = un, junkpaths = FALSE)
            file.remove(di)
        }
        
        fi <- list.files(un, full.names = T, recursive = T)
        ma <- grep('NAFO', fi, value = T)
        main <-fread(ma, data.table = F, stringsAsFactors = F)
        names(main) <- tolower(names(main))
        names(main)[which(names(main) == 'catches')] <- 'month_nk' # some years it is catches. other files use month_nk
        names(main)[which(names(main) %in% c('gearcode', 'gear'))] <- 'gear.code' # difference between files
        names(main)[which(names(main) %in% c('areacode', 'divcode'))] <- 'div.code' # difference between files
        names(main)[which(names(main) == 'country')] <- 'country.code' # because that's what it is
        
        # load meta data for each file, just to be sure that there are no changes from zip file to zip file in terms of codes
        # country code
        co <- grep('country', fi, value = T)
        test <- readLines(co, n = 1)
        if (grepl(pattern = '^-+$', test)){ # si c'est un fichier vraiment mal formaté à lire.
            country <- fread(input = co, sep = "|", fill = T, skip = 1, data.table = F, select = c('Code','Abbreviation','Country')) 
            country <- na.omit(country)
        } else {
            country <- fread(input = co, data.table = F, stringsAsFactors = F)
        }
        names(country) <- c('country.code', 'country.abbr', 'country')
        main <- merge(main, country, all.x = TRUE)
        
        # gear code
        ge <- grep('gear', fi, value = T)
        gear <- fread(input = ge, data.table = F, stringsAsFactors = F)
        names(gear) <- c('gear', 'gear.code', 'gear.cat')
        
        # tonnage
        to <- grep('tonnage', fi, value = T)
        tonnage <- fread(input = to, data.table = F, stringsAsFactors = F)[, c(1:2)]
        names(tonnage) <- c('tonnage', 'tonnage.class')
        toca <- tonnage[-4, ]
        toot <- tonnage[-c(1:2), ]
        idc <- grep('CAN', main$country.abbr)
        main <- rbind(merge(main[idc, ], toca, all.x = TRUE), merge(main[-idc, ], toot, all.x = TRUE))
        
        # main species
        msp <- grep('main', fi,value = T)
        mainsp <- fread(input = msp, data.table = F, stringsAsFactors = F)
        names(mainsp) <- c('mainspecies', 'mainspecies.abbr', 'mainspecies.name')
        
        #  division code
        div <- grep('division', fi, value = T)
        divisions <- fread(input = div, data.table = F, stringsAsFactors = F)
        names(divisions) <- c('div.code', 'nafo')
        
        # species code
        sp <- grep('/spe', fi, value = T)
        spt <- read_lines(sp) 
        spnotab <- gsub('\t', "  ", spt) # replace tabs by spaces
        if(substr(spt[1], 1, 4) != 'Code'){
            spl <- lapply(spnotab[-c(1:6)], function(x){ # split colun based on spaces or fixed interval width
                sep <- unlist(strsplit(x, "  +"))
                if(length(sep) == 5) sep <- c(sep[1:2], substr(sep[3], 1, 25), substr(sep[3], 26, nchar(sep[3])), sep[4:5])
                return(sep)
            })
            species <- data.frame(matrix(unlist(spl), nrow = length(spl), byrow = T))
        }else{
            spl <- lapply(spnotab[-c(1:4)],function(x){ # split colun based on spaces or fixed interval width
                sep <- unlist(strsplit(x, "  +"))
                if(length(sep) == 6) sep <- c(sep[1:2], substr(sep[3], 1, 25), substr(sep[3], 26, nchar(sep[3])), sep[4:6])
                return(sep)
            })
            species <- data.frame(matrix(unlist(spl), nrow = length(spl), byrow = T))[, -7]
        }
        names(species) <- c('code', 'species.name', 'species.common', 'species.lat', 'species.abbr', 'species.class')
        
        # add zip specific meta data 
        Reduce(function(...) mergeif(...), list(main, gear, mainsp, divisions, species))
    })
    nafo <- lapply(nafo, type.convert, as.is = TRUE) # equal attributes for binding
    nafo <- rbindlist(nafo, fill = TRUE) # for speed
    nafo <- as.data.frame(nafo)
    if(!is.null(year)) nafo <- nafo[nafo$year %in% year, ]
    if(!is.null(species)) nafo <- nafo[tolower(nafo$species.name) %in% tolower(species), ]

    # reformat month so numeric column
    nafo <- gather(nafo, 'month', 'catch', c('month_nk', tolower(month.abb)))
    nafo <- nafo[nafo$catch > 0, ]  
    nafo <- nafo[rowSums(is.na(nafo)) != ncol(nafo), ]
    nafo[nafo$month == 'month_nk', 'month'] <- NA
    nafo$month <- match(nafo$month, tolower(month.abb))
    
    # split country (provinces for Canada)
    nafo$prov <- ifelse(grepl('Canada', nafo$country), gsub('Canada ', '', nafo$country), NA)
    nafo$country <- ifelse(grepl('Canada', nafo$country), 'Canada', nafo$country)
    
    return(nafo)
}