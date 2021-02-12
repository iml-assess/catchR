##' Reads length-frequency data from dockside monitoring.
##' @param file .dat file to read. This file is the output of this SAS program: S:/SAS/Peche/autoexec_peche.sas.
##' @param year Vector of years to read (ex: 2015:2020). NULL by default, meaning the function will keep data from all available years.
##' @param language Language to use for column names, either "en" (default) or "fr".
##' @param ... Arguments used with read_fwf() (e.g., progress, skip_empty_rows, n_max).
##' @import readr lubridate
##' @details Reads length-frequency data from dockside monitoring. Follows structure of file S:/SAS/Peche/Infile_EntFrq.txt.
##' @rdname read.lf
##' @export
read.lf <- function(file, year = NULL, language = "en", ...){
    
    language <- match.arg(language, choices = c("en", "fr"))
    
    id_set  <- setNames(split(lf_key_set[, c('start', 'end')], seq(nrow(lf_key_set))), lf_key_set[, language]) # transform key into named list
    id_freq <- setNames(split(lf_key_freq[, c('start', 'end')], seq(nrow(lf_key_freq))), lf_key_freq[, language]) # transform key into named list

    # read in data
    lf_set <- read_fwf(file, do.call(fwf_cols, id_set), ...)     # read as if all lines 4
    lf_set <- as.data.frame(lf_set[lf_set[, 1] == 4, ])          # lines starting with 4
    lf_set$id <- 1:nrow(lf_set)                                  # sample id
    
    lf_freq <- read_fwf(file, do.call(fwf_cols, id_freq), ...)   # read as if all lines 5
    lf_freq <- as.data.frame(lf_freq)
    l <- rle(lf_freq[, 1])$lengths                               # number of rows each sample
    lf_freq <- lf_freq[lf_freq[, 1] == 5, ]                      # lines starting with 5
    lf_freq$id <- rep(1:(length(l)/2), l[seq(2, length(l), 2)])  # sample id
    
    # merge
    lf <- merge(lf_set, lf_freq, all.x = TRUE)                   # merge based on id, prov, district, trip
    lf[, c(5,29)] <- NULL                                        # remove identifiers (4 and 5)
    if(!is.null(year)) lf <- lf[lf[, 8] %in% year, ]             # select years
    
    # extra
    lf[, c(6,7,28,29)] <- sapply(lf[, c(6,7,28,29)], as.numeric)             # convert to numeric a bunch of columns (day, month, length, n)
    lf$date <- ymd(apply(lf[, 8:6], 1, paste, collapse = "-"))               # add date
    lf$trim <- quarter(lf$date)                                              # add trimester
    lf$doy  <- as.integer(format(lf$date, "%j"))                             # add date of year
    lf[, 2]  <- lf_prov[lf[, 2], language]                                   # replace keys by what they stand for (province)
    lf[, 17] <- lf_fishshape[match(lf[, 17], lf_fishshape[, 1]), language]   # replace keys by what they stand for (fishshape)
    lf[, 20] <- lf_subsample[match(lf[, 20], lf_subsample[, 1]), language]   # replace keys by what they stand for (subsample size)  
    lf[, 24] <- lf_bins[match(lf[, 24],lf_bins[, 1]), language]              # replace keys by what they stand for (length bins)   
    lf[, 25] <- lf_lengthtype[match(lf[, 25], lf_lengthtype[, 1]), language] # replace keys by what they stand for (length types)   
    
    lf <- merge(lf, bio_zones[, c(2,3)], all.x = TRUE, by.x = lf_key_set[lf_key_set$fr == 'opano', language], by.y = 'nafo.sub')
    lf$nafo <- toupper(lf$nafo)
    
    
    # lat long...
    return(lf)
}