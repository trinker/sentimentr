#' @importFrom lexicon available_data

make_class <- function(x, ...) {
    class(x) <- unique(c(..., class(x)))    
    x
}


#get_sents <- function(x) {
#    x <- stringi::stri_replace_all_regex(stringi::stri_trans_tolower(x), sent_regex, "")
#    stringi::stri_split_regex(x, "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?|\\!)\\s")
#}
abbr_rep <- lapply(list(
  Titles   = c('jr', 'mr', 'mrs', 'ms', 'dr', 'prof', 'sr', 'sen', 'rep',
         'rev', 'gov', 'atty', 'supt', 'det', 'rev', 'col','gen', 'lt',
         'cmdr', 'adm', 'capt', 'sgt', 'cpl', 'maj'),

  Entities = c('dept', 'univ', 'uni', 'assn', 'bros', 'inc', 'ltd', 'co',
         'corp', 'plc'),

  Months   = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul',
         'aug', 'sep', 'oct', 'nov', 'dec', 'sept'),

  Days     = c('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun'),

  Misc     = c('vs', 'etc', 'no', 'esp', 'cf', 'al', 'mt'),

  Streets  = c('ave', 'bld', 'blvd', 'cl', 'ct', 'cres', 'dr', 'rd', 'st')
), function(x){
    fl <- sub("(^[a-z])(.+)", "\\1", x)
    sprintf("[%s%s]%s", fl, toupper(fl), sub("(^[a-z])(.+)", "\\2", x))
})

period_reg <- paste0(
    "(?:(?<=[a-z])\\.\\s(?=[a-z]\\.))",
        "|",
    "(?:(?<=([ .][a-z]))\\.)(?!(?:\\s[A-Z]|$)|(?:\\s\\s))",
        "|",
    "(?:(?<=[A-Z])\\.(?=\\s??[A-Z]\\.))",
        "|",
    "(?:(?<=[A-Z])\\.(?!\\s+[A-Z][A-Za-z]))"  #added \\s to \\s+ to handle 'I went to AU.  Awesome school.'
)

#period_reg <- paste0(
#    "(?:(?<=[a-z])\\.\\s(?=[a-z]\\.))",
#        "|",
#   "(?:(?<=([ .][a-z]))\\.)(?!(?:\\s[A-Z]|$)|(?:\\s\\s))",
#        "|",
#    "(?:(?<=[A-Z])\\.\\s(?=[A-Z]\\.))",
#        "|",
#    "(?:(?<=[A-Z])\\.(?=\\s[A-Z][A-Za-z]))"
#)



sent_regex <- sprintf("((?<=\\b(%s))\\.)|%s|(%s)",
    paste(unlist(abbr_rep), collapse = "|"),
    period_reg,
	'\\.(?=\\d+)'
)

sent_regex2 <- sprintf("((?<=\\b(%s))\\.)|%s|(%s)",
    paste(unlist(abbr_rep), collapse = "|"),
    period_reg,
	'\\.(?=\\d+)'
)

get_sents <- function(x) {
    if (methods::is(x, "get_sentences")) return(x)
    y <- stringi::stri_replace_all_regex(trimws(x), sent_regex, "<<<TEMP>>>")
    z <- stringi::stri_split_regex(y, "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?|\\!)(\\s|(?=[a-zA-Z][a-zA-Z]*\\s))")
    lapply(z, function(x) gsub("<<<temp>>>", "", stringi::stri_trans_tolower(x)))
}

# get_sents <- function(x) {
#     if (methods::is(x, "get_sentences")) return(x)
#     y <- stringi::stri_replace_all_regex(trimws(x), sent_regex, "")
#     z <- stringi::stri_split_regex(y, "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?|\\!)(\\s|(?=[a-zA-Z][a-zA-Z]*\\s))")
#     lapply(z, stringi::stri_trans_tolower)
# }

# get_sents <- function(x) {
# 	if (methods::is(x, "get_sentences")) return(x)
#     y <- stringi::stri_trans_tolower(stringi::stri_replace_all_regex(trimws(x), sent_regex, ""))
#     stringi::stri_split_regex(y, "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?|\\!)(\\s|(?=[a-zA-Z][a-zA-Z]*\\s))")
# }


#get_sents <- function(x) {
#	if (is(x, "get_sentences")) return(x)
#    x <- stringi::stri_trans_tolower(stringi::stri_replace_all_regex(x, sent_regex, ""))
#    stringi::stri_split_regex(x, "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?|\\!)(\\s|(?=[a-zA-Z][a-zA-Z]*\\s))")
#}

get_sents2 <- function(x) {
    y <- stringi::stri_replace_all_regex(trimws(x), sent_regex, "<<<TEMP>>>")
    stringi::stri_split_regex(y, "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?|\\!)(\\s|(?=[a-zA-Z][a-zA-Z]*\\s))")
}

add_row_id <- function(x){
    lens <- lapply(x, length)
    rep(seq_along(lens), unlist(lens))
}

count_words <- function(x){
    stringi::stri_count_words(x)
}

# make_words <- function(x, hyphen = ""){ #retired 2017-12-10 because split on  single space and any [:;,] even 
#     if (hyphen != "") x <- gsub("-", hyphen, x)
#     lapply(stringi::stri_split_regex(gsub("^\\s+|\\s+$", "", x), "[[:space:]]|(?=[,;:])"), function(y) gsub('~{2,}', ' ', y))
# }

make_words <- function(x, hyphen = ""){
    if (hyphen != "") x <- gsub("-", hyphen, x)
  
    lapply(stringi::stri_split_regex(gsub('([^[[:space:]]])([,;:][[:space:]])', '\\1 \\2', gsub("^\\s+|\\s+$", "", x)), "[[:space:]]+"), function(y) gsub('~{2,}', ' ', y))
}


## count_row_length <- function(x){
##     x <- stringi::stri_count_words(x)
##     x[is.na(x) | x == 0] <- 1
##     x
## }
##
## # @importFrom data.table :=
## make_sentence_df <- function(x){
##
##     indx <- wc <- NULL
##
##     sents <- get_sents(x)
##     ids <- add_row_id(sents)
##     text.var <- gsub("[^a-z',;: ]|\\d:\\d|\\d ", "", unlist(sents))
##     dat <- data.frame(
##         id = ids,
##         sentences = text.var,
##     	wc = count_words(text.var),
##         stringsAsFactors = FALSE
##     )
##     data.table::setDT(dat)
##     dat[, indx:= wc < 1, by=c('id', 'sentences', 'wc')][(indx), c('sentences', 'wc'):=NA, with=FALSE][, indx:=NULL]
## }


#' @importFrom data.table :=
# make_sentence_df2 <- function(sents){
# 
#     indx <- wc <- NULL
# 
#     ids <- add_row_id(sents)
#     text.var <- gsub("[^a-z',;: ]|\\d:\\d|\\d ", "", unlist(sents))
#     dat <- data.frame(
#         id = ids,
#         sentences = text.var,
#     	  wc = count_words(text.var),
#         stringsAsFactors = FALSE
#     )
#     data.table::setDT(dat)
#     dat[, indx:= wc < 1, by=c('id', 'sentences', 'wc')][(indx), c('sentences', 'wc'):=NA][, 
#         indx:=NULL]
# }
make_sentence_df2 <- function(sents, retention_regex = "[^[:alpha:]',;: ]|\\d:\\d|\\d "){

    indx <- wc <- NULL

    ids <- add_row_id(sents)
    text.var <- stringi::stri_replace_all_regex(
        stringi::stri_trans_tolower(gsub("(\\s*)([;:,]+)", " \\2", unlist(sents))),        
        retention_regex, 
        " "
    )
    dat <- data.frame(
        id = ids,
        sentences = text.var,
    	wc = count_words(text.var),
        stringsAsFactors = FALSE
    )
    data.table::setDT(dat)
    dat[, indx:= wc < 1, by=c('id', 'sentences', 'wc')][(indx), c('sentences', 'wc'):=NA][, 
        indx:=NULL][]
}

.mgsub <- function (pattern, replacement, text.var, fixed = TRUE,
	order.pattern = fixed, ...) {

    if (fixed && order.pattern) {
        ord <- rev(order(nchar(pattern)))
        pattern <- pattern[ord]
        if (length(replacement) != 1) replacement <- replacement[ord]
    }
    if (length(replacement) == 1) replacement <- rep(replacement, length(pattern))

    for (i in seq_along(pattern)){
        text.var <- gsub(pattern[i], replacement[i], text.var, fixed = fixed, ..., perl = TRUE)
    }

    text.var
}


space_fill_senti <- function(x, doubles){

  .mgsub(paste0('(?<!~~)', doubles), gsub("\\s+", "~~", doubles, perl = TRUE), x, fixed = FALSE)

}

space_fill <- function(x, doubles){ ## for non-sentiment functions

  .mgsub(doubles, gsub("\\s+", "~~", doubles, perl = TRUE), x)

}

## before pol loc location was not tracked : tracking version with <B> used on 2018-09-24
# comma_reducer <- function(wrds, cl, pl, len, nb, na){
# 
# 	Map(function(wrds2, cl2, pl2, len2){
# 
# 	    ## just retun the words if no pol location
#         if (is.na(pl2)) return(wrds2[-pl2])
# 
#         # Find the upper and lower bound using words n.before and n. after
#         lb <- pl2 - nb
#         lb[lb < 1] <- 1
#         ub <- pl2 + na
#         ub[ub > len2] <- len2
# 
#         max_cl_lower <- any(cl2 < pl2)
#         min_cl_upper <- any(cl2 > pl2)
# 
#         # take into account upper and lower looking for [,:;]
# 		lower <- ifelse(!is.na(max_cl_lower) && max_cl_lower, max(cl2[cl2 < pl2]) + 1, lb)
# 	    upper <- ifelse(!is.na(min_cl_upper) && min_cl_upper, min(cl2[cl2 > pl2]) - 1, ub)
# 
# 	    ## grab these words in the upper and lower w/o the polarized word
#         ind <- lower:upper
# 
# 	    ind <- ind[!ind %in% pl2]
# 	    
# 	    if(identical(integer(0), ind)) return(c('')) #return(c("", wrds2[-pl2])) ## replaced return on 1/12/2018 bc https://github.com/trinker/sentimentr/issues/72 not sure why words were return b4
# 	    
#         wrds2[ind]
#         
# 	}, wrds, cl, pl, len)
# }
comma_reducer <- function(wrds, cl, pl, len, nb, na){

	Map(function(wrds2, cl2, pl2, len2){

	    ## just retun the words if no pol location
        if (is.na(pl2)) return(wrds2[-pl2])

        # Find the upper and lower bound using words n.before and n. after
        lb <- pl2 - nb
        lb[lb < 1] <- 1
        ub <- pl2 + na
        ub[ub > len2] <- len2

        max_cl_lower <- any(cl2 < pl2)
        min_cl_upper <- any(cl2 > pl2)

        # take into account upper and lower looking for [,:;]
		lower <- ifelse(!is.na(max_cl_lower) && max_cl_lower, max(cl2[cl2 < pl2]) + 1, lb)
	    upper <- ifelse(!is.na(min_cl_upper) && min_cl_upper, min(cl2[cl2 > pl2]) - 1, ub)

	    ## grab these words in the upper and lower w/o the polarized word
        ind <- lower:upper

        before <- ind[ind < pl2]
	    ind <- ind[!ind %in% pl2]

	    if(identical(integer(0), ind)) return(c('')) #return(c("", wrds2[-pl2])) ## replaced return on 1/12/2018 bc https://github.com/trinker/sentimentr/issues/72 not sure why words were return b4
	    
	    suffix <- ifelse(ind %in% before, '<B>', '') 
        paste0(wrds2[ind], suffix)
        
	}, wrds, cl, pl, len)
}



rm_na <- function(x) {
	log2NA(x[!is.na(x)])
}

log2NA <- function(x) {
	x[identical(logical(0), x)] <- NA
	x
}

sum2 <- function(x) sum(x, na.rm = TRUE)

paste2 <- function (multi.columns, sep = ".", handle.na = TRUE, trim = TRUE) {
    if (is.matrix(multi.columns)) {
        multi.columns <- data.frame(multi.columns, stringsAsFactors = FALSE)
    }
    if (trim)
        multi.columns <- lapply(multi.columns, function(x) {
            gsub("^\\s+|\\s+$", "", x)
        })
    if (!is.data.frame(multi.columns) & is.list(multi.columns)) {
        multi.columns <- do.call("cbind", multi.columns)
    }
    if (handle.na) {
        m <- apply(multi.columns, 1, function(x) {
            if (any(is.na(x))) {
                NA
            } else {
                paste(x, collapse = sep)
            }
        })
    } else {
        m <- apply(multi.columns, 1, paste, collapse = sep)
    }
    names(m) <- NULL
    return(m)
}

SE <- function(x) sqrt(stats::var(x, na.rm = TRUE)/length(x))

trimws <- function (x, which = c("both", "left", "right")) {
    which <- match.arg(which)
    mysub <- function(re, x) sub(re, "", x, perl = TRUE)
    if (which == "left")
        return(mysub("^[ \t\r\n]+", x))
    if (which == "right")
        return(mysub("[ \t\r\n]+$", x))
    mysub("[ \t\r\n]+$", mysub("^[ \t\r\n]+", x))
}

## helper to remove class
rm_class <- function (x, remove, ...) {
    class(x) <- class(x)[!class(x) %in% remove]
    x
}

## convert a data.table to tibble
set_tibble <- function(x, ...){
    stopifnot(is.data.frame(x))
    class(x) <- c("tbl_df", "tbl", "data.frame")
    x
}

if_tibble <- function(x, as.tibble, ...){
    if(!isTRUE(as.tibble)) return(x)
    set_tibble(x)
}

split_warn <- function(data, fun, len = 1000, nchar = 25000, sentimentr.warning = getOption("sentimentr.warning"), ...){
    
    if (!is.null(sentimentr.warning) && !isTRUE(sentimentr.warning)) return(NULL)
    if (length(data) <= len && max(nchar(data), na.rm = TRUE) <= nchar) return(NULL) 
    
    warning(call. = FALSE, paste0('Each time `', fun, '` is run it has to do sentence boundary ',
        'disambiguation when a\nraw `character` vector is passed to `text.var`. ', 
        'This may be costly of time and\nmemory.  It is highly recommended that ',
        'the user first runs the raw `character`\nvector through the `get_sentences` function.'
    ))
    
}

log_loss <- function(a, p){
    p[p == 0] <- .000000000000001
    p[p == 1] <- .999999999999999
    o <- -1 * (a * log(p) + (1 - a) * log(1 - p))
    mean(o)
}



fix_profanity_list <- function(x, warn = TRUE, ...){
    if(!is.atomic(x)) stop('A `profanity_list` must be an atomic character vector.')
    if(!is.character(x)) stop('A `profanity_list` must be a character vector.')  
    if (any(grepl('[A-Z]', x), na.rm = TRUE)) {
        if (warn) warning('Upper case characters found in `profanity_list`...\nConverting to lower', call. = FALSE)
        x <- tolower(x)
    }
    if (anyNA(x)) {
        if (warn) warning('missing values found in `profanity_list`...\nRemoving all `NA` values', call. = FALSE)
        x <- x[!is.na(x)]
    }    
    if (anyDuplicated(x) > 0) {
        if (warn) warning('duplicate values found in `profanity_list`...\nRemoving all duplicates', call. = FALSE)
        x <- unique(x)
    }   
    x
}




