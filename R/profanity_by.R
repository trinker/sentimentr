#' Profanity Rate By Groups
#'
#' Approximate the profanity of text by grouping variable(s).  For a
#' full description of the profanity detection algorithm see 
#' \code{\link[sentimentr]{profanity}}.  See \code{\link[sentimentr]{profanity}}
#' for more details about the algorithm, the profanity/valence shifter keys
#' that can be passed into the function, and other arguments that can be passed.
#'
#' @param text.var The text variable.  Also takes a \code{profanityr} or
#' \code{profanity_by} object.
#' @param by The grouping variable(s).  Default \code{NULL} uses the original
#' row/element indices; if you used a column of 12 rows for \code{text.var}
#' these 12 rows will be used as the grouping variable.  Also takes a single
#' grouping variable or a list of 1 or more grouping variables.
#' @param group.names A vector of names that corresponds to group.  Generally
#' for internal use.
#' @param \ldots Other arguments passed to \code{\link[sentimentr]{profanity}}.
#' @return Returns a \pkg{data.table} with grouping variables plus:
#' \itemize{
#'   \item  element_id - The id number of the original vector passed to \code{profanity}
#'   \item  sentence_id - The id number of the sentences within each \code{element_id}
#'   \item  word_count - Word count \code{\link[base]{sum}}med by grouping variable
#'   \item  profanity_count - The number of profanities used by grouping variable
#'   \item  sd - Standard deviation (\code{\link[stats]{sd}}) of the sentence level profanity rate by grouping variable
#'   \item  ave_profanity - Profanity rate
#' }
#' @export
#' @family profanity functions
#' @section Chaining:
#' See the  \code{\link[sentimentr]{sentiment_by}} for details about \pkg{sentimentr} chaining.
#' @examples
#' \dontrun{
#' bw <- sample(lexicon::profanity_alvarez, 4)
#' mytext <- c(
#'    sprintf('do you like this %s?  It is %s. But I hate really bad dogs', bw[1], bw[2]),
#'    'I am the best friend.',
#'    NA,
#'    sprintf('I %s hate this %s', bw[3], bw[4]),
#'    "Do you really like it?  I'm not happy"
#' )
#' 
#' ## works on a character vector but not the preferred method avoiding the 
#' ## repeated cost of doing sentence boundary disambiguation every time 
#' ## `profanity` is run
#' profanity(mytext)
#' profanity_by(mytext)
#' 
#' ## preferred method avoiding paying the cost 
#' mytext <- get_sentences(mytext)
#' 
#' profanity_by(mytext)
#' get_sentences(profanity_by(mytext))
#'
#' (myprofanity <- profanity_by(mytext))
#' stats::setNames(get_sentences(profanity_by(mytext)),
#'     round(myprofanity[["ave_profanity"]], 3))
#' 
#' brady <- get_sentences(crowdflower_deflategate)
#' library(data.table)
#' bp <- profanity_by(brady)
#' crowdflower_deflategate[bp[ave_profanity > 0,]$element_id, ]
#' 
#' vulgars <- bp[["ave_profanity"]] > 0
#' stats::setNames(get_sentences(bp)[vulgars],
#'     round(bp[["ave_profanity"]][vulgars], 3))
#'     
#' bt <- data.table(crowdflower_deflategate)[, 
#'     source := ifelse(grepl('^RT', text), 'retweet', 'OP')][,
#'     belichick := grepl('\\bb[A-Za-z]+l[A-Za-z]*ch', text, ignore.case = TRUE)][]
#' 
#' prof_bel <- with(bt, profanity_by(text, by = list(source, belichick)))
#' 
#' plot(prof_bel)
#' }
profanity_by <- function(text.var, by = NULL, group.names, ...){

    UseMethod("profanity_by")
}


#' @export
#' @method profanity_by get_sentences_character    
profanity_by.get_sentences_character <- function(text.var, by = NULL, group.names, ...){

    word_count <- ave_profanity <- profanity_count <- NULL
    out <- suppressWarnings(profanity(text.var = text.var, ...))

    if (is.null(by)){
        
        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
              'profanity_count' = sum(profanity_count, na.rm = TRUE),
        	  'sd' = stats::sd(profanity, na.rm = TRUE)), by = "element_id"][, 
              ave_profanity := replace_infinite(profanity_count/word_count)][]

        G <- "element_id"
        uncombined <- out

    } else {
        if (is.list(by) & length(by) > 1) {
            m <- unlist(as.character(substitute(by))[-1])
            G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            grouping <- by
        } else {
            G <- as.character(substitute(by))
            G <- G[length(G)]
            grouping <- unlist(by)
        }

        if(!missing(group.names)) {
            G <- group.names
        }


        group_dat <- stats::setNames(as.data.frame(grouping,
            stringsAsFactors = FALSE), G)

        data.table::setDT(group_dat)
        group_dat <- group_dat[out[["element_id"]], ]

        uncombined <- out2 <- cbind(group_dat, out)

        out2 <- out2[, list('word_count' = sum(word_count, na.rm = TRUE),
              'profanity_count' = sum(profanity_count, na.rm = TRUE),
        	  'sd' = stats::sd(profanity, na.rm = TRUE)), keyby = G][, 
              ave_profanity := replace_infinite(profanity_count/word_count)][]

    }

    class(out2) <- unique(c("profanity_by", class(out)))
    profanity <- new.env(FALSE)
    profanity[["profanity"]] <- out
    attributes(out2)[["profanity"]] <- profanity
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    out2

}


#' @export
#' @method profanity_by get_sentences_data_frame    
profanity_by.get_sentences_data_frame <- function(text.var, by = NULL, group.names, ...){

    x <- make_class(unname(split(text.var[[attributes(text.var)[['text.var']]]], 
        unlist(text.var[['element_id']]))), "get_sentences", "get_sentences_character")
    
    word_count <- ave_profanity <- profanity_count <- NULL
    out <- profanity(text.var = x, ...)

    if (is.null(by)){
        by <- "element_id"

    }
## right here the other vars are not given    
    uncombined <- out2 <- cbind(text.var, out[, c('word_count', 'profanity_count', 'profanity')])

    out2 <- out2[, list('word_count' = sum(word_count, na.rm = TRUE),
        'profanity_count' = sum(profanity_count, na.rm = TRUE),
        'sd' = stats::sd(profanity, na.rm = TRUE)), keyby = by][, 
        ave_profanity := replace_infinite(profanity_count/word_count)][]

    
    class(out2) <- unique(c("profanity_by", class(out)))
    profanity <- new.env(FALSE)
    profanity[["profanity"]] <- out
    attributes(out2)[["profanity"]] <- profanity
    attributes(out2)[["groups"]] <- by
    
    attributes(attributes(out2)[["profanity"]][["profanity"]])[['sentences']][['sentences']] <- x

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    out2

}



#' @export
#' @method profanity_by character    
profanity_by.character <- function(text.var, by = NULL, group.names, ...){

    split_warn(text.var, 'profanity_by', ...)
    
    profanity_count <- word_count <- ave_profanity <- NULL
    out <- suppressWarnings(profanity(text.var = text.var, ...))

    if (is.null(by)){

        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
              'profanity_count' = sum(profanity_count, na.rm = TRUE),
        	  'sd' = stats::sd(profanity, na.rm = TRUE)), by = "element_id"][, 
              ave_profanity := replace_infinite(profanity_count/word_count)][]

        G <- "element_id"
        uncombined <- out
    } else {
        if (is.list(by) & length(by) > 1) {
            m <- unlist(as.character(substitute(by))[-1])
            G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            grouping <- by
        } else {
            G <- as.character(substitute(by))
            G <- G[length(G)]
            grouping <- unlist(by)
        }

        if(!missing(group.names)) {
            G <- group.names
        }


        group_dat <- stats::setNames(as.data.frame(grouping,
            stringsAsFactors = FALSE), G)

        data.table::setDT(group_dat)
        group_dat <- group_dat[out[["element_id"]], ]

        uncombined <- out2 <- cbind(group_dat, out)

        out2 <- out2[, list('word_count' = sum(word_count, na.rm = TRUE),
              'profanity_count' = sum(profanity_count, na.rm = TRUE),
        	  'sd' = stats::sd(profanity, na.rm = TRUE)), by = G][, 
              ave_profanity := replace_infinite(profanity_count/word_count)][]

    }

    class(out2) <- unique(c("profanity_by", class(out)))
    profanity <- new.env(FALSE)
    profanity[["profanity"]] <- out
    attributes(out2)[["profanity"]] <- profanity
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    out2

}


#' @export
#' @method profanity_by profanity_by    
profanity_by.profanity_by <- function(text.var, by = NULL, group.names, ...){

	profanity_count <- word_count <- ave_profanity <- NULL
    out <- attributes(text.var)[['profanity']][['profanity']]

    if (is.null(by)){

        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
              'profanity_count' = sum(profanity_count, na.rm = TRUE),
        	  'sd' = stats::sd(profanity, na.rm = TRUE)), by = "element_id"][, 
              ave_profanity := replace_infinite(profanity_count/word_count)][]

        G <- "element_id"
        uncombined <- out
    } else {
        if (is.list(by) & length(by) > 1) {
            m <- unlist(as.character(substitute(by))[-1])
            G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            grouping <- by
        } else {
            G <- as.character(substitute(by))
            G <- G[length(G)]
            grouping <- unlist(by)
        }

        if(!missing(group.names)) {
            G <- group.names
        }


        group_dat <- stats::setNames(as.data.frame(grouping,
            stringsAsFactors = FALSE), G)

        data.table::setDT(group_dat)
        group_dat <- group_dat[out[["element_id"]], ]

        uncombined <- out2 <- cbind(group_dat, out)

        out2 <- out2[, list('word_count' = sum(word_count, na.rm = TRUE),
              'profanity_count' = sum(profanity_count, na.rm = TRUE),
        	  'sd' = stats::sd(profanity, na.rm = TRUE)), by = G][, 
              ave_profanity := replace_infinite(profanity_count/word_count)][]


    }

    class(out2) <- unique(c("profanity_by", class(out)))
    profanity <- new.env(FALSE)
    profanity[["profanity"]] <- out
    attributes(out2)[["profanity"]] <- profanity
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    out2

}


#' @export
#' @method profanity_by profanity    
profanity_by.profanity <- function(text.var, by = NULL, group.names, ...){

	profanity_count <- word_count <- ave_profanity <- NULL
    out <- text.var

    if (is.null(by)){

        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
              'profanity_count' = sum(profanity_count, na.rm = TRUE),
        	  'sd' = stats::sd(profanity, na.rm = TRUE)), by = "element_id"][, 
              ave_profanity := replace_infinite(profanity_count/word_count)][]

        G <- "element_id"
        uncombined <- out
    } else {
        if (is.list(by) & length(by) > 1) {
            m <- unlist(as.character(substitute(by))[-1])
            G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            grouping <- by
        } else {
            G <- as.character(substitute(by))
            G <- G[length(G)]
            grouping <- unlist(by)
        }

        if(!missing(group.names)) {
            G <- group.names
        }


        group_dat <- stats::setNames(as.data.frame(grouping,
            stringsAsFactors = FALSE), G)

        data.table::setDT(group_dat)
        group_dat <- group_dat[out[["element_id"]], ]

        uncombined <- out2 <- cbind(group_dat, out)

        out2 <- out2[, list('word_count' = sum(word_count, na.rm = TRUE),
              'profanity_count' = sum(profanity_count, na.rm = TRUE),
        	  'sd' = stats::sd(profanity, na.rm = TRUE)), by = G][, 
              ave_profanity := replace_infinite(profanity_count/word_count)][]

    }

    class(out2) <- unique(c("profanity_by", class(out)))
    profanity <- new.env(FALSE)
    profanity[["profanity"]] <- out
    attributes(out2)[["profanity"]] <- profanity
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    out2

}



#' Plots a profanity_by object
#'
#' Plots a profanity_by object.  Red centers are average profanity.  Alpha
#' jittered dots are raw sentence level profanity data.  Boxes are boxplots.
#'
#' @param x The profanity_by object.
#' @param ordered logical.  If \code{TRUE} order the output grouping by profanity.
#' @param \ldots ignored
#' @method plot profanity_by
#' @importFrom graphics plot
#' @return Returns a \pkg{ggplot2} object.
#' @export
plot.profanity_by <- function(x, ordered = TRUE, ...){

    ave_profanity <- profanity <- grouping.vars <- NULL

    dat2 <- uncombine(x)

    grps <- attributes(x)[["groups"]]
    if (length(grps) == 1 && grps == "element_id") return(plot(dat2))

    x[, "grouping.vars"] <- paste2(x[, attributes(x)[["groups"]], with=FALSE])
 
    dat2[, "grouping.vars"] <- paste2(dat2[, attributes(x)[["groups"]], with=FALSE])
    
    x <- x[order(-ave_profanity)]    
    x[, grouping.vars := factor(grouping.vars, levels = rev(grouping.vars))]
    
    dat2[, grouping.vars := factor(grouping.vars, levels = levels(x[["grouping.vars"]]))]

    ggplot2::ggplot()+
        ggplot2::geom_boxplot(data=dat2, ggplot2::aes(y=profanity, 
            x=grouping.vars),fill=NA, color='grey70',width=.55, size=.35, 
            outlier.color = NA)  +  
        ggplot2::geom_jitter(data=dat2, ggplot2::aes(y=profanity, 
            x=grouping.vars), width=.35, height=0, alpha=.15, size=1.5) +        
        ggplot2::theme_bw() +
        ggplot2::geom_point(data=x, ggplot2::aes(y=ave_profanity, x=grouping.vars), 
            colour = "red", shape=18, size=4) +
        ggplot2::ylab("Profanity Rate") +
        ggplot2::xlab("Groups") +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(labels = function(x) paste0(round(x*100, digits = 0), '%'))

}

replace_infinite <- function(x, y = 0, z = 1) ifelse(is.nan(x), y, ifelse(is.infinite(x), z, x))

