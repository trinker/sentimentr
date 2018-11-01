#' Emotion Rate By Groups
#'
#' Approximate the emotion of text by grouping variable(s).  For a
#' full description of the emotion detection algorithm see 
#' \code{\link[sentimentr]{emotion}}.  See \code{\link[sentimentr]{emotion}}
#' for more details about the algorithm, the emotion/valence shifter keys
#' that can be passed into the function, and other arguments that can be passed.
#'
#' @param text.var The text variable.  Also takes a \code{emotionr} or
#' \code{emotion_by} object.
#' @param by The grouping variable(s).  Default \code{NULL} uses the original
#' row/element indices; if you used a column of 12 rows for \code{text.var}
#' these 12 rows will be used as the grouping variable.  Also takes a single
#' grouping variable or a list of 1 or more grouping variables.
#' @param group.names A vector of names that corresponds to group.  Generally
#' for internal use.
#' @param \ldots Other arguments passed to \code{\link[sentimentr]{emotion}}.
#' @return Returns a \pkg{data.table} with grouping variables plus:
#' \itemize{
#'   \item  element_id - The id number of the original vector passed to \code{emotion}
#'   \item  sentence_id - The id number of the sentences within each \code{element_id}
#'   \item  word_count - Word count \code{\link[base]{sum}}med by grouping variable
#'   \item emotion_type - Type designation from the \code{emotion} column of the \code{emotion_dt} table
#'   \item  emotion_count - The number of profanities used by grouping variable
#'   \item  sd - Standard deviation (\code{\link[stats]{sd}}) of the sentence level emotion rate by grouping variable
#'   \item  ave_emotion - Emotion rate
#' }
#' @keywords emotion
#' @export
#' @family emotion functions
#' @section Chaining:
#' See the  \code{\link[sentimentr]{sentiment_by}} for details about \pkg{sentimentr} chaining.
#' @examples
#' \dontrun{
#' mytext <- c(
#'     "I am not afraid of you",
#'     NA,
#'     "",
#'     "I love it [not really]", 
#'     "I'm not angry with you", 
#'     "I hate it when you lie to me.  It's so humiliating",
#'     "I'm not happpy anymore.  It's time to end it",
#'     "She's a darn good friend to me",
#'     "I went to the terrible store",
#'     "There is hate and love in each of us",
#'     "I'm no longer angry!  I'm really experiencing peace but not true joy.",
#'     
#'     paste("Out of the night that covers me, Black as the Pit from pole to", 
#'       "pole, I thank whatever gods may be For my unconquerable soul.",
#'       "In the fell clutch of circumstance I have not winced nor cried",
#'       "aloud. Under the bludgeonings of chance My head is bloody, but unbowed.",
#'       "Beyond this place of wrath and tears Looms but the Horror of the", 
#'       "shade, And yet the menace of the years Finds, and shall find, me unafraid.",
#'       "It matters not how strait the gate, How charged with punishments", 
#'       "the scroll, I am the master of my fate: I am the captain of my soul."
#'     )    
#'     
#' )
#' 
#' ## works on a character vector but not the preferred method avoiding the 
#' ## repeated cost of doing sentence boundary disambiguation every time 
#' ## `emotion` is run
#' emotion(mytext)
#' emotion_by(mytext)
#' 
#' ## preferred method avoiding paying the cost 
#' mytext <- get_sentences(mytext)
#' 
#' emotion_by(mytext)
#' get_sentences(emotion_by(mytext))
#'
#' (myemotion <- emotion_by(mytext))
#' stats::setNames(get_sentences(emotion_by(mytext)),
#'     round(myemotion[["ave_emotion"]], 3))
#' 
#' pres <- get_sentences(presidential_debates_2012)
#' pres_emo_sent <- emotion_by(pres)
#' 
#' ## method 1
#' pres_emo_per_time <- presidential_debates_2012 %>%
#'     get_sentences() %>%
#'     emotion_by(by = c('person', 'time'))
#'     
#' pres_emo_per_time
#' 
#' ## method 2
#' library(magrittr)
#' presidential_debates_2012 %>%
#'     get_sentences() %$%
#'     emotion_by(., by = c('person', 'time'))
#' 
#' ## method 3
#' presidential_debates_2012 %>%
#'     get_sentences() %$%
#'     emotion_by(dialogue, by = list(person, time))
#' 
#' ## method 4
#' presidential_debates_2012 %>%
#'     get_sentences() %>%
#'     with(emotion_by(dialogue, by = list(person, time)))
#' 
#' plot(pres_emo_sent)
#' plot(pres_emo_per_time)
#' }
emotion_by <- function(text.var, by = NULL, group.names, ...){

    UseMethod("emotion_by")
}


#' @export
#' @method emotion_by get_sentences_character    
emotion_by.get_sentences_character <- function(text.var, by = NULL, group.names, ...){

    word_count <- ave_emotion <- emotion_count <- NULL
    out <- suppressWarnings(emotion(text.var = text.var, ...))

    if (is.null(by)){
        
        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
              'emotion_count' = sum(emotion_count, na.rm = TRUE),
        	  'sd' = stats::sd(emotion, na.rm = TRUE)), by = c("element_id", "emotion_type")][, 
              ave_emotion := replace_infinite(emotion_count/word_count)][]

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
              'emotion_count' = sum(emotion_count, na.rm = TRUE),
        	  'sd' = stats::sd(emotion, na.rm = TRUE)), keyby = c(G, 'emotion_type')][, 
              ave_emotion := replace_infinite(emotion_count/word_count)][]

    }

    class(out2) <- unique(c("emotion_by", class(out)))
    emotion <- new.env(FALSE)
    emotion[["emotion"]] <- out
    attributes(out2)[["emotion"]] <- emotion
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    out2

}


#' @export
#' @method emotion_by get_sentences_data_frame    
emotion_by.get_sentences_data_frame <- function(text.var, by = NULL, group.names, ...){

    emotion_sentimentr2 <- emotion_count_sentimentr2 <- NULL
    
    x <- make_class(unname(split(text.var[[attributes(text.var)[['text.var']]]], 
        unlist(text.var[['element_id']]))), "get_sentences", "get_sentences_character")
    
    word_count <- ave_emotion <- emotion_count <- NULL
    out <- emotion(text.var = x, ...)
    data.table::setnames(out, c('emotion_type', 'emotion_count', 'emotion'), 
        c('emotion_type_sentimentr2', 'emotion_count_sentimentr2', 'emotion_sentimentr2'))
    
    if (is.null(by)){
        by <- "element_id"

    }
    
## right here the other vars are not given 

    uncombined <- out2 <- merge(
        text.var, 
        out,
        all.x=TRUE, 
        allow.cartesian=TRUE,
        by = c('element_id', 'sentence_id')
    )

    out2 <- out2[, list('word_count' = sum(word_count, na.rm = TRUE),
        'emotion_count_sentimentr2' = sum(emotion_count_sentimentr2, na.rm = TRUE),
        'sd' = stats::sd(emotion_sentimentr2, na.rm = TRUE)), keyby = c(by, 'emotion_type_sentimentr2')][, 
        ave_emotion := replace_infinite(emotion_count_sentimentr2/word_count)][]

    data.table::setnames(out, 
        c('emotion_type_sentimentr2', 'emotion_count_sentimentr2', 'emotion_sentimentr2'),
        c('emotion_type', 'emotion_count', 'emotion')
    )
    
    data.table::setnames(out2, 
        c('emotion_type_sentimentr2', 'emotion_count_sentimentr2'),
        c('emotion_type', 'emotion_count')
    )
    
        
    class(out2) <- unique(c("emotion_by", class(out)))
    emotion <- new.env(FALSE)
    emotion[["emotion"]] <- out
    attributes(out2)[["emotion"]] <- emotion
    attributes(out2)[["groups"]] <- by
    
    attributes(attributes(out2)[["emotion"]][["emotion"]])[['sentences']][['sentences']] <- x

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    out2

}



#' @export
#' @method emotion_by character    
emotion_by.character <- function(text.var, by = NULL, group.names, ...){

    split_warn(text.var, 'emotion_by', ...)
    
    emotion_count <- word_count <- ave_emotion <- NULL
    out <- suppressWarnings(emotion(text.var = text.var, ...))

    if (is.null(by)){

        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
              'emotion_count' = sum(emotion_count, na.rm = TRUE),
        	  'sd' = stats::sd(emotion, na.rm = TRUE)), by = c("element_id", 'emotion_type')][, 
              ave_emotion := replace_infinite(emotion_count/word_count)][]

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
              'emotion_count' = sum(emotion_count, na.rm = TRUE),
        	  'sd' = stats::sd(emotion, na.rm = TRUE)), by = c(G, 'emotion_type')][, 
              ave_emotion := replace_infinite(emotion_count/word_count)][]

    }

    class(out2) <- unique(c("emotion_by", class(out)))
    emotion <- new.env(FALSE)
    emotion[["emotion"]] <- out
    attributes(out2)[["emotion"]] <- emotion
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    out2

}


#' @export
#' @method emotion_by emotion_by    
emotion_by.emotion_by <- function(text.var, by = NULL, group.names, ...){

	emotion_count <- word_count <- ave_emotion <- NULL
    out <- attributes(text.var)[['emotion']][['emotion']]

    if (is.null(by)){

        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
              'emotion_count' = sum(emotion_count, na.rm = TRUE),
        	  'sd' = stats::sd(emotion, na.rm = TRUE)), by = c("element_id", 'emotion_type')][, 
              ave_emotion := replace_infinite(emotion_count/word_count)][]

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
              'emotion_count' = sum(emotion_count, na.rm = TRUE),
        	  'sd' = stats::sd(emotion, na.rm = TRUE)), by = c(G, 'emotion_type')][, 
              ave_emotion := replace_infinite(emotion_count/word_count)][]


    }

    class(out2) <- unique(c("emotion_by", class(out)))
    emotion <- new.env(FALSE)
    emotion[["emotion"]] <- out
    attributes(out2)[["emotion"]] <- emotion
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    out2

}


#' @export
#' @method emotion_by emotion    
emotion_by.emotion <- function(text.var, by = NULL, group.names, ...){

	emotion_count <- word_count <- ave_emotion <- NULL
    out <- text.var

    if (is.null(by)){

        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
              'emotion_count' = sum(emotion_count, na.rm = TRUE),
        	  'sd' = stats::sd(emotion, na.rm = TRUE)), by = c("element_id", 'emotion_type')][, 
              ave_emotion := replace_infinite(emotion_count/word_count)][]

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
              'emotion_count' = sum(emotion_count, na.rm = TRUE),
        	  'sd' = stats::sd(emotion, na.rm = TRUE)), by = c(G, 'emotion_type')][, 
              ave_emotion := replace_infinite(emotion_count/word_count)][]

    }

    class(out2) <- unique(c("emotion_by", class(out)))
    emotion <- new.env(FALSE)
    emotion[["emotion"]] <- out
    attributes(out2)[["emotion"]] <- emotion
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    out2

}



#' Plots a emotion_by object
#'
#' Plots a emotion_by object.  Red centers are average emotion.  Alpha
#' jittered dots are raw sentence level emotion data.  Boxes are boxplots.
#'
#' @param x The emotion_by object.
#' @param ordered logical.  If \code{TRUE} order the output grouping by emotion.
#' @param \ldots ignored
#' @method plot emotion_by
#' @importFrom graphics plot
#' @return Returns a \pkg{ggplot2} object.
#' @export
plot.emotion_by <- function(x, ordered = TRUE, ...){

    emotion_type <- ave_emotion <- emotion <- grouping.vars <- NULL

    dat2 <- uncombine(x)

    grps <- attributes(x)[["groups"]]
    if (length(grps) == 1 && grps == "element_id") return(plot(dat2))

    x[, "grouping.vars"] <- paste2(x[, attributes(x)[["groups"]], with=FALSE])
 
    dat2[, "grouping.vars"] <- paste2(dat2[, attributes(x)[["groups"]], with=FALSE])
    
    x <- x[order(emotion_type, -ave_emotion)] 
  
    gp_lvls <- x[, list(ave_emotion = mean(ave_emotion)), keyby = 'grouping.vars'][order(-ave_emotion),][['grouping.vars']]
    emo_lvls <- x[, list(ave_emotion = mean(ave_emotion)), keyby = 'emotion_type'][order(ave_emotion),][['emotion_type']]
     
    
    
    x[, grouping.vars := factor(grouping.vars, levels = rev(gp_lvls))]
   
    dat2[, grouping.vars := factor(grouping.vars, levels = levels(x[["grouping.vars"]]))]

    ggplot2::ggplot()+
        # ggplot2::geom_boxplot(data=dat2, ggplot2::aes(y=emotion, 
        #     x=grouping.vars),fill=NA, color='grey70',width=.55, size=.35, 
        #     outlier.color = NA)  +  
        ggplot2::geom_jitter(data=dat2, ggplot2::aes(y=emotion, 
            x=grouping.vars), width=.25, height=0, alpha=.125, size=1.25) +        
        ggplot2::theme_bw() +
        # ggplot2::geom_point(data=x, ggplot2::aes(y=ave_emotion, x=grouping.vars), 
        #     colour = "red", shape='|', size=4) +
        ggplot2::ylab("Emotion Rate") +
        ggplot2::xlab("Groups") +
        ggplot2::coord_flip() +
        ggplot2::facet_wrap(.~emotion_type) +
        ggplot2::scale_y_continuous(labels = function(x) paste0(round(x*100, digits = 0), '%'))

}


