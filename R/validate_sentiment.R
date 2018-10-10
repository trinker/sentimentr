#' Validate Sentiment Score Sign Against Known Results
#' 
#' Provides a multiclass macroaverage/microaverage of precision, recall, 
#' accuracy, and F-score for the sign of the predicted sentiment against known 
#' sentiment scores.  There are three classes sentiment analysis generally 
#' predicts: positive (> 0), negative (< 0) and neutral (= 0).  In assessing 
#' model performance one can use macro- or micro- averaging across classes.  
#' Macroaveraging allows every class to have an equal say.  Microaveraging gives 
#' larger say to larger classes.
#' 
#' @param predicted A numeric vector of predicted sentiment scores or a 
#' \pkg{sentimentr} object that returns sentiment scores.
#' @param actual A numeric vector of known sentiment ratings.
#' @param \ldots ignored.
#' @return Returns a \code{\link[base]{data.frame}} with a macroaveraged and 
#' microaveraged model validation scores.  Additionally, the
#'  \code{\link[base]{data.frame}} has the following attributes:
#' \item{confusion_matrix}{A confusion matrix of all classes}
#' \item{class_confusion_matrices}{A \code{\link[base]{list}} of class level (class vs. all) confusion matrices}
#' \item{macro_stats}{A \code{\link[base]{data.frame}} of the macroaverged class level stats before averaging}
#' \item{mda}{Mean Directional Accuracy}
#' \item{mare}{Mean Absolute Rescaled Error}
#' @references \url{https://www.youtube.com/watch?v=OwwdYHWRB5E&index=31&list=PL6397E4B26D00A269} \cr
#' \url{https://en.wikipedia.org/wiki/Mean_Directional_Accuracy_(MDA)}
#' @note Mean Absolute Rescaled Error (MARE) is defined as: 
#' \eqn{\frac{\sum{|actual - predicted|}}{2n}} and gives a sense of, on average, 
#' how far off were the rescaled predicted values (-1 to 1) from the rescaled 
#' actual values (-1 to 1).  A value of 0 means perfect accuracy.  A value of
#' 1 means perfectly wrong every time.  A value of .5 represents expected value
#' for random guessing.  This measure is related to 
#' \href{https://en.wikipedia.org/wiki/Mean_absolute_error}{Mean Absolute Error}.
#' @export
#' @examples 
#' actual <- c(1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1, 1,-1)
#' predicted <- c(1, 0, 1, -1, 1, 0, -1, -1, -1, -1, 0, 1,-1)
#' validate_sentiment(predicted, actual)
#' 
#' scores <- hu_liu_cannon_reviews$sentiment
#' mod <- sentiment_by(get_sentences(hu_liu_cannon_reviews$text))
#' 
#' validate_sentiment(mod$ave_sentiment, scores)
#' validate_sentiment(mod, scores)
#' 
#' x <- validate_sentiment(mod, scores)
#' attributes(x)$confusion_matrix
#' attributes(x)$class_confusion_matrices
#' attributes(x)$macro_stats
#' 
#' ## Annie Swafford Example
#' swafford <- data.frame(
#'     text = c(
#'         "I haven't been sad in a long time.",
#'         "I am extremely happy today.",
#'         "It's a good day.",
#'         "But suddenly I'm only a little bit happy.",
#'         "Then I'm not happy at all.",
#'         "In fact, I am now the least happy person on the planet.",
#'         "There is no happiness left in me.",
#'         "Wait, it's returned!",
#'         "I don't feel so bad after all!"
#'     ), 
#'     actual = c(.8, 1, .8, -.1, -.5, -1, -1, .5, .6), 
#'     stringsAsFactors = FALSE
#' )
#' 
#' pred <- sentiment_by(swafford$text) 
#' validate_sentiment(
#'     pred,
#'     actual = swafford$actual
#' )
validate_sentiment <- function(predicted, actual, ...){

    UseMethod('validate_sentiment')
}


#' @export
#' @method validate_sentiment numeric
validate_sentiment.numeric <- function(predicted, actual, ...){

    pred <- predicted
    act <- actual
    
    mda <- mean(sign(predicted) == sign(actual))
    mare <- mean_absolute_rescaled_error(predicted, actual)
    
    predicted <- general_rescale(predicted, sign = TRUE)
    actual <- general_rescale(actual, sign = TRUE)
    confusion_matrix <- conf_matrix_temp
    confusion_matrix[1,1] <- sum(predicted == -1 & actual == -1, na.rm = TRUE)
    confusion_matrix[1,2] <- sum(predicted == 0 & actual == -1, na.rm = TRUE)
    confusion_matrix[1,3] <- sum(predicted == 1 & actual == -1, na.rm = TRUE)
    confusion_matrix[2,1] <- sum(predicted == -1 & actual == 0, na.rm = TRUE)
    confusion_matrix[2,2] <- sum(predicted == 0 & actual == 0, na.rm = TRUE)
    confusion_matrix[2,3] <- sum(predicted == 1 & actual == 0, na.rm = TRUE)
    confusion_matrix[3,1] <- sum(predicted == -1 & actual == 1, na.rm = TRUE)
    confusion_matrix[3,2] <- sum(predicted == 0 & actual == 1, na.rm = TRUE)
    confusion_matrix[3,3] <- sum(predicted == 1 & actual == 1, na.rm = TRUE)

    class_confusion_matrices <- stats::setNames(lapply(colnames(confusion_matrix), function(x){

        cmt[1,1] <- sum(predicted != x & actual != x, na.rm = TRUE)
        cmt[2,2] <- sum(predicted == x & actual == x, na.rm = TRUE)
        cmt[1,2] <- sum(predicted == x & actual != x, na.rm = TRUE)
        cmt[2,1] <- sum(predicted != x & actual == x, na.rm = TRUE)
        cmt
    
    }), colnames(confusion_matrix))



    macro_stats <- lapply(class_confusion_matrices, function(x){
        out <-  data.frame(precision = x[2,2]/sum(x[,2]), recall = x[2,2]/sum(x[2,]), accuracy = sum(diag(x))/sum(x))
        #if (is.nan(out[['recall']])) return(NULL)
        out[sapply(out, is.nan)] <- 1
        out[['F']] <- 2*((out[['precision']] * out[['recall']])/(out[['precision']] + out[['recall']]))
        out
    })
    
    #macro_stats <- macro_stats[!sapply(macro_stats, is.null)]
    macro_stats <- data.frame(
        class = as.numeric(names(macro_stats)),
        do.call(rbind.data.frame, macro_stats), 
        stringsAsFactors = FALSE, row.names = NULL
    )
    macro_averaged <- data.frame(average = 'macro', t(colMeans(macro_stats[-1])))


    mcm <- Reduce('+', class_confusion_matrices)
    micro_averaged <- data.frame(
        average = 'micro', 
        precision = mcm[2,2]/sum(mcm[,2]), 
        recall = mcm[2,2]/sum(mcm[2,]), 
        accuracy = sum(diag(mcm))/sum(mcm)
    )
    micro_averaged[['F']] <- 2*((micro_averaged[['precision']] * micro_averaged[['recall']])/(micro_averaged[['precision']] + micro_averaged[['recall']]))
    out <- rbind.data.frame(macro_averaged, micro_averaged, stringsAsFactors = FALSE)
    class(out) <- c('validate_sentiment', 'data.frame') 
    attributes(out)[['confusion_matrix']] <- confusion_matrix         
    attributes(out)[['class_confusion_matrices']] <- class_confusion_matrices
    attributes(out)[['macro_stats']] <- macro_stats
    attributes(out)[['mda']] <- mda
    attributes(out)[['mare']] <- mare
    out
}


mean_absolute_rescaled_error <- function(predicted, actual, rescale = TRUE) {

    if (isTRUE(rescale)) { rsfun <- general_rescale } else { rsfun <- c }
    stopifnot(length(actual) == length(predicted))
    mean(abs(rsfun(actual) - rsfun(predicted)) )/2

}


#' Prints a validate_sentiment Object
#' 
#' Prints a validate_sentiment object
#' 
#' @param x A \code{validate_sentiment} Object
#' @param \ldots ignored.
#' @method print validate_sentiment
#' @export
print.validate_sentiment <- function(x, ...){
    x1 <- x
    class(x1) <- 'data.frame'
    cat(paste0('Mean Directional Accuracy:    ', round(attributes(x)[['mda']], 3)), '\n')
    cat(paste0('Mean Absolute Rescaled Error: ', round(attributes(x)[['mare']], 3)), '\n\n')
    print(x1)
}



#' @export
#' @method validate_sentiment sentiment_by
validate_sentiment.sentiment_by <- function(predicted, actual, ...){
    
  
    if (missing(actual)) {

        if(!isTRUE(all.equal(attributes(predicted)[['groups']], 'element_id'))){
            stop('If you pass nothing to `actual` then `predicted` must be a `sentiment_by` object created with no `group.vars`.')
        }
        
        n <- list(...)[['n']]
        width <- list(...)[['width']]
        if (is.null(n)) n <- 30
        if (is.null(width)) width <- 50
  
        pred <- predicted[['ave_sentiment']]
    
        classes <- ifelse(pred == 0, '0   (neutral)', ifelse(pred > 0, '+   (positive)', '-   (negative)'))   
       
        sents <- lapply(split(get_sentences(predicted), classes), function(x){
            xn <- length(x)
            len <- ifelse(xn <= n, xn, n)
            locs <- sample.int(xn, len)
            txt <- x[locs]
            unlist(lapply(txt, paste, collapse = ' '))
        })
        
        dat <- textshape::tidy_list(sents, 'tag', 'text.var')
        
        results <- Map(tag_assessment, dat[['text.var']], dat[['tag']], seq_len(nrow(dat)), nrow(dat), width = width)    

        sign <- substring(dat[['tag']], 1, 1)
        predicted <- ifelse(sign == '0', 0, ifelse(sign == '-', -1, 1))
        actual <- unlist(results)
        out <- validate_sentiment(predicted, actual)
      
        attributes(out)[['text']] <- sents
        return(out)

    }
    
    
    validate_sentiment(predicted[['ave_sentiment']], actual)
}

tag_assessment <- function(text.var, tag, number, total, width = 50){
    lines <- paste(rep("-", width), collapse="")
    text <- strwrap(text.var, width)
    tag <- sprintf("\nTag: %s", tag)
    numb <- sprintf("[%s of %s]", number, total)
    clear <- paste(rep("\n", 20), collapse="")
    message(paste(c(clear, numb, lines,  text, tag, lines,  "\n\nDoes this tag fit?"), collapse="\n"))
    utils::menu(c("Yes", "No"))
}

#' @export
#' @method validate_sentiment sentiment
validate_sentiment.sentiment <- function(predicted, actual, ...){
    validate_sentiment(predicted[['sentiment']], actual, ...)
}


conf_matrix_temp <- structure(c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), .Dim = c(3L, 
3L), .Dimnames = structure(list(actual = c("-1", "0", "1"), predicted = c("-1", 
"0", "1")), .Names = c("actual", "predicted")), class = "table")

cmt <- structure(c(0L, 0L, 0L, 0L), .Dim = c(2L, 
2L), .Dimnames = structure(list(actual = c("no", "yes"), predicted = c("no", 
"yes")), .Names = c("actual", "predicted")), class = "table")
