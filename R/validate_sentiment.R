#' Validate Sentiment Score Sign Against Known Results
#' 
#' Provides a miulticlass macroaverage/microaverage of precision, recall, 
#' accuracy, and F-score for the sign of the predicted sentiment against known 
#' sentiment scores. Macroaveraging allows every class to have an equal say.  
#' Microaveraging gives larger say to larger classes.
#' 
#' @param predicted A numeric vector of predicted sentiment scores or a 
#' \pkg{sentimentr} object that returns sentiment scores.
#' @param actual A numeric vector of known sentiment ratings.
#' @param \ldots ignored.
#' @return Returns a \code{\link[base]{data.frame}} with a macroaveraged and 
#' microaveraged model validation scores.  Additionally, the
#'  \code{{\link[base]{data.frame}} has the following attributes:
#' \item{confusion_matrix}{A confusion matrix of all classes}
#' \item{class_confusion_matrices}{A \code{\link[base]{list}} of class level (class vs. all) confusion matrices}
#' \item{macro_stats}{A \code{\link[base]{data.frame}} of the macroaverged class level stats before averaging}
#' @references \url{https://www.youtube.com/watch?v=OwwdYHWRB5E&index=31&list=PL6397E4B26D00A269}
#' @export
#' @examples 
#' actual <- c(1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1, 1,-1)
#' predicted <- c(1, 0, 1, -1, 1, 0, -1, -1, -1, -1, 0, 1,-1)
#' validate_sentiment(actual, predicted)
#' 
#' scores <- cannon_reviews$opinion.score
#' mod <- sentiment_by(cannon_reviews$review)
#' 
#' validate_sentiment(mod$ave_sentiment, scores)
#' validate_sentiment(mod, scores)
#' 
#' x <- validate_sentiment(mod, actual)
#' attributes(x)$confusion_matrix
#' attributes(x)$class_confusion_matrices
#' attributes(x)$macro_stats
validate_sentiment <- function(predicted, actual, ...){

    UseMethod('validate_sentiment')
}


#' @export
#' @method validate_sentiment numeric
validate_sentiment.numeric <- function(predicted, actual, ...){

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
    out
}

#' @export
#' @method validate_sentiment sentiment_by
validate_sentiment.sentiment_by <- function(predicted, actual, ...){
    validate_sentiment(predicted[['ave_sentiment']], actual, ...)
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
