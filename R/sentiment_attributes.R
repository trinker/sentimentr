#' Extract Sentiment Attributes from Text
#' 
#' This function utilizes \pkg{gofastr} and \pkg{termco} to extract sentiment
#' based attributes (attributes concerning polarized words and valence 
#' shifters) from a text.  Attributes include the rate of polarized terms
#' and valence shifters relative to number of words.  Additionally, coocurrence
#' rates for valence shifters are computed.
#' 
#' @param text.var The text variable.
#' @param polarity_dt A \pkg{data.table} of positive/negative words and
#' weights with x and y as column names.
#' @param valence_shifters_dt A \pkg{data.table} of valence shifters that
#' can alter a polarized word's meaning and an integer key for negators (1),
#' amplifiers(2), de-amplifiers (3) and adversative conjunctions (4) with x and 
#' y as column names.
#' @param \ldots ignored.
#' @return Returns a list of four items:
#' \item{Meta}{The number of words, sentences, and questions in the text}
#' \item{Attributes}{The rate of sentiment attributes relative to the number of words}
#' \item{Polarized_Cooccurrences}{The rate that valence shifters cooccur with a polarized word in the same sentence}
#' \item{Cooccurrences}{A cooccurrence matrix of sentiment attributes; `polarized` is the sum of positive and negative}
#' @note \pkg{gofastr} and \pkg{termco} must be installed.  If they are not (which
#' they are not part of \pkg{sentimentr} install) then the function will prompt
#' you to attempt to install them using \code{install.packages} and
#' \code{ghit::install_github}.
#' @export
#' @examples 
#' \dontrun{
#' sentiment_attributes(presidential_debates_2012$dialogue)
#' }
sentiment_attributes <- function(text.var, polarity_dt = lexicon::hash_sentiment_jockers_rinker, 
    valence_shifters_dt = lexicon::hash_valence_shifters, ...) {
    
        fun <- source(system.file("sfp/sfp", package = "sentimentr"))[["value"]]
        fun(text.var, polarity_dt, valence_shifters_dt, ...)
    
}

