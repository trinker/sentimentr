#' Replace Emoticons With Words
#'
#' Replaces emoticons with word equivalents.
#'
#' @param text.var The text variable.
#' @param emoticon_dt A \pkg{data.table} of emoticons (graphical representations)
#' and corresponding word meanings.
#' @param \ldots Other arguments passed to \code{.mgsub} (see
#' \code{sentimentr:::.mgsub} for details).
#' @return Returns a vector of strings with emoticons replaced with word
#' equivalents.
#' @keywords emoticon
#' @export
#' @examples
#' x <- c(
#'     "text from: http://www.webopedia.com/quick_ref/textmessageabbreviations_02.asp",
#'     "... understanding what different characters used in smiley faces mean:",
#'     "The close bracket represents a sideways smile  )",
#'     "Add in the colon and you have sideways eyes   :",
#'     "Put them together to make a smiley face  :)",
#'     "Use the dash -  to add a nose   :-)",
#'     "Change the colon to a semi-colon ; and you have a winking face ;)  with a nose  ;-)",
#'     "Put a zero 0 (halo) on top and now you have a winking, smiling angel 0;) with a nose 0;-)",
#'     "Use the letter 8 in place of the colon for sunglasses 8-)",
#'     "Use the open bracket ( to turn the smile into a frown  :-("
#' )
#'
#' replace_emoticon(x)
replace_emoticon <- function(text.var, emoticon_dt = lexicon::hash_emoticons, ...){
    gsub("\\s+", " ", .mgsub(emoticon_dt[["x"]], paste0(" ", emoticon_dt[["y"]], " "), text.var, ...))
}
