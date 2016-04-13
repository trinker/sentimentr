#' Polarity Text Highlighting
#'
#' Highlight sentences within elements (row IDs) by sentiment polarity
#' (positive = green; negative = pink) as an html file.
#'
#' @param x A \code{sentiment_by} object.
#' @param original.text Optional original text (if the user wants to retain all
#' punctuation and capitalization).  Default uses the text stored in the
#' \code{sentiment_by} object that is striped of many punctuation marks and
#' capitalizations.
#' @param file A nam of the html file output.
#' @param open logical.  If ]code{TRUE} the text highlighting document will
#' attempt to be opened.
#' @param digits  The number of digits to print for each row level average
#' sentiment score.
#' @param \ldots Ignored.
#' @return Generates an html document with text highlighting.
#' @export
#' @examples
#' library(data.table)
#' dat <- presidential_debates_2012
#' setDT(dat)
#'
#' dat[, gr:={gr= paste(person, time); cumsum(c(TRUE, gr[-1]!= gr[-.N]))}]
#' dat <- dat[, list(person=person[1L], time=time[1L], dialogue=paste(dialogue,
#'     collapse = ' ')), by = gr][,gr:= NULL][]
#'
#' (sent_dat <- with(dat, sentiment_by(dialogue, list(person, time))))
#'
#' \dontrun{
#' highlight(sent_dat)
#' highlight(sent_dat, original.text = dat[["dialogue"]])
#'
#' highlight(with(cannon_reviews, sentiment_by(review, number)))
#' }
highlight <- function(x, original.text = NULL, file = "polarity.html",
    open = TRUE, digits = 3, ...){

    polarity <- grouping.var <- NULL

    if (!inherits(x, "sentiment_by")) stop("Must be a `sentiment_by` object")

    y <- uncombine(x)

    grps <- attributes(x)[["groups"]]

    data.table::setDT(y)
    y[, polarity := ifelse(sentiment > 0, "pos", ifelse(sentiment < 0, "neg", ""))][,
        polarity := ifelse(is.na(polarity), "", polarity)]

    if (!is.null(original.text)){
        txt <- get_sentences2(original.text)
    } else {
        txt <- get_sentences(x)
    }

    y[["txt"]] <- unlist(txt)

    y[, txt := ifelse(polarity == "", txt, sprintf("<mark class = \"%s\">%s</mark>", polarity, txt))]


    mygrps_1 <- paste(sprintf("%s=%s[1L]", grps, grps), collapse=", ")

    mygrps_2 <- parse(text=sprintf("paste(%s, sep=\", \")", paste(grps, collapse=", ")))
    suppressWarnings(y[, gr:={gr= eval(mygrps_2); cumsum(c(TRUE, gr[-1]!= gr[-.N]))}])

    y <- y[, list(sentiment = mean(sentiment, na.rm = TRUE),
             txt=paste(txt, collapse=' ')) , by = c(grps, "gr")]

    mygrps <- parse(text=sprintf("paste(%s, sep=\"_\")", paste(grps, collapse=", ")))

    y[, grouping.var:= eval(mygrps)]

    y[, txt := sprintf("<h1>%s: <em><span style=\"color: %s\">%s</span></em></h1><p class=\"indented\">%s</p>",
        grouping.var, ifelse(sentiment < 0, "red", ifelse(sentiment > 0, "green", "#D0D0D0")), formdig(sentiment, digits), txt)]

    body <- paste(y[["txt"]], collapse="\n")

    cat(sprintf(html, body), file = file)

    if (file.exists(file)){
        message(sprintf("Saved in %s", file))
        if (open){
            path <- normalizePath(file)
            message(sprintf("Oppening %s ...", file))
            utils::browseURL(paste0("file://", path))
        }
    }
}


formdig <- function(x, digits) {

    #reps <- x == 0
    pos <- x > 0
    if (is.null(digits)) digits <- 3

    if(length(digits) > 1) {
        digits <- digits[1]
        warning("Using only digits[1]")
    }

    x <- round(as.numeric(x), digits)

    if (digits > 0) x <- sprintf(paste0("%.", digits, "f"), x)
    out <- gsub("^0(?=\\.)|(?<=-)0", "", x, perl=TRUE)
    out[out %in% c("NA", "NaN")] <- ""
    #out[reps & !is.na(reps)] <- ""
    out[pos & !is.na(pos)] <- paste0("+", out[pos & !is.na(pos)])
    out
}


html <- c(
    "<!DOCTYPE html>", "<html lang=\"en\">", "", "<head>", "<meta charset=\"utf-8\">",
    "<title>Polarity</title>", "</head>", "", "<style>",
    "mark.pos {", "    background-color: lightgreen;", "    color: black;",
    "}", "", "mark.neg{", "    background-color: pink;", "    color: black;",
    "}", "%s", "</style>", "", "<body>", "", "%s", "", "</body>", "", "</html>\n"
)

style <- c("h1 { ", "    display: block;", "    font-size: 1.2em;", "    margin-top: 0.0em;",
    "    margin-bottom: 0.0em;", "    margin-left: 0;", "    margin-right: 0;",
    "    font-weight: bold;", "}", ".indented {", "    margin-left: 5%%;", "    margin-right: 5%%;",
    "}")

html <- sprintf(paste(html, collapse="\n"), paste(style, collapse="\n"), "%s")

