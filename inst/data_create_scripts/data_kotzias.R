if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/sentimentr", "trinker/stansent", "trinker/textshape")
pacman::p_load(syuzhet, dplyr, tidyr, downloader, pathr, qdapRegex)

loc <- "sentiment_data"
dir.create(loc)

'http://archive.ics.uci.edu/ml/machine-learning-databases/00331/sentiment%20labelled%20sentences.zip' %>%
    download(file.path(loc, "sentiment_labelled_sentences.zip"), mode = "wb")

unzip(file.path(loc, "sentiment_labelled_sentences.zip"), exdir = loc)


kotzias_reviews <- file.path(loc, "sentiment labelled sentences") %>%
    dir(pattern = "labelled\\.txt$", full.names = TRUE) %>%
    lapply(function(x){

    ## read in the data and split into sentences
    dat <- x %>%
        read.delim(sep = "\t", header=FALSE, stringsAsFactors = FALSE,
            strip.white = TRUE, quote = "") %>%
        setNames(c("text", "rating")) %>%
        na.omit() %>%
        mutate(
            rating = 2*(as.numeric(rating) - .5),
            text = qdapRegex::rm_non_ascii(text)

        ) %>%
        split_sentence()

})

names(kotzias_reviews) <- file_path(loc, "sentiment labelled sentences") %>%
    dir(pattern = "labelled\\.txt$") %>%
    no_file_ext() %>%
    gsub("_labelled", "", .)

pax::new_data(kotzias_reviews)


