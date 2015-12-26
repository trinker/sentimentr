pacman::p_load(qdapRegex, dplyr, data.table, readr, sentimentr, stansent)

sentiword <- read.csv("SentiWordNet_3.0.0_20130122.txt", sep="\t", stringsAsFactors = FALSE)



sentiword <- sentiword %>%
    tbl_df() %>%
    select(-Gloss) %>%
    mutate(y = PosScore - NegScore) %>%
    filter(!is.na(ID)) %>%
    as.data.frame(stringsAsFactors = FALSE) 

setDT(sentiword)
    
sentiword <- sentiword[, .(x = gsub("_|-", " ", unlist(strsplit(SynsetTerms, "#\\d{1,2}\\s*")))), by = y][, list(x, y)][which(y != 0)]

sentiword <- sentiword[, list(y = mean(y, na.rm=TRUE)), by=x]

rms <- sentiword[[1]][sentiword[[1]] %in% sentimentr::valence_shifters_table[[1]]]

sentiword <- sentiword[!x %in% rms & nchar(x) > 1 & nchar(x) < 30]

newp <- c('but', 'although', 'however')
sentiword[[1]][sentiword[[1]] %in% newp]

sentiword <- sentiword[!x %in% newp]

setkey(sentiword, "x")

sentiword %>% write_csv("sentiword.csv")

sentiword


sentiment(sentiword)

?sentiment_stanford

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/qdap", "trinker/sentimentr")
pacman::p_load(syuzhet)

temp <- tempdir()
pang_et_al <- "http://www.cs.cornell.edu/people/pabo/movie-review-data/review_polarity.tar.gz"
download.file(pang_et_al, file.path(temp, basename(pang_et_al)))

x <- c(
    "I haven't been sad in a long time.",
    "I am extremely happy today.",
    "It's a good day.",
    "But suddenly I'm only a little bit happy.",
    "Then I'm not happy at all.",
    "In fact, I am now the least happy person on the planet.",
    "There is no happiness left in me.",
    "Wait, it's returned!",
    "I don't feel so bad after all!"
)

untar(file.path(temp, basename(pang_et_al)), exdir = file.path(temp, "out"))
dirs <- sprintf(file.path(temp, "out/txt_sentoken/%s"), c("neg", "pos"))
text_vector <- paste(unlist(lapply(
    c(file.path(dirs[1], dir(dirs[1])[1]),
        file.path(dirs[2], dir(dirs[2])[1])
    ),  readLines)), collapse = " ")

sents <- sentimentr::get_sentences(x)

senti <- lapply(sents, sentiment_stanford)

syuzhet <- setNames(as.data.frame(lapply(c("bing", "afinn", "nrc"),
    function(x) get_sentiment(sents[[1]], method=x))), c("bing", "afinn", "nrc"))

width <- options()$width
options(width=1000)

left_just(data.frame(
    stanford = unlist(senti),
    sentimentr = round(sentiment(sents, question.weight = 0)[["sentiment"]], 2),
    sentimentr_sentiword = round(sentiment(sents, sentiword, question.weight = 0)[["sentiment"]], 2),
    syuzhet,
    sentences = unlist(sents),
    stringsAsFactors = FALSE
), "sentences")

options(width=width)



