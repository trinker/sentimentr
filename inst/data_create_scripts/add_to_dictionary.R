if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, data.table)
pacman::p_load_gh("trinker/sentimentr", "trinker/pax")

#====================================================
#polarity table

word <- "effective"
value <- -1



# check if word is already there
polarity_table[word, ]

polarity_table <- sentimentr::polarity_table %>%
	   as.data.frame()

qdapRegex::ex_default(presidential_debates_2012$dialogue, pattern="(?i)\\b\\w+\\s+(or\\s+)+((the|a|an)\\s+)*\\w+") %>% unlist() %>% na.omit() %>% c() %>% unique()

new_key <- update_key(as_key(polarity_table), x = data.frame(x = c("too good"), y = c(-1)))

sentiment("I too often that I pay too good for food.", new_key)

valence_shifters_table[c('extraordinarily')]

valence_shifters_table[c('too often')]

(check <- nrow(polarity_table))

polarity_table[c('too many')]
polarity_table[c('rarely')]

polarity_table[check + 1, ] <- NA
polarity_table[check + 1, "x"] <- word
polarity_table[check + 1, "y"] <- value


polarity_table <- polarity_table %>%
	   arrange(x)

nrow(polarity_table)
nrow(polarity_table) == check + 1

data.table::setDT(polarity_table)
data.table::setkey(polarity_table, "x")

pax::new_data(polarity_table)

# Removing term
polarity_table <- polarity_table[!1:nrow(polarity_table) %in% tail(which(polarity_table$x == "helped"), -1), ]
nrow(polarity_table)
rownames(polarity_table) <- NULL

#====================================================
#valence_shifters_table

newp <- c('but', 'although', 'however')
# check if word is already there
valence_shifters_table[newp, ]

valence_shifters_table <- sentimentr::valence_shifters_table %>%
	   as.data.frame()

(check <- nrow(valence_shifters_table))
valence_shifters_table[check + 1:3, ] <- NA
valence_shifters_table[check + 1:3, "x"] <- newp
valence_shifters_table[check + 1:3, "y"] <- 4


valence_shifters_table <- valence_shifters_table %>%
	   arrange(x)

nrow(valence_shifters_table) == check + 1

data.table::setDT(valence_shifters_table)
data.table::setkey(valence_shifters_table, "x")

pax::new_data(valence_shifters_table)


#====================================================
#emoticons

newp <- c('0;)', '0;-)')
# check if word is already there
emoticons[newp, ]

emoticons <- sentimentr::emoticons %>%
	   as.data.frame()

emoticons <- emoticons %>%
        arrange(x)

dups <- which(duplicated(emoticons[[1]]))
emoticons[c(dups), ]

(check <- nrow(emoticons))
emoticons[check + 1:2, ] <- NA
emoticons[check + 1:2, "x"] <- newp
emoticons[check + 1:2, "y"] <- c("angel", "angel")


emoticons <- emoticons %>%
	   arrange(x)


emoticons[which(duplicated(emoticons[[1]]) | duplicated(emoticons[[1]], fromLast=TRUE)), ]

emoticons <- emoticons[-c(16, 19, 23, 24, 27, 53, 64), ]

nrow(emoticons) == check + 1

data.table::setDT(emoticons)
data.table::setkey(emoticons, "x")

pax::new_data(emoticons)



valence_shifters_table <- update_key(sentimentr::valence_shifters_table, drop=NULL, 
    x = data.frame(
        x = c("exceedingly", "remarkably", "especially"), 
        y = c(2, 2, 2), stringsAsFactors = FALSE
    ), 
    comparison=sentimentr::polarity_table, 
    sentiment=F
)

polarity_table <- update_key(sentimentr::polarity_table, 
    x = data.frame(
        x = c('i wish'), 
        y = c(-2)
    ), drop = 'I wish'
)


pax::new_data(valence_shifters_table)
pax::new_data(polarity_table)
