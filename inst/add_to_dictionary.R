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

(check <- nrow(polarity_table))


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

# check if word is already there
valence_shifters_table["alot", ]

valence_shifters_table <- sentimentr::valence_shifters_table %>%
	   as.data.frame()

(check <- nrow(valence_shifters_table))
valence_shifters_table[check + 1, ] <- NA
valence_shifters_table[check + 1, "x"] <- "cares"
valence_shifters_table[check + 1, "y"] <- 1


valence_shifters_table <- valence_shifters_table %>%
	   arrange(x)

nrow(valence_shifters_table) == check + 1

data.table::setDT(valence_shifters_table)
data.table::setkey(valence_shifters_table, "x")

pax::new_data(valence_shifters_table)


