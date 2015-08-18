#========
# BUILD
#========
source("inst/build.R")

#==========================
# Run unit tests
#==========================
devtools::test()

#==========================
# knit README.md
#==========================
rmarkdown::render("README.Rmd", "all"); md_toc()

#==========================
# UPDATE NEWS
#==========================
update_news()

#==========================
# UPDATE VERSION
#==========================
update_version()

#========================
#staticdocs dev version
#========================

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("hadley/staticdocs", "trinker/acc.roxygen2")
p_load(rstudioapi, qdap)

R_USER <-  switch(Sys.info()[["user"]],
    Tyler = "C:/Users/Tyler",
    trinker = "C:/Users/trinker",
    message("Computer name not found")
)
build_site(pkg=file.path(R_USER, "GitHub", basename(getwd())), launch = FALSE)

#STEP 2: reshape index
path <- "inst/web"
path2 <- file.path(path, "/index.html")
rdme <- file.path(R_USER, "GitHub", basename(getwd()), "inst/extra_statdoc/readme.R")

extras <- qcv("")
## drops <- qcv()
expand_statdoc(path2, to.icon = extras, readme = rdme)

x <- readLines(path2)
x[grepl("<h2>Authors</h2>", x)] <- paste(
    c("<h2>Author</h2>"
    #rep("<h2>Contributor</h2>", 1)
    ),
    c("Tyler W. Rinker")
)

cat(paste(x, collapse="\n"), file=path2)

#STEP 3: move to trinker.guthub
library(reports)
file <- file.path(R_USER, "/GitHub/trinker.github.com")
# incoming <- file.path(file, basename(getwd()))
delete(incoming)
file.copy(path, file, TRUE, TRUE)
file.rename(file.path(file, "web"), incoming)
## delete(path)

#==========================
#staticdocs current version
#==========================

#STEP 3: move to trinker.guthub
library(reports)
file <- file.path(R_USER, "/GitHub/trinker.github.com")
incoming <- file.path(file, "discon")
##  delete(incoming); file.copy(path, file, TRUE, TRUE); file.rename(file.path(file, "web"), incoming)

#==========================
# NEWS new version
#==========================
x <- c("BUG FIXES", "NEW FEATURES", "MINOR FEATURES", "IMPROVEMENTS", "CHANGES")
cat(paste(x, collapse = "\n\n"), file="clipboard")



