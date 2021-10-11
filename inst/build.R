root <- Sys.getenv("USERPROFILE")
pack <- basename(getwd())

quick <-  TRUE
pdf <- FALSE

unlink(paste0(pack, ".pdf"), recursive = TRUE, force = TRUE)
devtools::document()
devtools::install(quick = quick, build_vignettes = FALSE, dependencies = TRUE, upgrade = 'never')

if(pdf){
    path <- find.package(pack)
    system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))
    file.copy(paste0(pack, '.pdf'), file.path(root,"Desktop", paste0(pack, '.pdf')))
    while (file.exists(paste0(pack, ".pdf"))) {unlink(paste0(pack, ".pdf"), recursive = TRUE, force = TRUE)}
    empts <- grep("^\\.Rd", dir(all.files = TRUE), value = TRUE)
    unlink(empts, recursive = TRUE, force = TRUE)    
}

message("Done!")



nh <- function() cat(paste(c("BUG FIXES", "NEW FEATURES", "MINOR FEATURES", "IMPROVEMENTS", "CHANGES"), collapse = "\n\n"), file="clipboard")



update_news <- function(repo = basename(getwd())) {

    News <- readLines("NEWS")

    News <- textclean::mgsub(News, 
        c("<", ">", "&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;", "BUG FIXES",
            "NEW FEATURES", "MINOR FEATURES", "CHANGES", "IMPROVEMENTS", " TRUE ", " FALSE ",
            " NULL ", "TRUE.", "FALSE.", "NULL.", ":m:"),
        c("&lt;", "&gt;", "**&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;**",
            "**BUG FIXES**", "**NEW FEATURES**", "**MINOR FEATURES**",
            "**CHANGES**", "**IMPROVEMENTS**", " `TRUE` ", "`FALSE`.", "`NULL`.", "`TRUE`.",
            " `FALSE` ", " `NULL` ", " : m : "),
            trim = FALSE, fixed=TRUE)

    News <- sub(pattern="issue *# *([0-9]+)",
        replacement=sprintf("<a href=\"https://github.com/trinker/%s/issues/\\1\">issue #\\1</a>",
        repo),
        x=News)

    News <- sub(pattern="pull request *# *([0-9]+)",
        replacement=sprintf("<a href=\"https://github.com/trinker/%s/issues/\\1\">pull request #\\1</a>",
        repo),
        x=News)

    News <- gsub(sprintf(" %s", repo),
        sprintf(" <a href=\"https://github.com/trinker/%s\" target=\"_blank\">%s</a>",
        repo, repo), News)
    
    News <- gsub(pattern="(#)([0-9]+)",
        replacement=sprintf("<a href=\"https://github.com/trinker/%s/issues/\\2\">#\\2</a>", repo),
        x=News)    

    cat(paste(News, collapse = "\n"), file = "NEWS.md")
    message("news.md updated")
}
