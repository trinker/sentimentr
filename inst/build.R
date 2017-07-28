root <- Sys.getenv("USERPROFILE")
pack <- basename(getwd())

quick <-  TRUE
pdf <- FALSE

unlink(paste0(pack, ".pdf"), recursive = TRUE, force = TRUE)
devtools::document()
devtools::install(quick = quick, build_vignettes = FALSE, dependencies = TRUE)

if(pdf){
    path <- find.package(pack)
    system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))
    file.copy(paste0(pack, '.pdf'), file.path(root,"Desktop", paste0(pack, '.pdf')))
    while (file.exists(paste0(pack, ".pdf"))) {unlink(paste0(pack, ".pdf"), recursive = TRUE, force = TRUE)}
    empts <- grep("^\\.Rd", dir(all.files = TRUE), value = TRUE)
    unlink(empts, recursive = TRUE, force = TRUE)    
}

message("Done!")