## UseResearch Template (Do not rename)

## Interactive Directions to Track Use of Functions
#
#  Note: Carefully select what functions you'd like to track and how functions are
#        embedded within other functions.  If `foo` is a function activated by
#        useResearch functionality, every time it is called, either directly or as a
#        dependency, the Google Form will be pinged.

## Steps
#  1. Set up a Google Form with a single text box (see `browseURL('https://goo.gl/jmHuLO')` )
#  2. Get the url from the form via `googleformr::make_url(URL , do = 'get')` like so:
#     `googleformr::make_url(
#                           'https://docs.google.com/forms/d/1tz2RPftOLRCQrGSvgJTRELrd9sdIrSZ_kxfoFdHiqD4/viewform'
#                           , do = 'get')`
#  3. Assign the url to the object `url` below:

# Assumes roxygen use or add useResearch manually
#' @importFrom useResearch gformr pingr

.ping <- useResearch::gformr('https://docs.google.com/forms/d/1FCgsqtP7ldrlYJuk14ov0AeDeX63Zx2Y25bF-HPSjSI/viewform')

#  4. Add tracking to the functions you desire in the space below using the `pingr`
#     function.  Remember to reassign the function back to itself as in the example.
#
#  **Attention**: We encourage the developer to add tracking responsibly, not simply out of
#     curiosity but rather with a desire to help your users benefit; and not on functions
#     haphazardly but rather only on those functions the developer already has questions
#     of where/how to invest their time.
#
#
#  Note: If you wish to add a batch of functions you can batch add them via the
#        `batch_process` function.  Simply use:
#
#        useResearch::batch_process(url = url, "myfun_1", "myfun_2", "myfun_n")
#
#        The contents will be printed to the console and attempted to be copied
#        to your clipboard for easy pasting.

sentiment <- useResearch::pingr(sentiment)  # Replace this function assignment with your own
#myfun_2 <- useResearch::pingr(myfun_2)  # Replace this function assignment with your own

#  5. Add the `useResearch` package to the Imports field of the 'DESCRIPTION' file.
#  6. Adjust documentation if needed (We use `devtools::document` and DO NOT need to adjust)
#  7. Push to GitHub, Build, or place in a repo.  Each time a useResearched
#     function is used by a user the form will receive a response with the
#     function name used and a time stamp.

## Stop/Alter User Monitoring
#
#  You can stop monitoring or change function monitoring by following these 3 steps:
#    (1) altering, removing this file from the package, or adding
#        `zzz_useResearch.R` to the '.Rbuildignore' file;
#    (2) removing `useResearch` from the Imports field in the
#        'DESCRIPTION' file (If you want to stop monitoring); and then
#    (3) rebuilding/pushing to the hosting repo
#
#  Google Forms can also be turned off to stop receiving responses.


## Disclosure
#
#  We recomend that developers using useResearch inform users of their package
#  about the information collected.  This could be displayed in:
#
#  1. The Project README
#  2. Using an `.onLoad` statement as follows...

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is the dev version of sentimentr\n",
                        "useResearch functionality is being utilized.\n"
  )
  invisible()
}
