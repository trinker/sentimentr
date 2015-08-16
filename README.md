sentimentr
============


[![Project Status: Wip - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/0.1.0/wip.svg)](http://www.repostatus.org/#wip)
[![Build
Status](https://travis-ci.org/trinker/sentimentr.svg?branch=master)](https://travis-ci.org/trinker/sentimentr)
[![Coverage
Status](https://coveralls.io/repos/trinker/sentimentr/badge.svg?branch=master)](https://coveralls.io/r/trinker/sentimentr?branch=master)
<a href="https://img.shields.io/badge/Version-0.0.1-orange.svg"><img src="https://img.shields.io/badge/Version-0.0.1-orange.svg" alt="Version"/></a>
</p>
<img src="inst/sentimentr_logo/r_sentimentr.png" width="150" alt="readability Logo">

**sentimentr** is designed to quickly calulate text polarity sentiment
at the sentence level and optionally aggregate by rows or grouping
variable(s).

equation used by the algorithm to assign value to polarity of each
sentence fist utilizes the sentiment dictionary (Hu and Liu, 2004) to
tag polarized words. A context cluster (*x*<sub>*i*</sub><sup>*T*</sup>)
of words is pulled from around this polarized word (default 4 words
before and two words after) to be considered as valence shifters. The
words in this context cluster are tagged as neutral
(*x*<sub>*i*</sub><sup>0</sup>), negator
(*x*<sub>*i*</sub><sup>*N*</sup>), amplifier
(*x*<sub>*i*</sub><sup>*a*</sup>), or de-amplifier
(*x*<sub>*i*</sub><sup>*d*</sup>). Neutral words hold no value in the
equation but do affect word count (*n*). Each polarized word is then
weighted *w* based on the weights from the `polarity_dt` argument and
then further weighted by the number and position of the valence shifters
directly surrounding the positive or negative word. The researcher may
provide a weight *c* to be utilized with amplifiers/de-amplifiers
(default is .8; deamplifier weight is constrained to -1 lower bound).
Last, these context cluster (*x*<sub>*i*</sub><sup>*T*</sup>) are summed
and divided by the square root of the word count ($\\sqrt{n}$) yielding
an unbounded polarity score (*δ*). Note that context clusters containing
a comma before the polarized word will only consider words found after
the comma.


Table of Contents
============

-   [Installation](#installation)
-   [Contact](#contact)
-   [Examples](#examples)

Installation
============


To download the development version of **sentimentr**:

Download the [zip
ball](https://github.com/trinker/sentimentr/zipball/master) or [tar
ball](https://github.com/trinker/sentimentr/tarball/master), decompress
and run `R CMD INSTALL` on it, or use the **pacman** package to install
the development version:

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load_gh("trinker/sentimentr")

Contact
=======

You are welcome to: 
* submit suggestions and bug-reports at: <https://github.com/trinker/sentimentr/issues> 
* send a pull request on: <https://github.com/trinker/sentimentr/> 
* compose a friendly e-mail to: <tyler.rinker@gmail.com>


Examples
========

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(sentimentr)

    mytext <- c(
        'do you like it?  But I hate really bad dogs',
        'I am the best friend.',
        'Do you really like it?  I\'m not a fan'
    )
    sentiment(mytext)

    ##    element_id sentence_id word_count  sentiment
    ## 1:          1           1          4  0.5000000
    ## 2:          1           2          6 -1.4696938
    ## 3:          2           1          5  0.4472136
    ## 4:          3           1          5  0.8049845
    ## 5:          3           2          4  0.0000000