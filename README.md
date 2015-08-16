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

The equation used by the algorithm to assign value to polarity of each
sentence fist utilizes the sentiment dictionary (Hu and Liu, 2004) to
tag polarized words. Each paragraph
(*p*<sub>*i*</sub> = {*s*<sub>1</sub>, *s*<sub>2</sub>, ..., *s*<sub>*n*</sub>})
composed of sentences, is broken into element sentences
(*s*<sub>*i*</sub>*j* = {*w*<sub>1</sub>, *w*<sub>2</sub>, ..., *w*<sub>*n*</sub>})
where *w* are the words within sentences. Each sentence
(*s*<sub>*j*</sub>) is broken into a an ordered bag of words.
Punctuation is removed with the exception of pause punctuations (commas,
colons, semicolons) which are considered a word within the sentence. I
will denote pause words as *c**w* (comma words) for convience. We can
represent these words as an ijk notation as *w*<sub>*i**j**k*</sub>. For
example *w*<sub>325</sub> would be the fifth word of the second sentence
of the third paragraph. While I use the term paragraph this merely
represent a complete turn of talk. For example t may be a cell level
response in a questionare comprosed of sentences.

The words in each sentence (*w*<sub>*i**j**k*</sub>) are searched and
compared to a modified version of Hu, M., & Liu, B.'s (2004) dictionary
of polarized words. Positive (*w*<sub>*i**j**k*</sub><sup> + </sup>) and
negative (*w*<sub>*i**j**k*</sub><sup> − </sup>) words are tagged with a
 + 1 and  − 1 respectively (or other positive/negative weighting if the
user provides the sentiment dictionary). I will denote polarized words
as *p**w* for convience. These will form a polar cluster
(*c*<sub>*i**j**l*</sub>) which is a subset of the a sentence
(*c*<sub>*i**j**l*</sub> ⊆ *s*<sub>*i*</sub>*j*).

The polarized context cluster (*c*<sub>*i**j**l*</sub>) of words is
pulled from around the polarized word ($pw}) and defaults to 4 words
before and two words after *p**w*) to be considered as valence shifters.
The cluster can be represented as
(*c*<sub>*i**j**l*</sub> = {*p**w*<sub>*i**j**k* − *n**b*</sub>, ..., *p**w*<sub>*i**j**k*</sub>, ..., *p**w*<sub>*i**j**k* − *n**a*</sub>}),
where *n**b* & *n**a* are the parameters `n.before` and `n.after` set by
the user. The words in this polarized context cluster are tagged as
neutral (*w*<sub>*i**j**k*</sub><sup>0</sup>), negator
(*w*<sub>*i**j**k*</sub><sup>*n*</sup>), amplifier
(*w*<sub>*i**j**k*</sub><sup>*a*</sup>), or de-amplifier
(*w*<sub>*i**j**k*</sub><sup>*d*</sup>). Neutral words hold no value in
the equation but do affect word count (*n*). Each polarized word is then
weighted (*w*) based on the weights from the `polarity_dt` argument and
then further weighted by the function and number of the valence shifters
directly surrounding the positive or negative word (*p**w*). Pause
(*c**w*) locations (punctuation that denotes a pause including commas,
colons, and semicolons) are indexed and considered in calculating the
upper and lower bounds in the polarized context cluster. This is because
these marks indicate a change in thought and words prior are not
necessarily connected with words after these punctuation marks. The
lower bound of the polarized context cluster is constrained to
max{*p**w*<sub>*i**j**k* − *n**b*</sub>, 1, max{*c**w*<sub>*i**j**k*</sub> \< *p**w*<sub>*i**j**k*</sub>}}
and the upper bound is constrained to
min{*p**w*<sub>*i**j**k* + *n**a*</sub>, *w*<sub>*i**j**n*</sub>, min{*c**w*<sub>*i**j**k*</sub> \> *p**w*<sub>*i**j**k*</sub>}}
where *w*<sub>*i**j**n*</sub> is the number of words in the sentence.

The core value in the cluster, the polarized word is acted uppon by
valence shifters. Amplifiers increas the polarity by 1.8 (.8 is the
default weight (*z*)). Amplifiers
(*w*<sub>*i**j**k*</sub><sup>*a*</sup>) become de-amplifiers if the
clontext cluster contains an odd number of negators
(*w*<sub>*i**j**k*</sub><sup>*n*</sup>). De-amplifiers work to decrease
decrease the polarity. Negation (*w*<sub>*i**j**k*</sub><sup>*n*</sup>)
acts on amplifiers/de-amplifiers as discussed but also flip the sign of
the polarized word. Negation is determined by raising  − 1 to the power
of the number of negators (*w*<sub>*i**j**k*</sub><sup>*n*</sup>).
Simply, this is a result of a belief that two negatives qual a positive,
3 negatives a negative and so on.

The researcher may provide a weight (*z*) to be utilized with
amplifiers/de-amplifiers (default is .8; de-amplifier weight is
constrained to  − 1 lower bound). Last, these weighted context clusters
(*c*<sub>*i**j**l*</sub>) are summed (*c*′<sub>*i**j*</sub>) and divided
by the square root of the word count
(<sqrt><em>w\_<sub>ijn</sub></em></sqrt>) yielding an unbounded polarity
score (*δ*<sub>*i**j*</sub>) for each sentence.

$$
\\delta\_{ij}=\\frac{c'\_{ij}}{w\_{ijn}}}
$$

Where:

*c*′<sub>*i**j*</sub> = ∑((1 + *w*<sub>*a**m**p*</sub> + *w*<sub>*d**e**a**m**p*</sub>) ⋅ *w*<sub>*i**j**k*</sub><sup>*p*</sup>( − 1)<sup>∑*w*<sub>*i**j**k*</sub><sup>*n*</sup></sup>)

*w*<sub>*a**m**p*</sub> = ∑(*w*<sub>*n**e**g*</sub> ⋅ (*z* ⋅ *w*<sub>*i**j**k*</sub><sup>*a*</sup>))

*w*<sub>*d**e**a**m**p*</sub> = max(*w*<sub>*d**e**a**m**p*′</sub>,  − 1)

*w*<sub>*d**e**a**m**p*′</sub> = ∑(*z*( − *w*<sub>*n**e**g*</sub> ⋅ *w*<sub>*i**j**k*</sub><sup>*a*</sup> + *w*<sub>*i**j**k*</sub><sup>*d*</sup>))

$$w\_{neg}= \\left(\\sum{w\_{ijk}^{n}}\\right) \\bmod {2}$$


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