sentimentr   [![Follow](https://img.shields.io/twitter/follow/tylerrinker.svg?style=social)](https://twitter.com/intent/follow?screen_name=tylerrinker)
============


[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Build
Status](https://travis-ci.org/trinker/sentimentr.svg?branch=master)](https://travis-ci.org/trinker/sentimentr)
[![Coverage
Status](https://coveralls.io/repos/trinker/sentimentr/badge.svg?branch=master)](https://coveralls.io/r/trinker/sentimentr?branch=master)
[![DOI](https://zenodo.org/badge/5398/trinker/sentimentr.svg)](https://zenodo.org/badge/latestdoi/5398/trinker/sentimentr)
[![](http://cranlogs.r-pkg.org/badges/sentimentr)](https://cran.r-project.org/package=sentimentr)
<a href="https://img.shields.io/badge/Version-0.5.3-orange.svg"><img src="https://img.shields.io/badge/Version-0.5.3-orange.svg" alt="Version"/></a>
</p>
<img src="inst/sentimentr_logo/r_sentimentr.png" width="150" alt="readability Logo">

**sentimentr** is designed to quickly calculate text polarity sentiment
at the sentence level and optionally aggregate by rows or grouping
variable(s).

**sentimentr** is a response to my own needs with sentiment detection
that were not addressed by the current **R** tools. My own `polarity`
function in the **qdap** package is slower on larger data sets. It is a
dictionary lookup approach that tries to incorporate weighting for
valence shifters (negation and amplifiers/deamplifiers). Matthew
Jocker's created the
[**syuzhet**](http://www.matthewjockers.net/2015/02/02/syuzhet/) package
that utilizes dictionary lookups for the Bing, NRC, and Afinn methods.
He also utilizes a wrapper for the [Stanford
coreNLP](http://nlp.stanford.edu/software/corenlp.shtml) which uses much
more sophisticated analysis. Jocker's dictionary methods are fast but
are more prone to error in the case of valence shifters. Jocker's
[addressed these
critiques](http://www.matthewjockers.net/2015/03/04/some-thoughts-on-annies-thoughts-about-syuzhet/)
explaining that the method is good with regard to analyzing general
sentiment in a piece of literature. He points to the accuracy of the
Stanford detection as well. In my own work I need better accuracy than a
simple dictionary lookup; something that considers valence shifters yet
optimizes speed which the Stanford's parser does not. This leads to a
trade off of speed vs. accuracy. The equation below describes the
dictionary method of **sentimentr** that may give better results than a
dictionary approach that does not consider valence shifters but will
likely still be less accurate than Stanford's approach. Simply,
**sentimentr** attempts to balance accuracy and speed.

***So what does*** **sentimentr** ***do that other packages don't and
why does it matter?***

> **sentimentr** attempts to take into account valence shifters (i.e.,
> negators, amplifiers, de-amplifiers, and adversative conjunctions)
> while maintaining speed. Simply put, **sentimentr** is an augmented
> dictionary lookup. The next questions address why it matters.

***So what are these valence shifters?***

> A *negator* flips the sign of a polarized word (e.g., "I do ***not***
> like it."). See `lexicon::hash_valence_shifters[y==1]` for examples.
> An *amplifier* increases the impact of a polarized word (e.g., "I
> ***really*** like it."). See `lexicon::hash_valence_shifters[y==2]`
> for examples. A *de-amplifier* reduces the impact of a polarized word
> (e.g., "I ***hardly*** like it."). See
> `lexicon::hash_valence_shifters[y==3]` for examples. An *adversative
> conjunction* overrule the previous clause with a polarized word (e.g.,
> "I like it ***but*** it's not worth it."). See
> `lexicon::hash_valence_shifters[y==4]` for examples.

***Do valence shifters really matter?***

> Well valence shifters affect the polarize word. In the case of
> *negators* and *adversative conjunctions* the entire sentiment of the
> clause may be reversed or overruled. So if valence occur fairly
> frequently a simple dictionary lookup may not be modeling the
> sentiment appropriately. You may be wondering how frequently these
> valence shifters co-occur with polarized words, potentially changing,
> or even reversing and overruling the clause's sentiment. The table
> below shows the rate of sentence level co-occurrence of valence
> shifters with polarized words across a few types of texts.

<table>
<thead>
<tr class="header">
<th align="left">Text</th>
<th align="right">Negator</th>
<th align="right">Amplifier</th>
<th align="right">Deamplifier</th>
<th align="right">Adversative</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Cannon reviews</td>
<td align="right">20%</td>
<td align="right">23%</td>
<td align="right">9%</td>
<td align="right">12%</td>
</tr>
<tr class="even">
<td align="left">2012 presidential debate</td>
<td align="right">23%</td>
<td align="right">20%</td>
<td align="right">1%</td>
<td align="right">11%</td>
</tr>
<tr class="odd">
<td align="left">Trump speeches</td>
<td align="right">12%</td>
<td align="right">15%</td>
<td align="right">2%</td>
<td align="right">10%</td>
</tr>
<tr class="even">
<td align="left">Trump tweets</td>
<td align="right">20%</td>
<td align="right">18%</td>
<td align="right">4%</td>
<td align="right">5%</td>
</tr>
<tr class="odd">
<td align="left">Dylan songs</td>
<td align="right">19%</td>
<td align="right">6%</td>
<td align="right">2%</td>
<td align="right">7%</td>
</tr>
<tr class="even">
<td align="left">Austen books</td>
<td align="right">21%</td>
<td align="right">19%</td>
<td align="right">6%</td>
<td align="right">11%</td>
</tr>
<tr class="odd">
<td align="left">Hamlet</td>
<td align="right">27%</td>
<td align="right">18%</td>
<td align="right">2%</td>
<td align="right">18%</td>
</tr>
</tbody>
</table>

Indeed *negators* appear ~20% of the time a polarized word appears in a
sentence. Conversely, *adversative conjunctions* appear with polarized
words ~10% of the time. Not accounting for the valence shifters could
significantly impact the modeling of the text sentiment.

The script to replicate the frequency analysis from the table can be
downloaded via:

    val_shift_freq <- system.file("the_case_for_sentimentr/valence_shifter_cooccurrence_rate.R", package = "sentimentr")
    file.copy(val_shift_freq, getwd())


Table of Contents
============

-   [Functions](#functions)
-   [The Equation](#the-equation)
-   [Installation](#installation)
-   [Examples](#examples)
    -   [Plotting](#plotting)
        -   [Plotting at Aggregated Sentiment](#plotting-at-aggregated-sentiment)
        -   [Plotting at the Sentence Level](#plotting-at-the-sentence-level)
    -   [Making and Updating Dictionaries](#making-and-updating-dictionaries)
    -   [Annie Swafford's Examples](#annie-swaffords-examples)
    -   [Comparing sentimentr, syuzhet, RSentiment, meanr, and Stanford](#comparing-sentimentr-syuzhet-rsentiment-meanr-and-stanford)
    -   [Text Highlighting](#text-highlighting)
-   [Contact](#contact)

Functions
============


There are two main functions (top 2 in table below) in **sentimentr**
with several helper functions summarized in the table below:

<table style="width:100%;">
<colgroup>
<col width="25%" />
<col width="74%" />
</colgroup>
<thead>
<tr class="header">
<th>Function</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>sentiment</code></td>
<td>Sentiment at the sentence level</td>
</tr>
<tr class="even">
<td><code>sentiment_by</code></td>
<td>Aggregated sentiment by group(s)</td>
</tr>
<tr class="odd">
<td><code>uncombine</code></td>
<td>Extract sentence level sentiment from <code>sentiment_by</code></td>
</tr>
<tr class="even">
<td><code>get_sentences</code></td>
<td>Regex based string to sentence parser (or get sentences from <code>sentiment</code>/<code>sentiment_by</code>)</td>
</tr>
<tr class="odd">
<td><code>replace_emoticon</code></td>
<td>Replace emoticons with word equivalent</td>
</tr>
<tr class="even">
<td><code>replace_grade</code></td>
<td>Replace grades (e.g., &quot;A+&quot;) with word equivalent</td>
</tr>
<tr class="odd">
<td><code>replace_rating</code></td>
<td>Replace ratings (e.g., &quot;10 out of 10&quot;, &quot;3 stars&quot;) with word equivalent</td>
</tr>
<tr class="even">
<td><code>as_key</code></td>
<td>Coerce a <code>data.frame</code> lexicon to a polarity hash key</td>
</tr>
<tr class="odd">
<td><code>is_key</code></td>
<td>Check if an object is a hash key</td>
</tr>
<tr class="even">
<td><code>update_key</code></td>
<td>Add/remove terms to/from a hash key</td>
</tr>
<tr class="odd">
<td><code>highlight</code></td>
<td>Highlight positive/negative sentences as an HTML document</td>
</tr>
<tr class="even">
<td><code>general_rescale</code></td>
<td>Generalized rescaling function to rescale sentiment scoring</td>
</tr>
<tr class="odd">
<td><code>sentiment_attribute</code></td>
<td>Extract the sentiment based attributes from a text</td>
</tr>
<tr class="even">
<td><code>validate_sentiment</code></td>
<td>Validate sentiment score sign against known results</td>
</tr>
</tbody>
</table>

The Equation
============

The equation used by the algorithm to assign value to polarity of each
sentence fist utilizes the sentiment dictionary (Hu and Liu,
[2004](http://www.cs.uic.edu/~liub/publications/kdd04-revSummary.pdf))
to tag polarized words. Each paragraph
(*p*<sub>*i*</sub> = {*s*<sub>1</sub>, *s*<sub>2</sub>, ..., *s*<sub>*n*</sub>})
composed of sentences, is broken into element sentences
(*s*<sub>*i*</sub>, *j* = {*w*<sub>1</sub>, *w*<sub>2</sub>, ..., *w*<sub>*n*</sub>})
where *w* are the words within sentences. Each sentence
(*s*<sub>*j*</sub>) is broken into a an ordered bag of words.
Punctuation is removed with the exception of pause punctuations (commas,
colons, semicolons) which are considered a word within the sentence. I
will denote pause words as *c**w* (comma words) for convenience. We can
represent these words as an i,j,k notation as
*w*<sub>*i*, *j*, *k*</sub>. For example *w*<sub>3, 2, 5</sub> would be
the fifth word of the second sentence of the third paragraph. While I
use the term paragraph this merely represent a complete turn of talk.
For example it may be a cell level response in a questionnaire composed
of sentences.

The words in each sentence (*w*<sub>*i*, *j*, *k*</sub>) are searched
and compared to a modified version of Hu, M., & Liu, B.'s (2004)
dictionary of polarized words. Positive
(*w*<sub>*i*, *j*, *k*</sub><sup>+</sup>) and negative
(*w*<sub>*i*, *j*, *k*</sub><sup>−</sup>) words are tagged with a +1 and
−1 respectively (or other positive/negative weighting if the user
provides the sentiment dictionary). I will denote polarized words as
*p**w* for convenience. These will form a polar cluster
(*c*<sub>*i*, *j*, *l*</sub>) which is a subset of the a sentence
(*c*<sub>*i*, *j*, *l*</sub> ⊆ *s*<sub>*i*</sub>, *j*).

The polarized context cluster (*c*<sub>*i*, *j*, *l*</sub>) of words is
pulled from around the polarized word (*p**w*) and defaults to 4 words
before and two words after *p**w* to be considered as valence shifters.
The cluster can be represented as
(*c*<sub>*i*, *j*, *l*</sub> = {*p**w*<sub>*i*, *j*, *k* − *n**b*</sub>, ..., *p**w*<sub>*i*, *j*, *k*</sub>, ..., *p**w*<sub>*i*, *j*, *k* − *n**a*</sub>}),
where *n**b* & *n**a* are the parameters `n.before` and `n.after` set by
the user. The words in this polarized context cluster are tagged as
neutral (*w*<sub>*i*, *j*, *k*</sub><sup>0</sup>), negator
(*w*<sub>*i*, *j*, *k*</sub><sup>*n*</sup>), amplifier
(*w*<sub>*i*, *j*, *k*</sub><sup>*a*</sup>), or de-amplifier
(*w*<sub>*i*, *j*, *k*</sub><sup>*d*</sup>). Neutral words hold no value
in the equation but do affect word count (*n*). Each polarized word is
then weighted (*w*) based on the weights from the `polarity_dt` argument
and then further weighted by the function and number of the valence
shifters directly surrounding the positive or negative word (*p**w*).
Pause (*c**w*) locations (punctuation that denotes a pause including
commas, colons, and semicolons) are indexed and considered in
calculating the upper and lower bounds in the polarized context cluster.
This is because these marks indicate a change in thought and words prior
are not necessarily connected with words after these punctuation marks.
The lower bound of the polarized context cluster is constrained to
max{*p**w*<sub>*i*, *j*, *k* − *n**b*</sub>, 1, max{*c**w*<sub>*i*, *j*, *k*</sub> &lt; *p**w*<sub>*i*, *j*, *k*</sub>}}
and the upper bound is constrained to
min{*p**w*<sub>*i*, *j*, *k* + *n**a*</sub>, *w*<sub>*i*, *j**n*</sub>, min{*c**w*<sub>*i*, *j*, *k*</sub> &gt; *p**w*<sub>*i*, *j*, *k*</sub>}}
where *w*<sub>*i*, *j**n*</sub> is the number of words in the sentence.

The core value in the cluster, the polarized word is acted upon by
valence shifters. Amplifiers increase the polarity by 1.8 (.8 is the
default weight (*z*)). Amplifiers
(*w*<sub>*i*, *j*, *k*</sub><sup>*a*</sup>) become de-amplifiers if the
context cluster contains an odd number of negators
(*w*<sub>*i*, *j*, *k*</sub><sup>*n*</sup>). De-amplifiers work to
decrease the polarity. Negation
(*w*<sub>*i*, *j*, *k*</sub><sup>*n*</sup>) acts on
amplifiers/de-amplifiers as discussed but also flip the sign of the
polarized word. Negation is determined by raising −1 to the power of the
number of negators (*w*<sub>*i*, *j*, *k*</sub><sup>*n*</sup>) plus 2.
Simply, this is a result of a belief that two negatives equal a
positive, 3 negatives a negative, and so on.

The adversative conjunctions (i.e., 'but', 'however', and 'although')
also weight the context cluster. An adversative conjunction before the
polarized word
(*w*<sub>*a**d**v**e**r**s**a**t**i**v**e* *c**o**n**j**u**n**c**t**i**o**n*</sub>, ..., *w*<sub>*i*, *j*, *k*</sub><sup>*p*</sup>)
up-weights the cluster by 1 +
*z*<sub>2</sub> \* {|*w*<sub>*a**d**v**e**r**s**a**t**i**v**e* *c**o**n**j**u**n**c**t**i**o**n*</sub>|,...,*w*<sub>*i*, *j*, *k*</sub><sup>*p*</sup>}
(.85 is the default weight (*z*<sub>2</sub>) where
|*w*<sub>*a**d**v**e**r**s**a**t**i**v**e* *c**o**n**j**u**n**c**t**i**o**n*</sub>|
are the number of adversative conjunctions before the polarized word). A
adversative conjunction after the polarized word down-weights the
cluster by 1 +
{*w*<sub>*i*, *j*, *k*</sub><sup>*p*</sup>, ..., |*w*<sub>*b**u**t* *c**o**n**j**u**n**c**t**i**o**n*</sub>|\* − 1}\**z*<sub>2</sub>.
This corresponds to the belief that an adversative conjunction makes the
next clause of greater values while lowering the value placed on the
prior clause.

The researcher may provide a weight (*z*) to be utilized with
amplifiers/de-amplifiers (default is .8; de-amplifier weight is
constrained to −1 lower bound). Last, these weighted context clusters
(*c*<sub>*i*, *j*, *l*</sub>) are summed (*c*′<sub>*i*, *j*</sub>) and
divided by the square root of the word count
(√*w*<sub>*i*, *j**n*</sub>) yielding an unbounded polarity score
(*δ*<sub>*i*, *j*</sub>) for each sentence.

*δ*<sub>*i**j*</sub> =
<em>c</em>'<sub>*i**j*</sub>/√*w*<sub>*i**j**n*</sub>

Where:

*c*′<sub>*i*, *j*</sub> = ∑((1 + *w*<sub>*a**m**p*</sub> + *w*<sub>*d**e**a**m**p*</sub>)⋅*w*<sub>*i*, *j*, *k*</sub><sup>*p*</sup>(−1)<sup>2 + *w*<sub>*n**e**g*</sub></sup>)

*w*<sub>*a**m**p*</sub> = ∑(*w*<sub>*n**e**g*</sub> ⋅ (*z* ⋅ *w*<sub>*i*, *j*, *k*</sub><sup>*a*</sup>))

*w*<sub>*d**e**a**m**p*</sub> = max(*w*<sub>*d**e**a**m**p*′</sub>, −1)

*w*<sub>*d**e**a**m**p*′</sub> = ∑(*z*(−*w*<sub>*n**e**g*</sub> ⋅ *w*<sub>*i*, *j*, *k*</sub><sup>*a*</sup> + *w*<sub>*i*, *j*, *k*</sub><sup>*d*</sup>))

*w*<sub>*b*</sub> = 1 + *z*<sub>2</sub> \* *w*<sub>*b*′</sub>

*w*<sub>*b*′</sub> = ∑(|*w*<sub>*a**d**v**e**r**s**a**t**i**v**e* *c**o**n**j**u**n**c**t**i**o**n*</sub>|,...,*w*<sub>*i*, *j*, *k*</sub><sup>*p*</sup>, *w*<sub>*i*, *j*, *k*</sub><sup>*p*</sup>, ..., |*w*<sub>*a**d**v**e**r**s**a**t**i**v**e* *c**o**n**j**u**n**c**t**i**o**n*</sub>|\* − 1)

*w*<sub>*n**e**g*</sub> = (∑*w*<sub>*i*, *j*, *k*</sub><sup>*n*</sup> )
mod 2

To get the mean of all sentences (*s*<sub>*i*, *j*</sub>) within a
paragraph (*p*<sub>*i*</sub>) simply take the average sentiment score
*p*<sub>*i*, *δ*<sub>*i*, *j*</sub></sub> = 1/n ⋅ ∑
*δ*<sub>*i*, *j*</sub>.

Installation
============

To download the development version of **sentimentr**:

Download the [zip
ball](https://github.com/trinker/sentimentr/zipball/master) or [tar
ball](https://github.com/trinker/sentimentr/tarball/master), decompress
and run `R CMD INSTALL` on it, or use the **pacman** package to install
the development version:

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load_current_gh("trinker/lexicon", "trinker/sentimentr")

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
    ## 2:          1           2          6 -2.6781088
    ## 3:          2           1          5  0.4472136
    ## 4:          3           1          5  0.8049845
    ## 5:          3           2          4  0.0000000

To aggregate by element (column cell or vector element) use
`sentiment_by` with `by = NULL`.

    mytext <- c(
        'do you like it?  But I hate really bad dogs',
        'I am the best friend.',
        'Do you really like it?  I\'m not a fan'
    )
    sentiment_by(mytext)

    ##    element_id word_count       sd ave_sentiment
    ## 1:          1         10 2.247262    -1.0890544
    ## 2:          2          5       NA     0.4472136
    ## 3:          3          9 0.569210     0.4392690

To aggregate by grouping variables use `sentiment_by` using the `by`
argument.

    (out <- with(presidential_debates_2012, sentiment_by(dialogue, list(person, time))))

    ##        person   time word_count        sd ave_sentiment
    ##  1:     OBAMA time 1       3598 0.4489512    0.17963450
    ##  2:     OBAMA time 2       7476 0.3878883    0.21629706
    ##  3:     OBAMA time 3       7241 0.4408708    0.19103322
    ##  4:    ROMNEY time 1       4085 0.3669465    0.11715169
    ##  5:    ROMNEY time 2       7534 0.3271200    0.10396263
    ##  6:    ROMNEY time 3       8302 0.3866709    0.13649872
    ##  7:   CROWLEY time 2       1672 0.2356299    0.24872950
    ##  8:    LEHRER time 1        765 0.3634981    0.32699539
    ##  9:  QUESTION time 2        583 0.3282897    0.04967577
    ## 10: SCHIEFFER time 3       1445 0.3810998    0.16056579

Plotting
--------

### Plotting at Aggregated Sentiment

    plot(out)

![](inst/figure/unnamed-chunk-9-1.png)

### Plotting at the Sentence Level

The `plot` method for the class `sentiment` uses **syuzhet**'s
`get_transformed_values` combined with **ggplot2** to make a reasonable,
smoothed plot for the duration of the text based on percentage, allowing
for comparison between plots of different texts. This plot gives the
overall shape of the text's sentiment. The user can see
`syuzhet::get_transformed_values` for more details.

    plot(uncombine(out))

![](inst/figure/unnamed-chunk-10-1.png)

Making and Updating Dictionaries
--------------------------------

It is pretty straight forward to make or update a new dictionary
(polarity or valence shifter). To create a key from scratch the user
needs to create a 2 column `data.frame`, with words on the left and
values on the right (see `?lexicon::hash_sentiment` &
`?lexicon::hash_valence_shifters` for what the values mean). Note that
the words need to be lower cased. Here I show an example `data.frame`
ready for key conversion:

    set.seed(10)
    key <- data.frame(
        words = sample(letters),
        polarity = rnorm(26),
        stringsAsFactors = FALSE
    )

This is not yet a key. **sentimentr** provides the `is_key` function to
test if a table is a key.

    is_key(key)

    ## [1] FALSE

It still needs to be **data.table**-ified. The `as_key` function coerces
a `data.frame` to a **data.table** with the left column named `x` and
the right column named `y`. It also checks the key against another key
to make sure there is not overlap using the `compare` argument. By
default `as_key` checks against `valence_shifters_table`, assuming the
user is creating a sentiment dictionary. If the user is creating a
valence shifter key then a sentiment key needs to be passed to `compare`
instead and set the argument `sentiment = FALSE`. Below I coerce `key`
to a dictionary that **sentimentr** can use.

    mykey <- as_key(key)

Now we can check that `mykey` is a usable dictionary:

    is_key(mykey)

    ## [1] TRUE

The key is ready for use:

    sentiment_by("I am a human.", polarity_dt = mykey)

    ##    element_id word_count sd ave_sentiment
    ## 1:          1          4 NA    -0.7594893

You can see the values of a key that correspond to a word using
**data.table** syntax:

    mykey[c("a", "b")][[2]]

    ## [1] -0.2537805 -0.1951504

Updating (adding or removing terms) a key is also useful. The
`update_key` function allows the user to add or drop terms via the `x`
(add a `data.frame`) and `drop` (drop a term) arguments. Below I drop
the "a" and "h" terms (notice there are now 24 rows rather than 26):

    mykey_dropped <- update_key(mykey, drop = c("a", "h"))
    nrow(mykey_dropped)

    ## [1] 24

    sentiment_by("I am a human.", polarity_dt = mykey_dropped)

    ##    element_id word_count sd ave_sentiment
    ## 1:          1          4 NA     -0.632599

Next I add the terms "dog" and "cat" as a `data.frame` with sentiment
values:

    mykey_added <- update_key(mykey, x = data.frame(x = c("dog", "cat"), y = c(1, -1)))

    ## Warning in as_key(x, comparison = comparison, sentiment = sentiment): Column 1 was a factor...
    ## Converting to character.

    nrow(mykey_added)

    ## [1] 28

    sentiment("I am a human. The dog.  The cat", polarity_dt = mykey_added)

    ##    element_id sentence_id word_count  sentiment
    ## 1:          1           1          4 -0.7594893
    ## 2:          1           2          2  0.7071068
    ## 3:          1           3          2 -0.7071068

Annie Swafford's Examples
-------------------------

[Annie
Swafford](https://annieswafford.wordpress.com/2015/03/02/syuzhet/)
critiqued Jocker's approach to sentiment and gave the following examples
of sentences (`ase` for Annie Swafford example). Here I test each of
Jocker's 4 dictionary approaches (syuzhet, Bing, NRC, Afinn), his
Stanford wrapper (note I use my own [GitHub Stanford wrapper
package](https://github.com/trinker/stansent) based off of Jocker's
approach as it works more reliably on my own Windows machine), the
[RSentiment](https://cran.r-project.org/package=RSentiment) package, the
lookup based
[SentimentAnalysis](https://github.com/sfeuerriegel/SentimentAnalysis)
package, the [meanr](https://github.com/wrathematics/meanr) package
(written in C level code), and my own algorithm with both the default Hu
& Liu (2004) polarity lexicon as well as [Baccianella, Esuli and
Sebastiani's (2010)](http://sentiwordnet.isti.cnr.it/) SentiWord lexicon
from the [**lexicon**](https://github.com/trinker/lexicon) package.

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load_gh("trinker/sentimentr", "trinker/stansent", "sfeuerriegel/SentimentAnalysis", "wrathematics/meanr")
    pacman::p_load(syuzhet, qdap, microbenchmark, RSentiment)

    ase <- c(
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

    syuzhet <- setNames(as.data.frame(lapply(c("syuzhet", "bing", "afinn", "nrc"),
        function(x) get_sentiment(ase, method=x))), c("syuzhet", "bing", "afinn", "nrc"))

    SentimentAnalysis <- apply(analyzeSentiment(ase)[c('SentimentGI', 'SentimentLM', 'SentimentQDAP') ], 2, round, 2)
    colnames(SentimentAnalysis) <- gsub('^Sentiment', "SA_", colnames(SentimentAnalysis))

    left_just(data.frame(
        stanford = sentiment_stanford(ase)[["sentiment"]],
        hu_liu = round(sentiment(ase, question.weight = 0)[["sentiment"]], 2),
        sentiword = round(sentiment(ase, lexicon::hash_sentiword, question.weight = 0)[["sentiment"]], 2),    
        RSentiment = calculate_score(ase), 
        SentimentAnalysis,
        meanr = score(ase)[['score']],
        syuzhet,
        sentences = ase,
        stringsAsFactors = FALSE
    ), "sentences")

      stanford hu_liu sentiword RSentiment SA_GI SA_LM SA_QDAP meanr syuzhet
    1     -0.5   0.35      0.18         -1 -0.25     0   -0.25    -1    -0.5
    2        1    0.8      0.65          1  0.33  0.33       0     1    0.75
    3      0.5    0.5      0.32          1   0.5   0.5     0.5     1    0.75
    4     -0.5      0         0          0     0  0.25    0.25     1    0.75
    5     -0.5  -0.41     -0.56         -1     1     1       1     1    0.75
    6     -0.5   0.06      0.11          1  0.17  0.17    0.33     1    0.75
    7     -0.5  -0.38     -0.05          0   0.5   0.5     0.5     1    0.75
    8        0      0     -0.14          0     0     0       0     0   -0.25
    9     -0.5   0.38      0.24         -1 -0.33 -0.33   -0.33    -1   -0.75
      bing afinn nrc sentences                                              
    1   -1    -2   0 I haven't been sad in a long time.                     
    2    1     3   1 I am extremely happy today.                            
    3    1     3   1 It's a good day.                                       
    4    1     3   1 But suddenly I'm only a little bit happy.              
    5    1     3   1 Then I'm not happy at all.                             
    6    1     3   1 In fact, I am now the least happy person on the planet.
    7    1     2   1 There is no happiness left in me.                      
    8    0     0  -1 Wait, it's returned!                                   
    9   -1    -3  -1 I don't feel so bad after all!                         

Also of interest is the computational time used by each of these
methods. To demonstrate this I increased Annie's examples by 100
replications and **microbenchmark** on a few iterations (Stanford takes
so long I didn't extend to more). Note that if a text needs to be broken
into sentence parts **syuzhet** has the `get_sentences` function that
uses the **openNLP** package, this is a time expensive task.
**sentimentr** uses a much faster regex based approach that is nearly as
accurate in parsing sentences with a much lower computational time. We
see that **RSentiment** and Stanford take the longest time while
**sentimentr** and **syuzhet** are comparable depending upon lexicon
used. **meanr** is lighting fast. **RSentiment** is a bit slower than
othe rmethods but is returning 3 scores from 3 different dictionaries.

    ase_100 <- rep(ase, 100)

    stanford <- function() {sentiment_stanford(ase_100)}

    sentimentr_hu_liu <- function() sentiment(ase_100)
    sentimentr_sentiword <- function() sentiment(ase_100, lexicon::hash_sentiword) 
        
    RSentiment <- function() calculate_score(ase_100) 
        
    SentimentAnalysis <- function() analyzeSentiment(ase_100)

    meanr <- function() score(ase_100)

    syuzhet_syuzhet <- function() get_sentiment(ase_100, method="syuzhet")
    syuzhet_binn <- function() get_sentiment(ase_100, method="bing")
    syuzhet_nrc <- function() get_sentiment(ase_100, method="nrc")
    syuzhet_afinn <- function() get_sentiment(ase_100, method="afinn")
         
    microbenchmark(
        stanford(),
        sentimentr_hu_liu(),
        sentimentr_sentiword(),
        RSentiment(), 
        SentimentAnalysis(),
        syuzhet_syuzhet(),
        syuzhet_binn(), 
        syuzhet_nrc(),
        syuzhet_afinn(),
        meanr(),
        times = 3
    )

    Unit: microseconds
                       expr           min             lq           mean
                 stanford()  24142020.272  24298337.6530  24354361.4377
        sentimentr_hu_liu()    265770.770    307730.7730    333707.0687
     sentimentr_sentiword()    988950.116   1003806.5965   1068438.5350
               RSentiment() 127583738.954 129559866.0305 133258929.1337
        SentimentAnalysis()   1902871.452   2152961.5670   2332010.5503
          syuzhet_syuzhet()    487677.008    492210.6835    494078.9483
             syuzhet_binn()    259257.025    310379.2150    358523.9783
              syuzhet_nrc()    715554.658    803274.6915    849399.2877
            syuzhet_afinn()    160650.094    161578.3420    165991.8637
                    meanr()       663.446       697.3155       708.6057
            median             uq           max neval
      24454655.034  24460532.0205  24466409.007     3
        349690.776    367675.2180    385659.660     3
       1018663.077   1108182.7445   1197702.412     3
     131535993.107 136096524.2235 140657055.340     3
       2403051.682   2546580.0995   2690108.517     3
        496744.359    497279.9185    497815.478     3
        361501.405    408157.4550    454813.505     3
        890994.725    916321.6025    941648.480     3
        162506.590    168662.7485    174818.907     3
           731.185       731.1855       731.186     3

Comparing sentimentr, syuzhet, RSentiment, meanr, and Stanford
--------------------------------------------------------------

The accuracy of an algorithm weighs heavily into the decision as to what
approach to take in sentiment detection. I have selected
algorithms/packages that stand out as fast and/or accurate to perform
benchmarking on actual data. Both **syuzhet** and **sentimentr** provide
multiple dictionaries with a general algorithm to compute sentiment
scores. **syuzhet** provides 4 approaches while **sentimentr** provides
2, but can be extended easily using the 4 dictionaries from the
**syuzhet** package. **meanr** is a very fast algorithm. The follow
visualization provides the accuracy of these approaches in comparison to
Stanford's **Java** based implementation of sentiment detection. The
visualization is generated from testing on three reviews data sets from
Kotzias, Denil, De Freitas, & Smyth (2015). These authors utilized the
three 1000 element data sets from:

-   amazon.com
-   imdb.com
-   yelp.com

The data sets are hand scored as either positive or negative. The
testing here merely matches the sign of the algorithm to the human coded
output to determine accuracy rates.

-   Kotzias, D., Denil, M., De Freitas, N., & Smyth,P. (2015). *From
    group to individual labels using deep features*. Proceedings of the
    21th ACM SIGKDD International Conference on Knowledge Discovery and
    Data Mining. 597-606.
    <http://mdenil.com/media/papers/2015-deep-multi-instance-learning.pdf>

<img src="inst/figure/comparisons_between_sentiment_detectors_b.png" width="100%" alt="sent comp">

The bar graph on the left shows the accuracy rates for the various
sentiment set-ups in the three review contexts. The rank plot on the
right shows how the rankings for the methods varied across the three
review contexts.

The take away here seems that, unsurprisingly, Stanford's algorithm
consistently outscores **sentimentr**, **syuzhet**, and **meanr**. The
**sentimentr** approach loaded with the Jockers' custom **syuzhet**
dictionary is a top pick for speed and accuracy. In addition to Jockers'
custom dictionary the `bing` dictionary also performs well within both
the **syuzhet** and **sentimentr** algorithms. Generally, the
**sentimentr** algorithm out performs **syuzhet** when their dictonaries
are comparable.

It is important to point out that this is a small sample data set that
covers a narrow range of uses for sentiment detection. Jockers'
**syuzhet** was designed to be applied across book chunks and it is, to
some extent, unfair to test it out of this context. Still this initial
analysis provides a guide that may be of use for selecting the sentiment
detection set up most applicable to the reader's needs.

The reader may access the R script used to generate this visual via:

    testing <- system.file("sentiment_testing/sentiment_testing.R", package = "sentimentr")
    file.copy(testing, getwd())

In the figure below we compare raw table counts as a heat map, plotting
the predicted values from the various algorithms on the x axis versus
the human scored values on the y axis.

<img src="inst/figure/comparisons_between_sentiment_detectors2.png" width = "80%" alt="sent comp">

Across all three contexts, notice that the Stanford coreNLP algorithm is
better at:

-   Detecting negative sentiment as negative
-   Discrimination (i.e., reducing neutral assignments)

The Jockers, Bing, Hu & Lu, and Afinn dictionaries all do well with
regard to not assigning negative scores to positive statements, but
perform less well in the reverse, often assigning positive scores to
negative statements, though Jockers' dictionary outperforms the others.
We can now see that the reason for the NRC's poorer performance in
accuracy rate above is its inability to discriminate. The Sentiword
dictionary does well at discriminating (like Stanford's coreNLP) but
lacks accuracy. We can deduce two things from this observation:

1.  Larger dictionaries discriminate better (Sentiword \[n =
    20,100\] vs. Hu & Lu \[n = 6,875\])
2.  The Sentiword dictionary may have words with reversed polarities

A reworking of the Sentiword dictionary may yield better results for a
dictionary lookup approach to sentiment detection, potentially,
improving on discrimination and accuracy.

The reader may access the R script used to generate this visual via:

    testing2 <- system.file("sentiment_testing/raw_results.R", package = "sentimentr")
    file.copy(testing2, getwd())

Text Highlighting
-----------------

The user may wish to see the output from `sentiment_by` line by line
with positive/negative sentences highlighted. The `highlight` function
wraps a `sentiment_by` output to produces a highlighted HTML file
(positive = green; negative = pink). Here we look at three random
reviews from Hu and Liu's (2004) Cannon G3 Camera Amazon product
reviews.

    set.seed(2)
    highlight(with(subset(cannon_reviews, number %in% sample(unique(number), 3)), sentiment_by(review, number)))

![](inst/figure/highlight.png)

Contact
=======

You are welcome to:    
- submit suggestions and bug-reports at: <https://github.com/trinker/sentimentr/issues>    
- send a pull request on: <https://github.com/trinker/sentimentr/>    
- compose a friendly e-mail to: <tyler.rinker@gmail.com>    
