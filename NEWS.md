NEWS
====

Versioning
----------

Releases will be numbered with the following semantic versioning format:

&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor
  and patch)
* New additions without breaking backward compatibility bumps the minor
  (and resets the patch)
* Bug fixes and misc changes bumps the patch



sentimentr 2.9.1 - 
---------------------------------------------------------------

**BUG FIXES**

**NEW FEATURES**

**MINOR FEATURES**

**IMPROVEMENTS**

**CHANGES**


sentimentr 2.8.0 - 2.9.0
---------------------------------------------------------------

**BUG FIXES**

* `sentiment_by` did not capture `averaging.function` for some data types (e.g.,
  'character' vectors) and was not able to be used by `highlight`.  Spotted by
  Ken McGarry (see <a href="https://github.com/trinker/sentimentr/issues/104">#104</a> for details).
  
* `sentiment` would not work if the polarity table contained no spaced words.
  Spotted by GitHub user mrwunderbar666 (see <a href="https://github.com/trinker/sentimentr/issues/117">#117</a> for details).

* `emotion` would not give the correct response when the `text.var` contained no
  negated words.  Spotted by git-igor (see <a href="https://github.com/trinker/sentimentr/issues/108">#108</a> for details).
  

**MINOR FEATURES**

* `sentiment` and `emotion` (`sentiment_by`, `emotion_by` & their `extract_` 
  methods inherit this as well) pick up a `retention_regex` argument.  This 
  regex was previously hard-coded in the function and didn't give users access 
  to change this.  The previous version `"\\d:\\d|\\d\\s|[^a-z',;: ]"` was switched 
  to `"\\d:\\d|\\d\\s|[^[:alpha:]',;: ]"` as the later swaps `a-z` for `[:alpha:]` meaning 
  more alphabetic characters are retained.  While <a href="https://github.com/trinker/sentimentr" target="_blank">sentimentr</a> has not been 
  tested on other languages, this opens up the possibility for use with other
  (especially Germanic) languages.  Thank you to johanneswaage and Matthias2018 
  for raising awareness of this issue and Dominique EMMANUEL for suggesting a 
  potetial way forward.  This suggestion led to the reworking and current 
  approach (see issues <a href="https://github.com/trinker/sentimentr/issues/74">#74</a>, <a href="https://github.com/trinker/sentimentr/issues/79">#79</a> & <a href="https://github.com/trinker/sentimentr/issues/118">#118</a> for more).

**IMPROVEMENTS**

* Added description of what the numeric value of `sentiment()` means (see 
  **Results** in `?sentiment`) and examples of how to bin the score to a
  3 category `c('Negative', 'Neutral', 'Positive')` factor output.  These 
  improvements in documentation came from an issue raised by Sadettin Demirel 
  (see <a href="https://github.com/trinker/sentimentr/issues/128">#128</a>).



sentimentr 2.7.0 - 2.7.1 
----------------------------------------------------------------


**BUG FIXES**

* The `plot` method for `sentiment` and `profanity` failed for n &lt; 100 observations.
  Interpolation via `stats::approx` provides a means to fill in the gaps in cases
  of n &lt; 100.
  
* The `crowdflower_self_driving_cars` dataset contained text that read as 
  `"Error in gsub(replaces[i], c("'", "'", "\\"", "\\"")[i], x, fixed = TRUE): 
  input string 12 is invalid UTF-8"`.  Spotted thanks to Shantanu Kumar.
  
* Sequential bigram polarized word chunks resulted in a concatenation that rendered
  the trigram chunk as non-polar.  For example, "he gave time honored then" contains
  both the bigram chunk "gave time" and "time honored"  this results in word 
  chunking that created the tokens {'he', 'gave time honored', 'then'}.  The
  token 'gave time honored' was not matched by either "gave time" or "time honored"
  resulting in a zero polarity score.  Spotted thanks to GitHub user @swlazlowski
  (see <a href="https://github.com/trinker/sentimentr/issues/102">#102</a>).
  
* `highlight()` used `mean()` as the averaging function regardless of the 
  `averaging.function` argument supplied to `sentiment_by()`.  This behavior has
  been corrected.  Spotted thanks to Kelvin Lam (see <a href="https://github.com/trinker/sentimentr/issues/103">#103</a>).

**NEW FEATURES**

* `emotion` added as a means to assess the use of emotion in text.

* `extract_emotion_terms` added to extract emotion terms from text.


**IMPROVEMENTS**

* The default profanity list in `profanity` & `extract_profanity_terms` was not 
  lower cased or unique which resulted in a warning every time it was run.  This 
  list is now passed as `unique(tolower(lexicon::profanity_alvarez))` to avoid 
  the warnings.



sentimentr 2.5.0 - 2.6.1
----------------------------------------------------------------

**BUG FIXES**

* `plot` returned an error for `sentiment` objects created by 
  `sentiment.get_sentences.data.frame` due to the class assignments of the 
  output ('sentiment' was not assigned as a class) and thus `plot.sentiment`
  was not called.
  
* `combine_data` contained a bug in which data sets with extra columns were not 
  combined and resulted in an error (see <a href="https://github.com/trinker/sentimentr/issues/94">#94</a>).
  
* If a dataset was passed to `get_sentences()` that had a column named 
  `sentiment` and was then passed to `sentiment_by()`, the `sentiment` from the 
  original data set was returned as `ave_sentiment` not the **sentimentr** 
  computed value.
  
**NEW FEATURES**

* `profanity` added as a means to assess the use of profanity in text.

* `extract_profanity_terms` added to extract profanity terms from text.

* The remaining four Hu & Liu data sets (see 
  http://www.cs.uic.edu/~liub/FBS/CustomerReviewData.zip) have been added in 
  addition to the Cannon reviews data set.  The family of sentiment tagged data
  from Hu & Liu now includes: "hu_liu_apex_reviews", "hu_liu_cannon_reviews",
  "hu_liu_jukebox_reviews", "hu_liu_nikon_reviews", & "hu_liu_nokia_reviews".
  

**CHANGES**

* The `cannon_reviews` data set has been renamed to `hu_liu_cannon_reviews` to be
  consistent with the other `hu_liu_` data sets that have been added.  This data
  set is also now cleaner, excludes Hu & Liu's original categories that were some
  times still visible.  Cleaning includes better capitalization and removal of 
  spaces before punctuation to look less normalized.  Additionally, the `number`
  column is now called `reviewer_id` to convey what the data actually is.


sentimentr 2.4.0 - 2.4.2
----------------------------------------------------------------

**BUG FIXES**

* In `sentiment` when there was a larger de-amplifier, negator, & polarized word 
  all in the same chunk the sentiment would equal 0.  This occurred because the 
  de-amplifier weights below -1 are capped at -1 lower bound.  To compute the 
  weight for de-amplifiers this was added with 1 and then multiplied by the
  polity score.  Adding 1 and -1 resulted in 0 * polarity = 0.  This was spotted 
  thanks to Ashley Wysocki (see <a href="https://github.com/trinker/sentimentr/issues/80">#80</a>).  In the case Ashley's example was with an
  adversative conjunction which is treated as an extreme amplifier, which when 
  combined with a negator, is treated as a de-amplifier.  This resulted in a -1 
  De-amplifier score.  De-amplifiers are now capped at -.999 rather than -1 to 
  avoid this.

* Chunks containing adversative conjunctions were supposed to act in the following 
  way: "An adversative conjunction before the polarized word...up-weights the 
  cluster...An adversative conjunction after the polarized word down-weights the 
  cluster...".  A bug was introduced in which up-weighting happened to the first 
  clause as well.  This bug has been reversed.  See <a href="https://github.com/trinker/sentimentr/issues/85">#85</a>.

* The **README** contained a reference to the **magritrr** rather than the 
  **magrittr** package.


**CHANGES**

* `highlight` now writes the .html file to the temp directory rather than the 
  working directory by default.


sentimentr 2.3.0 - 2.3.2
----------------------------------------------------------------

**BUG FIXES**

* The README and `highlight` function documentation both contained code that 
  produced an error.  This is because all the data sets within **sentimentr**
  have been normalized to include the same columns, including `cannon_reviews`.
  The code that caused the error referred to a column `number` which no longer 
  existed in the data set.  This column now exists in `cannon_reviews` again.  
  Spotted thanks to Tim Fisher.
  
**CHANGES**

Maintenance release to bring package up to date with the lexicon package API changes.


sentimentr 2.1.0 - 2.2.3
----------------------------------------------------------------

**BUG FIXES**

* `sentiment` contained a bug that caused sentences with multiple polarized 
  words and comma/semicolon/colon breaks to inappropriate replicate rows too many
  times (a recycling error).  This in turn caused the same polarized word to be
  counted multiple times resulting in very extreme polarity values.  This was 
  spotted by Lilly Wang.
  
* `validate_sentiment` contained an error in the documentation; the predicted
  and actual data were put into the wrong arguments for the first example.

**NEW FEATURES**

* The default sentiment sentiment lookup table used within **sentimentr** is now
  `lexicon::hash_sentiment_jockers_rinker`, a combined and augmented version of
  `lexicon::hash_sentiment_jockers` (Jockers, 2017) & Rinker's augmented 
  `lexicon::hash_sentiment_huliu` (Hu & Liu, 2004) sentiment lookup tables.

* Five new sentiment scored data sets added: `kaggle_movie_reviews`, `nyt_articles`
  `hotel_reviews`, `crowdflower_self_driving_cars`, `crowdflower_products`,
  `crowdflower_deflategate`, `crowdflower_weather`, & `course_evaluations` for 
  testing nd exploration.

*  `replace_emoji` and `replace_emoji_identifier` rexported from the **textclean**
  package for replacing emojis with word equivalents or an identifier token
  that can be detected by the `lexicon::hash_sentiment_emoji` polarity table 
  within the `sentiment` family of functions.
  
**MINOR FEATURES**

* `sentiment` picks up the `neutral.nonverb.like` argument.  This allows the
  user to treat specific non-verb uses of the word 'like' as neutral since 'like' 
  as a verb is usually when the word is polarized.
  
* `combine_data` added to easily combine trusted **sentimentr** sentiment 
  scored data sets.

**CHANGES**

* The sentiment data sets have been reformatted to conform to one another.  This
  means columns have been renamed, ratings have been rescales to be zero as neutral,
  and columns other than `sentiment` score and `text` have been removed.  This
  makes it easier to compare and combine data sets.

* `update_key` now allows a **data.table** object for `x` meaning **lexicon**
  `hash_sentiment_xxx` polarity tables can be combined.  This is particularly 
  useful for combining `hash_sentiment_emojis` with other polarity tables.


sentimentr 2.0.1
----------------------------------------------------------------

**BUG FIXES**

* `get_sentences` assigned the class to the data.frame when a data.frame was 
  passed but not to the text column, meaning the individual column could not be 
  passed to `sentiment` or `sentiment_by` without having sentence boundary 
  detection re-done.  This has been fixed.  See <a href="https://github.com/trinker/sentimentr/issues/53">#53</a>.



sentimentr 1.0.1 - 2.0.0
----------------------------------------------------------------

**BUG FIXES**

* `sentiment_attributes` gave an incorrect count of words.  This has been fixed 
  and number of tokens is reported as well now.  Thanks to Siva Kottapalli for
  catching this (see <a href="https://github.com/trinker/sentimentr/issues/42">#42</a>).
  
* `extract_sentiment_terms` did not return positive, negative, and/or neutral
  columns if these terms didn't exist in the data passed to `text.var` making it 
  difficult to use for programming.  Thanks to Siva Kottapalli for
  catching this (see <a href="https://github.com/trinker/sentimentr/issues/41">#41</a>).
  
* `rescale_general` would allow `keep.zero` when `lower` &gt;= 0 meaning the 
original mid values were rescaled lower than the lowest values.
  
  
**MINOR FEATURES**

* `validate_sentiment` picks up Mean Directional Accuracy (MDA) and Mean 
  Absolute Rescaled Error (MARE) measures accuracy.  These values are printed 
  for the `validate_sentiment` object and can be accessed via `attributes`.

**CHANGES**

* Many **sentimentr** functions performed sentence splitting (sentence boundary 
  disambiguation) internally.  This made it (1) difficult to maintain the code,
  (2) slowed the functions down and potentially increased overhead memory, and 
  (3) required a repeated cost of splitting the text every time one of these
  functions was called.  Sentence splitting is now handled vie the **textshape**
  package as the backend for `get_sentences`.  It is recommended that the user
  spits their data into sentences prior to using the sentiment functions.  Using
  a raw character vector still works but results in a warning.  While this won't
  break any code it may cause errors and is a fundamental shift in workflow,
  thus the major bump to 2.0.0
  
  
sentimentr 0.5.0 - 1.0.0
----------------------------------------------------------------

**BUG FIXES**

* Previously `update_polarity_table` and `update_valence_shifter_table` were
  accidentally not exported.  This has been corrected.
  
**NEW FEATURES**

* `downweighted_zero_average`, `average_weighted_mixed_sentiment`, and 
  `average_mean` added for use with `sentiment_by` to reweight
  zero and negative values in the group by averaging (depending upon the 
  assumptions the analyst is making).
  
* `general_rescale` added as a means to rescale sentiment scores in a 
  generalized way.
  
* `validate_sentiment` added as a means to assess sentiment model performance
  against known sentiment scores.
  
* `sentiment_attributes` added as a means to assess the rate that sentiment
  attributes (attributes about polarized words and valence shifters) occur and 
  co-occur.

**MINOR FEATURES**

* `sentiment_by` becomes a method function that now accepts `sentiment_by`
  and `sentiment` objects for `text.var` argument in addition to default
  `character`.

**IMPROVEMENTS**

* `sentiment_by` picks up an `averaging.function` argument for performing the 
  group by averaging.  The default uses `downweighted_zero_average`, which 
  downweights zero values in the averaging (making them have less impact).  To
  get the old behavior back use `average_mean` as follows.  There is also an
  `average_weighted_mixed_sentiment` available which upweights negative 
  sentences when the analysts suspects the speaker is likely to surround 
  negatives with positives (mixed) as a polite social convention but still the 
  affective state is negative.

**CHANGES**

* The hash keys `polarity_table`, `valence_shifters_table`, and `sentiword` have
  been moved to the **lexicon** (https://github.com/trinker/lexicon) package in
  order to make them more modular and maintainable.  They have been renamed to
  `hash_sentiment_huliu`, `hash_valence_shifters`, and `hash_sentiment_sentiword`.
  
* The `replace_emoticon`, `replace_grade` and `replace_rating` functions have 
  been moved from **sentimentr** to the **textclean** package as these are 
  cleaning functions.  This makes the functions more modular and generalizable 
  to all types of text cleaning.  These functions are still imported and 
  exported by **sentimentr**.

* `but.weight` argument in `sentiment` function renamed to `adversative.weight`
  to better describe the function with a linguistics term.

* `sentimentr` now uses the Jockers (2017) dictionary by default rather than the 
  Hu & Liu (2004).  This may result in breaks to backwards compatibility,
  hence the major version bump (1.0.0).

sentimentr 0.3.0 - 0.4.0
----------------------------------------------------------------

**BUG FIXES**

* Missing documentation for `but' conjunctions added to the documentation.  
  Spotted by Richard Watson (see <a href="https://github.com/trinker/sentimentr/issues/23">#23</a>).
  
**NEW FEATURES**

* `extract_sentiment_terms` added to enable users to extract the sentiment terms 
  from text as `polarity` would return in the **qdap** package.
  
**MINOR FEATURES**

* `update_polarity_table` and `update_valence_shifter_table` added to abstract 
  away thinking about the `comparison` argument to `update_key`.


sentimentr 0.2.0 - 0.2.3
----------------------------------------------------------------

**BUG FIXES**

* Commas were not handled properly in some cases.  This has been fixed (see <a href="https://github.com/trinker/sentimentr/issues/7">#7</a>).

* `highlight` parsed sentences differently than the main `sentiment` function 
  resulting in an error when `original.text` was supplied that contained a colon
  or semi-colon.  Spotted by Patrick Carlson (see <a href="https://github.com/trinker/sentimentr/issues/2">#2</a>).

**MINOR FEATURES**

* `as_key` and `update_key` now coerce the first column of the `x` argument 
  data.frame to lower case and warn if capital letters are found.

**IMPROVEMENTS**

* A section on creating and updating dictionaries was added to the README:
  https://github.com/trinker/sentimentr#making-and-updating-dictionaries
  
* `plot.sentiment_by` no longer color codes by grouping variables.  This was
  distracting and removed.  A jitter + red average sentiment + boxplot visual
  representation is used.
  
**CHANGES**

* Default sentiment and valence shifters get the following additions: 
  - `polarity_table`: "excessively", 'overly', 'unduly', 'too much', 'too many', 
  'too often', 'i wish', 'too good', 'too high', 'too tough'
  - `valence_shifter_table`: "especially"


sentimentr 0.1.0 - 0.1.3
----------------------------------------------------------------

**BUG FIXES**

* `get_sentences` converted to lower case too early in the regex parsing,
  resulting in missed sentence boundary detection.  This has been corrected.

* `highlight` failed for some occasions when using `original.text` because the
  splitting algorithm for `sentiment` was different. `sentiment`'s split algorithm
  now matches and is more accurate but at the cost of speed.

**NEW FEATURES**

* `emoticons` dictionary added.  This is a simple dataset containing common
  emoticons (adapted from [Popular Emoticon List](http://www.lingo2word.com/lists/emoticon_listH.html))

* `replace_emoticon` function added to replace emoticons with word equivalents.

* `get_sentences2` added to allow for users that may want to get sentences from
  text and retain case and non-sentence boundary periods.  This should be
  preferable in such instances where these features are deemed important to the
  analysis at hand.

* `highlight` added to allow positive/negative text highlighting.

* `cannon_reviews` data set added containing Amazon product reviews for the
  Cannon G3 Camera compiled by Hu and Liu (2004).

* `replace_ratings` function + `ratings` data set added to replace ratings.

* `polarity_table` gets an upgrade with new positive and negative words to
  improve accuracy.

* `valence_shifters_table` picks up a few non-traditional negators.  Full list
  includes: "could have", "would have", "should have", "would be",
  "would suggest", "strongly suggest".

* `is_key` and `update_key` added to test and easily update keys.

* `grades` dictionary added.  This is a simple dataset containing common
  grades and word equivalents.

* `replace_grade` function added to replace grades with word equivalents.


**IMPROVEMENTS**

* `plot.sentiment` now uses `...` to pass parameters to **syuzhet**'s
  `get_transformed_values`.

* `as_key`, `is_key`, & `update_key` all pick up a logical `sentiment` argument
  that allows keys that have character y columns (2nd column).



sentimentr 0.0.1
----------------------------------------------------------------

This package is designed to quickly calculate text polarity sentiment at the
sentence level and optionally aggregate by rows or grouping variable(s).