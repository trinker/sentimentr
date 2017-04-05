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


sentimentr 1.0.1 -
----------------------------------------------------------------


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
  cooccur.

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
  Spotted by Richard Watson (see #23).
  
**NEW FEATURES**

* `extract_sentiment_terms` added to enable users to extract the sentiment terms 
  from text as `polarity` would return in the **qdap** package.
  
**MINOR FEATURES**

* `update_polarity_table` and `update_valence_shifter_table` added to abstract 
  away thinking about the `comparison` argument to `update_key`.


sentimentr 0.2.0 - 0.2.3
----------------------------------------------------------------

**BUG FIXES**

* Commas were not handled properly in some cases.  This has been fixed (see #7).

* `highlight` parsed sentences differently than the main `sentiment` function 
  resulting in an error when `original.text` was supplied that contained a colon
  or semi-colon.  Spotted by Patrick Carlson (see #2).

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