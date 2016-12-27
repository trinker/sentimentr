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


sentimentr 0.3.0 -
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


**IMPROVEMENTS**



**CHANGES**


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