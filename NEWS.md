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


sentimentr 0.1.0
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

**MINOR FEATURES**

IMPROVEMENTS

* `plot.sentiment` now used `...` to pass parameters to **syuzhet**'s
  `get_transformed_values`.


**CHANGES**

sentimentr 0.0.1
----------------------------------------------------------------

This package is designed to quickly calculate text polarity sentiment at the
sentence level and optionally aggregate by rows or grouping variable(s).