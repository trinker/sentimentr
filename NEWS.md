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

**NEW FEATURES**

* `emoticons` dictionary added.  This is a simple dataset containing common
  emoticons (adapted from [Popular Emoticon List](http://www.lingo2word.com/lists/emoticon_listH.html))

* `replace_emoticon` function added to replace emoticons with word equivalents.

**MINOR FEATURES**

IMPROVEMENTS

**CHANGES**

sentimentr 0.0.1
----------------------------------------------------------------

This package is designed to quickly calulate text polarity sentiment at the
sentence level and optionally aggregate by rows or grouping variable(s).