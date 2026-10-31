# Complete reference only

Before binning or fitting, spades_prepare removes every row containing:
- An actual R missing value (including numeric NaN).
- A textual NA or the internal __MISSING__ marker.
- An additional label declared in missing_tokens.

Matching is case-sensitive after trimws; surviving values are not rewritten.
The reserved NA/__MISSING__ labels are always excluded. Empty strings and other
codes are not guessed to be missing. "Missing" is ordinary data unless declared.
For example:

```r
prepared <- spades_prepare(data, missing_tokens=c("NA", "Missing", "N/A"))
result <- spades_synthesise(data, n=9000, target="response",
                           missing_tokens=c("NA", "Missing", "N/A"))
```

These are alternative staged/full entry points. Output reports input/excluded/used
counts. prepared$retained_rows contains original row positions; prepared$original
contains exactly the cleaned raw reference in that order. All downstream stages
use this same reference: BN, XGB real class and fake-generation domains, RF pair
selection, RF utility guide, privacy nearest-record distances, fidelity and utility.
Numeric binning is fitted after removal, so excluded outliers do not affect bins.
The n argument remains the requested synthetic target size; it is not automatically
reduced when training rows are excluded.

No synthetic missingness is generated. A column entirely missing leaves no
complete records and is rejected clearly. Constant columns and insufficient
complete sample/domain sizes remain unsupported. Discarded real records are not
part of the distance-based privacy reference or the fidelity/utility reference.

When using the staged API, supply prepared to fit/structure/utility/land. If
reference_values is provided explicitly with a prepared object, it must be
identical to prepared$original. This prevents regression responses from being
silently misaligned after filtering. Downstream functions reject incomplete
references and pools; they never silently remove rows during SA or scoring.
An old prepared object containing missing values must be regenerated from its
raw data with spades_prepare; saved incomplete models are not silently changed.

This is a modeling restriction: quality is assessed against the complete-case
reference, not against the original incomplete population. No assumptions about
missingness mechanisms are estimated by this package.
