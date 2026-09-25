# How this forecast will be scored

This commitment was fixed well before the election, and before any forecast was deliberately published. (A forecast file was exposed by mistake for a few hours on 25 September 2026; see MODEL-LOG L033.) It is frozen by the git tag `prereg-2026-r1` and a GitHub Release made from that tag; the Release date is recorded by GitHub's servers and cannot be backdated. **Corrected on 25 September 2026**, before any 2026 result and before the corrected backtest was run: the first version (tag `prereg-2026`, kept as a record) compared forecasts and simple rules over different sets of cases, which favoured the model (ERRATA E1). Any later change is listed on the corrections page with its reason, and the scoring uses the tagged version.

## What is scored

The last forecast committed to this repository before polls open at 07:00 on 4 November 2026, identified by the run ID and commit shown on the site. The scope is the 25 Western Cape local and metropolitan councils. District councils are not forecast and are not scored.

1. **Council seats.** Every party on a council's ballot is a case, as is any party that wins seats there. A party that a forecast or simple rule does not include counts as a forecast of zero seats. Every forecast and every simple rule is scored on this same set of cases.
   - Seat CRPS, a proper score for the whole seat distribution, averaged over all cases (lower is better).
   - Mean absolute error of the median seat count, averaged over all cases.
   - Coverage: for parties with at least 2% of the vote in that council at the 2021 local or 2024 national election (a set fixed before the forecast), the share of actual seat counts inside the 50% and 90% intervals, measured as the exact expected value of the randomised PIT. A calibrated forecast scores close to 50% and 90%.
2. **Council control.** The Brier score of the probabilities given to "party X majority" and "no majority", and the share of councils where the most likely outcome happened.
3. **Ward winners.** The log score and Brier score of the probability given to each ward's actual winner, and the share of wards where the most likely winner won.

## What it is compared against

A forecast is only useful if it beats simple rules. Both are computed with the same seat allocator and the same code:

- **Repeat 2021:** the 2021 local-election vote, mapped onto the 2026 wards.
- **2024 as-is:** the 2024 national election's provincial-ballot vote, used unchanged.

Backtest figures computed on this basis are on the site's backtest page. The first version of this file quoted backtest figures computed on the flawed basis (ERRATA E1).

The model lost on ward winners in the backtest (MODEL-LOG O7). That comparison will be reported for 2026 whichever way it goes.

## Truth and code

The truth is the IEC's final declared results, from its Seat Calculation Detail reports and ward results, downloaded by the same pipeline. If a result is changed by a court or a re-run election, the result declared by the IEC at the time of scoring is used, and any later change is noted.

The scoring code is `score_chain()` and `naive_rule()` in `R/backtest.R`, as frozen by the tag `prereg-2026-r1`. It is the code that scores the 2021 backtest, applied unchanged to 2026.

## When and where

On this site and in this repository, within two weeks of the IEC declaring the final results, whatever they show.
