# How this forecast will be scored

This commitment was fixed well before the election, and before any forecast was deliberately published. (A forecast file was exposed by mistake for a few hours on 25 September 2026; see MODEL-LOG L033.) It is frozen by the git tag `prereg-2026` and by a GitHub Release made from that tag; the Release date is recorded by GitHub's servers and cannot be backdated. Any later change to this file is listed on the corrections page with its reason, and the scoring uses the tagged version.

## What is scored

The last forecast committed to this repository before polls open at 07:00 on 4 November 2026, identified by the run ID and commit shown on the site. The scope is the 25 Western Cape local and metropolitan councils. District councils are not forecast and are not scored.

1. **Council seats.** Every party that won seats in a council, or had at least a 5% chance of one, is one case.
   - Coverage: the share of actual seat counts inside the 50% and 90% intervals, measured with the randomised PIT, which is exact for whole numbers. A calibrated forecast scores close to 50% and 90%.
   - Seat CRPS, a proper score for the whole seat distribution (lower is better).
   - Mean absolute error of the median seat count.
2. **Council control.** The Brier score of the probabilities given to "party X majority" and "no majority", and the share of councils where the most likely outcome happened.
3. **Ward winners.** The log score and Brier score of the probability given to each ward's actual winner, and the share of wards where the most likely winner won.

Seats won by parties the model did not include count as a forecast of zero. They are scored, not excluded.

## What it is compared against

A forecast is only useful if it beats simple rules. Both are computed with the same seat allocator and the same code:

- **Repeat 2021:** the 2021 local-election vote, mapped onto the 2026 wards.
- **2024 as-is:** the 2024 national election's provincial-ballot vote, used unchanged.

For reference, the blind 2021 backtest, run with the settings the rule chose (MODEL-LOG L028), gave:

| | Model | Repeat previous local | National vote as-is |
|---|---|---|---|
| Seat error (mean absolute, per party per council) | 0.83 | 1.70 | 1.46 |
| Council control correct | 64% | 48% | 56% |
| Ward winners correct | 85.5% | 87.2% | 90.9% |
| 90% interval coverage (PIT) | 91% | | |

The model lost on ward winners in the backtest (MODEL-LOG O7). That comparison will be reported for 2026 whichever way it goes.

## Truth and code

The truth is the IEC's final declared results, from its Seat Calculation Detail reports and ward results, downloaded by the same pipeline. If a result is changed by a court or a re-run election, the result declared by the IEC at the time of scoring is used, and any later change is noted.

The scoring code is `score_chain()` and `naive_rule()` in `R/backtest.R`, as frozen by the tag. It is the code that scored the 2021 backtest, applied unchanged to 2026.

## When and where

On this site and in this repository, within two weeks of the IEC declaring the final results, whatever they show.
