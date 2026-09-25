# Corrections

Corrections to anything this site has published: what was wrong, from when to when, how much it changed the forecast, and the commit that fixed it. Errors caught before publication are recorded in the [model log](MODEL-LOG.md) instead.

No forecast has been published yet. The corrections below concern the published scoring commitment.

Each correction will be added as a numbered section in this form:

    E1. <one-line summary> (published <date>, corrected <date>)
    What was wrong. What it changed (with before and after numbers). Commit.

## E1. Scoring commitment corrected: forecasts and simple rules were compared over different sets of cases (published 25 September 2026 at 10:01 UTC, corrected the same day)

**What was wrong.** A seat "case" was every party that won seats in a council or had at least a 5% chance of one *under the forecast being scored*. A forecast that spreads probability over more parties gets more cases, and most of them are easy (small chance, no seats), so its average score improves without it being better. Averages were then compared across forecasts and simple rules with different numbers of cases. In the 2021 backtest the number of cases ranged from 215 to 237 across settings, and a newcomer module added about 80.

**What it changed.** The backtest figures quoted in the first commitment (seat error 0.83 against 1.46 and 1.70; council control 64%; coverage 91%) were computed on this basis, as was the choice of spreads (MODEL-LOG L028). They are withdrawn pending the corrected backtest (MODEL-LOG L037). The flaw favoured the model, so the correction makes the 2026 test harder, not easier.

**The correction.** Every party on a council's ballot is a case for every forecast and every rule, and a party a forecast does not include counts as a forecast of zero. Coverage is measured on parties with at least 2% at the previous local or latest national election, a set fixed before any forecast. The corrected commitment is frozen by the tag `prereg-2026-r1`; the original tag `prereg-2026` is kept as a record.
