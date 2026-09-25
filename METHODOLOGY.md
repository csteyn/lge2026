# How the model works

Model v0.1. This page describes what the code in `R/model.R` does, what it cannot do, and where it is most likely to be wrong. When the code and this page disagree, the code is right and this page has a bug: please report it.

## What is predicted

For each council: the number of seats each party wins, which party (if any) holds a majority, and which combinations of parties reach a majority. For each ward: the probability that each party wins it.

Seats follow the **combined** ward and PR votes, not the PR ballot alone. Schedule 1 of the Municipal Structures Act, as printed on the IEC's Seat Calculation Detail reports:

    Q = floor(A / (B - C - D)) + 1
    A = valid votes for all parties, ward and PR ballots added together
    B = council seats, C = wards won by independents, D = wards won by parties with no PR list

Each party's entitlement is `floor(votes / Q)`, with remaining seats by largest remainder; PR seats are the entitlement minus wards won. A party whose ward wins exceed its entitlement keeps its wards, receives no list seats, and the quota is recomputed for everyone else with the council size fixed (Structures Amendment Act 3 of 2021). The trigger is strictly greater: we first implemented "equal to or greater", and the IEC's own 2021 calculations showed that was wrong (MODEL-LOG L019). The allocator is in `R/seats.R`. It is tested against hand-worked cases and, on every live run, against the IEC's published 2021 Seat Calculation Detail for each council: fed the IEC's own vote totals and ward wins, it must reproduce every party's seats exactly (check C14). It reproduces all 25 Western Cape councils of 2021 exactly, including the three where the excessive-seat rule applied (Mossel Bay, Oudtshoorn, Laingsburg).

## The unit of analysis

The voting district (VD), about 1 000 to 3 000 registered voters. It is the smallest unit the IEC publishes results for, and it nests inside wards, so ward and council results are sums of VD results.

## Step 1: party groups

In each municipality, parties with at least 1% of the vote in the 2021 local election or the 2024 national election are modelled individually (at most ten). The rest are pooled as OTHER for modelling, then split back into their real parties, in fixed proportions from recent elections, before seats are allocated (assumption A04).

## Step 2: how national votes become local votes

Local and national elections differ: turnout is lower, local issues and civic parties matter more, and some parties do systematically better on one ballot than the other. We learn this from the one observed pair on comparable geography, the 2019 national (provincial ballot) and 2021 local elections, matched by VD.

Shares are compared on the centred log-ratio scale, `clr(s)_p = log s_p - mean_k log s_k`, which makes the regression indifferent to how many parties there are. For each party:

    clr_LGE2021 = a_p + b_p * clr_NPE2019 + e

weighted by votes. Parties that did not stand in both elections in a municipality do not inform the fit (MODEL-LOG L003). The most important in the Western Cape is the Patriotic Alliance, which was not on the 2019 provincial ballot. For these parties `b = 1`, and the local premium `a` is treated as unknown rather than zero. Its prior is the spread of `a` among the parties that could be fitted. It is drawn afresh in every simulation and shared by every council in that draw, so the uncertainty reaches the seat probabilities (L022). Where a party contested at least 3 by-election wards in the modelled councils after the 2024 national election, the prior is updated with that evidence. In each voting district, the party's vote relative to the fitted parties is compared with the prediction from the district's 2024 vote, and the two are combined by precision. A placebo run on fitted parties checks whether by-elections are systematically distorted; any distortion found is added to the uncertainty (L023). For the Patriotic Alliance this gives a premium of +0.12 ± 0.17 on the log-ratio scale. Check C06 lists these parties, and the assumptions page shows their current premium and its source.

**Known bias.** Because the 2019 shares are themselves noisy, `b` is biased towards zero (attenuation), which compresses spatial contrasts. An errors-in-variables correction (dividing each slope by the party's reliability, estimated from the sampling noise of its 2019 shares) halves the slope error on synthetic data with known truth. Whether to use it, together with the ward/PR ratio and the size of the premiums, is decided by a pre-registered experiment on ward winners (L039).

## Step 3: the 2026 baseline

The fitted transfer is applied to 2024 national results for every VD on the 2026 voting-district layer. Parties that stand only in local elections keep their 2021 share (A05) and the national parties share the remainder. VDs with no history, mostly new districts split off by the 2026 ward delimitation, take the registered-voter-weighted average of their 2026 ward, or of their municipality if the ward has no history (check C05). Every VD's shares are then renormalised to sum to one (MODEL-LOG L004).

## Step 4: simulation

Each of 2 000 draws adds shocks on the log-share scale at four levels:

| Level | Spread | Source |
|---|---|---|
| Province, per party (shared by all municipalities in a draw) | 0.35 | chosen by the pre-registered 2021 backtest (A02) |
| Municipality, per party | 0.15 | chosen by the pre-registered 2021 backtest (A02) |
| Ward, per party | estimated | between-ward residual spread of the step 2 fit |
| VD, per party | estimated | within-ward residual spread of the step 2 fit |

Shares are renormalised with a softmax. The ward ballot is the PR ballot shifted by each party's observed 2021 ward/PR ratio in that municipality, which carries ticket-splitting; parties with no ward candidate in a ward get zero there (A07 until candidate lists are parsed). Turnout per VD starts from its 2021 value and receives province, municipality and VD shocks on the logit scale. Registered voters come from the 2026 VD layer, which is a 2024 snapshot (A06).

Ward winners are the plurality on the ward ballot. Combined votes go through Schedule 1. Coalition arithmetic then enumerates every subset of the ten largest parties and records which are winning, and which are minimal winning (drop the smallest member and it loses).

## Parties with no history

Parties standing in a council for the first time have no past vote to build on. Each simulated election draws their council share from how past Western Cape newcomers of the same breadth (1, 2-10 or 11+ councils) and ward coverage actually did, with a party's draws correlated across the councils it stands in. The share is taken proportionally from all other parties. A party-specific prior, such as one based on polls, can replace this only with a named source. Whether this improves the forecast is decided by a pre-registered backtest (L035).

## The backtest

The model is run as if it were 2021, using only what was known before that election, and scored against the official 2021 results. It is compared with two simple rules: repeat the previous local election, and use the latest national vote as it stands. The backtest also estimates the province- and council-level spreads directly from how far 2021 departed from its prediction (L026). The spreads in the simulation were chosen this way, by a rule fixed before the results were seen (L027-L028). Predicting 2021 blind, and scored on the same cases as the simple rules, the model beat both on seats (error 0.50 against 0.65 for the national vote as it stands and 0.77 for repeating the previous local election). It did not beat them on ward winners (85.5% against 90.9%). On council control it beat them only with the newcomer module (76% against 56%). Earlier figures computed on a flawed basis were withdrawn (ERRATA E1). One election is a small sample, and 2021 was an unusual one.

## What the probabilities mean, and what they leave out

The province and municipality spreads were estimated from a single backtest election, so the headline probabilities are only as good as that one test. They reflect a forecast made without polls. They are tested by the 2021 backtest above, and the forecast will be scored after the election as set out in [SCORING](SCORING.md).

Not modelled in v0.1: independent candidates (A09), district council PR seats (A10), new local parties that did not exist in 2021, campaign events after the data cut-off, and any change in who is registered since 2024.
