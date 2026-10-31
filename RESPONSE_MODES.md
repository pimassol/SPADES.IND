# Three response scenarios, version 0.1.8

Main entry point: spades_synthesise(data, n, target=NULL, mode=...).

1. Categorical: target="diagnosis", mode="classification". Response categories retained even for numeric codes; categorical fidelity plus absolute accuracy gap. Numerical predictors reconstructed after landing.
2. Continuous: target="income", mode="regression", target_bins=10L, regression_metric="RMSE" (or "MAE"). Training response quantile deciles; ties merge boundaries. Categorical flight pool. Fixed RF regression guide trained on categorical predictors and real numerical response. Each SA swap draws a new response uniformly inside the candidate bin and evaluates fidelity plus normalized absolute error gap. Acceptance preserves that value; rejection preserves the previous record and value. Other numeric predictors reconstructed after landing. No full numeric pool is required.
3. No response: target=NULL, mode="none". No response utility guide; fidelity-only landing. BN and plausibility models remain, as does RF-based pair discovery for fidelity when requested automatically.

Each selected pool index is unique, but identical categorical record contents are allowed. Integer-valued responses draw uniformly among integers belonging to the original interval. Fidelity always uses categorical representations.

Dynamic kernel acceptance is defined relative to the product of within-bin uniform base distributions and uniform index selection. This cancels the within-bin proposal-density ratio. No claim is made of proposal symmetry in numerical Lebesgue space for unequal-width bins.

Random response reconstruction is not a formal or demonstrated additional privacy guarantee. External validation trains regression models on final numerical releases and tests on the same held-out real numerical response.

spades_land_response provides the standalone dynamic landing API. spades_utility(mode="regression") retains the older fixed-numeric-pool helper for backward compatibility; the main regression workflow no longer uses it. spades_target_stability with fixed regression scores retains its original fixed-pool semantics; repeat spades_land_response with specified seeds for dynamic-response targets, then audit their released outputs. Flight resume is unchanged; dynamic landing does not support interrupted-SA resume.

Existing categorical case-study results remain valid and must not be regenerated. Main-workflow numeric reconstruction is now explicitly post-landing, so its reconstructed values need not be bitwise identical to previous wrapper versions at the same seed. Core categorical SA and flight kernels remain unchanged.
