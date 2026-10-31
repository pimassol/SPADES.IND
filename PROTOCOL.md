# Approved case-study migration

Source package: SPADES 0.0.0.9005, supplied in SPADES_sorgenti_20261030_110600.zip.
Algorithm references:
- SPADES_flight_BN_energy_diversity_6worker.R
- SPADES_landing_FA_nuovo_pool_1000000.R

Preserved: independent BN proposals and unit BN energy coefficient; static
plausibility/privacy weights 1, diversity weight .7, distance scales .25;
chain-local growing archive, same-archive old/new comparison, repeated states;
no pool deduplication; exact real categorical matches excluded; simple mean of
all fidelity terms; one-index symmetric landing swaps; final state export;
fixed in-sample RF guide; absolute accuracy gap; unit F/U weights; geometric
cooling with 400 pilot proposals, target uphill acceptance .8, ratio .001.

Generalized: data/path/column names/target size; supplied or RF-selected pairs;
conditioning column chosen by maximum cardinality; number of logical chains
separate from number of execution workers; no utility mode; normalized RMSE mode.
Numeric values fixed before regression scoring; integer reconstruction honors bin
boundaries. Thus workflow seeds/output numerics need not be bitwise identical to
old case-study scripts. The classifier guide still evaluates prepared categories.
For exact case-study comparisons supply the saved pairs explicitly and align
preprocessing, fitted models, chain seeds, landing seeds and initial indices.

Version 0.1.1 adds completed-pool reuse and optional exact flight checkpoints.
Landing checkpoint resume is not implemented. The original case-study scripts
and outputs remain untouched. See RESUME.md for the recovery contract.

Legacy extracted modules remain for provenance/research access; new exported
flight/landing functions do not call the old MH/SA engines. Their old statistical
options are intentionally not accepted by the new control API.

Version 0.1.2 filters the reference once, before binning or fitting. All model
training, privacy distances and fidelity/utility references use these complete
rows. This intentionally changes the population used when input is incomplete;
there is no model of missingness. Flight and SA energy formulas are unchanged.
