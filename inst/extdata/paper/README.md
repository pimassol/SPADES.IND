# Classification case-study input data

`training_data.csv`: 9,000 records used as the real training reference.
`test_real_1000.csv`: 1,000 records reserved for evaluation.
Both files have the same eight columns in the original order. Their bytes and
original SHA-256 checksums are retained unchanged from the supplied archive.
`data_inventory.csv` records observed cardinalities and literal missing markers.

The split is supplied, not recreated by a new random split. Duplicate profiles
are retained. There are 7 distinct complete profiles shared by the files;
profile equality does not establish that these are the same sampled individual.

The text `Missing` occurs in physical_activity and genetic_predisposition in the
training file. This archive preserves the historical input. The package treats
this literal label as a category unless included in `missing_tokens`. Declaring
it missing invokes complete-case exclusion and changes the training reference;
that is a different analysis from preserving this supplied file unchanged.

Read categorical codes as character values or factors, including diagnosis,
occupation, physical_activity and genetic_predisposition. Age is numeric.
Do not allow numeric diagnosis codes to select regression automatically.
The loader in `examples/load_paper_data.R` returns typed data without fitting,
generation, filtering, deduplication or changes to the split.

Column descriptions (code meanings are not inferred):

| Column | Representation | Description |
|---|---|---|
| municipality_residence | categorical | Municipality of residence |
| age | integer | Age; observed values 18 to 65 |
| civil_status | categorical | Civil-status labels as supplied |
| gender | categorical | Gender labels as supplied |
| occupation | categorical | Codes 0 and 1 |
| physical_activity | categorical | Codes 1 to 5 and literal Missing |
| genetic_predisposition | categorical | Codes 1 to 8 and literal Missing |
| diagnosis | categorical response | Codes 1 to 4 |

These files were supplied by the maintainer as the inputs used for the paper.
The maintainer has indicated that they may be made public. No external source,
collection method, codebook or data reuse license is inferred from the CSVs.
