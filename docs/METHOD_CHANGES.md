# FES-CF V3.2.0: implementation and methods changes

This document describes the implemented change from the earlier pipeline. The authors supplied the V3.2.0 result exports on 29 September 2026 and the current execution audits on 30 September 2026. Those files were reconciled with the source and are included in this repository. The transition originally required regenerated results; repository preparation does not require another empirical run. This record does not certify independent exact replication from the raw frozen inputs.

## Suggested methods wording

Anchor ridge penalties are selected by purged chronological cross-validation. For
FPCA-X, each fold learns the response mean, principal-component basis and retained
dimension exclusively from its training block. A common ridge penalty is applied
to the retained scores. Candidate penalties are assessed by the unweighted mean
squared error of the reconstructed validation surfaces, so that losses remain
comparable when the retained dimension differs across folds. The one-standard-error
rule selects the strongest eligible penalty. The FPCA representation is then
re-estimated on the enclosing training sample for the final anchor fit. For the
graph-augmented anchor, the correlation graph is similarly relearned within each
fold's training block and re-estimated on the full training sample after tuning.
Predictor centering and scaling use only the relevant training observations.

Residual-model penalties are selected using the pre-existing training-only guard
partition. For each eligible correction, the residual ridge path is fit using the
fit-block residuals. Its penalty minimizes unweighted residual prediction MSE on
the corresponding guard observations, with ties resolved toward stronger
regularization. Conditional on that penalty, the correction magnitude is selected
using the projected, weighted guard loss and the prespecified activation rule.
The selected residual penalty is carried unchanged into the full-training refit;
cross-validation is not reapplied to residual targets constructed using enclosing
full-training anchor fits. State-specific corrections require the prespecified
minimum fit and guard sample sizes. State thresholds are learned from the fit
partition for guard selection and from the full training partition for the final
forecast. No calibration or test responses enter these tuning decisions.

The guard partition is reused for anchor/blend selection and correction tuning,
and its residuals subsequently supply the training-based conformal scale. Thus
guard improvements are model-selection diagnostics, rather than independent
estimates of forecasting performance. Reported forecast comparisons must use the
separate evaluation observations. These implementation changes do not establish
coverage guarantees for dependent financial observations or imply that FES-CF
outperforms the competing models.

## Diagnostics and reproducibility

Post-projection violation counts use the configured absolute feasibility tolerance
of 1e-10, while retaining the maximum unrounded violation. The Dykstra stopping
check requires both iterate stabilization and feasibility within tolerance. F7
and A20 express both guard and test improvements as relative weighted-MSE reductions
against the selected anchor. The prior test-axis relative RMSE reduction has been
replaced to make the two axes comparable.

V3.2.0 uses the unchanged V3.1.5 cached asset and VIX observations as frozen inputs,
records their SHA256 hashes, and fits the revised models afresh. Completed split
results and their audit entries are saved with a key incorporating code identity,
runtime/package versions, design data, split definitions and configuration. Split
random seeds are derived from these keys, so resuming a completed split does not
require repeating its stochastic calculations. This also changes the inference
random stream relative to V3.1.5; old bootstrap or MCS numbers are not carried over.
An interrupted unfinished split restarts from its beginning.

The configured sample, fixed study universe, FULL analysis modules, horizon/tail
grids, primary loss weights, embargo, conformal specification and default bootstrap
replication counts are inherited from V3.1.5. The separate synthetic test uses
smaller settings for verification and does not modify the FULL analysis settings.

Primary implementation reference: [glmnet documentation](https://glmnet.stanford.edu/articles/glmnet.html).
The supplied synthetic validation was executed with glmnet 4.1-8; the actual
empirical run records its installed package versions in session-info.txt.
