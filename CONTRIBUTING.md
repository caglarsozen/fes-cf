# Contributing

For a bug report, include the software version/commit, R version, relevant package versions, the command used, and a small reproducible example or concise error log. Avoid attaching provider caches or full private working folders to a public issue.

Use a branch and a pull request for changes. Explain the problem, the change and the validation performed. Changes to fitting, timing, calibration or inference should include a focused synthetic check and explain which empirical outputs need regeneration.

The V3.2.0 scientific files are recorded in `PROVENANCE_SHA256.csv`. Do not update their hashes merely to hide a discrepancy. A deliberate change should receive a version/change-log entry, its own validation and a newly reviewed provenance manifest. Documentation-only updates do not require refitting models.

Contributions intended for inclusion should be compatible with the repository's MIT license. Citation is requested through `CITATION.cff`; it is not an additional restriction added to the MIT terms.
