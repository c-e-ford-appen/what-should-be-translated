# What Should Be Translated? A Large-Scale Human Evaluation of
# Source-Language Preservation Decisions in Multilingual LLM Translation: data and analysis code

Annotations and analysis code for **What Should Be Translated? A Large-Scale Human Evaluation of
Source-Language Preservation Decisions in Multilingual LLM Translation** ([venue, year]).

Seventy-five native-speaker raters across 15 locales judged 104 segments
from five English commercial emails, each translated by seven multilingual
LLMs. For every segment, raters recorded whether it was translated or left
in English, the quality of the translation, and, where English was
retained, whether retaining it was the best choice. This repository
contains the anonymised annotations (54,598 judgments) and the R code that
produces every model, table and figure in the paper.

## Contents

| Path | What it is |
|---|---|
| `data/annotations.csv` | One row per judgment: segment, model, locale, rater and the three answers. Codebook in [`data/README.md`](data/README.md). |
| `data/exclusions_log.csv` | Judgments and raters remaining after each data-preparation step (Appendix D.1). |
| `analysis.R` | Fits SM1–SM3, computes inter-rater reliability, and writes all tables and figures. |
| `install.R` | Installs the required R packages. |
| `session_info.txt` | R and package versions used for the reported results. |

## What the released data leave out

Rater identities (emails and platform IDs), free-text comments and
demographic answers are not released. Raters appear only as anonymous IDs
(`R01`–`R75`). Exclusions were applied before release: the steps are
described in Appendix D.1 and their counts are in
`data/exclusions_log.csv`. The preprocessing script that applied them
works on identifiable data and is therefore not included.

## Reproducing the results

1. Install R (4.3 or later) and a C++ toolchain (see `install.R`).
2. From the repository root, install the packages:
   ```
   Rscript install.R
   ```
3. Check that the pipeline runs (short chains, a few minutes; results are
   not meant for reporting):
   ```
   QUICK=1 Rscript analysis.R
   ```
4. Run the full analysis (4 chains × 4,000 iterations per model; expect
   several hours):
   ```
   Rscript analysis.R
   ```

Outputs are written to `output/` (`output_quick/` in quick mode):

| Folder | Contents |
|---|---|
| `tables/` | CSV tables. Files starting `D1_` to `D5_` correspond to Appendix D.1–D.5. |
| `figures/` | `fig_*.pdf`: paper figures. `*_pp_check_panel.pdf`: posterior predictive checks. `*.png`: other plots. |
| `summaries/` | Model summaries and contrasts as text. |
| `fits/` | Fitted models. Re-running reloads them instead of refitting, unless the data or model change. |

All models use seed 2026. MCMC results can still differ slightly across
operating systems and package versions; `session_info.txt` lists the
versions used for the paper.

## Citation

@inproceedings{preserve2026,
  title     = {What Should Be Translated? A Large-Scale Human Evaluation of Source-Language Preservation Decisions in Multilingual LLM Translation},
  author    = {Anonymous},
  booktitle = {Proceedings of the 64th Annual Meeting of the Association for Computational Linguistics},
  year      = {2026},
  note      = {Under review}
}

## License

This work is licensed under a Creative Commons Attribution 4.0 International License (CC BY 4.0).

You are free to share and adapt the material for any purpose, including commercially, provided appropriate credit is given, a link to the license is included, and any changes are indicated. See LICENSE for full terms.
