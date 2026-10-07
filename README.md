# DataAnalysisInSportScience

**Fall 2024 · SKKU Data Analysis in Sport Science (SPT5062) Weekly Projects**

## Overview

This repository archives selected weekly R exercises from the SKKU course **Data Analysis in Sport Science (SPT5062) Weekly Projects**.
The course covered data collection, regression, panel-data methods, causal inference, matching, and mediation/moderation analysis.

The final project is intentionally maintained separately and is not included in this repository.

## Weekly Contents

| Week | Topic | Public script |
|---|---|---|
| 01 | Basic code structure and descriptive analysis | `week01/basic_code_structure.R` |
| 02 | Web scraping | `week02/web_scraping.R` |
| 03 | YouTube Data API | `week03/youtube_api.R` |
| 04 | Regression analysis | `week04/regression.R` |
| 05 | Binary outcome models | `week05/binary_outcome_models.R` |
| 06 | Panel regression | `week06/panel_regression.R` |
| 07 | Instrumental variables | `week07/instrumental_variables.R` |
| 08 | Difference-in-differences | `week08/difference_in_differences.R` |
| 09 | Synthetic control | `week09/synthetic_control.R` |
| 10 | Propensity score matching | `week10/propensity_score_matching.R` |
| 12 | Mediation and moderation analysis | `week12/mediation_moderation.R` |

## Data and Course Materials

Course-provided slides, PDFs, session files, and datasets are not redistributed in this repository.
The scripts retain the original input filenames where local data are required, so the corresponding data must be supplied separately to reproduce those exercises.

Expected local inputs referenced by the scripts include:

- Week 01: `mlb_pitch_lecture.csv`, `mlb_batter.csv`
- Week 03: `klgfull.csv`
- Week 04: `weight-height.csv`
- Week 05: `hn22_all.sas7bdat`
- Week 06: `klgfull.csv`, `2023weather.csv`
- Week 07: `hn22_all.sas7bdat` and the regional unemployment-rate CSV used in the class exercise
- Week 08: `olympicapt.csv`
- Week 09: `olympicapt.csv`
- Week 10: `gamelog_class.csv`, `batstat2024.csv`
- Week 12: `samplekhn22.csv`

## API Credentials

API credentials are not stored in this repository.
For Week 03, set the YouTube API key as an environment variable before running the script:

```bash
export YOUTUBE_API_KEY="your_key_here"
```

## Notes

Machine-specific working-directory commands from the original coursework files were removed for repository portability.
The analytical contents of the weekly scripts are otherwise preserved as coursework records rather than rewritten as a new project.

This repository was later organized and published on GitHub for archival and portfolio purposes.
