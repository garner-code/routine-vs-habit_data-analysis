# Routine vs Habit: Data Analysis

An R-based data analysis pipeline for processing and visualising behavioural data from the Routine vs Habit visual foraging experiment. This repository is a companion to the task code at [garner-code/routine-vs-habit_task-code](https://github.com/garner-code/routine-vs-habit_task-code), which runs the experiment and produces the raw data consumed here.

## Overview

Participants complete a visual foraging task in which they learn to find hidden targets behind doors across two probabilistic contexts. This repository contains scripts to:

- Load and wrangle raw behavioural data from BIDS-inspired TSV files
- Compute trial-level and subject-level summary metrics (accuracy, setting errors, response times)
- Classify setting errors into *sticks* (perseveration) and *slips* (slip-of-action)
- Process Five Facet Mindfulness Questionnaire (FFMQ) survey data
- Process working memory task (MTS) data
- Produce publication-quality figures

## Repository Structure

```
.
├── README.md
├── routine-vs-habit_data-analysis.Rproj   # RStudio project file
└── src/
    ├── run_wrangling.R             # Main data extraction and wrangling pipeline
    ├── run_re-wrangling.R          # Re-run wrangling with updated parameters
    ├── run_wrangling_survey.R      # FFMQ survey data processing
    ├── run_wrangling_wmt.R         # Working memory task (MTS) data processing
    ├── vis_data.R                  # Main visualisation script
    │
    ├── get_data.R                  # Load and format raw TSV files per participant/session
    ├── get_subs.R                  # Return list of participant IDs to process
    ├── get_switch.R                # Identify context-switch trials
    ├── get_setting_stability.R     # Classify sticks vs slips in setting errors
    ├── get_rts.R                   # Extract and outlier-trim response times
    ├── get_learned_doors.R         # Track which doors were learned per participant
    ├── get_transition_probabilities.R  # Compute door-to-door transition matrices
    ├── get_wrangled_data.R         # Load pre-processed results
    ├── join_multi_data.R           # Combine data across blocks/sessions
    ├── count_stereo.R              # Count stereotyped door sequences
    │
    ├── plot_accuracy_types.R       # Plot accuracy by door type
    ├── plot_dots_and_lines.R       # Dot-and-line summary plots
    ├── plot_rt_distributions.R     # Response time distribution plots
    ├── plot_sticks_and_slips.R     # Sticks vs slips visualisation
    ├── plot_summary.R              # Multi-panel summary figure
    ├── plot_violins.R              # Violin plots by condition
    ├── plot_violins_subses.R       # Violin plots split by sub-session
    └── theme_doors.R               # Custom ggplot2 theme
```

## Requirements

### R Version

R ≥ 4.1 is recommended.

### R Packages

Install the required packages from CRAN:

```r
install.packages(c(
  "tidyverse",   # data wrangling and ggplot2
  "zeallot",     # destructuring assignment (%<-%)
  "stringr",     # string manipulation
  "here",        # project-relative paths
  "wesanderson", # colour palettes for figures
  "data.table",  # fast data loading (survey/WMT scripts)
  "magrittr",    # pipe operators
  "ggplot2",     # plotting (also included in tidyverse)
  "ggsci"        # scientific colour palettes
))
```

## Data Format

Raw data should be placed in a `data/` directory at the root of the project. Data are organised in a BIDS-inspired structure matching the output of the task code:

```
data/
└── sub-XXX/
    ├── ses-learn-uncertainty/
    │   └── beh/
    │       ├── sub-XXX_ses-learn-uncertainty_task-mforage_beh.tsv
    │       └── sub-XXX_ses-learn-uncertainty_task-mforage_trls.tsv
    └── ses-main-task/
        └── beh/
            ├── sub-XXX_ses-main-task_b-mt1_task-mforage_beh.tsv
            ├── sub-XXX_ses-main-task_b-mt1_task-mforage_trls.tsv
            ├── sub-XXX_ses-main-task_b-mt2_task-mforage_beh.tsv
            ├── sub-XXX_ses-main-task_b-mt2_task-mforage_trls.tsv
            ├── sub-XXX_ses-main-task_b-st1_task-mforage_beh.tsv
            ├── sub-XXX_ses-main-task_b-st1_task-mforage_trls.tsv
            ├── sub-XXX_ses-main-task_b-st2_task-mforage_beh.tsv
            └── sub-XXX_ses-main-task_b-st2_task-mforage_trls.tsv
```

`beh.tsv` files contain one row per door-click event; `trls.tsv` files contain one row per trial. See the [task code repository](https://github.com/garner-code/routine-vs-habit_task-code) for a full description of these file formats.

## Usage

Open the project via `routine-vs-habit_data-analysis.Rproj` so that `here()` correctly resolves to the project root. All scripts in `src/` are designed to be run from the project root.

### 1. Wrangle behavioural data

```r
source("src/run_wrangling.R")
```

This script:

1. Discovers all participants in `data/` whose files match the expected naming pattern
2. Loads and formats event-level data for each participant and session using `get_data()`
3. Classifies each door click as:
   - **Current context** (`door_cc`) — door belongs to the active context
   - **Other context** (`door_oc`) — door belongs to the inactive context
   - **Neither context** (`door_nc`) — door belongs to neither context
4. Identifies setting errors and further classifies them as *sticks* or *slips* via `get_setting_stability()`
5. Saves three output files to `res/`:
   - `data_evt_.csv` — one row per click event
   - `data_trl_.csv` — one row per trial with accuracy and error summaries
   - `data_avg-ss_.csv` / `data_avg_.csv` — per-participant averages by sub-session and session

### 2. Wrangle survey data (FFMQ)

```r
source("src/run_wrangling_survey.R")
```

Scores the Five Facet Mindfulness Questionnaire, computes sub-scale totals (Observing, Describing, Acting with Awareness, Non-judging, Non-reactivity), and saves results to `res/<exp>_svy_.csv`.

### 3. Wrangle working memory task data

```r
source("src/run_wrangling_wmt.R")
```

Loads match-to-sample (MTS) trial data, computes accuracy and mean RT by condition, and saves to `res/<exp>_mts_.csv` and `res/<exp>_mts_avg_.csv`.

### 4. Visualise results

```r
source("src/vis_data.R")
```

Generates a set of PDF figures saved to `fig/`. Figures include violin plots, scatter plots with regression lines, and density plots for accuracy, setting errors, response time, and learning onset (k4) as a function of training group and trial type.

## Key Metrics

| Metric | Description |
|--------|-------------|
| `accuracy` | Proportion of clicks on current-context doors |
| `setting_errors` | Proportion of clicks on other-context doors |
| `setting_sticks` | Proportion of setting errors that are perseverations (sticks) |
| `setting_slips` | Proportion of setting errors that are slip-of-action errors |
| `n_clicks` | Total door clicks per trial |
| `switch` | Whether the trial began with a context switch (0 = stay, 1 = switch) |
| `rt` | Response time from trial onset (or previous door close) to door selection |

## Output Files

All output is written to the `res/` directory (created automatically if absent). Figures are saved to `fig/` (must exist or be created before running `vis_data.R`).

| File pattern | Contents |
|---|---|
| `<exp>_evt_.csv` | Click-level event data for all participants |
| `<exp>_trl_.csv` | Trial-level summaries |
| `<exp>_avg-ss_.csv` | Per-participant averages grouped by sub-session |
| `<exp>_avg_.csv` | Per-participant averages grouped by session |
| `<exp>_svy_.csv` | FFMQ survey scores |
| `<exp>_mts_.csv` | Working memory task (MTS) event data |
| `<exp>_mts_avg_.csv` | Working memory task averages |

## Related Repository

The experimental task code that generates the raw data processed here is available at:

> **[garner-code/routine-vs-habit_task-code](https://github.com/garner-code/routine-vs-habit_task-code)**

That repository contains the MATLAB/Psychtoolbox code for running the visual foraging experiment, including instructions for installation, hardware requirements, and the data output format.

## Citation

If you use this code in your research, please cite:

```
Garner, K. (2026). Routine vs. Habit Data Analysis [Software].
GitHub: https://github.com/garner-code/routine-vs-habit_data-analysis
```

## License

Please contact the repository maintainers via GitHub for usage permissions and academic collaboration enquiries.
