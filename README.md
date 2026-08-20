# USCIS Processing Times

A dashboard showing how USCIS form and office processing times have changed,
built from a stacked archive of daily snapshots.

USCIS publishes only today's estimate for each form, office and subtype. When a
figure changes it is replaced in place, with no changelog and no notification,
and the previous value is not retained anywhere. Stacking daily snapshots is the
only way to recover the history.

> [!IMPORTANT]
> **This is not our data, and this is not a government project.**
>
> USCIS publishes these figures. We are not part of USCIS or any government
> agency. Nothing here is official.
>
> We did not collect the daily record either. Another public project saves a copy
> of the agency's figures every day. We read those copies and built this from
> them. We do not name that project here.

## Using the dashboard

Hover any bar, dot or cell to see the forms, offices and figures behind it.

| Page | Question it answers |
|---|---|
| Overview | What is USCIS publishing now, and what changed in the newest release? |
| Forms | How have a form's published bounds moved, by subtype and office? |
| Offices | How far apart are offices for the same form, today and since the start? |
| Trends | Which forms deteriorated or recovered, and did waits grow on net? |
| Backslides | When did the case-inquiry date move backwards, and by how much? |
| Disclosure | Which keys stopped being published, and where do the data contradict itself? |
| Freshness | How old is the number you are shown, and do the published figures agree? |
| Methods | How every figure on the dashboard is derived, and the rules applied |
| Codebook | Every table and variable, with types, examples and missing rates |

## Layout

Scripts are sourced in the order shown. The dashboard is a single Quarto
document that reads the panel they produce.

```sh
.
├── scripts/            # the pipeline, sourced in order by the workflows
│   ├── config.R        # constants; the upstream source comes from the environment
│   ├── fetch.R         # download and validate a snapshot
│   ├── normalize.R     # units to months, with a reason code when nothing is usable
│   ├── extract.R       # one snapshot to rows, plus the office dimension
│   ├── panel.R         # stack snapshots into the raw panel
│   ├── derive.R        # cycles, events, inquiry moves, disclosure, monthly
│   ├── checks.R        # standing checks run against the newest snapshot
│   ├── manifest.R      # per-snapshot provenance and status
│   ├── artifacts.R     # write the SQLite panel and the monthly extracts
│   ├── backfill.R      # full rebuild over every snapshot
│   └── update.R        # incremental run over what is new
├── tests/              # the unit suite, run on every push
├── site/
│   ├── index.qmd       # the Quarto dashboard
│   ├── custom.scss     # tokens driving both the chrome and the charts
│   └── dark.scss       # the same tokens restepped for the dark ground
├── data/
│   └── xwalk_key_rekey.csv   # keys that were renamed upstream
└── .github/workflows/  # test, update and render
```

## Database overview

A single SQLite panel. Each table below is one grain, meaning the unit that one
row stands for.

| Table | Grain |
|---|---|
| `pt_snapshots` | one row per form, office, subtype and day |
| `pt_events` | one row per observed change to a published bound |
| `pt_cycles` | one row per run of identical published values |
| `inquiry_events` | one row per movement of the case-inquiry date |
| `disclosure_events` | one row per key appearing, disappearing or returning |
| `coverage` | one row per calendar day, marking whether a snapshot exists |
| `offices` | office code to name, folded across every snapshot era |

The Codebook page documents every variable in each of them.

## Data conventions

Some of these numbers are easy to misread, so it is worth knowing what they
actually mean.

- Bounds are **percentile positions** under the agency's 80% completion method,
  never a minimum and a maximum, and never averaged.
- A missing bound is `NA` with a reason code, **never `0`**. Reading an absent
  lower bound as zero would claim instant processing.
- A key counts as **withdrawn** only once two consecutive snapshots that both
  exist lack it, so a gap in collection can never look like a withdrawal.
- Days with no snapshot are recorded as **absent**, and a change that lands
  inside a gap carries the date collection resumed, not the date it happened.
- Bounds are published to the **half month** and nothing finer. The dates are
  exact, so day counts are unaffected by that rounding.

## Running it

The pipeline needs the upstream source, which is supplied by the environment
rather than carried in the repository:

```sh
echo 'SOURCE_RELEASES_REPO=owner/repo' > .Renviron   # git-ignored
```

Without it the pipeline stops with a clear error rather than running.

```sh
Rscript tests/testthat.R                             # unit tests
quarto preview site/index.qmd                        # the dashboard
```

The dashboard reads the panel from `USCIS_PANEL`. Office names come from the
panel's own `offices` table, so it needs nothing else.

## Source

> [!NOTE]
> USCIS publishes these processing times. Another public project saves a copy of
> them every day. We use those copies. We did not collect them.
>
> We do not name that project here. It does the hard part, and we do not want to
> cause it any trouble.
