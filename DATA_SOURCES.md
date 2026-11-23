# Data Sources

## Primary source

**SportsDataverse / hoopR NBA shot releases**

- R package: `hoopR`
- Primary loader planned for Stage 2: `hoopR::load_nba_shots()`
- Source family: ESPN-derived NBA play-by-play shot releases distributed through SportsDataverse
- Project data repository license: CC BY 4.0
- hoopR package license: MIT

## Planned season coverage

| hoopR season | NBA season | Planned role |
|---:|---|---|
| 2023 | 2022-23 | Development / training |
| 2024 | 2023-24 | Development / validation |
| 2025 | 2024-25 | Development / validation |
| 2026 | 2025-26 | Final untouched test season |

## Planned scope

- NBA regular-season field-goal attempts
- Made/missed outcomes
- Shot coordinates and derived court geometry
- Shot/event type where available and validated
- Game, team, player, period, and clock identifiers/context
- No defender-tracking, contest-distance, or player-movement variables unless a separate public and redistributable source is verified

## Public repository policy

- Raw season downloads will be reproducible but excluded from Git.
- Public code will document all acquisition and transformation steps.
- Compact derived application artifacts may be committed when appropriate.
- No restricted material from prior interview/technical exercises will be included.

## Attribution

This project will provide attribution to SportsDataverse/hoopR and the underlying source documentation in the final README and methodology documentation.
