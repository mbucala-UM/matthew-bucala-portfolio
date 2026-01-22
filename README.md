# Measles Transmission in Outpatient Waiting Rooms

## Agent-Based Simulation Model for Infection Prevention and Control

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R Version](https://img.shields.io/badge/R-%3E%3D4.0.0-blue.svg)](https://www.r-project.org/)
[![Simulation](https://img.shields.io/badge/Simulation-simmer-green.svg)](https://r-simmer.org/)

---

## 📖 Table of Contents

- [Overview](#overview)
- [Background](#background)
- [Key Findings](#key-findings)
- [Model Description](#model-description)
  - [Conceptual Framework](#conceptual-framework)
  - [State Variables](#state-variables)
  - [Stochastic Elements](#stochastic-elements)
  - [Transmission Model](#transmission-model)
- [Parameters](#parameters)
- [Intervention Scenarios](#intervention-scenarios)
- [Installation](#installation)
- [Usage](#usage)
- [Output Files](#output-files)
- [Reproducibility](#reproducibility)
- [Citation](#citation)
- [License](#license)
- [Contact](#contact)

---

## Overview

This repository contains the complete simulation code, analysis scripts, and reproducibility materials for the manuscript:

> **Comparative Modeling of Masking, Room Ventilation, and Vaccination to Reduce Measles Transmission in Outpatient Waiting Rooms**
>
> Submitted to *Infection Control & Hospital Epidemiology*

---

## Background

### The Problem

Measles is one of the most contagious diseases known, with a basic reproduction number (R₀) of 12–18 and approximately 90% of susceptible individuals becoming infected after exposure. In outpatient settings, pre-symptomatic or undiagnosed patients can expose susceptible individuals before airborne isolation can be implemented.

Healthcare facilities need evidence-based guidance on which infection prevention strategies to prioritize when isolation is not feasible.

### Our Approach

We developed a stochastic agent-based simulation model to compare:

1. **Universal masking** — surgical masks for all patients
2. **Enhanced ventilation** — doubling air changes per hour from 6 to 12 ACH
3. **Bundled interventions** — masking + ventilation + reduced wait times

The model follows established frameworks:

- **ODD Protocol** (Grimm et al., 2020) for standardized model description
- **MInD-Healthcare Framework** (Slayton et al., 2020) for healthcare-associated infection modeling

---

## Key Findings

| Intervention | Median Infections | IQR | Reduction vs. Baseline |
|--------------|-------------------|-----|------------------------|
| Baseline | 2.0 | 1.0–4.0 | — |
| Universal Masking | 1.0 | 0.0–2.0 | **50%** ✓ |
| Enhanced Ventilation | 2.0 | 1.0–4.0 | ~0% (NS) |
| Bundled | 1.0 | 0.0–2.0 | **50%** ✓ |

### Main Conclusions

- ✅ **Universal masking** is the most effective single intervention
- ⚠️ **Enhanced ventilation** alone provides minimal benefit under typical conditions
- 📈 **Vaccination status** is the dominant driver of transmission risk
- ➡️ **Bundled interventions** offer no advantage over masking alone

---

## Model Description

### Conceptual Framework

The simulation represents a typical outpatient clinic waiting room over an 8-hour day:

```
┌─────────────────────────────────────────────────────────────────┐
│                    OUTPATIENT WAITING ROOM                      │
│                                                                 │
│                        ┌─────────┐                              │
│                        │  INDEX  │                              │
│                        │  CASE   │                              │
│                        └────┬────┘                              │
│                             │                                   │
│                    Airborne viral particles                     │
│                   (infectious 120 minutes)                      │
│                             │                                   │
│              ┌──────────────┼──────────────┐                    │
│              │              │              │                    │
│              ▼              ▼              ▼                    │
│        ┌─────────┐    ┌─────────┐    ┌─────────┐                │
│        │CONTACT 1│    │CONTACT 2│    │CONTACT 3│    ...         │
│        │         │    │         │    │         │                │
│        │ Vax: Y/N│    │ Vax: Y/N│    │ Vax: Y/N│                │
│        │Infect: ?│    │Infect: ?│    │Infect: ?│                │
│        └─────────┘    └─────────┘    └─────────┘                │
│                                                                 │
│    ═══════════════════════════════════════════════════════════  │
│    Ventilation: 6–12 ACH        Capacity: 10 seats              │
│    Duration: 8-hour day         Arrivals: 12 patients/hour      │
└─────────────────────────────────────────────────────────────────┘
```

### Simulation Environment

| Attribute | Value | Rationale |
|-----------|-------|-----------|
| Setting | Single well-mixed waiting room | Homogeneous mixing assumption |
| Capacity | 10 seats | Typical medium-sized facility |
| Duration | 8 hours (480 minutes) | Standard clinic day |
| Patient arrivals | Poisson, λ = 12/hour | ED benchmarking data |
| Wait times | Exponential, μ = 30 min | Literature estimate |
| Index cases | 1 per simulation | Conservative scenario |

### State Variables

Each agent (patient) has three state variables:

| Variable | Type | Description |
|----------|------|-------------|
| `arrival_time` | Continuous | Time of arrival (minutes from start) |
| `vaccination_status` | Binary | 0 = protected, 1 = susceptible |
| `infection_status` | Binary | 0 = uninfected, 1 = infected |

### Stochastic Elements

Four model components are stochastic (randomly determined each simulation):

| Element | Distribution | Parameters | Rationale |
|---------|--------------|------------|-----------|
| Arrival times | Poisson | λ = 12/hour | Standard queuing theory |
| Wait durations | Exponential | mean = 30 min | Memoryless property |
| Vaccination | Bernoulli | p = 0.08 | US coverage ~92% |
| Transmission | Bernoulli | p = P_trans | Per-contact risk |

### Transmission Model

The probability of transmission from the infectious index case to each susceptible contact is calculated as:

```
P_transmission = p_base × f_vent(ACH) × f_mask
```

#### Component 1: Baseline Transmission Probability

```
p_base = 0.25
```

**Sources:**
- Riley et al. (1978): Airborne spread in schools
- Bloch et al. (1985): Pediatric practice outbreak
- Remington et al. (1985): Physician's office transmission
- Zemouri et al. (2020): Healthcare transmission 4.5–99.8%

#### Component 2: Ventilation Factor

```
f_vent(ACH) = exp(−0.05 × ACH)
```

| ACH | f_vent | Interpretation |
|-----|--------|----------------|
| 2 | 0.905 | Poor ventilation |
| 6 | 0.741 | Standard (baseline) |
| 12 | 0.549 | Enhanced |
| 15 | 0.472 | Maximum practical |

**Derivation:** Calibrated to Wells-Riley framework; doubling ACH produces ~26% reduction.

#### Component 3: Masking Factor

```
f_mask = 1 − (adherence × 0.60)
```

| Adherence | f_mask | Interpretation |
|-----------|--------|----------------|
| 0% | 1.00 | No masking |
| 30% | 0.82 | Low compliance |
| 60% | 0.64 | Moderate |
| 100% | 0.40 | Universal |

**Sources:**
- Chu et al. (2020): 60% effectiveness (95% CI: 45–75%)
- Jefferson et al. (2023): Cochrane systematic review

#### Example Calculations

| Scenario | ACH | Mask | Calculation | P_trans |
|----------|-----|------|-------------|---------|
| Baseline | 6 | 0% | 0.25 × 0.741 × 1.00 | **0.185** |
| Masking only | 6 | 100% | 0.25 × 0.741 × 0.40 | **0.074** |
| Ventilation only | 12 | 0% | 0.25 × 0.549 × 1.00 | **0.137** |
| Bundled | 12 | 100% | 0.25 × 0.549 × 0.40 | **0.055** |

---

## Parameters

### Complete Parameter Table

| Parameter | Symbol | Value | Source |
|-----------|--------|-------|--------|
| **Epidemiological** | | | |
| Baseline transmission | p_base | 0.25 | Calibrated |
| Measles R₀ | R₀ | 12–18 | Guerra et al. 2017 |
| Airborne persistence | — | 120 min | WHO guidelines |
| **Facility** | | | |
| Room capacity | — | 10 seats | Design guidelines |
| Simulation duration | — | 480 min | 8-hour clinic |
| Arrival rate | λ | 12/hour | Liang et al. 2021 |
| Mean wait time | μ | 30 min | ED benchmarks |
| Baseline ventilation | ACH | 6 | ASHRAE 170-2021 |
| Enhanced ventilation | ACH | 12 | ASHRAE 170-2021 |
| **Interventions** | | | |
| Mask effectiveness | e | 60% | Chu et al. 2020 |
| Baseline unvaccinated | p | 8% | Lo & Hotez 2017 |
| **Simulation** | | | |
| Iterations per scenario | n | 500 | Power analysis |

---

## Intervention Scenarios

### Primary Scenarios

| Scenario | ACH | Mask | Wait | Unvax |
|----------|-----|------|------|-------|
| Baseline | 6 | 0% | 30 min | 8% |
| Universal Masking | 6 | 100% | 30 min | 8% |
| Enhanced Ventilation | 12 | 0% | 30 min | 8% |
| Bundled | 12 | 100% | 10 min | 8% |

### Sensitivity Analyses

**One-Way:**
| Parameter | Range | Increments |
|-----------|-------|------------|
| % Unvaccinated | 50–95% | 5% |
| Ventilation | 2–15 ACH | 1 ACH |
| Mask adherence | 0–100% | 10% |

**Two-Way:**
- Vaccination × Ventilation
- Masking × Vaccination

---

## Installation

### Requirements

- R ≥ 4.0.0
- ~4 GB RAM
- ~15 minutes runtime

### Install Packages

```r
install.packages(c(
  "simmer",     # Discrete-event simulation
  "dplyr",      # Data manipulation
  "tibble",     # Data frames
  "ggplot2",    # Visualization
  "MASS",       # Negative binomial
  "emmeans",    # Pairwise comparisons
  "effsize",    # Effect sizes
  "ggpubr",     # Publication plots
  "reshape2",   # Data reshaping
  "viridis"     # Color palettes
))
```

### Clone Repository

```bash
git clone https://github.com/[username]/measles-outpatient-transmission-model.git
cd measles-outpatient-transmission-model
```

---

## Usage

### Quick Start

```r
# Set working directory
setwd("path/to/repository")

# Run complete analysis (~15 min)
source("measles_simulation.R")
```

### Run Custom Scenario

```r
# Load functions
source("measles_simulation.R")

# Custom scenario: 75% mask compliance, 15% unvaccinated
results <- run_scenario(
  ach = 6,
  mask_adherence = 0.75,
  p_unvaccinated = 0.15,
  n_runs = 1000
)

# Summary
mean(results)
quantile(results, c(0.25, 0.50, 0.75))
```

### Calculate Transmission Probability

```r
# Baseline
calc_transmission_prob(ach = 6, mask_adherence = 0)
# → 0.185

# Universal masking
calc_transmission_prob(ach = 6, mask_adherence = 1.0)
# → 0.074
```

---

## Output Files

### Directory Structure

```
repository/
├── measles_simulation.R              # Main script
├── README.md                         # This file
├── LICENSE                           # MIT License
│
├── figures/                          # PNG, 300 DPI
│   ├── fig1_boxplot_scenarios.png
│   ├── fig2_histogram_scenarios.png    ← Revised per reviewer
│   ├── fig3_ecdf_scenarios.png
│   ├── fig4_histogram_comparison.png
│   ├── fig5_tornado_sensitivity.png
│   ├── fig6_masking_doseresponse.png
│   ├── fig7_heatmap_vax_ach.png
│   └── fig8_heatmap_mask_vax.png
│
└── results/                          # CSV files
    ├── primary_scenario_results.csv
    ├── oneway_sensitivity_results.csv
    ├── twoway_vaccination_ventilation.csv
    └── twoway_masking_vaccination.csv
```

### Figure Descriptions

| # | File | Description |
|---|------|-------------|
| 1 | `fig1_boxplot_scenarios.png` | Box plots by scenario |
| 2 | `fig2_histogram_scenarios.png` | **Histograms** (revised for discrete data) |
| 3 | `fig3_ecdf_scenarios.png` | Cumulative distributions |
| 4 | `fig4_histogram_comparison.png` | Side-by-side comparison |
| 5 | `fig5_tornado_sensitivity.png` | One-way sensitivity |
| 6 | `fig6_masking_doseresponse.png` | Mask adherence dose-response |
| 7 | `fig7_heatmap_vax_ach.png` | Vaccination × Ventilation |
| 8 | `fig8_heatmap_mask_vax.png` | Masking × Vaccination |

---

## Reproducibility

### Random Seeds

| Analysis | Seed | Location |
|----------|------|----------|
| Primary scenarios | 42 | Section 5 |
| One-way sensitivity | 123 | Section 8 |
| Two-way sensitivity | 456 | Section 9 |

### Session Info

```
R version 4.3.2
simmer 4.4.6
dplyr 1.1.4
ggplot2 3.4.4
```

---

## Citation

### Paper

```bibtex
@article{author2026measles,
  title   = {Comparative Modeling of Masking, Room Ventilation, 
             and Vaccination to Reduce Measles Transmission 
             in Outpatient Waiting Rooms},
  author  = {[Authors]},
  journal = {Infection Control \& Hospital Epidemiology},
  year    = {2026},
  doi     = {10.1017/ice.2026.XXX}
}
```

### Code

```bibtex
@software{author2026code,
  author  = {[Authors]},
  title   = {Measles Outpatient Transmission Model},
  year    = {2026},
  url     = {https://github.com/[user]/measles-outpatient-transmission-model}
}
```

---

## License

MIT License — see [LICENSE](LICENSE)

---

## Contact

- **Corresponding Author:** [Name], [Email]
- **GitHub Issues:** [Open an issue](https://github.com/[user]/measles-outpatient-transmission-model/issues)

---

## Acknowledgments

- Editor and Reviewers at *ICHE*
- Developers of the `simmer` R package
- Grimm et al. (ODD Protocol)
- Slayton et al. (MInD-Healthcare Framework)

---

*Last updated: January 2026*
