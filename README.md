  # Spatiotemporal Dengue Network Analysis in Selangor, Malaysia

R workflow for spatiotemporal dengue network construction, social network analysis, ERGM modelling, and Gephi visualisation in Selangor, Malaysia.

This repository contains a curated and reproducible R workflow for constructing and analysing spatiotemporal dengue linkage networks.

The workflow is based on a 2023 dengue surveillance study in Selangor, Malaysia, and is intended for:

- research reproducibility;
- teaching and training;
- public-health network analysis;
- adaptation to other infectious-disease datasets.

The original confidential surveillance dataset is **not included** in this repository.

---

## Study Concept

Each laboratory-confirmed dengue case is represented as a **node**.

An **undirected edge** is created between two cases when both conditions are met:

- residential distance ≤ 200 metres;
- difference in illness-onset date ≤ 14 days.

This linkage represents **spatiotemporal epidemiological proximity** and should not be interpreted as confirmed person-to-person transmission.

---

## Analytical Workflow

The repository follows this sequence:

```text
Data cleaning
      ↓
Spatiotemporal linkage construction
      ↓
Social Network Analysis
      ↓
Centrality analysis
      ↓
Gephi export
      ↓
Statnet network preparation
      ↓
ERGM analysis
      ↓
MCMC diagnostics
      ↓
Goodness-of-fit assessment
      ↓
Sensitivity analysis

## Software

- R 4.3.2
- statnet
- ergm
- network
- igraph
- coda
- Gephi 0.10.1
- QGIS

## Data availability

The original dengue surveillance dataset is not included in this repository
because it contains confidential individual-level health and residential
geolocation information.

Synthetic data will be provided for demonstration of the analytical workflow.

## Citation

If you use or adapt code from this repository for research, teaching,
or publication, please cite this repository using the citation
information provided in `CITATION.cff`.

## License

This project is licensed under the GNU General Public License v3.0
(GPL-3.0). See the `LICENSE` file for details.

## Disclaimer

The code in this repository is provided for research and educational
purposes. The original individual-level dengue surveillance data are
not publicly distributed due to confidentiality and data-governance
requirements.
