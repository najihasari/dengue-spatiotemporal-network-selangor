# dengue-spatiotemporal-network-selangor
R workflow for spatiotemporal dengue network construction, social network analysis, ERGM modelling and GIS/Gephi visualisation in Selangor, Malaysia.
# Spatiotemporal Dengue Network Analysis in Selangor

This repository contains the analytical workflow used to construct and analyse
spatiotemporal dengue linkage networks in Selangor, Malaysia.

## Study overview

Laboratory-confirmed dengue cases were represented as nodes.

An undirected edge was created between two cases when:

- residential distance was ≤ 200 metres; and
- difference in illness onset was ≤ 14 days.

The analytical workflow includes:

- data preprocessing
- construction of spatiotemporal linkages
- social network analysis
- ERGM modelling
- model diagnostics and goodness-of-fit assessment
- sensitivity analysis
- export for Gephi and QGIS visualisation

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
