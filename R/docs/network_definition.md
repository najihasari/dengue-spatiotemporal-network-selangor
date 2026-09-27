# Network Definition

## Overview

This project models dengue notifications in Selangor, Malaysia, as a spatiotemporal linkage network.

The analytical period is 1 January to 31 December 2023.

## Nodes

Each node represents one laboratory-confirmed dengue case, located using the residential location recorded in the surveillance dataset.

## Edges

An undirected edge is created between case *i* and case *j* when both of the following conditions are satisfied:

\[
d_{ij} \leq 200 \text{ metres}
\]

and

\[
|t_i - t_j| \leq 14 \text{ days}
\]

where:

- \(d_{ij}\) is the residential distance between the two cases;
- \(t_i\) and \(t_j\) are their illness-onset dates.

In simplified form:

```text
Edge = 1 if:
distance <= 200 metres
AND
onset difference <= 14 days
