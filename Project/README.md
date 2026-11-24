# Eurovision Project

Group 1 presentation
QFin Computing
WU Vienna

## How to compile the slides

The slides read their numbers from `results/` and `figures/`, which are in
the repo, so everyone can compile without rerunning the analysis.

Needs R and a LaTeX installation with beamer (e.g. `tinytex`).

```bash
cd Project/slides
R CMD Sweave eurovision.Rnw
pdflatex eurovision.tex
bibtex eurovision
pdflatex eurovision.tex
pdflatex eurovision.tex
```

## Rerun the analysis

Not necessary to compile the slides, but if someone wants to rerun it below are the steps.

Requires R packages `igraph`, `readxl`, `jsonlite` for 

To rerun `build_data.R` clone https://github.com/EurovisionAPI/dataset