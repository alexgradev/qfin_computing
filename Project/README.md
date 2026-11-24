# Eurovision Project

**Authors:** Group 1 (Bachtiary, Gradev, Hartai, Isakova, Pemsel)

**Course:** QFin Computing WS26/27 (WU Vienna)

### How to compile the slides

The slides read their numbers from `results/` and `figures/`, which are in
the repo, so everyone can compile without rerunning the analysis.

Needs R and a LaTeX installation with beamer (e.g. `tinytex`).

For MacOS:
```bash
cd Project/slides
R CMD Sweave eurovision.Rnw
pdflatex eurovision.tex
bibtex eurovision
pdflatex eurovision.tex
pdflatex eurovision.tex
```

For Windows/Linux ask AI.

### Rerun the analysis

Requires R packages `igraph`, `readxl`, `jsonlite` 

To rerun `build_data.R` clone https://github.com/EurovisionAPI/dataset