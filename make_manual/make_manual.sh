#!/bin/bash
rm *.tex gdf.*
python3 extract_manual.py
touch gdf.ind
pdflatex gdf
bibtex gdf
makeindex gdf
pdflatex gdf
makeindex gdf
pdflatex gdf
