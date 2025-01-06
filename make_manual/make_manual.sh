#!/bin/bash
python3 extract_manual.py
pdflatex gdf
makeindex gdf
bibtex gdf
pdflatex gdf
pdflatex gdf
