# Extract LaTeX sections from gdf.for - Python interpretation of GDF$MANUAL
# Stephen Fegan - 2025-01-05

import re

def extract_latex_sections(fortran_file):
    """
    Extracts LaTeX sections from Fortran source code and writes them to their respective files.
    :param fortran_file: Path to the Fortran source code file.
    """
    doc_marker = re.compile(r"CDOC (\S+)")
    ver_shift_marker = re.compile(r"CVER-(\d+)")
    ver_marker = re.compile(r"CVER")
    tex_marker = re.compile(r"CTEX")
    end_marker = re.compile(r"CEND")
    inside_doc = False
    inside_ver = False
    ver_shift = 1
    current_file = None
    latex_content = []

    with open(fortran_file, 'r') as file:
        for line in file:
            if not inside_doc:
                doc_match = doc_marker.search(line)
                if doc_match:
                    current_file = doc_match.group(1)
                    inside_doc = True
                    latex_content = []
            else:
                if ver_marker.search(line):
                    ver_shift = 1
                    latex_content.append(r"\begin{verbatim}")
                    inside_ver = True
                    ver_shift_match = ver_shift_marker.search(line)
                    if ver_shift_match:
                        ver_shift = int(ver_shift_match.group(1))
                elif tex_marker.search(line):
                    if inside_ver:
                        latex_content.append(r"\end{verbatim}")
                        ver_shift = 1
                        inside_ver = False
                elif end_marker.search(line):
                    if inside_ver:
                        latex_content.append(r"\end{verbatim}")
                        ver_shift = 1
                        inside_ver = False
                    inside_doc = False
                    if current_file:
                        with open(current_file, 'w') as out_file:
                            out_file.write('\n'.join(latex_content))
                        print(f"Extracted {current_file}")
                    current_file = None
                elif inside_ver:
                    latex_content.append(line[(ver_shift-1):].strip())
                else:
                    latex_line = line[1:].strip()
                    if latex_line.startswith('\\usepackage{draftcopy}'):
                        # Replace old DRAFT watermark package with new
                        latex_line = '\\usepackage{draftwatermark}\n\SetWatermarkText{DRAFT}'
                    elif latex_line.startswith('\\usepackage[dvips]{graphics}'):
                        # Conflict with class options
                        latex_line = '\\usepackage{graphics}' 
                    latex_content.append(latex_line)

if __name__ == "__main__":
    extract_latex_sections("../gdf.for")
    extract_latex_sections("../gdf_example.for")
    print("LaTeX sections extraction completed.")