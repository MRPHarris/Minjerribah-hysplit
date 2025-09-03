Code used to produce and analyse HYSPLIT trajectories associated with large (>100 mm) rainfall events at K'gari (Fraser Island) and Minjerribah (North Stradbroke Island) in Queensland, Australia.

A contribution to the manuscript by Tibby et al., *Mid-Holocene drying of K'gari lakes (subtropical eastern Australia) necessitates re-evaluation of El Niño-Southern Oscillation intensification and future drying risk*, currently accepted for publication in the Journal of Quaternary Science. 

The only data not packaged with this repository are the outputs from the trajectory model, which are too large for github at around 110 MB each. Contact me at m.harris@gns.cri.nz if you would like this data.

FILES
/.gitignore 
/Kgari-ENSO-reporting.Rmd a dual-function .Rmd script that renders text and figures to word or, if the param `TIFF = TRUE` is specified in the setup chunk, should render the .tiff files found in /figure tiffs/
/Kgari-ENSO-reporting.docx the word doc rendered by the above .Rmd file
/Minjerribah-hysplit.Rproj R project file used in Rstudio
/README.md this readme

DIRECTORIES
/data/ contains data (e.g., rainfall, summarised trajectory data)
/external images/ contains BOM MSLP charts used for some per-event validation whilst testing whether the NCEP/NCAR data was appropriate for the analysis
/figure TIFFs/ contains manuscript .tiff image files rendered by the .Rmd file when the param `TIFF = TRUE` is specified in the setup chunk. 
/scripts/ contains three scripts that, when run/worked through in order, should allow the HYSPLIT runs for the manuscript to be replicated. 
/themes/ contains an .R file sourced by the .Rmd file for thematic settings (ggplot2 themes, mainly)
