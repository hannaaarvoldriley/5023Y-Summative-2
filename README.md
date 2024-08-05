# Summative 2 BIO 5023Y
# An analysis of the impact of inbreeding on flight inhibition in Bicyclus anynana

Author: Hanna Aarvold-Riley

This GitHub project is intended to serve as a complete and reproducible pipeline from raw data to a complete and referenced report on the statistical analysis of a provided dataset about inbreeding depression in butterflies.

The raw data can be found in the "data" folder under "inbreeding_butterfly.csv". In the "project folder" a script called "butterflies.R" provides annotated code walking the reader through my process from raw data, tidy data, exploratory analysis, model fitting to producing figures. A markdown file also in the "projects" folder called "butterflies_report.Rmd" sources this script and compiles figures and text to produce a report on the my findings in the dataset, complete with previous published research, full reported statistical analysis and discussion.


The raw data contains the following variables:

Variable - Definition
-ID -	Individual
-IC -	Inbreeding coefficient - three levels indicating the number of generations of sib-matings in recent family history
-FII	- flight inhibition index – the number of times the butterfly settled during a two-minute period when it was being stimulated to take off immediately once it settled
-body_part -	Thorax or rest of body
-dry_weight -	The dry weight in (mg) of the part of the butterfly
This raw data is available as a .csv in the data folder


To view the report please first run the entirety of the butterflies.R script followed by the entire butterflies_report.Rmd markdown script, then press knit to generate a pdf.

Packages used:
-tidyverse
-janitor
-dplyr
-kableExtra
-ggplot2

*please note all code should run without breaks. The install.packages() function may be used to install any packages which library() is unable to call.