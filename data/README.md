# Project: NYC Open Data
### Data folder

The data directory contains data used in the analysis. This is treated as read only; in paricular the R/python files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.

**school.select.csv** contains data used in the first tab for searching and filtering colleges and universities in the map. 
**College2014_15.new.csv** contains data used in the second tab for comparing different colleges and universities. 
**variable_names.docx** contains variables we choose to focus on when analyzing the features of colleges and universities.

The dictionary excel file contains the defintion of each variable recorded in school.select and College2014_15.new.
The rds files are created during deploying process.