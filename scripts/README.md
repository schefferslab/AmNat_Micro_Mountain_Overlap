# Scripts


Folders and scripts flagged with `00_` do not need to be run. They are either source code (and will be called via `source()` somewhere else) or are exploratory scripts used to inform other decisions (and therefore do not write out intermediate/final products that are necessary for the workflow).  

**00: Source Code**  
R source code that is called elsewhere in the program  

**01: Site Processing**  
R code related to curating datasets in prep for figure generation and analysis  

**02: Database Compile**  
R code related to compiling the database  

**03: Analysis**  
R code related to statistical analyses/modeling  

**04: Figure Scripts**  
R code related to developing data visualizations  


_note: some file paths in these scripts may be broken. If they are, it's my fault, but it also is likely that those files may not be especially important to reproducing paper results._  