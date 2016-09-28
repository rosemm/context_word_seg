# The Role of Context in Syllable Co-Occurence Patterns for Word Segmentation

## About this repo
This repository contains all of the code and most of the materials needed to run this project from start to finish.
Materials not included here are omitted because they are not mine (such as the materials kindly shared with me from Swingley, 2005; Goldwater, Griffiths, & Johnson, 2009; and Börschinger, Demuth, & Johnson, 2012), or because storing them here would be cumbersome (such as the thousands of directories and files produced by the computational modeling simulations).
The data themselves are available in the cache folder, in various stages of processing, so even without all of the raw materials, nearly all of the analyses can be replicated. 

This project is organized with [Project Template](http://projecttemplate.net/). 
The datasets are cached in the cache folder, or they can be recreated from the raw corpus material using the scripts in the munge folder. 
All R packages needed are listed in the global.dcf file in the config directory. 
All of the functions written by me for this project are saved in the lib directory. 
When you run ProjectTemplate::load.project() in the directory for this project, it will load the libraries, scripts, and cached data, as directed in the global.cdf config file.

## Defining contexts by coder judgment
 
_OPEN_ME.r has instrutcions for RAs, including the code to access the coding_scripts.r file 

context_cleaning_keys.txt and categories_cleaning_keys.txt are files for standardizing RA codes and combining codes into groups, respectively. For example, the first file standardizes spelling differences and synonyms (eating, meal, and eatting all get changed one code). The second file groups similar codes together into categories (changing clothes, brushing hair, undressing, and dressing all get changed to dressing). 
    
## Defining context by key words    

## Defining context with topic modeling

## Computational models to segment speech using statistical regularities

## Boostrapping nontext distributions
I am using [AWS](http://aws.amazon.com). 
