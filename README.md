# The Role of Context in Syllable Co-Occurence Patterns for Word Segmentation

## Defining contexts by coder judgment
 
_OPEN_ME.r has instrutcions for RAs, including the code to access the coding_scripts.r file 

context_cleaning_keys.txt and categories_cleaning_keys.txt are files for standardizing RA codes and combining codes into groups, respectively. For example, the first file standardizes spelling differences and synonyms (eating, meal, and eatting all get changed one code). The second file groups similar codes together into categories (changing clothes, brushing hair, undressing, and dressing all get changed to dressing). 
    
## Defining context by key words    

## Defining context with topic modeling

## Computational models to segment speech using statistical regularities

## Boostrapping nontext distributions
I am using [AWS](http://aws.amazon.com). 

## Code tools
This project is organized with [Project Template](http://projecttemplate.net/). 
The datasets are cached in the cache folder, or they can be recreated from the raw corpus material using the scripts in the munge folder. 
All R packages needed are listed in the global.dcf file in the config directory. 
All of the functions written by me for this project are saved in the lib directory. 
When you run ProjectTemplate::load.project() in the directory for this project, it will load the libraries, scripts, and cached data, as directed in the global.cdf config file.