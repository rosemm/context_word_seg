# The Role of Context in Syllable Co-Occurence Patterns for Word Segmentation

## Defining contexts by coder judgment
coding_scripts.R has the functions research assistants use to code the transcripts. Instructions given to RAs working on this are saved in instructions_for_RAs.txt
    
_OPEN_ME.r has instrutcions for RAs, including the code to access the coding_scripts.r file 

context_coding_cleaning.R has the functions for cleaning the RAs' context coding, and for compiling them all into one document.
    
## Defining context by key words    
data_processing.r has the analysis code.

data_processing_functions.r has a list of specialized functions called by data_processing.r

data_processing_orth_to_phon.r is a procedure for translating utterances from the .cha files from CHILDES to phon via Swingley's (2005) dictionary

words_by_contexts.csv is the list of key words (seed words) for each context. These lists are based on MCDI words supplemented with some especially frequent content words from the corpus that we expected would be associated with particular contexts – for example, milk was on the list for mealtime words, and wipe was on the list for diaper change words. 

## Defining context with topic modeling


## Writing
The manuscript draft is in paper.Rnw (a sweave file, combining Latex and R code)

Some latex resources:     
[http://www.tug.org/pracjourn/2008-1/zahn/zahn.pdf](http://www.tug.org/pracjourn/2008-1/zahn/zahn.pdf)
[http://mirror.jmu.edu/pub/CTAN/macros/latex/contrib/apa6/apa6.pdf](http://mirror.jmu.edu/pub/CTAN/macros/latex/contrib/apa6/apa6.pdf)
[http://merkel.zoneo.net/Latex/natbib.php](http://merkel.zoneo.net/Latex/natbib.php)
[http://yihui.name/knitr/demo/sweave/](http://yihui.name/knitr/demo/sweave/)
