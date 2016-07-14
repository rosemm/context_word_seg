####################################################################
# to read in CHILDES transcripts and clean to match conventions in Swingley's dictionary
####################################################################

# use blank_doc to turn all of the transcripts into one neat dataframe (but omit the coder columns)
df <- blank_doc(wd = "corpora/transcripts/", for.coding = FALSE) %>%
  unite(utt, file, UttNum) %>%
  select(utt, orth=utterance)

# delete the 7 utterances with "unintelligible" content
# (note that that this corpus does not adhere to all modern CHAT guidelines for indicating, for example, untranscribable speech)
df <- df[!grepl(pattern="unintelligible", x=df$orth), ] 

# tidying the utterances to match conventions in the dict file
orth <- df$orth # to make the following commands a little easier to read
orth <- gsub(pattern="@[[:alpha:]]+", replacement="", x=orth) # delete @ tags at the end of words (used to denote, for examle, word play or singing)
orth <- gsub(pattern="[?]", replacement="", x=orth) # delete question marks
orth <- gsub(pattern="[.]", replacement="", x=orth) # delete periods
orth <- gsub(pattern="[!]", replacement="", x=orth) # delete exclamation points
orth <- gsub(pattern="[;]", replacement="", x=orth) # delete semicolons
orth <- gsub(pattern="mummmy", replacement="mummy", x=orth) #typo
orth <- gsub(pattern="nosy", replacement="nosey", x=orth) #typo
orth <- gsub(pattern="loulou", replacement="lou-lou", x=orth)
orth <- gsub(pattern="tata's", replacement="ta-ta's", x=orth)
orth <- gsub(pattern="cmon", replacement="c'mon", x=orth)
orth <- gsub(pattern="uhuh", replacement="uh-huh", x=orth)
orth <- gsub(pattern="tumtum", replacement="tum tum", x=orth)
orth <- gsub(pattern="haha", replacement="ha ha", x=orth)
orth <- gsub(pattern="ahhah", replacement="aah ha", x=orth)
orth <- gsub(pattern="[[].*[]]", replacement="", x=orth) # delete bracket patterns from the utterances (used to provide notes or translations of unconventional speech)
orth <- gsub(pattern="bath+room", replacement="bathroom", x=orth, fixed=T)
orth <- gsub(pattern="play+mate", replacement="playmate", x=orth, fixed=T)
orth <- gsub(pattern="t+shirt", replacement="t-shirt", x=orth, fixed=T)
orth <- gsub(pattern="sack+cloth", replacement="sackcloth", x=orth, fixed=T)
orth <- gsub(pattern="+", replacement=" ", x=orth, fixed=T) # replace remaining + with a space (used to join words, e.g. "patty+cake")
orth <- gsub(pattern="_", replacement=" ", x=orth, fixed=T) # also used to join words (e.g. "all_gone")
orth <- gsub(pattern="[(]i)t", replacement="it", x=orth)
orth <- gsub(pattern="y[(]ou[)]", replacement="you", x=orth)
orth <- gsub(pattern="[(]i[)]f", replacement="if", x=orth)
orth <- gsub(pattern="d[(]o[)]", replacement="do", x=orth)
orth <- gsub(pattern="t[(]o[)]", replacement="to", x=orth)
orth <- gsub(pattern="[(]h[)]ave", replacement="'ave", x=orth)
orth <- gsub(pattern="[(]h[)]ere", replacement="'ere", x=orth)
orth <- gsub(pattern="[(].*[)]", replacement="", x=orth) # delete remaining () patterns from the utterances (used for shortened speech, e.g. "(be)cause")
orth <- gsub(pattern="[:]", replacement="", x=orth) # delete remaining colons (used to indicate drawn-out words, e.g. "oooooh" as "o:h")
df$orth <- tolower(orth) # copy it back over to the dataframe

