# Function to clean a string of characters into just a-z text.

require(stringr)

Clean_String <- function(string){
     # Lowercase
     temp <- tolower(string)
     #' Remove everything that is not a number or letter.
     temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", "")
     # Shrink down to just one white space
     temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
     
     # Insert apostraphes in all contactions
     temp <- gsub("\\b(are|ca|could|did|does|do|had|has|have)nt\\b", "\\1n't", temp)
     temp <- gsub("\\b(is|might|mus|sha|should|was|were|would|wo)nt\\b", 
                  "\\1n't", temp)
     temp <- gsub("\\b(that|there|where|who)(d|ll|s)\\b", "\\1'\\2", temp)
     temp <- gsub("\\b(she|he)(s)\\b", "\\1'\\2", temp)
     temp <- gsub("\\bi(m|ve)", "i'\\1", temp)
     temp <- gsub("\\b(what|they|you)(d|ll|ve|re)", "\\1'\\2", temp)
     temp <- gsub("\\bweve", "we've", temp)
    
     return(temp)
}

# Function to clean a block of text (several strings)
Clean_Text_Block <- function(text){

     # Loop through the lines in the text and use the append() function to
     clean_text <- Clean_String(text[1])
     for(i in 2:length(text)){
          # add them to a vector
          clean_text <- append(clean_text,Clean_String(text[i]))
     }
     return(clean_text)
}