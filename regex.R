####### REGEX in R ######

#### 1. Finding Strings - grep() ####
# Creae a vector variable and assign four string values ot the variable
strings <- c("abcd", "cdab", "cabd", "c abd")

# Find string values containing 'ab', return indices
grep("ab", strings)

# Find string values containing 'ab', return indices
grep("ab", strings, value = FALSE)

# Find string values containing 'ab', return values
grep("ab", strings, value = TRUE)


###### 2. Finding and replacing patterns: sub() and gsub() #######

# gsub(pattern, replacement, string) returns the modified string after replacing every pattern occurrence with replacement in string.
# sub(pattern, replacement, string) replaces the first pattern occurrence.

fruits <- c("apple", "orange", "pineapple")
# Specify a string pattern
pattern <- "a"
# Specify a replacement value
replacement <- "A"
# Run gsub to replace all 'a' occurrences with 'A'
gsub(pattern, replacement, fruits)

# or simply
gsub("a","A",fruits)
