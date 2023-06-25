#################################################
### Strings with stringr ########################
#################################################
library(tidyverse)
library(stringr)
double_quote <- "\""
double_quote
single_quote <- '\''
single_quote
backslash <- "\\"
backslash
writeLines(c(double_quote, single_quote, backslash))
?'"'
"\u00b5"
str_length(c('a', 'R for data science', NA))

# str_c - combining strings
str_c('x', 'y')
'x' + 'y'  # error
df <- tibble('col1' = c('A','n','d','r','e','w'), 'col2' = c('K','r','i','s','t','o'))
df$col1
str_c(df$col1, df$col2, collapse = '*')
str_c(df$col1, df$col2)
str_c(df$col1, collapse = '')


df1 <- tribble(
  ~x, ~y,
  'a',  '1',
  'b',  '2',
  'c', '7'
)
df1
str_c(df1$x, df1$y)
str_c(df1$x, df1$y, sep = '-')

# Replace NA to 'NA' string
# missing values are contagious. If you want them to print as "NA", use str_replace_na()
x <- c('abc', NA)
x
str_c("|-", x, "-|")
x <- c('abc', 'g')
str_c("|-", x, "-|")
x <- c('abc', NA)
str_c("|-", str_replace_na(x), "-|")
str_c("prefix-", c('a', 'b', 'c'), "-suffix")
name <- "Andrei"
time_of_day <- "morning"
birthday <- F
str_c("Good ", time_of_day, ' ', name,
      if (birthday) ' and HAPPY BIRTHDAY!', '.')
# To collapse a vector of strings into a single string
str_c(c('x', 'y', 'z'), collapse = ', ')
str_c(c('a','b', 'c'), c('1','2','3'))
str_c(c('a','b', 'c'), c('1','2','3'), collapse = '-')

# Subsetting string
x <- c('Apple', 'Banana', "Pear"); x
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub('a', 1, 3)
str_sub(x, 1, 30)
str_sub(x, 1, 1) <- str_to_lower(str_sub(x,1,1))
x
s <- str_sub(x, 1, 1)
s
s <- str_to_lower(str_sub(x,1,1))
s

# Locales
vignette("locales")
str_to_upper(c("i", "ı"))
str_to_upper(c("i", "ı"), locale = "tr")
# If you want robust behavior across different computers, you may want to use 
# str_sort() and str_order(), which take an additional locale argument
x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")
str_sort(x, locale = 'haw')
# function that can accept an ascii character and return the decimal value, something like this:
asc <- function(x) { strtoi(charToRaw(x),16L) }
asc("a")
charToRaw('a')
typeof(charToRaw('a'))
strtoi(charToRaw('a'))
strtoi(charToRaw('a'), base = 16L)
strtoi('a', base = 16L)
# To use as a function to return the ascii character given it’s decimal code value
chr <- function(n) { rawToChar(as.raw(n)) }
chr(asc("a")) 
chr(97)
# To print the ascii table (for 32:126)
coderange <-  c(32:126)
asciitable_printable <-  data.frame(coderange,as.raw(coderange),row.names=rawToChar(as.raw(coderange),multiple=TRUE))
colnames(asciitable_printable) <- c("dec","hex")
asciitable_printable

# Exercise
# 1. paste() and paste0() what’s the difference between the two 
# functions? What stringr function are they equivalent to?
?paste()
paste('a', 'b')
paste('a', 'b', sep = '+')
str_c('a', 'b')
str_c('a', 'b', sep = '+')
# 3. Use str_length() and str_sub() to extract the middle character from a 
# string. What will you do if the string has an even number of characters?
s <- 'What wiLl you do?'
str_sub(s, start = str_length(s) %/% 2, end = (str_length(s) %/% 2))
# 4. What does str_wrap() do? When might you want to use it?
?str_wrap()
s <- str_wrap(string = 'ads dsf sadf asas aa fggfgds  wqefwefw we eweeppwkomkm',
              width = 10, indent = 3, exdent = 1)
writeLines(s)
# 5. What does str_trim() do? What’s the opposite of str_trim()?
?str_trim()
s <- '   ads dsf sadf asas aa fggfgds   wqefwefw we eweeppwkomkm     '
str_trim(s)
str_trim(s, side = c('left'))
str_trim(s, side = c('right'))
str_squish(s) # removed white spaces inside

# Regular expressions
# To learn regular expressions, we’ll use str_view() and str_view_all(). 
# These functions take a character vector and a reg‐ ular expression, and 
# show you how they match
x <- c('apple', 'banana', 'pear')
str_view(string = x, pattern = 'an')
str_view_all(string = x, pattern = 'an')
# . which matches any character
str_view(x, '.a.')
str_view(x, '..a..')
# if "." matches any character, how do you match the character "."? You 
# need to use an “escape” 
# So to match an ., you need the regexp \.. Unfortunately this creates a 
# problem. We use strings to represent regular expressions, and \ is also 
# used as an escape symbol in strings. So to create the regular expression \. 
# we need the string "\\."
dot <- '\\.'
writeLines(dot)
str_view(c('abc', 'wsa.cm', 'bef'), 'a\\.c')
# how to match \
x <- 'a\b'
x <- 'a\\b, dfd\\g'
writeLines(x)
str_view(x, '\\\\')
str_view_all(x, '\\\\')
# How would you match the sequence ("'\)
x <- "wef'\\"
x
writeLines(x)
str_view(x, "'\\\\")
# What patterns will the regular expression \..\..\.. match?
# How would you represent it as a string?
x <- 'sssd.d.v.bbl,l;p'
x
writeLines(x)
str_view(x, pattern = '\\..\\..\\..')
x <- 'fdjnvdkjf2019.11.07lklkfdv'
x
writeLines(x)
str_view(x, pattern = "....\\...\\...")
x <- 'fdjnvdkjf2019\\11\\07lklkfdv'
x
writeLines(x)
str_view(x, pattern = "....\\\\..\\\\..")

# Anchors
x <- c('apple', 'banana', 'pear')
str_view(x, '^a')
str_view(x, 'a$')
x <- c('apple pie', 'apple', 'apple cake')
str_view(x, 'apple')
str_view(x, '^apple$')
# We can also match the boundary between words with \b
x <- c('apple pie', 'applefruit', 'apple cake')
str_view(x, 'apple\\b')

# How would you match the literal string "$^$"?
x <- 'lksjfl$^$kkclcmn'
x
str_view(x, '\\$\\^\\$')
# Given the corpus of common words in stringr::words, create
# regular expressions that find all words that:
#  a. Start with “y”
words <- stringr::words
str_view_all(words, '^x', match = T)
# b. End with “x”.
str_view_all(words, 'x$', match = T)
# c. Are exactly three letters long
str_view_all(words, "^...$", match = T)
# d. Have seven letters or more
str_view_all(words, '^.......', match = T)
str_view_all(words, '.......$', match = T)

# Special patterns
# \d matches any digit.
# \s matches any whitespace (e.g., space, tab, newline). • [abc] matches a, b, or c.
# [^abc] matches anything except a, b, or c.
str_view(c('grey', 'gray'), pattern = "gr(e|a)y")

# 1. Create regular expressions to find all words that:
# a. Start with a vowel
str_view_all(string = words, pattern = '^[aie]', match = T)
# b. Only contain consonants. (Hint: think about matching “not”-vowels.)
str_view_all(string = words, pattern = '^[aie]', match = T)
# c. End with ed, but not with eed.
str_view_all(string = words, pattern = "[^e]ed$", match = T)
# d. End with ing or ize
str_view_all(string = words, pattern = '((ing)|(ize))$', match = T)
# 2. Empirically verify the rule “i before e except after c.”
str_view(words, pattern = "cie", match = T)
# 3. Is “q” always followed by a “u”?
str_view(words, pattern = "q", match = T)
# 5. Create a regular expression that will match telephone numbers as commonly 
# written in your country
# +375(33)3047867
x <- '+375(33)304-78-67'
x <- '+375(33)3047867'
x <- '+375(29)254-42-94'
x <- c('+375(33)304-78-67', '+375(33)3047867', '+375(29)254-42-94',
       '8(33)304-78-67', '8(033)3047867', '8(29)254-42-94', 
       '8(25)3450978', '8(25)345-09-78', '+375(25)3450978')
x
writeLines(x)
str_view(x, pattern = "\\+(375)\\((29|33|25)\\)[0-9]+-?[0-9]+-?[0-9]+")
str_view(x, pattern = "\\+(375)\\((29|33|25)\\)[0-9]+-?[0-9]+-?[0-9]+")
str_view(x, pattern = "((\\+375)|(8))\\((29|33|25)\\)[0-9]{3}-?[0-9]{2}-?[0-9]{2}")
str_view(x, pattern = "((\\+375\\((29|33|25)\\))|(8\\((033|029|025)\\)))[0-9]{3}-?[0-9]{2}-?[0-9]{2}")

# Repetition
# ?: 0 or 1
# +: 1 or more
# *: 0 or more
library(stringr)
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
x
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, "CC*")
str_view(x, "C[LX]+")
x <- "1888 is the longest year 1888 in Roman numerals: MDCCCLXXLXVIII"
str_view(x, "C[LX]+")
str_view(x, "^18+")
str_view_all(x, "18{3}")
x <- "1888 is the longest year 1888 in Roman 1888 numerals: MDCCCLXXLXVIII"
x <- "1888kjkljlk1888kljlkjlk1888"
stringr::str_view_all(x, "18{3}")
stringr::str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")
# By default these matches are “greedy”: they will match the longest string 
# possible. You can make them “lazy,” matching the shortest string possible, 
# by putting a ? after them
x <- "MDCCCLXXLXVCIII"
str_view(x, 'C{2,3}')
str_view(x, 'C{2,3}?')
str_view_all(x, 'C{1,3}?')
str_view_all(x, 'C{1,3}+')
str_view(x, 'C[LX]+')
str_view(x, 'C[LX]+?')
# 2. What these regular expressions match (read carefully to see if I’m using a regular expression or a string that defines a regular expression):
# a. ^.*$
x <- "MDCCCLXXLX9'VCIII"
str_view(x, "^.*$") # matches the whole string
# b. "\\{.+\\}" 
x <- "flfkj49_ {4}ldks;..."
x <- "flfkj49_ {458}ldks;..."
str_view(x, "\\{.+\\}") # 1 or more symbols inside {}
# c. \d{4}-\d{2}-\d{2}
x <- 'ljkdsfl2019-11-08lkdsjf'
x <- 'ljkdsfl2019-1-08lkdsjf'
str_view(x, "\\d{4}-\\d{2}-\\d{2}") # date YYYY-MM-DD
# d. "\\\\{4}"
x <- c("akdsjf\\\\\\\\dsmflkds", "akdsjf\\\\\\smflkds")
x
writeLines(x)
str_view(x, "\\\\{4}")
# 3. Create regular expressions to find all words that:
# a. Start with three consonants.
str_view(words, "^[b,c,d,f,g,h,j,k,l,m,n,p,q,r,s,t,v,w,x,y]{3}", match = T)
# b. Have three or more vowels in a row.
str_view(words, "[aeioue]{3,}", match = T)
# c. Have two or more vowel-consonant pairs in a row.
str_view(words, "([aeioue]{1}[b,c,d,f,g,h,j,k,l,m,n,p,q,r,s,t,v,w,x,y]{1}){2,}", match = T)
# d. Have two or more consonant-vowel pairs in a row.
str_view(words, "([b,c,d,f,g,h,j,k,l,m,n,p,q,r,s,t,v,w,x,y]{1}[aeioue]{1}){2,}", match = T)
# 4. Solve the beginner regexp crosswords at https://regexcross word.com/challenges/beginner
# HE|LL|O+
x <- "jkdsHEl;c_LL-sdof000"
str_view_all(x, "HE|LL|0+")

# Grouping and Backreferences
str_view(words, "(..)\\1", match = T)
str_view(words, "(.)\\1", match = T)
str_view('asfsoooooodscxgv', "(...)\\1")
# 1. Describe, in words, what these expressions will match:
# a. (.)\1\1
str_view(words, "(.)\\1\\1", match = T) # three equal letters
str_view("lksdjfmmmmmmmsdkkkkkkkpuqwpoe", "(.)\\1\\1", match = T)
# b. "(.)(.)\\2\\1" 
str_view(words, "(.)(.)\\2\\1", match = T) # 'ebbe'
str_view(words, "(.)(.)\\1\\2", match = T) # 'ebeb'
# c. (..)\1
str_view(words, "(..)\\1", match = T)
# d. "(.).\\1.\\1" 
str_view(words, "(.).\\1.\\1", match = T) # 'ebete'
str_view("jkhfsykypywyqnal", "(.).\\1.\\1", match = T)
# e. "(.)(.)(.).*\\3\\2\\1"
str_view(words, "(.)(.)(.).*\\3\\2\\1", match = T) # 'ertxtre'
str_view("manfnamkypywyqnal", "(.)(.)(.).*\\3\\2\\1", match = T)
# 2. Construct regular expressions to match words that
# a. Start and end with the same character
str_view(words, "^(.).*\\1$", match = T)
str_view(words, "^(.)(.*)\\1$", match = T) # the same
str_view(c("wow", "wowa"), "^(.).*\\1$", match = T)
# b. Contain a repeated pair of letters (e.g., “church” contains “ch” repeated twice)
str_view(words, "(.)(.).*\\1\\2", match = T)
# Contain one letter repeated in at least three places (e.g., “eleven” contains three “e”s).
str_view(words, "(.).*\\1.*\\1", match = T)
str_view(words, "(.)(.*)\\1(.*)\\1", match = T)
str_view(c("XlkdjsfXljkXkjd","XlkdjsfXljk", "kdjfXXXlkj"), "(.).*\\1.*\\1", match = T)

# Detect matches
x <- c("apple", "banana", "pear")
str_detect(x, "e")
str_detect(x, "e$")
str_detect(x, "^a.*e$")
# How many common words start with t?
sum(str_detect(words, "^t"))
mean(str_detect(words, "[eioua]$"))
# When you have complex logical conditions (e.g., match a or b but not c unless 
# d) it’s often easier to combine multiple str_detect() calls with logical 
# operators, rather than trying to create a single reg‐ ular expression. For 
# example, here are two ways to find all words that don’t contain any vowels
# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aioue]")
sum(no_vowels_1)
words[no_vowels_1]
# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aioue]+$")
sum(no_vowels_2)
words[no_vowels_2]
identical(no_vowels_1, no_vowels_2)
# A common use of str_detect() is to select the elements that match a pattern. 
# You can do this with logical subsetting, or the convenient str_subset() wrapper
words[str_detect(words, "x$")]
str_subset(words, "x$")
# Typically, however, your strings will be one column of a data frame, and 
# you’ll want to use filter instead
df <- tibble(
  wword = words, 
  i = seq_along(words)
)
df
head(df)
df %>% 
  filter(str_detect(wword, "x$"))
# A variation on str_detect() is str_count(): rather than a simple yes or no, 
# it tells you how many matches there are in a string
x <- c("apple", "banana", "pear")
str_count(x, "a")
# On average, how many vowels per word?
mean(str_count(words, "[aioue]"))
df %>% 
  mutate(
    vowels = str_count(wword, "[aioue]"),
    consonants = str_count(wword, "[^aioue]")
  )
# Matches never overlap
str_count("abababa", "aba")
str_view_all("abababa", "aba")
# 1. For each of the following challenges, try solving it by using both a single 
# regular expression, and a combination of multiple str_detect() calls:
# a. Find all words that start or end with x
words[str_detect(words, "^x|x$")]
words[str_detect(words, "^x") | str_detect(words, "x$")]
# b. Find all words that start with a vowel and end with a consonant
words[str_detect(words, "^[aioue].*[^aioue]$")]
words[str_detect(words, "^[aioue]") & str_detect(words, "[^aioue]$")]
# c. Are there any words that contain at least one of each different vowel?
words[str_detect(words, ".*[aioue].*[aioue].*[aioue].*[aioue].*[aioue]")] # how by one regexp?
words[
  str_detect(words, "a") & 
    str_detect(words, "o") &
    str_detect(words, "i") &
    str_detect(words, "u") &
    str_detect(words, "e")
  ]
words[
  str_detect(words, "a") & 
    str_detect(words, "o") &
    str_detect(words, "i") &
    str_detect(words, "u")
  ]
# d. What word has the highest number of vowels? 
df %>% 
  mutate(
    i = seq_along(wword),
    n_vowels = str_count(wword, "[aioue]")
  ) %>%
  arrange(desc(n_vowels))
words[which.max(str_count(words, "[aioue]"))]
# What word has the highest proportion of vowels? (Hint: what is the denominator?)
df %>% 
  mutate(
    i = seq_along(wword),
    n_vowels = str_count(wword, "[aioue]"),
    n_char = str_count(wword, "."),
    vowels_prop = n_vowels / n_char
  ) %>%
  arrange(desc(vowels_prop))
words[which.max( str_count(words, "[aioue]") / str_count(words, ".") )]

# Extract Matches
stringr::sentences
head(sentences)
# Imagine we want to find all sentences that contain a color
colors <- c("red", "orange", "yellow", "green", "blue", "purple")
colors_str <- str_c(colors, collapse = "|")
colors_str
# Now we can select the sentences that contain a color, and then extract the 
# color to figure out which one it is
sentences_with_color <- str_subset(sentences, colors_str)
sentences_with_color
colors_matches <- str_extract(sentences_with_color, colors_str)
head(colors_matches)
# Note that str_extract() only extracts the first match. We can see that most 
# easily by first selecting all the sentences that have more than one match
more <- sentences[str_count(sentences, colors_str) > 1]
str_view_all(more, colors_str)
str_extract(more, colors_str)
str_extract_all(more, colors_str)
str_extract_all(more, colors_str, simplify = T)
x <- c('a', 'a b', 'a b c')
str_extract_all(x, "[a-z]", simplify = T)
# Exercise
# 2. From the Harvard sentences data, extract:
# a. The first word from each sentence
str_extract(sentences, "^[a-zA-Z']+\\s+")
str_extract(sentences, "^[A-Z']{1}[a-z]*\\s")
str_extract(sentences, "^[A-Z']{1}[a-z]*(\\s|')")
as.data.frame(sentences) %>% 
  mutate(first_word = str_extract_all(sentences, "^[A-Z']{1}[a-z]*(\\s|')")) %>%
  head(30)
str_extract(c("I'm very proud of you", 
              "Marina is beatiful girl",
              "TV show golos is popular"), "^[a-zA-Z']+\\s+")
# b. All words ending in ing
library(stringr)
s <- sentences[str_detect(sentences, ".+ing\\s")]
b1 <- (str_detect(sentences, "[A-Z]{1}[a-z]*(ing)\\s"))
sentences[b1]
sum(b1)
b2 <- str_detect(sentences, "\\s[a-z]*(ing)\\s")
sentences[b2]
sum(b2)
b3 <- str_detect(sentences, "\\s[a-z]*(ing)\\.")
sentences[b3]
sum(b3)
b <- str_detect(sentences, "([A-Z]{1}|\\s)[a-z]*(ing)(\\s|\\.)") # universal
sentences[b]
sum(b)
sentences[b1 | b2]
cbind(sentences[b], str_extract(sentences[b], "([A-Z]{1}|\\s)[a-z]*(ing)(\\s|\\.)"))
str_extract(s, "\\s[a-zA-Z]+ing\\s")
# c. All plurals

# Grouped Matches
# imagine we want to extract nouns from the sentences. As a heuristic, 
# we’ll look for any word that comes after “a” or “the”
has_noun <- sentences %>% str_subset(pattern = "(a|the) ([^ ]+)")
head(has_noun)
str_extract(string = has_noun, pattern = "(a|the) ([^ ]+)")
str_match(string = has_noun, pattern = "(a|the) ([^ ]+)")
str_match_all(string = has_noun, pattern = "(a|the) ([^ ]+)")
# Unsurprisingly, our heuristic for detecting nouns is poor, and also picks up adjectives like smooth and parked
# If your data is in a tibble, it’s often easier to use tidyr::extract().
# It works like str_match() but requires you to name the matches, which 
# are then placed in new columns
tibble(s = sentences) %>%
  tidyr::extract(col = s, into = c("article", "noun"), 
                 regex = "(a|the) ([^ ]+)", remove = F)
# 1. Find all words that come after a “number” like “one”, “two”, “three”,etc.
# Pull out both the number and the word.
s <- sentences[str_detect(sentences, pattern = "(one|two|three) ([^ ]+)")]
s
str_extract(string = s, pattern = "(one|two|three) ([^ ]+)")
# 2. Find all contractions. Separate out the pieces before and after the apostrophe.
s <- str_subset(string = sentences, pattern = "([^ ]+)(')([^ ]+)")
s
str_match(string = s, pattern = "([^ ]+)(')([^ ]+)")

# Replacing Matches
x <- c('apple', 'pear', 'banana')
str_replace(string = x, pattern = "[aeoui]", replacement = "-")
str_replace_all(string = x, pattern = "[aeoui]", replacement = "-")
# Instead of replacing with a fixed string you can use backreferences to 
# insert components of the match. In the following code, I flip the order 
# of the second and third words
sentences %>% str_replace(pattern = "([^ ]+) ([^ ]+) ([^ ]+)", 
                          replacement = "\\1 \\3 \\2") %>%
  head(5)
head(sentences,5)
# 1. Replace all forward slashes in a string with backslashes.
str_subset(string = c("https://promusculus.ru/", "https:promusculus.ru"), pattern = "/")
str_detect(string = c("https://promusculus.ru/", "https:promusculus.ru"), pattern = "/")
str_replace(string = c("https://promusculus.ru/", "https:promusculus.ru"),
            pattern = "/", replacement = "\\")
str_replace_all(string = c("https://promusculus.ru/", "https:promusculus.ru"),
                pattern = "/", replacement = '+')

# 2. Implement a simple version of str_to_lower() using replace_all().
sentences %>% str_replace(pattern = 'T', replacement = "t") %>% head(5)
sentences %>% str_replace(pattern = '[A-Z]', replacement = "[a-z]") %>% head(5)

# 3. Switch the first and last letters in words. Which of those strings
# are still words?
str_replace_all(string = 'Rice is often served in round bowls.',
                pattern = " ([a-z]{1})([^ ]*)([a-z]{1}) ",
                replacement = " \\3\\2\\1 ") # doesn't work well...

# Splitting
s <- sentences %>% head(5) %>%
  str_split(" ")
s
s[1]
s[[1]][2]
s %>% .[[1]]
# If you’re working with a length-1 vector, the easiest thing is to just extract the first element of the list
"a|b|c|d" %>%
  str_split(pattern = "\\|") %>%
  .[[1]]
# you can use simplify = TRUE to return a matrix
s <- sentences %>% head(5) %>%
  str_split(" ", simplify = T)
s
s[1,2]
s[2,3]
# You can also request a maximum number of pieces
s <- c("Name: Andrei, Surname: Kristov", "Country: Italy Sicilia", "Age: 35")
s
str_split(string = s, 
          pattern = ": ",
          n = 2,
          simplify = T)
str_split(string = s, 
          pattern = ": ",
          simplify = T)
# Instead of splitting up strings by patterns, you can also split up by 
# character, line, sentence, and word boundary()s
s <- "This is a sentence.  This is another sentence."
str_view_all(s, boundary(type = "word"))
str_split(string = s, pattern = " ")
str_split(string = s, pattern = boundary("word"))
str_split(string = s, pattern = boundary("word"))[[1]]
str_split(string = s, pattern = boundary("word"))[[1]][4]
# 1. Split up a string like "apples, pears, and bananas" into indi‐ vidual components.
s <- "apples, pears, and bananas"
s
str_split(string = s, pattern = "(, | )")
# 2. Why is it better to split up by boundary("word") than " "?
s1 <- "This is a sentence. This is another sentence."
s2 <- "This's a sentence. This is another sentence."
str_split(s1, pattern = " ")
str_split(s2, pattern = " ")
str_split(s1, pattern = boundary("word"))
str_split(s2, pattern = boundary("word"))
# 3. What does splitting with an empty string ("") do? Experiment, and then read the documentation.
s <- "This is a sentence. This is another sentence."
str_split(string = s, pattern = "")

# Find Matches
s <- "This is a sentence.  This is another sentence."
str_locate(string = s, pattern = "is")
str_locate_all(string = s, pattern = "is")
str_locate_all(string = s, pattern = "is")[[1]][,1]
str_sub(string = s, 
        start = str_locate_all(string = s, pattern = "is")[[1]][,1],
        end = str_locate_all(string = s, pattern = "is")[[1]][,2])
str_sub(string = s, 
        start = str_locate_all(string = s, pattern = "is")[[1]][,1],
        end = str_locate_all(string = s, pattern = "is")[[1]][,2]) <- "IS"
s
hw <- "Hadley Wickham"

str_sub(hw, 1, 6)
str_sub(hw, end = 6)
str_sub(hw, 8, 14)
str_sub(hw, 8)
str_sub(hw, c(1, 8), c(6, 14))

# Negative indices
str_sub(hw, -1)
str_sub(hw, -7)
str_sub(hw, end = -7)
# Alternatively, you can pass in a two colum matrix, as in the
# output from str_locate_all
pos <- str_locate_all(hw, "[aeio]")[[1]]
str_sub(hw, pos)
str_sub(hw, pos[, 1], pos[, 2])
# Vectorisation
str_sub(hw, seq_len(str_length(hw)))
str_sub(hw, end = seq_len(str_length(hw)))
# Replacement form
x <- "BBCDEF"
str_sub(x, 1, 1) <- "A"; x
str_sub(x, -1, -1) <- "K"; x
str_sub(x, -2, -2) <- "GHIJ"; x
str_sub(x, 2, -2) <- ""; x
# If you want to keep the original if some argument is NA,
# use omit_na = TRUE
x1 <- x2 <- x3 <- x4 <- "AAA"
str_sub(x1, 1, NA) <- "B"; x1
str_sub(x2, 1, 2) <- NA; x2
str_sub(x3, 1, NA, omit_na = TRUE) <- "B"; x3
str_sub(x4, 1, 2, omit_na = TRUE) <- NA; x4
x1; x2; x3; x4

# Other Types of Pattern
install.packages("microbenchmark")
library(microbenchmark)
microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)

# Check locale
stringi::stri_locale_info()

# 2. What are the five most common words in sentences?
library(stringr)
m <- str_extract_all(string = sentences, 
                     pattern = boundary("word"),
                     simplify = T)
dim(m)
v <- matrix(m, nrow = 720 * 12, ncol = 1)
sort(table(v), decreasing = T)

# Other Uses of Regular Expressions
# There are two useful functions in base R that also use regular expressions:
# 1. apropos() searches all objects available from the global environment. 
# This is useful if you can’t quite remember the name of the function:
apropos(what = "replace")
apropos(what = "^replace")
apropos(what = "replace$")
apropos(what = "^[^ ]{4}replace")
apropos(what = "str[^ ]*replace")
# dir() lists all the files in a directory. The pattern argument takes a 
# regular expression and only returns filenames that match the pattern
getwd()
dir(pattern = ".*")
dir(path = ".", pattern = ".*")
dir(path = "/Users/Andrew/Desktop/Data Science", pattern = "\\.R")

# stringi
?stringi
library(stringi)
apropos(what = "^stri_")
?stri_sort()
stri_sort(c("hladny", "chladny"), locale="pl_PL")
stri_sort(c("hladny", "chladny"), locale="sk_SK")
















