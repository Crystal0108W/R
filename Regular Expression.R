temp <- strsplit(movie_metadata$genres[1], "\\|")
temp <- as.data.frame(temp)
colnames(temp) <- c("Gener")
temp2 <- strsplit(movie_metadata$genres[2], "\\|")
temp2 <- as.data.frame(temp2)
colnames(temp2) <- c("Gener")
gener.df <- rbind(temp, temp2)

GenreSplit <- strsplit(movie_metadata$genres, "\\|")
GenreSplit <- as.matrix(GenreSplit)
str(GenreSplit)
GenreSplit[1,2]

typeof(movie_metadata$genres[1])

(strings <- c("^ab", "ab", "abc", "abd", "abe", "ab 12", "ab bc"))
grep("ab.", strings, value = TRUE)
grep("ab[c-e]", strings, value = TRUE)
grep("ab[^c]", strings, value = TRUE)
grep("^ab", strings, value = TRUE)



# Escape Sequences
# \': single quote. You don’t need to escape single quote inside a double-quoted string, so we can also use "'" in the previous example.
# \": double quote. Similarly, double quotes can be used inside a single-quoted string, i.e. '"'.
# \n: newline.
# \r: carriage return.
# \t: tab character.
# \\: backslash \


# QUantifier: 
# *: matches at least 0 times.
# +: matches at least 1 times.
# ?: matches at most 1 times.
# {n}: matches exactly n times.
# {n,}: matches at least n times.
# {n,m}: matches between n and m times.


# Position of pattern within the string
# ^: matches the start of the string.
# $: matches the end of the string.
# \b: matches the empty string at either edge of a word. Don’t confuse it with ^ $ which marks the edge of a string.
# \B: matches the empty string provided it is not at an edge of a word.

# Operators 
# .: matches any single character
# [...]: a character list, matches any one of the characters inside the square brackets. We can also use inside the brackets to specify a range of characters.
# [^...]: an inverted character list, similar to [...], but matches any characters except those inside the square brackets.
# \: suppress the special meaning of metacharacters in regular expression, i.e.  $ * + . ? [ ] ^ { } | ( ) \, similar to its usage in escape sequences. Since \ itself needs to be escaped in R, we need to escape these metacharacters with double backslash like \\$.
# |: an “or” operator, matches patterns on either side of the |.
# (...): grouping in regular expressions. This allows you to retrieve the bits that matched various parts of your regular expression so you can alter them or use them for building up a new string. Each group can than be refer using \\N, with N being the No. of (...) used. This is called backreference.

?gsub
txt <- c("arm","foot","lefroo", "bafoobar")
gsub("([ab])", "\\1~\\1-", "abc and ab and ABC")
grep("([ab])", c("abc", "and", "ab", "and", "ABC"), value = TRUE)
gsub(pattern = "\\|", replacement = " ", movie_metadata$genres[1])


# Character classes


