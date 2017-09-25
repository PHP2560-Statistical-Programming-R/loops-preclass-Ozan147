# Sept 27 Preclass
Ozan Adiguzel  




### Question 1:

Using a loop, print the integers from 1 to 50. 


```r
for(i in 1:50) {
  print(i)
  
}
```

```
## [1] 1
## [1] 2
## [1] 3
## [1] 4
## [1] 5
## [1] 6
## [1] 7
## [1] 8
## [1] 9
## [1] 10
## [1] 11
## [1] 12
## [1] 13
## [1] 14
## [1] 15
## [1] 16
## [1] 17
## [1] 18
## [1] 19
## [1] 20
## [1] 21
## [1] 22
## [1] 23
## [1] 24
## [1] 25
## [1] 26
## [1] 27
## [1] 28
## [1] 29
## [1] 30
## [1] 31
## [1] 32
## [1] 33
## [1] 34
## [1] 35
## [1] 36
## [1] 37
## [1] 38
## [1] 39
## [1] 40
## [1] 41
## [1] 42
## [1] 43
## [1] 44
## [1] 45
## [1] 46
## [1] 47
## [1] 48
## [1] 49
## [1] 50
```

### Question 2:

A.  Using a loop, add all the integers between 0 and 1000.

  - I assume the question includes 0 and 1000. Otherwise, the sequence can be adjusted.
  

```r
aggregate.result.A <- 0
for(i in 0:1000) {
  aggregate.result.A <- aggregate.result.A + i
}
aggregate.result.A
```

```
## [1] 500500
```
B. Now, add all the EVEN integers between 0 and 1000 (hint: use seq())

  - I assume the question includes 0 and 1000. Otherwise, the sequence can be adjusted.
  

```r
aggregate.result.B <- 0
for(i in seq(0, 1000, 2)) {
  aggregate.result.B <- aggregate.result.B + i
}
aggregate.result.B
```

```
## [1] 250500
```
C. Now, repeat A and B WITHOUT using a loop.

  - One way to solve this problem would be using built-in sum function 
  

```r
sum(1:1000)
```

```
## [1] 500500
```

```r
sum(seq(0, 1000, 2))
```

```
## [1] 250500
```
  
  - Another way is to write a generic function that adds all the numbers in a sequence by taking the increment of the sequence in account. In other words, this function merges the built-in sum and seq functions.
  

```r
sequence_sum <- function(first, last, by=1) {
  # mathematical formula to add the numbers of a sequence. (I could also use stopifnot function to check for negative values instead of absolute values below)
  return((first + last) * (abs(last - first) / abs(by) + 1) / 2)
}

sequence_sum(0, 1000)
```

```
## [1] 500500
```

```r
sequence_sum(0, 1000, 2)
```

```
## [1] 250500
```

### Question 3:

Here is a dataframe of survey data containing 5 questions :


```r
survey <- data.frame(
                     "participant" = c(1, 2, 3, 4, 5, 6),
                     "q1" = c(5, 3, 2, 7, 11, 0),
                     "q2" = c(4, 2, 2, 5, -10, 99),
                     "q3" = c(-4, -3, 4, 2, 9, 10),
                     "q4" = c(-30, 5, 2, 23, 4, 2),
                     "q5" = c(88, 4, -20, 2, 4, 2)
                     )
```
The response to each question should be an integer between 1 and 5. Obviously, we have some bad values in the dataframe. The goal of this problem is to fix them.

A. Using a loop, create a new dataframe called survey.clean where all the invalid values (those that are not integers between 1 and 5) are set to NA.


```r
# copy survey into survey.clean so that I can work on survey.clean without losing survey 
survey.clean <- survey
# loop from 2nd to last columns since first one includes the participants
for(j in 2:ncol(survey.clean)) {
  # copy the column into a vector to check for invalid values 
  y <- survey.clean[, j]
  # change all the invalid values with NA in the vector
  y[(y %in% 1:5) != T] <- NA
  # replace the vector with the corresponding columns in the dataframe
  survey.clean[, j] <- y
}
survey.clean
```

```
##   participant q1 q2 q3 q4 q5
## 1           1  5  4 NA NA NA
## 2           2  3  2 NA  5  4
## 3           3  2  2  4  2 NA
## 4           4 NA  5  2 NA  2
## 5           5 NA NA NA  4  4
## 6           6 NA NA NA  2  2
```
B. Now, again using a loop, add a new column to the dataframe called “invalid.answers” that indicates, for each participant, how many bad answers they gave.


```r
# create the new column and assign temporary 0 values to every row
survey.clean$invalid.answers <- 0
# loop through rows since we are interested in total number of NAs in each row
for(i in 1:nrow(survey.clean)) {
  # copy the row into a vector to check for NAs
  x <- survey.clean[i, ]
  # check if the values are NA, count the total number of NAs and assign this sum to corresponding rows of the new columns
  survey.clean$invalid.answers[i] <- sum(is.na(x))
}
survey.clean
```

```
##   participant q1 q2 q3 q4 q5 invalid.answers
## 1           1  5  4 NA NA NA               3
## 2           2  3  2 NA  5  4               1
## 3           3  2  2  4  2 NA               1
## 4           4 NA  5  2 NA  2               2
## 5           5 NA NA NA  4  4               3
## 6           6 NA NA NA  2  2               3
```
