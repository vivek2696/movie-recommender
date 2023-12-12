---
title: "Movie Recommender Project"
date: "12/11/2023"
author: "Vivek Patel"
output:
  html_document:
    toc: yes
    df_print: paged
    keep_md: true
  html_notebook:
    theme: readable
    toc: yes
    toc_float: yes
  pdf_document:
    toc: yes
---


```r
# Libraries
library(coop)
```


```r
set.seed(7612)
```

# Main System: Recommendation Based on IBCF

### Step 1: Normalizing R matrix


```r
file_path = 'Rmat.csv'
R = read.csv(file_path)

# row means
row_means = as.vector(rowMeans(R, na.rm = TRUE))

# normalize it
R_norm = R - row_means
```


### Step 2: Creation of S (cosine similarities) matrix


```r
cos = coop::cosine(R_norm, use = "pairwise.complete.obs")
S = 1/2 + cos/2
diag(S) = NA

# Creating mask matrix
A = as.matrix(R_norm)
A[!is.na(A)] = 1
A[is.na(A)] = 0
B = t(A) %*% A

# replace values to NA for users less than 3 for two movies
for(i in 1:nrow(S)){
  not_significant = as.vector(unname(which(B[i, ] < 3)))
  S[i, not_significant] = NA
}
```


#### TEST: some of the movies cosine similarities


```r
col_test = c("m1", "m10", "m100", "m1510", "m260", "m3212")
S[col_test, col_test]
```

```
##              m1       m10      m100 m1510      m260 m3212
## m1           NA 0.5121055 0.3919999    NA 0.7411482    NA
## m10   0.5121055        NA 0.5474583    NA 0.5343338    NA
## m100  0.3919999 0.5474583        NA    NA 0.3296943    NA
## m1510        NA        NA        NA    NA        NA    NA
## m260  0.7411482 0.5343338 0.3296943    NA        NA    NA
## m3212        NA        NA        NA    NA        NA    NA
```

### Step 3: Saving the matrix online to github (manually performed)



```r
S_online = matrix(NA, nrow = nrow(S), ncol = ncol(S))
colnames(S_online) = colnames(S)
rownames(S_online) = rownames(S)

for (i in 1:nrow(S)) {
  row = S[i, ]
  top_indices = order(row, decreasing = TRUE)[1:30]
  S_online[i, top_indices] = row[top_indices]
}
```


```r
csv_file = "./S.csv"
write.csv(S_online, file = csv_file)
```


### Step 4: Custom IBCF function from scratch


```r
myIBCF = function(newuser){
  w = newuser
  wi_non_na = which(!is.na(w))
  wi_na = which(is.na(w))
  wi_pred = w
  
  # Download the S matrix from server
  S_downloaded = read.csv("https://raw.githubusercontent.com/Vivek2696/movie-recommender/main/S.csv", row.names = 1)
  S_online = as.matrix(S_downloaded)
  
  for(i in 1:length(wi_na)){
    current_pred_index = wi_na[i]
    Si_non_na = as.vector(which(!is.na(S_online[current_pred_index, ])))
    intersected_non_na = intersect(wi_non_na, Si_non_na)
    
    if(length(intersected_non_na) > 0){
      numerator = sum(as.vector(S_online[current_pred_index, intersected_non_na]) * w[intersected_non_na])
      denominator = sum(as.vector(S_online[current_pred_index, intersected_non_na]))
      prediction = numerator / denominator
      wi_pred[current_pred_index] = prediction
    }
  }
  
  # removing the user rated movies from the predictions
  wi_pred[wi_non_na] = NA
  movies_indices_pred = order(wi_pred, decreasing = TRUE)
  return(movies_indices_pred)
  
}
```


```r
#w = as.vector(R_norm["u1351",])
u1181_test = as.vector(unlist(R["u1181",]))
u1181_result = myIBCF(u1181_test)
```



```r
u1351_test = as.vector(unlist(R["u1351",]))
u1351_result = myIBCF(u1351_test)
```


```r
hypo_user = R[FALSE,]
hypo_user[1,] = NA
hypo_user[1,"m1613"] = 5
hypo_user[1,"m1755"] = 4
hypo_user_test = as.vector(unlist(hypo_user))
hypo_user_result = myIBCF(hypo_user_test)
```

#### Test results for u1181:

```r
colnames(R[1, u1181_result])[1:10]
```

```
##  [1] "m3732" "m749"  "m3899" "m1039" "m1235" "m1253" "m1734" "m1914" "m2082"
## [10] "m2361"
```

#### Test results for u1351:

```r
colnames(R[1, u1351_result])[1:10]
```

```
##  [1] "m1102" "m1234" "m1514" "m1532" "m1659" "m1780" "m1871" "m1877" "m1901"
## [10] "m2000"
```

#### Test results for hypothetical user:

```r
colnames(R[1, hypo_user_result])[1:10]
```

```
##  [1] "m2836" "m338"  "m3466" "m1017" "m1100" "m1468" "m1541" "m158"  "m1688"
## [10] "m1752"
```


In case of fewer than 10 prediction that are non-NA then we can use following method:

- We will determine all the movies score by adding all 30-NN 
- Then we remove the ones that are rated by user 
- Finally we pick the top 10 from the rated movies


```r
edge_case = function(newuser){
  # get the S matrix from server
  # S_online = read.csv(...)
  
  S_summed = rowSums(S_online, na.rm = TRUE)
  user_rated = newuser[!is.na(newuser)]
  movies_picked = order(S_summed[-user_rated], decreasing = TRUE)[1:10]
  
  return(movies_picked)
}
```


