library(hausekeep)

MADtest <- function(oi, oe){
  # ratio of bonds
  ratio <- oe / oi

  # calculating the median & median absolute deviation (MAD)
  const = 1/quantile(ratio, probs=0.75)

  median <- median(ratio)
  mad <- mad(ratio, center=median, constant=const)

  deviation <- (ratio - median) / mad

  print("-----")
  # define outlier bounds
  lowBound = median - 3*mad
  upBound = median + 3*mad

  # identify outliers & reject them
  outliers <- outliers_mad(ratio)

  print("-----")
  # print(outliers)
  result <- ifelse(is.na(outliers), "Reject", "Keep")
  return(result)
}

oe <- c(1, 2, 4, 4, 2, 3, 1, 2, 3, 1, 4, 4, 2, 3, 0, 4, 5, 2, 2, 2, 4, 4, 28, 7, 3, 0, 4, 2, 4, 3, 5, 1, 11, 1, 5, 11, 2, 2, 3, 3, 1, 1, 0, 1, 2, 4, 1, 0, 8, 0, 0, 1, 1, 2, 0, 2, 3, 0, 1, 0, 0, 0, 0, 0, 2, 0, 1, 0, 1, 1, 2, 0, 4, 2)

oi <- c(49, 48, 44, 46, 47, 47, 49, 48, 46, 47, 53, 46, 55, 45, 50, 45, 50, 45, 45, 48, 47, 48, 44, 45, 21, 43, 47, 50, 45, 48, 43, 41, 45, 48, 38, 48, 80, 125, 48, 47, 47, 41, 46, 54, 50, 47, 44, 45, 49, 48, 41, 45, 49, 48, 46, 46, 50, 48, 47, 136, 47, 49, 49, 48, 50, 50, 44, 48, 49, 50, 37, 39, 47, 130, 47)

# oe <- c(1, 2, 4, 4, 2, 3, 1, 2, 3, 1, 4, 4, 2, 3, 0, 4, 5, 2, 2, 2, 4, 4, 28, 7, 3, 0, 4, 2, 4, 3, 5, 1, 11, 1, 2, 2, 3, 3, 1, 1, 0, 1, 2, 4, 1, 0, 8, 0, 0, 1, 1, 2, 0, 2, 3, 1, 0, 0, 0, 0, 0, 2, 0, 1, 0, 1, 1, 2, 0, 2)

# oi <- c(49, 48, 44, 46, 47, 47, 49, 48, 46, 47, 53, 46, 55, 45, 50, 45, 50, 45, 45, 48, 47, 48, 44, 45, 21, 43, 47, 50, 45, 48, 43, 41, 45, 48, 38, 48, 48, 47, 47, 41, 46, 54, 50, 47, 44, 45, 49, 48, 41, 45, 49, 48, 46, 46, 50, 48, 47, 47, 49, 49, 48, 50, 50, 44, 48, 49, 50, 37, 39, 47, 47)
result <- MADtest(oi, oe)
print(result)

ratio = oe/oi
cleaned = outliers_mad(ratio, show_mad_values=TRUE)
print(cleaned)

# boxplot(ratio, main='raw values')
# boxplot(cleaned, main='cleaned data')