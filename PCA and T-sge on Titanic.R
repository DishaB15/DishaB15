library(dplyr)

train = read.csv("/Users/dishabhatnagar/Desktop/Titanic/train.csv")

str(train)
summary(train)
## check for missing values and impute them.
train$Age[is.na(train$Age)] = mean(train$Age, na.rm=TRUE)

## Embarked is imputed with the mode value since its categorical
train$Embarked[train$Embarked == ""] <- "S"
X <- train[1:200,]
## changing 0(Not Survived) from survived to 2 
index <- X$Survived == 0
X$Survived[index] <- 2
y<- X$Survived[1:200]
## Find out the categorical and numerical columns in the dataset.
set.seed(1)
## Converting categorical to dummy variables
dummy <- model.matrix( ~ Sex - 1, data = X)
dummy2 <- model.matrix( ~ Embarked - 1, data = X)
## Cabin and Ticket are alphanumeric hence removing them. Name doesn't have any 
## relation with the response 'Survived' so removing that as well
df <- subset(X, select = -c(Ticket, Cabin,Survived ,Name))
dummy2 <- subset(dummy2, select = -c(Embarked))
df$female <- dummy[, c("Sexfemale")]
df$male <- dummy[, c("Sexmale")]
df$Embarked_C <- dummy2[, c("EmbarkedC")]
df$Embarked_Q <- dummy2[, c("EmbarkedQ")]
df$Embarked_S <- dummy2[, c("EmbarkedS")]
df <- subset(df, select = -c(Sex,Embarked))
df[is.na(df)] <- 0
df <- df[, which(colSums(df) != 0)]
scale_df <- scale(df)

set.seed(823)
## T-SNE Algorithm
SNE <- Rtsne::Rtsne(
  X = scale_df
)

plot(
  SNE$Y,
  col = y,
  pch = as.character(y),
  main = "Scatterplot of Titanic T-SNE two dimensions"
) 

## PCA Algorithm
scale_df %>% prcomp(center = TRUE ,scale. = FALSE) -> fit

fit$x %>% as.data.frame() %>%
  select(PC1, PC2) %>% 
  ggplot() +
  geom_point(aes(x = PC1, y = PC2))


prcomp_titanic <- prcomp(
  x = scale_df,
  center = TRUE,
  scale. = TRUE,
  rank = 2
)

plot(
  prcomp_titanic$x[,1:2],
  col = y,
  pch = as.character(y),
  main = "Scatterplot of PCA two dimensions"
)

##  Non negative matrix factorization
library(NMF)
library(tm)
M <- sapply(df, as.numeric )
x <- rmatrix(df$Pclass, df$Age)

res <- nmf(x = as.matrix(x), rank = 2)

basis_acq <- NMF::basis(res)
coef_acq <- NMF:: coef(res)

dim(basis_acq)
dim(coef_acq)

colnames(basis_acq) <- c("col_1", "col_2")
rownames(coef_acq) <- c("row_1", "row_2")
round(head(basis_acq),3)
round(head(coef_acq[,1:12]),3)
