##Cleaning
##Importing Amazon Data(KAGGLE)

library(dplyr)

df <- readxl::read_xlsx("AmazonEarbuds_Kaggle.xlsx")

###Columns Required : Discount, Amazon_Price, ProductRating, ProductName, ReviewCount, MRP, Date, BatteryCount

df <- df[, c(1:5, 8, 10, 28)]

#tail(df)

###dropping rows with MRP == 0 or Rating == 0

df <- df %>%
  filter(complete.cases(.))

#View(df)

###Renaming dataFrame :

df <- df %>%
  rename(
    Discount_percentage = Discount,
    ProductRating = Ratings,
    ReviewCount = ratings,
    AiredDate = Date_First_Available,
    `MRP(₹)` = MRP
  )


###Dropping rows for which BatteryCount[1] is not a number

df <- df %>%
  filter(grepl("Lithium", Batteries))


### fURTHER cLEANING

df$Discount_percentage <- as.numeric(substring(df$Discount_percentage, 2, 3))

df$Amazon_Price <- as.numeric(gsub(",","", df$Amazon_Price))

df$ProductRating <- as.numeric(substring(df$ProductRating, 1, 3))

temp <- strsplit(df$ReviewCount, " ")
temp <- sapply(temp, function(x) x[1])
df$ReviewCount <- as.numeric(gsub(",", "", temp)) 

df$`MRP(₹)` <- as.numeric(gsub(",", "", substring(df$`MRP(₹)`, 2, )))


df$Batteries <- as.numeric(substring(df$Batteries, 2, 2))

# dim(df)

rm(temp)# Removing temporary data

###You may now export the dataset
