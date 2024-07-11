library(rvest)
library(tidyverse)
library(dplyr)

file_path <- "smart-watches.txt"
product_urls <- readLines(file_path)

len <- length(product_urls)

#Declaring variable vectors under consideration 

#Titles <- Name of the product
#Reviews <- No. of reviews registered
#Ratings <- Rating of the product
#Sale_Price <- Price of the Product with discount
#Regular_Price <- Original price of the product without discount

#Country_Of_Origin <- Country in which the product is made

#Charging_Time <- Time required to charge the product

#Charging_Duration <- length of the time that the battery works


Titles <- character(length = len)
Reviews <- character(length = len)
Ratings <- character(length = len)
Sale_Price <- character(length = len)
Regular_Price <- character(length = len)

#Discount percentage
Offer <- character(length = len)
No_Of_Colors <- integer(length = len)

Warranty <- character(length = len)


#Country of origin
Country_Of_Origin <- character(length = len)

#Charging Time
Charging_Time <- character(length = len)

#Charging Duration
Charging_Duration <- character(length = len)

#Scrapping information from each product page
for(i in 1:len){
  
  print(i)
  new_html <- read_html(product_urls[i])
  
  Titles[i] <- (new_html %>% html_elements(".product-meta__title.heading.h3")%>%html_text())[1]
  
  Reviews[i] <- new_html %>% html_element(".rating__caption")%>%html_text()
  Ratings[i] <- (new_html %>% html_elements(".rating__stars")%>%html_text())[1]
  
  Sale_Price[i] <- new_html %>% html_elements(".price.price--highlight.price--large")%>%html_text()
  Regular_Price[i] <- new_html %>% html_elements(".price.price--compare.line-through")%>%html_text()
  
  #Discount percentage
  Offer[i] <- new_html %>% html_elements(".custom-saved-price") %>% html_text()
  
  No_Of_Colors[i] <- length(new_html %>% html_elements(".color-swatch__item") %>% html_text())
  Warranty[i] <- new_html %>% html_element(".mb-0.promis_icon_text")%>% html_text()
  
  tables = html_nodes(new_html , "table")
  if( length(tables) > 0 )
  {
    a <- (new_html %>% html_table())[[1]]
    
    Country_Of_Origin[i] <- a[a[1:dim(a)[1],1] == "Country Of Origin" , 2]
    
    Charging_Time[i] <- a[a[1:dim(a)[1],1] == "Charging Time" , 2]
    
    Charging_Duration[i] <- a[a[1:dim(a)[1],1] == "Working Time" , 2]
    
  }
  
}


Country_Of_Origin <- sapply(1:len,function(k) Country_Of_Origin[[k]][1])
Charging_Time <- sapply(1:len,function(k) Charging_Time[[k]][1])
Charging_Duration <- sapply(1:len,function(k) Charging_Duration[[k]][1])


##DO CLEANING HERE
#Data Frame of the Products
Watches <- data.frame(ProductName = Titles,
                      ProductRating = Ratings,
                      ReviewCount =  Reviews,
                      Sale_Price,
                      Regular_Price,
                      Discount_percentage = Offer,
                      ColourOptions = No_Of_Colors,
                      Warranty = Warranty,
                      Country_Of_Origin,
                      Charging_Time,
                      Charging_Duration)


#########################################################################################################
#########################################################################################################


#Data cleaning 
dims = dim(Watches)
n = dims[1] 

#Making Warranty in year numeric variable
Watches$Warranty = as.numeric(substring(Watches$Warranty , 1, 2))

#Making Discount_percentage  numeric variable
foo = Watches$Discount_percentage 

foo = strsplit(foo , "%")

temp = as.numeric(sapply(1:n , function(k) substring(foo[[k]][1] , nchar(foo[[k]][1])-1 ,nchar(foo[[k]][1]))))

Watches$Discount_percentage = temp

Watches$Discount_percentage

#Making Regular_Price numeric variable  

foo = Watches$Regular_Price

foo = strsplit(foo , "₹")

temp =sapply(1:n , function(k) substring(foo[[k]][2] , 1 ,nchar(foo[[k]][2])-3))

temp = as.numeric(gsub("," , "" , temp))

Watches$Regular_Price = temp

#Making Sale_Price numeric variable

foo = Watches$Sale_Price

foo = strsplit(foo , "₹")

temp =sapply(1:n , function(k) substring(foo[[k]][2] , 1 ,nchar(foo[[k]][2])))

temp = as.numeric(gsub("," , "" , temp))

Watches$Sale_Price = temp 


#Making ReviewCount numeric variable
foo = Watches$ReviewCount

foo[is.na(foo)] = 0

foo = strsplit( foo , " ")

temp =sapply(1:n , function(k) substring(foo[[k]][1] , 1 ,nchar(foo[[k]][1])))

temp = as.numeric(temp)

Watches$ReviewCount = temp

#MAking ProductRating numeric variable 
foo = Watches$ProductRating

foo = strsplit(foo , "\n")

temp =sapply(1:n , function(k) substring(foo[[k]][3] ,  nchar(foo[[k]][3])-2 ,nchar(foo[[k]][3])))

temp = as.numeric(temp)

temp[is.na(temp)]=0 

Watches$ProductRating = temp

#Making Product Names clean
foo = Watches$ProductName

foo = strsplit(foo , "\n") 

temp =sapply(1:n , function(k) substring(foo[[k]][2] ,   1, nchar(foo[[k]][2])))

excpt = c(94 , 76,60 , 56,40,14,11,9,5)
for( i in 1:n)
{
  if( sum(i==excpt)==1)
  {
    temp[i] = substring(temp[i] , 13)
  }
  else
  {
    temp[i] = substring(temp[i] , 14)
  }
  
}

Watches$ProductName = temp


#New column for category
Watches$Category = "Wireless"


#New column to specify that they are watches 
Watches$Type = "SmartWatches"


#Cleaning Country_Of_Origin 
temp <- Watches$Country_Of_Origin
temp[is.na(temp)] <- ""
temp -> Watches$Country_Of_Origin



#Changing Charging_Time to numeric(in hrs)
temp <- Watches$Charging_Time
temp[is.na(temp)] <- ""

for(i in 1:len){
  a <- nchar(temp[i])
  for(j in 1:a){
    if((substring(temp[i],1,1) >='1') & (substring(temp[i],1,1)<='9')){
      break
    }
    else{
      temp[i] <- substring(temp[i],2)
    }
  }
  if(i==3 ||i==34 || i==39 ||i==42 ||i==48 ||i==70 ||i==81 ||i==93||i==105){
    temp[i] <- "0.5"
  }
}
temp <- sapply(1:len,function(k) strsplit(temp[k]," ")[[1]][1])
temp[is.na(temp)] <- ""
temp <- as.numeric(temp)
Watches$Charging_Time <- temp


#Changing Charging_Duration to numeric(in hrs)
temp <- Watches$Charging_Duration 
temp[is.na(temp)] <- ""


for(i in 1:len){
  a <- nchar(temp[i])
  for(j in 1:a){
    if((substring(temp[i],1,1) >='1') & (substring(temp[i],1,1)<='9')){
      break
    }
    else{
      temp[i] <- substring(temp[i],2)
    }
  }
}

temp <- sapply(1:len,function(k) strsplit(temp[k]," ")[[1]][1])
temp <- sapply(1:len,function(k) strsplit(temp[k],"-")[[1]][1])
temp[is.na(temp)] <- ""
temp <- as.numeric(temp)
temp -> Watches$Charging_Duration


#Cleaned data frame 
Watches 
save(Watches, file = "watches.Rdata")