## Lets start of reading out test and train files into R

```
library(xlsx)
train <- read.xlsx('Train_UWu5bXk.xlsx',sheetIndex = 1)
test <- read.xlsx('Test_u94Q5KV.xlsx', sheetIndex = 1)
```

## We’ll be performing some basic data exploration here and come up with some inferences about the data. 
## We’ll try to figure out some irregularities and address them in the next section. 
## We will combine the test and train in order to perform our feature engineering  efficiently and later divide them again.

```
library(xlsx)
temp <- data.frame(Item_Outlet_Sales=rep("None",nrow(test)),test[,])
full_data <- rbind(train,temp)
```
## Lets perform some basic exploratory analysis of full_data
```
str(full_data)
```
'data.frame':	14204 obs. of  12 variables:
     $ Item_Identifier          : Factor w/ 1559 levels "DRA12","DRA24",..: 157 9 663 1122 1298 759 697 739 441 991 ...
     $ Item_Weight              : num  9.3 5.92 17.5 19.2 8.93 ...
     $ Item_Fat_Content         : Factor w/ 5 levels "LF","low fat",..: 3 5 3 5 3 5 5 3 5 5 ...
     $ Item_Visibility          : num  0.016 0.0193 0.0168 0 0 ...
     $ Item_Type                : Factor w/ 16 levels "Baking Goods",..: 5 15 11 7 10 1 14 14 6 6 ...
     $ Item_MRP                 : num  249.8 48.3 141.6 182.1 53.9 ...
     $ Outlet_Identifier        : Factor w/ 10 levels "OUT010","OUT013",..: 10 4 10 1 2 4 2 6 8 3 ...
     $ Outlet_Establishment_Year: num  1999 2009 1999 1998 1987 ...
     $ Outlet_Size              : Factor w/ 3 levels "High","Medium",..: 2 2 2 NA 1 2 1 2 NA NA ...
     $ Outlet_Location_Type     : Factor w/ 3 levels "Tier 1","Tier 2",..: 1 3 1 3 3 3 3 3 2 2 ...
     $ Outlet_Type              : Factor w/ 4 levels "Grocery Store",..: 2 3 2 1 2 3 2 4 2 2 ...
     $ Item_Outlet_Sales        : chr  "3735.138" "443.4228" "2097.27" "732.38" ...
```
summary(full_data)
```
Item_Identifier  Item_Weight     Item_Fat_Content Item_Visibility                   Item_Type       Item_MRP     
 DRA24  :   10   Min.   : 4.555   LF     : 522     Min.   :0.00000   Fruits and Vegetables:2013   Min.   : 31.29  
 DRA59  :   10   1st Qu.: 8.710   low fat: 178     1st Qu.:0.02704   Snack Foods          :1989   1st Qu.: 94.01  
 DRB25  :   10   Median :12.600   Low Fat:8485     Median :0.05402   Household            :1548   Median :142.25  
 DRC25  :   10   Mean   :12.793   reg    : 195     Mean   :0.06595   Frozen Foods         :1426   Mean   :141.00  
 DRC27  :   10   3rd Qu.:16.750   Regular:4824     3rd Qu.:0.09404   Dairy                :1136   3rd Qu.:185.86  
 DRC36  :   10   Max.   :21.350                    Max.   :0.32839   Baking Goods         :1086   Max.   :266.89  
 (Other):14144   NA's   :2439                                        (Other)              :5006                   
 Outlet_Identifier Outlet_Establishment_Year Outlet_Size   Outlet_Location_Type            Outlet_Type   Item_Outlet_Sales 
 OUT027 :1559      Min.   :1985              High  :1553   Tier 1:3980          Grocery Store    :1805   Length:14204      
 OUT013 :1553      1st Qu.:1987              Medium:4655   Tier 2:4641          Supermarket Type1:9294   Class :character  
 OUT035 :1550      Median :1999              Small :3980   Tier 3:5583          Supermarket Type2:1546   Mode  :character  
 OUT046 :1550      Mean   :1998              NA's  :4016                        Supermarket Type3:1559                     
 OUT049 :1550      3rd Qu.:2004                                                                                            
 OUT045 :1548      Max.   :2009                                                                                            
 (Other):4894
 
## Some useful observations
##   1) Item_Visibility has a min value of zero. This makes no practical sense because when a product is being sold in a store,
##      the visibility cannot be 0.
##   2) Outlet_Establishment_Years vary from 1985 to 2009. The values might not be apt in this form. Rather, 
##      if we can convert them to how old the particular store is, it should have a better impact on sales.
##   3) The lower ‘count’ of Item_Weight and Item_Outlet_Sales confirms the findings from the missing value check
## Lets check for missing values in the data 
```
colnames(full_data)[colSums(is.na(full_data)) > 0]
```
"Item_Weight" "Outlet_Size"

## Thus we have two columns with missing values, we will impute the missing data in data cleaning section.
## also some of the columns are factor,num and char.
## Now lets look at the unique values present in each of the categorical columns
```
unique_values <- apply(full_data, 2, function(x)length(unique(x)))
```
              Item_Identifier               Item_Weight          Item_Fat_Content           Item_Visibility 
                     1559                       416                         5                     13006 
                Item_Type                  Item_MRP         Outlet_Identifier Outlet_Establishment_Year 
                       16                      8052                        10                         9 
              Outlet_Size      Outlet_Location_Type               Outlet_Type         Item_Outlet_Sales 
                        4                         3                         4                      3494 
                        
## This tells us that there are 1559 products and 10 outlets/stores.Another thing that should catch attention is that Item_Type has 16 unique values.
## Let’s explore further using the frequency of different categories in each nominal variable. I’ll exclude the ID and source variables for obvious reasons
```
var1 <- sapply(full_data , is.factor)
cat_var <- full_data[var1]
cat_var <- subset(cat_var, select = - Item_Identifier)
unique_values1 <- apply(cat_var, 2, unique)
unique_values1
```
$Item_Fat_Content
[1] "Low Fat" "Regular" "low fat" "LF"      "reg"    

$Item_Type
 [1] "Dairy"                 "Soft Drinks"           "Meat"                  "Fruits and Vegetables"
 [5] "Household"             "Baking Goods"          "Snack Foods"           "Frozen Foods"         
 [9] "Breakfast"             "Health and Hygiene"    "Hard Drinks"           "Canned"               
[13] "Breads"                "Starchy Foods"         "Others"                "Seafood"              

$Outlet_Identifier
 [1] "OUT049" "OUT018" "OUT010" "OUT013" "OUT027" "OUT045" "OUT017" "OUT046" "OUT035" "OUT019"

$Outlet_Size
[1] "Medium" NA       "High"   "Small" 

$Outlet_Location_Type
[1] "Tier 1" "Tier 3" "Tier 2"

$Outlet_Type
[1] "Supermarket Type1" "Supermarket Type2" "Grocery Store"     "Supermarket Type3"

