# Section 1
ndf = list() # create a list of dataframes
listcsv = dir(pattern = "*.csv") # create the list of all csv files in wd

for (i in 1:length(listcsv)){
  ndf[[i]] = read.csv(listcsv[i])   # read in data
  var = c("id", "sale", "units", "rating", "product", "industry", 
          "country", "return.client")   # subset by 8 var
  ndf[[i]] = ndf[[i]][, var]
  for (k in 2014:2016){
    ndf[[i]] $ year = paste(k)    # add a new "year" variable
  }
  data_full = do.call(rbind, ndf)     # combine datasets
}

# Section 2:
str(data_full)
# As it is shown below: we are having 1673 observations of 9 variables
# for variable "product", "industry" and "country": their types are factor;
# "product" variable has 7 levels of data; "industry" has 4; 
# and "country" has 17.

# Section 3:
# 1.
levels(data_full$country)
data_full$country = as.character(data_full$country)
data_full$country[data_full$country == "Switzerland, Switzerland"] = "Switzerland"

# 2.
data_full$country[!(data_full$country %in% c("Switzerland", "United States", 
                                             "United Kingdom"))] = "other"
data_full$country = as.factor(data_full$country)
levels(data_full$country)

# 3.
levels(data_full$product)
data_full$product = as.character(data_full$product)
data_full$product[data_full$product == "Series A13" 
                  | data_full$product == "Series A2"] = "Series A" 
data_full$product[data_full$product == "Series B55"] = "Series B"
data_full$product = as.factor(data_full$product)
levels(data_full$product)

# 4.
levels(data_full$industry)
data_full$industry = as.character(data_full$industry)
data_full$industry[data_full$industry == "999"] = NA
data_full$industry = as.factor(data_full$industry)
levels(data_full$industry)

# Section 4
# 1.
for (i in names(data_full)){
  na_sum = sum(is.na(data_full[,i]))
  output = paste(i,": ",na_sum,sep="")
  print(output)
}

# for (i in data_full){
#   print(sum(is.na(i)))
# }

# 2. 
data_full$product[is.na(data_full$product)] = "Delta"

# 3.
newdata_full = na.omit(data_full)
str(newdata_full)
# We have 1626 observations remaining

# Section 5
# 1. 
newdata_full$sale.per.unit = newdata_full$sale / newdata_full$units

# 2. 
newdata_full$rating.level[newdata_full$rating > 4 
                          && newdata_full$rating < 5] = "satisfactory"
newdata_full$rating.level[newdata_full$rating >= 5] = "excellent"
newdata_full$rating.level[newdata_full$rating <= 4] = "poor"

# 3.
newdata_full$priority = 0
newdata_full$priority[newdata_full$return.client == 1 
                      & newdata_full$rating.level == "poor"] = 1

# Section 6
write.csv(newdata_full, "dang_minh.csv", row.names = FALSE)

