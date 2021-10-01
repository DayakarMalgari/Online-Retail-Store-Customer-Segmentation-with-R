########################################################################################### 
#  Data Science with R: E-commerce Project
#
#  By Dayakar Malgari
#
###########################################################################################
#  Business Scenario: 
###########################################################################################
#  A UK-based online retail store has captured the sales data for different products 
#  for the period of one year 
#  (Nov 2016 to Dec 2017). The organization sells gifts primarily on the online platform. 
#  The customers who make a purchase consume directly for themselves. There are small 
#  businesses that buy IN bulk and sell to other customers through the retail outlet channel. 
#  The organization wants to roll out an offer to the high-value customers after 
#  identification of segments.
#############################################################################################
#  Expectation /Goals: 
#############################################################################################
#  1.To find the significant customers FOR the business who make high purchases of their 
#    favourite products. 
#  2.To identify right customer segments by using clustering methodologies to segment 
#    customers into groups. 
#  3.Use clustering algorithms such as K means and Hierarchical. 
#  4.Identify the clustering algorithm that gives maximum accuracy and explains robust 
#    clusters. 
#  5.To find the number of customers who are highly valued. 
#  
#  In addition to the above goals, I'm also going to perform the below analysis 
#  
#  1.Country wise analysis - to find top sales by countries
#  2.Product wise analysis - to find top selling products
#  3.Month wise analysis - to find out which months are having top sales 
#  4.Market Basket Analysis - to analyse which products are likely to be sold
#    with other products
#  5.Create Association rules using Apriori FUNCTION and visualize rules using graphs
#    by confidence value, lift value, and top selling product
#  
############################################################################################  
#  Analysis: 
############################################################################################    
#  1.Analysed the dataset by using SUMMARY, STR, HEAD  and other R functions
#  2.Removed an unwanted column which had all NA's
#  3.Performed data cleaning by removing 135080 rows that had NA's in the CustomerID column  
#  4.Used mutate function to format InvoiceDate field and factored Description, InvoiceNo, 
#    and Country fields, also added numeric labels for Country column to be used in Kmeans 
#  5.Added new column TotalSales and populated Quantity * Units value
#  6.Used functions such as mutate, group_by, summerise, arrange etc to perform Customer 
#    Analysis and found CustomerID 14646, 18102 and 17450 are top 3 customers
#  7.Extracted 97 customers who are highly valued and made purchases of at least 10000 
#    dollars during that period
#  8.Extracted 1632 customers as significant customers who made at least 1000 dollar purchases 
#    of their favourite products 
#  9.Performed KMEANS,HIRARCHIAL and AGGLOMERATIVE clustering and found 5 segments as the right 
#    number of segments. Visualized the segments with multiple graphs. 
#  10.HIRARCHIAL Clustering gave better/evenly spread segmentation compared to KMEANS 
#  11.Performed Country wise sales analysis and found UK has the highest sales followed by Netherlands,
#     EIRE , Germany and France. Visualized the total sales figures with and without UK
#  12.Performed Product wise analysis to find out which products are earning the highest revenue- 
#     Found out REGENCY CAKESTAND 3 TIER, WHITE HANGING HEART T-LIGHT HOLDER, JUMBO BAG RED RETROSPOT 
#     are the top3 most valued products and visualized the results using graphs 
#  13.Performed Product wise analysis also to find out which products are selling highest quantity 
#     wise - Found out WORLD WAR 2 GLIDERS ASSTD DESIGNS, JUMBO BAG RED RETROSPOT, ASSORTED COLOUR 
#     BIRD ORNAMENT as the top 3 products sold highest units during that period, and visualized the 
#     results using graphs
#  14.Performed month wise analysis to find out which months are earning highest revenue- Found out 
#     September thru December as the highest earning months 
#  15.Performed Market Basket Analysis to find which products likely to be sold with which products - 
#     most frequent items:
#     WHITE HANGING HEART T-LIGHT HOLDER     REGENCY CAKESTAND 3 TIER             JUMBO BAG RED RETROSPOT 
#     1683                               		 1445                              		1420 
#     LUNCH BAG RED RETROSPOT                PARTY BUNTING                        (Other) 
#     1206                              		 1162                             		392974
#     Visualized the results using itemFrequencyPlot
#  16.Created Association rules using Apriori function and visualized top 10 rules using graphs with 
#     criteria as - by decreasing Confidence value, by decreasing lift value and by one most frequently 
#     sold product- WORLD WAR 2 GLIDERS ASSTD DESIGNS.
#  
#######################################################################################################
#  Code and Output Screenshots:
#######################################################################################################
# Read the retail data file which is given as project 2_dataset.csv 
######################################################################################################

retail_data <- 
    read.csv("C:/Users/Dayakar Malgari/LEARN/RDOCS/project 2_dataset.csv",header = T)
summary(retail_data)

# lets veiw data structure and variables in our dataset 

str(retail_data)

# creating a fresh copy of the data to work on so that the imported original 
# data is intact and can be reverted back easily if required.
# and also removing the last column that has all NA's as seen above 

retail <- retail_data[-9]

head(retail)
tail(retail)
str(retail)
summary(retail)
################################################################################
# Data cleaning
################################################################################
# Check the dataset to find out which column has missing value and how many 
# missing value are present corresponding to each column
# We ignore the entire row(ie observation), if any column has a missing value

 retail <- retail[complete.cases(retail$CustomerID), ]

# Check whether all the missing values have been eliminated by summing the 
# missing values of each column separately. we should  get zeros for all columns


str(retail)

################################################################################
# Data manipulation
################################################################################
# Note that InvoiceDate is in <chr>, Country and Description are also in <chr>
# need to change InvoiceDate to <dmy> 
# need to change InvoiceID, Country and Description as factor for proper analysi
# Idea is to replace the columns after transforming the data-type of each column
# keeping their values fixed.
# need to change the country column to factor and adding labels with numeric 
# values so that we don't have problems in KMeans clustering algoritham
# coerces Description as a factor with each item as individual level of a factor
# coerces InvoiceDate in a Date(dmy) format
library(plyr)
library(dplyr)
library(lubridate)


retail_cleaned <- retail   %>%
  mutate(InvoiceDate = dmy(InvoiceDate)) %>% 
  mutate(Description = factor(Description, levels = unique(Description))) %>% 
  mutate(Country = factor(Country, levels = unique(Country), 
                          labels = c(1:37))) %>%
  mutate(InvoiceNo = factor(InvoiceNo, levels = unique(InvoiceNo))) %>%
  mutate(TotalPrice = Quantity * UnitPrice)


head(retail_cleaned)

################################################################################
# Customer Analysis 
################################################################################
# Arrange the CustomerId based on their total sales and group by CustomerID and 
# country, and find out which Customer has # maximum sales. Group the data 
# cust/country wise then find the frequency of sales per customer/country using 
# the summary function step3 : Arrange the customers by descending sales amount
################################################################################

customers_mostsales <- retail_cleaned %>% 
  group_by(CustomerID, Country) %>%
  summarize(customers_totalSales = sum(TotalPrice)) %>%
  arrange(desc(customers_totalSales))

# find the top 10 customers per sales

by_top10_customers <- customers_mostsales %>% 
  top_n(n = 10, wt = customers_totalSales)

by_top10_customers

################################################################################
##### sgnificant sales customers extraction------------------------------------
################################################################################

customers_mostsales <- filter(customers_mostsales, 
                              customers_totalSales > 1000 )

count(customers_mostsales)

################################################################################
##### High valued customers extraction------------------------------------------
################################################################################

customers_mostsales <- filter(customers_mostsales, 
                              customers_totalSales > 10000 )

count(customers_mostsales)


rawt <- customers_mostsales[,c(1,3)]

tail(rawt)

################################################################################
##### kmeans clustering of customers---------------------------
################################################################################
################################################################################
# code to find the optimum number of clusters based on withinSS
################################################################################
wss<-(nrow(customers_mostsales[,3])-1)*sum(apply(customers_mostsales[,3],2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(customers_mostsales[,3],centers=i)$withinss)
  
}
plot(1:15, wss, type='b', xlab='Number of Clusters',col='red',cex=1,
     ylab='Within groups sum of squares')
################################################################################
# KMeans Clustering by gradually increasing the groups from 2 to 5
################################################################################
set.seed(1)

grpcustomers <- kmeans(rawt, centers=2, nstart=100)
table(grpcustomers$cluster)

grpcustomers <- kmeans(rawt, centers=3, nstart=100)
table(grpcustomers$cluster)

grpcustomers <- kmeans(rawt, centers=4, nstart=100)
table(grpcustomers$cluster)

grpcustomers <- kmeans(rawt, centers=5, nstart=100)


str(grpcustomers)

table(grpcustomers$cluster)

################################################################################
# Visualization of customer KMEANS clusters in various graphs
################################################################################

# plot 1
library(factoextra)
fviz_cluster(grpcustomers,data=rawt)

# plot 2
Clusters <- grpcustomers$cluster
plot(Clusters,xlab = "Customers",ylab = 'Clusters',col='red',
     main="KMEANS Cluster chart")

# plot 3
library(mclust)

fit <- Mclust(customers_mostsales)
plot(fit)

# plot 4
library(ggplot2)
ggplot(data = rawt,aes(x=customers_totalSales,
                       y=Clusters,
                       col=factor(Clusters),
                       size=factor(Clusters))) + geom_point() + 
                       ggtitle('Kmeans Clustering') +
                       coord_flip()

################################################################################
# Adding the cluster_KMeans number as a column to the customer details file
################################################################################
customers_mostsales  <- cbind(customers_mostsales, Cluster_KMeans = Clusters)
head(customers_mostsales)
################################################################################
# Cluster profiling for KMeans
################################################################################
custprofile = aggregate(customers_mostsales[,3],
                        list(customers_mostsales$Cluster_KMeans),FUN="mean")
custprofile
################################################################################
#### HIRARCHCAL CLUSTERING -----------------------------------------------------
################################################################################

# Calculating Euclidean distance
#--------------------------------

cluster_h <- dist(customers_mostsales, method='euclidean', diag=FALSE,
                  upper=FALSE)
fit <- hclust(cluster_h, method='ward.D')
H_groups <- cutree(fit, k=5)

table(H_groups)

################################################################################
# Visualization of customer HIRARCHIAL clusters in various graphs
################################################################################

# plot 1
plot(fit,xlab = "Customer Clusters",
     main = 'Hirarchial Dendogram Cluster Chart')
rect.hclust(fit, k=5, border = 'red')

# plot 2

ggplot(data = rawt,aes(x=customers_totalSales,
                       y=H_groups,
                       col=factor(H_groups),
                       size=factor(H_groups))) + geom_point() + 
                       ggtitle('Hirarchical Clustering chart') +
                       coord_flip()

################################################################################
# Adding the cluster_Hira number as a column to the customer details file
################################################################################
customers_mostsales  <- cbind(customers_mostsales, Cluster_Hira = H_groups)
head(customers_mostsales)
################################################################################
# Cluster profiling for Hrarchical clustering
################################################################################
custprofile = aggregate(customers_mostsales[,3],
                        list(customers_mostsales$Cluster_Hira),FUN="mean")
custprofile
################################################################################
# Agglomerative Hirarchical clustering
################################################################################

library(cluster)
grpagnes <-agnes(rawt)
a_groups <- cutree(grpagnes, k=5)

summary(a_groups)
table(a_groups)
################################################################################
# Visualization of Agglomerative HIRARCHIAL clusters in various graphs
################################################################################

# plot 1
plot(a_groups,xlab = "Customers",ylab = "Clusters",col='red',pch=17,
     main = 'Hirarchial Agglomerative Cluster Chart')

# plot 2
ggplot(data = rawt,aes(x=customers_totalSales,
                       y=a_groups,
                       col=factor(a_groups),
                       size=factor(a_groups))) + geom_point() + 
                       ggtitle('AGGLOMERATIVE Clustering') +
                       coord_flip()
################################################################################
# Adding the cluster_Agglo number as a column to the customer details file
################################################################################
customers_mostsales  <- cbind(customers_mostsales, Cluster_Agglo = a_groups)
head(customers_mostsales)
################################################################################
# Cluster profiling for Agglomerative
################################################################################
custprofile = aggregate(customers_mostsales[,3],
                        list(customers_mostsales$Cluster_Agglo),FUN="mean")
custprofile
################################################################################
#---------------country wise details--------------------------------------------
################################################################################
# Arrange the countries based on their total sales? and which country has
# maximum sales? Group the data countrywise then find the frequency of sales per
# country using the summary function step3 : Arrange the countries by descending
# sales. 
################################################################################
library(forcats)
retail_cleaned <- retail   %>%
  mutate(InvoiceDate = dmy(InvoiceDate)) %>% 
  mutate(Description = factor(Description, levels = unique(Description))) %>% 
  mutate(Country = factor(Country, levels = unique(Country))) %>%
  mutate(InvoiceNo = factor(InvoiceNo, levels = unique(InvoiceNo))) %>%
  mutate(TotalPrice = Quantity * UnitPrice)


country_mostsales <- retail_cleaned %>% 
  group_by(Country) %>%
  summarize(countrywise_totalrevenue = sum(TotalPrice))%>%
  arrange(desc(countrywise_totalrevenue))


by_top10_countries <- country_mostsales %>% 
  top_n(n = 10, wt = countrywise_totalrevenue)

by_top10_countries

# Visualize the top 10 countries as per total sales. scatterplot of country
# versus sales.Country has no natural ordering,so we use fact-reorder() to
# display the conutries as per increasing sales. In other words, The scatterplot
# is arranged so that the country having minimum sales is plotted first and then
# the country having second lowest sales and so on till the final country which
# has maximum sales.

by_top10_countries %>% 
  mutate(Country = fct_reorder(Country, countrywise_totalrevenue)) %>% 
  ggplot(aes(Country, countrywise_totalrevenue,col=factor(Country)))+
  geom_point()+
  ggtitle('Top 10 Country wise sales chart') +
  coord_flip()

# So UK has the highest sale followed by Netherlands, EIRE , Germany and France


# However, UK is the home country of the firm and that explains why this country
# has large amount of sales. Hence lets check the trend by separating UK. We
# visualize how sales are distributed over various countries.

country_mostsales_without_uk <-  country_mostsales[-1,]

country_mostsales_without_uk %>% 
  mutate(Country = fct_reorder(Country, countrywise_totalrevenue)) %>% 
  ggplot(aes(Country, countrywise_totalrevenue))+
  geom_point()+
  ggtitle('Country wise sales chart without UK ') +
  coord_flip()

################################################################################
# Product wise analysis
################################################################################
# Most valued product-The product briging largest turnover
################################################################################


mostvalued_product <- retail_cleaned %>% 
  group_by(Description) %>%
  summarize(value_of_product = sum(TotalPrice)) %>% 
  arrange(desc(value_of_product)) 

# top10 most valued product

top10_valued_products <- mostvalued_product %>% 
  top_n(n = 10, wt = value_of_product)

top10_valued_products

# Categorical variable like Description does not have an intrinsic order, so we
# reorder it as per increasing count.

# graphical representation of most valued product

top10_valued_products %>% 
  mutate(Description = fct_reorder(Description, value_of_product)) %>% 
  ggplot(aes(Description, value_of_product))+
  geom_point()+
  ggtitle('Top 10 Products sold revenue wise chart ') +
  coord_flip()

# So REGENCY CAKESTAND 3 TIER, WHITE HANGING HEART T-LIGHT HOLDER, JUMBO BAG RED
# RETROSPOT are the top3 most valued products


# which product is most sold worldwide?
# -----------------------------------------

mostsold_product <- retail_cleaned %>% 
  group_by(Description) %>%
  summarize(total_units_sold = sum(Quantity)) %>% 
  arrange(desc(total_units_sold)) 

# top10 most sold products--
top10_mostsoldproducts <- mostsold_product %>% 
  top_n(n = 10, wt = total_units_sold)


top10_mostsoldproducts %>% 
  mutate(Description = fct_reorder(Description, total_units_sold)) %>% 
  ggplot(aes(Description, total_units_sold))+
  geom_point()+
  ggtitle('Top 10 Products sold quantity wise chart ') +
  coord_flip()

# Most sold products worldwide are WORLD WAR 2 GLIDERS ASSTD DESIGNS, JUMBO BAG
# RED RETROSPOT, ASSORTED COLOUR BIRD ORNAMENT

################################################################################
# Month wise analysis 
################################################################################
# Which month of the year sees maximum turnover?
################################################################################

data_with_month <- retail_cleaned %>%
  mutate(months = month(InvoiceDate,label = T))

# the month() function separates out the month name from a date(dmy)
# column. We create a separate column name months with the month names of the
# sales data.

data_with_month %>% 
  group_by(months)%>%
  summarize(monthwise_totalrevenue = sum(TotalPrice))%>%
  ggplot()+
  geom_bar(mapping =  aes(x = months, y = monthwise_totalrevenue), 
           stat = "identity")+
  ggtitle('Monthly wise revenue chart ') +
  coord_flip()

# September to December appear to be the months with highest sales.This is not
# surprising as these months are winter months for Europian countries where most
# sales occur and winter time is festive time.

################################################################################
# MARKET BASKET ANALYSIS
################################################################################
# Lets perform a marketbasket analysis to analyse which product is likely to be
# sold with which product
################################################################################

transaction_df <-  select(retail_cleaned, 'InvoiceNo', 'Description')

# How many unique levels of InvoiceNo and Description of the product are there
str(transaction_df)
transaction_df[1:10, ]

# creating a itemList from the Description column of the data.
# for each InvoiceNo, description of all the products brought together are 
# written together

itemList <- plyr :: ddply(transaction_df, c("InvoiceNo"), 
                      function(transaction_df)paste(transaction_df$Description, 
                                                        collapse = ","))

itemList[1:3, ]
# ddply will Split data frame, apply function, and return result in a data frame.

# deleting the InvoiceNO from the itemList data as this is not required anymore
itemList$InvoiceNo <- NULL

#Write out the itemlist per transaction in a csv file
write.csv(itemList,'market_basket_tr.csv', row.names = FALSE)

# Read the csv in 'basket' format
# rm.duplicates removes duplicate items in a particular transaction.
library(arules)
library(arulesViz)



transaction <- read.transactions('market_basket_tr.csv', 
                           format = 'basket', quote = "", cols = NULL, sep=',', 
                           skip = 1, rm.duplicates = T)
transaction
summary(transaction)

################################################################################
# Visualize using itemFrequencyPlot
################################################################################
# Make a frequency plot of the transactions with a support of 0.05 or greater.
# This shows the the most popular gift items sold.
################################################################################

itemFrequencyPlot(transaction, support = .05, col = rainbow(6),
                  main='Item Frequency Chart')


################################################################################
# Creating Association rules using Apriori function
################################################################################
# create association rules with a minimum support value ,where support indicates 
# appearance of commodity A and B together out of total transactions of allitems
################################################################################

rules <- apriori(transaction, parameter = list(supp = 0.01, 
                                               conf = 0.5, minlen = 2))
summary(rules)
options(digits=2)
top10rules <-rules[1:10]
inspect(top10rules)

################################################################################
# Visualize the rules 
################################################################################

plot(top10rules, method = "graph")


# if A => B is the rule, confidence shows the proportion of transactions having 
# both A and B, out of total transactions having A.

# sort the rules by decreasing confidence and show top 10 rules

rules_by_confidence <- sort(rules, by ='confidence', decreasing = TRUE)
summary(rules_by_confidence)
toprules_by_confidence <- rules_by_confidence[1:10]
options(digits=2)
inspect(toprules_by_confidence)

################################################################################
# Visualize the top 10 rules by confidence value
################################################################################

plot(toprules_by_confidence, method="graph",shading = NA)

# Lift is the factor by which, the co-occurence of A and B exceeds the expected
# probability of A and B co-occuring, had they been independent. So, higher the
# lift, higher the chance of A and B occurring together.
# sort the rules by decreasing lift and show top 10 rules

rules_by_lift <- sort(rules, by='lift', decreasing = TRUE)
summary(rules_by_lift)
toprules_by_lift <- rules_by_lift[1:10]
options(digits=2)
inspect(toprules_by_lift)
################################################################################
# Visualize the top 10 rules by lift value
################################################################################

#plot(toprules_by_lift, method="graph",shading = NA,engine = 'interactive')
plot(toprules_by_lift, method="graph",shading = NA)

################################################################################
# rules associated with one particualr popular product 
# Since WORLD WAR 2 GLIDERS ASSTD DESIGNS  is the most popular item, we are
# interested in the items bought with it.
################################################################################

rules_lhs_wW2GAD <-apriori(data=transaction, parameter=list(supp=0.001,
                                                        conf = 0.1, minlen = 2), 
                   appearance = list(default="rhs",
                                     lhs="WORLD WAR 2 GLIDERS ASSTD DESIGNS"),
                   control = list(verbose=F))
summary(rules_lhs_wW2GAD)
rules_lhs_wW2GAD <- sort(rules_lhs_wW2GAD, decreasing=TRUE,by="confidence")
inspect(rules_lhs_wW2GAD)
gifts_with_wW2GAD <- rules_lhs_wW2GAD[1:10]

################################################################################
# Visualize the top 10 rules by confidence value
################################################################################

plot(gifts_with_wW2GAD, method="graph",shading = NA)



