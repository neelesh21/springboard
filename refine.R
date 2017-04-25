

library(dplyr)

library(tidyr)





#factor column

my.data <- read.csv('refine.csv', stringsAsFactors=FALSE)


# Clean up brand names

#clean company names and factor column
my.data <- my.data %>% 
  mutate(company=ifelse(grepl("^phil|^fil|^phl", company, ignore.case=TRUE), "phillips", company)) %>%
  mutate(company=ifelse(grepl("^ak", company, ignore.case=TRUE), "akzo", company)) %>%
  mutate(company=ifelse(grepl("^van", company, ignore.case=TRUE), "van_houten", company)) %>%
  mutate(company=ifelse(grepl("^Uni", company, ignore.case=TRUE), "unilever", company)) %>%
  mutate(company = factor(company))





my.data <- my.data %>% separate(Product.code...number,c("Product_code","Product_number"),sep = "-")

#Add product categories

my.data <- my.data %>% mutate(product_category = recode(Product_code,
                                                        p = 'Smartphone',
                                                        v = 'TV',
                                                        x = 'Laptop',
                                                        q = 'Tablet'))


#address
my.data <- my.data %>% unite(full_address,address,city,country, sep = ", ")

#dummy variables for company and product category

my.data$company_vanhouten <- as.numeric(my.data$company %in% "van houten")

my.data$company_unilever <- as.numeric(my.data$company %in% "unilever")

my.data$company_philips <- as.numeric(my.data$company %in% "philips")

my.data$company_akzo <- as.numeric(my.data$company %in% "akzo")



#Add four binary (1 or 0) columns for product category- product_smartphone, product_tv, product_laptop and product_tablet

my.data$product_smartphone <- as.numeric(my.data$product_category %in% "Smartphone")

my.data$product_tv <- as.numeric(my.data$product_category %in% "TV")


my.data$product_laptop <- as.numeric(my.data$product_category %in% "Laptop")

my.data$product_tablet <- as.numeric(my.data$product_category %in% "Tablet")



