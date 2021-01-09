#Load packages
library(dplyr)
library(readr)
library(stringr)
library(sf)
library(h3forr)
library(googlesheets4)
#--------------------------------------------------------------
#Read data
addtocart <- read.table('AddToCart.csv', header=TRUE, sep=',', stringsAsFactors=FALSE, allowEscapes=TRUE, quote="\"")
removecart <- read.table('DropCart.csv', header=TRUE, sep=',', stringsAsFactors=FALSE, allowEscapes=TRUE, quote="\"")
creorder <- read.table('Buy.csv', header=TRUE, sep=',', stringsAsFactors=FALSE, allowEscapes=TRUE, quote="\"")
#--------------------------------------------------------------
#Read map
peru <- st_read("PEdistritos.kml") %>% st_transform(32718)
#--------------------------------------------------------------
#Sf and H3 resolution 8
delpoint <- addtocart %>%
            st_as_sf(coords=c("delivery_longitude", "delivery_latitude"), crs=4326, na.fail=FALSE) %>%
            st_transform(32718)
delpoint <- delpoint %>% mutate(h3_8=geo_to_h3(delpoint, res=8)) #No proyectar!!!
##Delivery district and H3 resolution 8
addtocart <- st_join(delpoint, peru, join=st_intersects) %>%
             st_drop_geometry() %>%
             select(date:product_quantity, "delivery_district"="Name", h3_8) %>%
             mutate(delivery_district = str_to_title(delivery_district))
#Sf and H3 resolution 8
dellocat <- creorder %>%
            st_as_sf(coords=c("delivery_longitude", "delivery_latitude"), crs=4326, na.fail=FALSE) %>%
            st_transform(32718)
dellocat <- dellocat %>% mutate(h3_8=geo_to_h3(dellocat, res=8))
##Delivery district and H3 resolution 8
creorder <- st_join(dellocat, peru, join = st_intersects) %>%
            st_drop_geometry() %>%
            select(date:final_status, "delivery_district"="Name", h3_8) %>%
            mutate(delivery_district=str_to_title(delivery_district))
#--------------------------------------------------------------
#Transform individual tables (to avoid error summarising)
session <-  addtocart %>%
            filter(!is.na(store_name) & !is.na(product_quantity) & !is.na(delivery_district)) %>%
            select(-event_id,-product_quantity) %>%
            distinct(.keep_all=TRUE) %>%
            group_by(store_address_id, session_id, customer_id, product_id)
addprod <-  addtocart %>% #IGUAL
            filter(product_quantity>0) %>%
            distinct(.keep_all=TRUE) %>%
            group_by(store_address_id, session_id, customer_id, product_id) %>%
            mutate(product_price=min(product_price, na.rm=TRUE)) %>% #delete duplicate prices
            ungroup() %>%
            group_by(store_address_id, session_id, customer_id, product_id, delivery_district, h3_8) %>%
            summarise(add_product=sum(as.numeric(product_quantity), na.rm=TRUE)) #%>% ungroup() %>% summarise(sessions = n_distinct(session_id), customers = n_distinct(customer_id),add_product = sum(as.numeric(add_product), na.rm = TRUE))
dropprod <- removecart %>% #IGUAL
            filter(product_quantity>0) %>%
            distinct(.keep_all=TRUE) %>%
            group_by(store_address_id, session_id, customer_id, product_id) %>%
            summarise(drop_product=sum(as.numeric(product_quantity), na.rm=TRUE)) #%>% ungroup() %>% summarise(sessions = n_distinct(session_id), customers = n_distinct(customer_id),drop_product = sum(as.numeric(drop_product), na.rm = TRUE))
creatord <- creorder %>% #IGUAL
            group_by(store_address_id, session_id, customer_id, product_id, order_id, final_status, delivery_district, h3_8) %>%
            summarise(bought_product=sum(as.numeric(product_quantity), na.rm=TRUE)) %>% #%>% ungroup() %>% summarise(sessions = n_distinct(session_id), customers = n_distinct(customer_id),bought_product = sum(as.numeric(bought_product), na.rm = TRUE))
            ungroup() %>%
            group_by(store_address_id, session_id, customer_id, product_id)
sessiord <- creorder %>%
            select(order_id, date:store_id, store_name, product_id:price) %>%
            distinct(.keep_all=TRUE) %>%
            group_by(order_id, product_id)
#Transform (Usar left_join y full_join) (ningun otro mas)
funnprod <- addprod %>% #filter(customer_id=='41000661' & store_address_id=='11102') %>% #aca hay algo mal
            ungroup() %>%
            group_by(store_address_id, session_id, customer_id, product_id) %>%
            left_join(dropprod, by=c("store_address_id"="store_address_id", "session_id"="session_id", "customer_id"="customer_id", "product_id"="product_id")) %>%
            full_join(creatord, by=c("store_address_id"="store_address_id", "session_id"="session_id", "customer_id"="customer_id", "product_id"="product_id")) %>%
            ungroup() %>% #
            group_by(session_id, store_address_id, customer_id, product_id) %>% #as.data.frame()
            rename("delivery_district"="delivery_district.x", "h3_8"="h3_8.x") %>%
            mutate(delivery_district=case_when(is.na(delivery_district) ~ delivery_district.y, TRUE ~ delivery_district),
                   h3_8=case_when(is.na(h3_8) ~ h3_8.y, TRUE ~ h3_8)
                   ) %>% 
            select(-delivery_district.y, -h3_8.y) %>% #filter(customer_id=='41000661' & store_address_id=='11102') %>% as.data.frame()
            full_join(session, by=c("store_address_id"="store_address_id", "session_id"="session_id", "customer_id"="customer_id", "product_id"="product_id")) %>%
            ungroup() %>%
            rename("delivery_district"="delivery_district.x", "h3_8"="h3_8.x") %>%
            mutate(delivery_district=case_when(is.na(delivery_district) ~ delivery_district.y, TRUE ~ delivery_district),
                   h3_8=case_when(is.na(h3_8) ~ h3_8.y, TRUE ~ h3_8)
                   ) %>%
            select(-delivery_district.y, -h3_8.y) %>% #filter(order_id =='133311316') %>% as.data.frame() Bien!!
            select(date:store_id,store_address_id,store_name,customer_id,session_id,product_id,product_name:product_price,add_product:drop_product,bought_product,order_id,final_status,delivery_district,h3_8) #%>% as.data.frame()
#funnprod %>% filter(customer_id=='41000661') %>% as.data.frame()
#Orders
funnords <- funnprod %>%
            filter(is.na(date)) %>%
            select(store_address_id,customer_id:product_id,bought_product:h3_8) %>%
            group_by(order_id, product_id) %>%
            left_join(sessiord, by=c("order_id"="order_id", "product_id"="product_id")) %>%
            ungroup() %>%
            mutate(add_product=0, drop_product=0) %>%
            select(date:store_id,store_address_id,store_name,customer_id:product_id,product_name,add_product,drop_product,bought_product,price,order_id:h3_8) %>%
            rename("country_code"="country","city_code"="city","product_price"="price")
#Join
funfinal <- rbind(funnprod %>% filter(!is.na(date)), funnords)
#Clean data
funfinal <- funfinal %>%
            group_by(order_id,product_id) %>%
            #Drop products
            mutate(drop_product=case_when(drop_product > add_product ~ add_product-bought_product, 
                                          TRUE ~ as.numeric(drop_product)
                                          )
                   ) %>%
            mutate(drop_product=case_when(drop_product < 0 ~ 0, 
                                          is.na(drop_product) ~ 0,
                                          TRUE ~ as.numeric(drop_product)
                                          )
                   ) %>%
            #Add products
            mutate(add_product=case_when(add_product < bought_product-drop_product ~ bought_product+drop_product, 
                                         TRUE ~ as.numeric(add_product)
                                         )
                   ) %>%
            mutate(add_product=case_when(add_product < bought_product+drop_product ~ bought_product+drop_product, 
                                         TRUE ~ as.numeric(add_product)
                                         )
                   ) %>%
            #Cart values
            mutate(add_value=add_product*product_price,
                   drop_value=drop_product*product_price,
                   bought_value=bought_product*product_price
                   ) %>%
            #Filter null customer ids and dates
            filter(!is.na(customer_id) & !is.na(date)) %>%
            ungroup()
#--------------------------------------------------------------
#Descriptions
funfinal <- funfinal %>%
            mutate(product_name=str_to_title(product_name)) %>%
            mutate(product_name=str_squish(product_name))
#--------------------------------------------------------------
#Brands
brands <- read.table('C:/Users/Gerardo Flores/Documents/RGroceries/RProductsClassification/brandsgroceries.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE) %>% 
          rename("brand"="ï..brand") %>%
          mutate(brand=str_to_title(brand)#, 
                 #provider=str_to_title(provider)
                 )
pattern <- paste(paste0(paste0("\\b",brands %>% distinct(brand) %>% pull()),"\\b"), collapse = '|')
funfinal <- funfinal %>%
            mutate(product_brand=as.character(str_match(product_name, pattern))) %>%
            #left_join(brands %>% select(brand, "product_provider"="provider"), by = c("product_brand"="brand")) %>%
            select(date:product_name,product_brand,everything())
#--------------------------------------------------------------
#Provider
#--------------------------------------------------------------
#Export CSV
write_csv(funfinal, 'C:/Users/Gerardo Flores/Documents/RMkt/RFunnelInt/funfinal.csv')
