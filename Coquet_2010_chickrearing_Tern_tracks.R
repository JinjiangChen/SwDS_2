library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(tidytext)

# Read Excel files and merge
thrift_data_1 <- read_excel("data_thrift1.xlsx", sheet=1)
thrift_data_2 <- read_excel("data_thrift2.xlsx", sheet=1)
names(thrift_data_1)[8] <- "Did the item sell?"# rename column name so both are the same
names(thrift_data_2)[8] <- "Did the item sell?"

thrift_data <- rbind(thrift_data_1, thrift_data_2)


# Items Sold
thrift_data$`Item Sold` <- gsub(",", "", thrift_data$`Item Sold`) # remove commas
thrift_data$`Item Sold` <- as.numeric(word(thrift_data$`Item Sold`, 1)) # only keep the first word


# Price
thrift_data$Price <- gsub("��", "£", thrift_data$Price) # replace �� with £ so they're the same as the others
thrift_data$Price <- gsub(",", "", thrift_data$Price) # remove commas
thrift_data$Price <- gsub("£", " ", thrift_data$Price) # replace the £ with a space (to separate prices when there are 2 prices)
thrift_data$Price <- as.double(word(thrift_data$Price, 2)) # the price is the second word (since the 1st is a space) 

View(thrift_data)

for (i in 2:nrow(thrift_data)) {
  if (is.na(thrift_data$`Search Term`[i])){
    thrift_data$`Search Term`[i] = thrift_data$`Search Term`[i-1]
  }else{
    thrift_data$`Search Term`[i] = thrift_data$`Search Term`[i]
  }
    
}


for (i in 2:nrow(thrift_data)) {
  if (is.na(thrift_data$`System Mean Price`[i])){
    thrift_data$`System Mean Price`[i] = thrift_data$`System Mean Price`[i-1]
  }else{
    thrift_data$`System Mean Price`[i] = thrift_data$`System Mean Price`[i]
  }
  
}


thrift_data$Category <- gsub("Selected category", "", thrift_data$Category)
thrift_data$Category <- gsub("Clothing", "Clothes", thrift_data$Category)
thrift_data$Category <- gsub(" Equipment", "", thrift_data$Category)
thrift_data$Category <- gsub("Books & Magazines", "Books, Comics & Magazines", thrift_data$Category)
thrift_data$Category <- gsub("Mobile Phones, Smart Watches, Accessories & Communication", "Mobile Phones & Communication", thrift_data$Category)
thrift_data$Category <- gsub("Industrial Supplies", "Industrial", thrift_data$Category)
thrift_data$Category <- gsub("Photography", "Photo", thrift_data$Category)
thrift_data$Category <- gsub("Dolls & Bears", "Dolls & Teddy Bears", thrift_data$Category)
thrift_data$Category <- gsub("Baby Essentials", "Baby", thrift_data$Category)
thrift_data$Category <- gsub("Jewellery", "Jewelry", thrift_data$Category)
thrift_data$Category <- gsub('Computers/Tablets & Networking', "Computers, Tablets & Network Hardware", thrift_data$Category)
thrift_data$Category <- gsub('Collectables', "Collectibles", thrift_data$Category)
thrift_data$Category <- gsub('Movies & TV', "Films & TV", thrift_data$Category)
thrift_data$Category <- gsub('Coins, Banknotes & Bullion', "Collectibles", thrift_data$Category)
thrift_data$Category <- gsub('Sports Memorabilia', "Collectibles", thrift_data$Category)
thrift_data$Category <- gsub('Stamps', "Collectibles", thrift_data$Category)
thrift_data$Category[is.na(thrift_data$Category)] <- "Unknown"




colSums(is.na(thrift_data))
which(is.na(thrift_data[, 7]))


thrift = thrift_data
thrift$'Item Sold'[is.na(thrift$'Item Sold')] = 0

names(thrift)[1] <- 'Search_Term'
names(thrift)[7] <- 'Item_Sold'
active = which(is.na(thrift$`Did the item sell?`))
active_list = thrift[active,]
sold_list = thrift[-active,]
sold = sold_list %>% 
  group_by(Search_Term) %>% 
  summarise(Item_Sold = max(Item_Sold)) %>% 
  ungroup() %>% 
  inner_join(sold_list)


thrift <- rbind(sold, active_list)

colSums(is.na(thrift))
which(is.na(thrift[, 2]))


thrift$Condition[is.na(thrift$Condition)] = 'Unknown'
thrift$Category[is.na(thrift$Category)] = 'Unknown'
thrift$`Did the item sell?`[is.na(thrift$`Did the item sell?`)] = 'No'


thrift$Condition = tolower(thrift$Condition)

thrift$Condition <- gsub("new without tags", "opened – never used", thrift$Condition)


thrift %>%
  select(Category, Price) %>%
  ggplot(aes(x = Category, y = log(Price)))+
  geom_boxplot(aes(fill = Category), outlier.size = 0.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
thrift %>%
  select(Condition, Price) %>%
  ggplot(aes(x = Condition, y = log(Price)))+
  geom_boxplot(aes(fill = Condition), outlier.size = 0.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df = as.data.frame(thrift %>%
                     filter(!is.na(thrift$`Date Sold`)))

df %>% 
  mutate(Date = str_split_fixed(df$`Date Sold`,'-',3)[,2]) %>%
  select(Date, Price) %>%
  ggplot(aes(x = Date, y = log(Price)))+
  geom_boxplot(aes(), outlier.size = 0.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  



separate(data = thrift, col = thrift$`Did the item sell?`, into = c("sell?", "Date"), sep = " ")

data("stop_words")

thrift %>%
  select(Title) %>%
  unnest_tokens(word, Title) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 10000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


thrift_word = thrift %>%
  select(Title, Search_Term) %>%
  unnest_tokens(word, Title) %>%
  anti_join(stop_words) %>%
  count(Search_Term, word, sort = TRUE)

total_word <- thrift_word %>% 
  group_by(Search_Term) %>% 
  summarize(total = sum(n))

thrift_word <- left_join(thrift_word, total_word)


thrift_tf_idf <- thrift_word %>%
  bind_tf_idf(word, Search_Term, n)

thrift_tf_idf


austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)



thrift_bigrams = thrift %>%
  select(Title, Search_Term) %>%
  unnest_tokens(bigram, Title, token = "ngrams", n = 2) 

thrift_bigrams %>%
  count(bigram, sort = TRUE)


names(thrift)[3] <- 'System_Mean_Price'

mod1 <- lm(Price ~ Search_Term + System_Mean_Price + Condition + Category, data = thrift)


thrift1 = thrift %>% 
  mutate(fCategory = factor(Category),
         fCondition = factor(Condition))

thrift1 = thrift1 %>%
  mutate(Date = substr(thrift1$`Did the item sell?`,9,12))

thrift1$Date = gsub(" ","",thrift1$Date)



options(repr.plot.width=7,repr.plot.height = 7)
p1=thrift_data %>% 
  mutate(len_title=str_length(Title)) %>% 
  group_by (len_title) %>% 
  summarize(mean_log_price=mean(log(Price))) %>%
  ggplot(aes(x=len_title,y=mean_log_price)) +
  geom_point(size=0.5) +
  geom_smooth(method='loess',color="red",size=0.5) +
  #ggtitle('Mean log(Price) Versus Length of Title') +
  theme(aspect.ratio=1)

options(repr.plot.width=7,repr.plot.height = 7)
p2=New_thrift_data %>% 
  mutate(num_token_title=str_count(Title,'\\S+')) %>% 
  group_by (num_token_title) %>% 
  summarize(mean_log_price=mean(log(Price))) %>%
  ggplot(aes(x=num_token_title,y=mean_log_price)) +
  geom_point(size=0.5) +
  geom_smooth(method='loess',color="red",size=0.5) +
  #ggtitle('Mean log(Price) Versus Token of Title') +
  theme(aspect.ratio=1)


grid.arrange(p1,p2,ncol=2)


install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library(readxl)
Coquet_2010_chickrearing_Tern_tracks = read_excel("Coquet_2010_chickrearing_Tern_tracks.xlsx", sheet=1)



library (ggplot2) #install ggplot2 package first
ggplot () + geom_point (data=Coquet_2010_chickrearing_Tern_tracks, aes (x=BNGX, y=BNGY))
library(sp) #install sp package first
library(rgdal)
uk<-readOGR("GBR_adm0.shp")
ukgrid <- "+init=epsg:27700" #specify coordinate system
uk_ukgrid <- spTransform(uk, ukgrid) #transform shapefile to specified coordinate system 
ggplot() + geom_polygon(data = uk_ukgrid, aes(x = long, y = lat, group = group))

ggplot() + 
  geom_polygon(data = uk_ukgrid, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data=Coquet_2010_chickrearing_Tern_tracks, aes (x=BNGX,y=BNGY), colour="red")

library(sf)
ggplot() + 
  coord_sf(xlim = c(-2, 7), ylim = c(50, 58), expand = FALSE) +
  geom_polygon(data = uk_ukgrid, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data=Coquet_2010_chickrearing_Tern_tracks, aes (x=BNGX,y=BNGY), colour="red") 
  



library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-2, 0), ylim = c(54, 56)) + 
  geom_point(data = Coquet_2010_chickrearing_Tern_tracks, aes(x = LONGITUDE, y = LATITUDE))











ID = Coquet_2010_chickrearing_Tern_tracks$TRACKID
longitude = Coquet_2010_chickrearing_Tern_tracks$LONGITUDE
latitude = Coquet_2010_chickrearing_Tern_tracks$LATITUDE

data1 <- data.frame(ID, longitude, latitude)

data1

data1 = data1[sort(data1$ID,index.return=TRUE)$ix,]

data2 <- prepData(data1,type="LL",coordNames=c("longitude","latitude"))
head(data2)

plot(data2,compact=T)


## initial parameters for gamma and von Mises distributions
mu0 <- c(0.1,1) # step mean (two parameters: one for each state) 
sigma0 <- c(0.1,1) # step SD
zeromass0 <- c(0.1,0.05) # step zero-mass
stepPar0 <- c(mu0,sigma0,zeromass0)
angleMean0 <- c(pi,0) # angle mean 
kappa0 <- c(1,1) # angle concentration 
anglePar0 <- c(angleMean0,kappa0)

## call to fitting function
m <- fitHMM(data=data2,nbStates=2,stepPar0=stepPar0,
            anglePar0=anglePar0,formula=~1)
m

CI(m)


plot(m, plotCI=TRUE)

states <- viterbi(m)


DIST2COL = Coquet_2010_chickrearing_Tern_tracks$DIST2COL


data11 <- data.frame(ID, longitude, latitude, DIST2COL)

data11

data11 = data11[sort(data11$ID,index.return=TRUE)$ix,]

data22 <- prepData(data11,type="LL",coordNames=c("longitude","latitude"))
head(data22)


data22$DIST2COL <-
  (data22$DIST2COL-mean(data22$DIST2COL))/sd(data22$DIST2COL)


plot(data22,compact=T)

## initial parameters for gamma and von Mises distributions
mu0 <- c(0.1,1) # step mean (two parameters: one for each state) 
sigma0 <- c(0.1,1) # step SD
zeromass0 <- c(0.1,0.05) # step zero-mass
stepPar0 <- c(mu0,sigma0,zeromass0)
angleMean0 <- c(pi,0) # angle mean 
kappa0 <- c(1,1) # angle concentration 
anglePar0 <- c(angleMean0,kappa0)

## call to fitting function
m1 <- fitHMM(data=data22,nbStates=2,stepPar0=stepPar0,
            anglePar0=anglePar0,formula=~DIST2COL)

m1

plot(m1, plotCI=TRUE)

states1 <- viterbi(m1)

AIC(m,m1)



BRG2COL = Coquet_2010_chickrearing_Tern_tracks$BRG2COL
data111 <- data.frame(ID, longitude, latitude, DIST2COL,BRG2COL)

data111

data111 = data111[sort(data111$ID,index.return=TRUE)$ix,]

data222 <- prepData(data111,type="LL",coordNames=c("longitude","latitude"))
head(data222)


data222$BRG2COL <-
  (data222$BRG2COL-mean(data222$BRG2COL))/sd(data222$BRG2COL)


plot(data22,compact=T)

## initial parameters for gamma and von Mises distributions
mu0 <- c(0.1,1) # step mean (two parameters: one for each state) 
sigma0 <- c(0.1,1) # step SD
zeromass0 <- c(0.1,0.05) # step zero-mass
stepPar0 <- c(mu0,sigma0,zeromass0)
angleMean0 <- c(pi,0) # angle mean 
kappa0 <- c(1,1) # angle concentration 
anglePar0 <- c(angleMean0,kappa0)

## call to fitting function
m2 <- fitHMM(data=data222,nbStates=2,stepPar0=stepPar0,
             anglePar0=anglePar0,formula=~DIST2COL+cos(BRG2COL))

m2

plot(m1, plotCI=TRUE)

states11 <- viterbi(m2)

AIC(m,m2)




m3 <- fitHMM(data=data222,nbStates=2,stepPar0=stepPar0,
             anglePar0=anglePar0,formula=~BRG2COL)

m3

plot(m1, plotCI=TRUE)

states111 <- viterbi(m3)

AIC(m1,m3)
