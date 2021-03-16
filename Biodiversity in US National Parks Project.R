library(dplyr)
library(tidyr)
library(openintro)
library(ggplot2)

species<-read.csv("species.csv",sep=",", stringsAsFactors = FALSE)
parks<-read.csv("parks.csv",sep=",", stringsAsFactors = FALSE)
summary(parks)
summary(species)

#Data Wrangling and Cleaning
parks<-separate(parks,State,into=c( "State_1", "State_2"),sep=",")
parks<-select(parks,-State_2)
names(parks)[names(parks)=="State_1"] <- "State"
parks$State<-abbr2state(parks$State)
library(states)
state_region_table<-data.frame(State = state.name,Region= state.region)
levels(state_region_table$Region)[levels(state_region_table$Region)=="North Central"] <- "Midwest"
dataset_merged<-merge(parks,state_region_table, by ="State")
dataset_merged<-merge(dataset_merged,species, by ="Park.Name")
dataset_merged<-select(dataset_merged,-X)
dataset_merged_confirmed <- dataset_merged[which(dataset_merged$Occurrence=='Present')]
dataset_merged_confirmed <- subset(dataset_merged, Occurrence =='Present')
dataset_merged_confirmed$Occurrence[dataset_merged_confirmed$Occurrence=="Present"] <- "1"
dataset_merged_confirmed$Occurrence<-as.numeric(dataset_merged_confirmed$Occurrence)
dataset_merged_confirmed$Category<-as.character(dataset_merged_confirmed$Category)
dataset_merged_confirmed$Main_Category<-ifelse(dataset_merged_confirmed$Category=="Algae"|dataset_merged_confirmed$Category=="Fungi"|dataset_merged_confirmed$Category=="Nonvascular Plant"|dataset_merged_confirmed$Category=="Vascular Plant","Plant","Animal")
dataset_merged_confirmed$Main_Category<-as.character(dataset_merged_confirmed$Main_Category)
dataset_merged_confirmed_Plant <- dataset_merged_confirmed %>% filter(Main_Category=='Plant')
dataset_merged_confirmed_Animal <- dataset_merged_confirmed %>% filter(Main_Category=='Animal')

# Tree Map for Plant Biodiversity
data_subset_Plant<-select(dataset_merged_confirmed_Plant,State,Region,Occurrence)
Treemap_State_Region_Plant <- data_subset_Plant %>%group_by(Region,State) %>%summarise(Freq=n())
library("treemap")
treemap(
  Treemap_State_Region_Plant,
  index = c("Region", "State"),
  vColor = "Freq",
  vSize = "Freq",
  type = "dens",
  title = "Distribution of Plant Species across the USA",
  fontsize.title = 14,
  position.legend = "bottom",
  title.legend = "Number of Plant species across the USA",
  fontface.labels = "bold",
  fontsize.labels = 7)

# Tree Map for Animal Biodiversity
data_subset_Animal<-select(dataset_merged_confirmed_Animal,State,Region,Occurrence)
Treemap_State_Region_Animal <- data_subset_Animal %>%group_by(Region,State) %>%summarise(Freq=n())
library("treemap")
treemap(
  Treemap_State_Region_Animal,
  index = c("Region", "State"),
  vColor = "Freq",
  vSize = "Freq",
  type = "dens",
  title = "Distribution of Animal Species across the USA",
  fontsize.title = 14,
  position.legend = "bottom",
  title.legend = "Number of Animal species across the USA",
  fontface.labels = "bold",
  fontsize.labels = 7)

# Endangered Analysis by National Parks
data_subset_endangered<-select(dataset_merged_confirmed,Category,Park.Name,Conservation.Status)
data_subset_endangered$Conservation.Status<-as.factor(data_subset_endangered$Conservation.Status)
levels(data_subset_endangered$Conservation.Status)
data_subset_endangered$Conservation.Status[data_subset_endangered$Conservation.Status=="Proposed Endangered"] <- "Endangered"
data_subset_endangered$Conservation.Status[data_subset_endangered$Conservation.Status=="Proposed Threatened"] <- "Endangered"
data_subset_endangered$Conservation.Status[data_subset_endangered$Conservation.Status=="Species of Concern"] <- "Endangered"
data_subset_endangered$Conservation.Status[data_subset_endangered$Conservation.Status=="Threatened"] <- "Endangered"
data_subset_endangered<-subset(data_subset_endangered, Conservation.Status=="Endangered")
data_subset_endangered<-select(data_subset_endangered,-Conservation.Status)
endangered_count <- data_subset_endangered %>%group_by(Park.Name,Category) %>%summarise(Freq=n())
names(endangered_count)[names(endangered_count)=="Freq"] <- "Count"
plot_endangered_count <-ggplot(data = endangered_count, aes(x = Category, y = Park.Name))+ geom_tile(aes(fill = Count))+ggtitle("Number of Endangered Species across the National Parks in the US")+theme(plot.title = element_text(hjust=0.5,size=14,face="bold"),axis.title.x = element_text(color = "black", size = 10, face = "bold"),axis.title.y = element_text(color = "black", size = 10, face = "bold"))+scale_fill_gradient(low="red",high="red4")+labs(x="Species",y="Park Name")
print(plot_endangered_count)

#Correlation between Plant and Animal Biodiversity
data_subset_plant<-select(dataset_merged_confirmed_Plant,Park.Name,Main_Category)
Table_Biodiversity_Plant<- data_subset_plant %>%group_by(Park.Name,Main_Category) %>%summarise(No.of.species.Plants=n())
data_subset_animal<-select(dataset_merged_confirmed_Animal,Park.Name,Main_Category)
Table_Biodiversity_Animal<- data_subset_animal %>%group_by(Park.Name,Main_Category) %>%summarise(No.of.species.Animals=n())
Table_Bio<-merge(Table_Biodiversity_Plant,Table_Biodiversity_Animal, by ="Park.Name")
ggplot(Table_Bio, aes(x=No.of.species.Plants, y=No.of.species.Animals)) + geom_point()+labs(x="Plant Biodiversity",y="Animal Biodiversity")+ geom_point(size=2,color="springgreen3")+theme(plot.title = element_text(hjust=0.5,size=14,face="bold"),axis.title.x = element_text(color="black",size = 10, face = "bold"),axis.title.y = element_text(color="black",size = 10, face = "bold"))+ggtitle("Distribution of Biodiversity across USA")+geom_smooth(method="lm",color="slateblue3")

# Top 3 Common Species across Regions
dataset_merged_confirmed<- subset(dataset_merged_confirmed,Common.Names!="None")
split_data <- strsplit(dataset_merged_confirmed$Common.Names, split = ",")
split<-data.frame(Region = rep(dataset_merged_confirmed$Region, sapply(split_data, length)), Common.Names = unlist(split_data))
table_common <- split %>%group_by(Region,Common.Names) %>%summarise(Freq=n())%>%top_n(n = 3)%>%arrange(Freq)
Common_Species <- ggplot(table_common, aes(x =Region, y = Freq))+coord_flip()+
  geom_col(aes(fill = Common.Names), width = 0.7)+ggtitle("Common Species across various Regions in the US")+theme(plot.title = element_text(hjust=0.5,size=14,face="bold"),legend.title=element_text(size=10,face = "bold"),axis.title.x = element_text(color = "black", size = 10, face = "bold"),axis.title.y = element_text(color = "black", size = 10, face = "bold"))+labs(x="Region",y="Number of Species")+scale_fill_discrete(name = "Species")
print(Common_Species)

#Endangered top Region

data_subset_endangered<-select(dataset_merged_confirmed,Category,Park.Name,Conservation.Status,Region,Main_Category,Common.Names)
data_subset_endangered$Conservation.Status<-as.factor(data_subset_endangered$Conservation.Status)
data_subset_endangered$Conservation.Status[data_subset_endangered$Conservation.Status=="Proposed Endangered"] <- "Endangered"
data_subset_endangered$Conservation.Status[data_subset_endangered$Conservation.Status=="Proposed Threatened"] <- "Endangered"
data_subset_endangered$Conservation.Status[data_subset_endangered$Conservation.Status=="Species of Concern"] <- "Endangered"
data_subset_endangered$Conservation.Status[data_subset_endangered$Conservation.Status=="Threatened"] <- "Endangered"
data_subset_endangered<-subset(data_subset_endangered, Main_Category=="Animal")
data_subset_endangered<-subset(data_subset_endangered, Conservation.Status=="Endangered")
data_subset_endangered<-select(data_subset_endangered,Region,Common.Names)
data_subset_endangered<- subset(data_subset_endangered,Common.Names!="None")
split_data <- strsplit(data_subset_endangered$Common.Names, split = ",")
split<-data.frame(Region = rep(data_subset_endangered$Region, sapply(split_data, length)), Common.Names = unlist(split_data))
table_top_endangered <- split %>%group_by(Common.Names) %>%summarise(Freq=n())%>%top_n(n = 5)%>%arrange(Freq)
top_endangered<-ggplot(data=table_top_endangered, aes(x=reorder(Common.Names,-Freq), y=Freq))+
  geom_bar(stat="identity",fill = "#FF6666")+ggtitle("Top Endangered Animal Species in the US")+theme(plot.title = element_text(hjust=0.5,size=14,face="bold"),legend.title=element_text(size=10,face = "bold"),axis.title.x = element_text(color = "black", size = 10, face = "bold"),axis.title.y = element_text(color = "black", size = 10, face = "bold"))+labs(x="Animal Species",y="Number of Species")
print(top_endangered)