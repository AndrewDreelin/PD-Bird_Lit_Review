#Final_LR_Summary_Sep22

#load general library
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lme4)
library(visreg)
library(mgcv)
library(ggmap)
library(broom)
library(mapdata)
library(terra)
library(ggspatial)
library(viridis)
library(ggnewscale)
library(forcats)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(metR)

library(sf)
library(here)
library(maps)
library(mapdata)

rev<-read.csv("PD_Bird_LR_Jan_31.csv") #read in final data set

str(rev)

#re-assign variable formats
rev$Loc_Name<-as.factor(rev$Loc_Name)
rev$Lat<-as.numeric(rev$Lat)
rev$Long<-as.numeric(rev$Long)
rev$PD_Species<-as.factor(rev$PD_Species)
rev$Mean_PD_Size<-as.numeric(rev$Mean_PD_Size)
rev$Total_PD_Size<-as.numeric(rev$Total_PD_Size)
rev$Study_Duration<-as.numeric(rev$Study_Duration)
rev$Min_PD_Size<-as.numeric(rev$Min_PD_Size)
rev$Max_PD_Size<-as.numeric(rev$Max_PD_Size)
rev$Bird_Species<-as.factor(rev$Bird_Species)
rev$Bird_Category<-as.factor(rev$Bird_Category)
rev$Sampling_Period<-as.factor(rev$Sampling_Period)
rev$Country<-as.factor(rev$Country)
rev$Tribal_Land<-as.factor(rev$Tribal_Land)
rev$Effect_Type<-as.factor(rev$Effect_Type)
rev$Hypothesis_1<-as.factor(rev$Hypothesis_1)
rev$Hypothesis_2<-as.factor(rev$Hypothesis_2)
rev$Hypothesis_3<-as.factor(rev$Hypothesis_3)

mopl_rev<-rev[grep("MOPL",rev$Bird_Species),] #filtering by category
buow_rev<-rev[grep("BUOW",rev$Bird_Species),] 
raptor_rev<-rev[grep("TUVU|OSPR|GOEA|BAEA|FEHA|RTHA|RLHA|SWHA|NOHA|MIKI|AMKE|MERL|PRFA|APFA|PEFA",rev$Bird_Species),]
gen_rev<-filter(rev, Bird_Category == "General") 

rev_study_og<-rev %>% distinct(Ref_Num, .keep_all = TRUE) #filtering analyses down to unique studies

rev_study_birdpubs<-rev_study_og

rev_study_birdpubs$Bird_Species<-as.character(rev_study_birdpubs$Bird_Species)

rev_study_birdpubs<-dplyr::select(rev_study_birdpubs, -c(6:75)) #dropping unnecessary columns
rev_study_birdpubs<-filter(rev_study_birdpubs, Ref_Num != "57") #removing just to calculate bird species pubs (doesn't specify on vs. off colony)
rev_study_birdpubs<-filter(rev_study_birdpubs, Ref_Num != "46.2") #removing just to calculate bird species pubs (different PD species)
rev_study_birdpubs<-filter(rev_study_birdpubs, Ref_Num != "65.2") #removing just to calculate bird species pubs (different PD species)
rev_study_birdpubs<-rbind(rev_study_birdpubs, list(46.3,'Campbell and Clark',1,'WTPD','AMAV')) #custom row for AMAV since that species gets dropped with 46.2

#Bird species breakdown (% of studies for each category, total # of species). 

#number of studies involving BUOW
BUOW_count<-lengths(regmatches(rev_study_birdpubs$Bird_Species, gregexpr("BUOW", rev_study_birdpubs$Bird_Species)))
BUOW_count<-as.table(BUOW_count)
table(BUOW_count)

buow_study<-rev_study_birdpubs[grep("BUOW",rev_study_birdpubs$Bird_Species),] 

#number of studies involving MOPL
MOPL_count<-lengths(regmatches(rev_study_birdpubs$Bird_Species, gregexpr("MOPL", rev_study_birdpubs$Bird_Species)))
MOPL_count<-as.table(MOPL_count)
table(MOPL_count)

mopl_study<-rev_study_birdpubs[grep("MOPL",rev_study_birdpubs$Bird_Species),] 

#number of studies involving FEHA
FEHA_count<-lengths(regmatches(rev_study_birdpubs$Bird_Species, gregexpr("FEHA", rev_study_birdpubs$Bird_Species)))
FEHA_count<-as.table(FEHA_count)
table(FEHA_count)

#number of studies for all species
rev_study_birdpubs$Bird_Species<-as.character(rev_study_birdpubs$Bird_Species)

pubs_per_species<-table(unlist(strsplit(rev_study_birdpubs$Bird_Species, ', ')))

#remove artificial, CCLO/TBLO, and WEME/EAME since these are non-species options

pubs_per_species<-as.data.frame(pubs_per_species)

pubs_per_species<-filter(pubs_per_species, Var1 != "Artificial")
pubs_per_species<-filter(pubs_per_species, Var1 != "CCLO/TBLO")
pubs_per_species<-filter(pubs_per_species, Var1 != "WEME/EAME")

view(pubs_per_species) 

#write.csv(pubs_per_species, "LR_Species_Publications_Final.csv",) #save as file for supplementary table



#updating rev_study_og for the rest of the summary statistics now that we're not calculating bird pubs

rev_study<-rev_study_og

#rev study has 113 obs instead of 111 because two studies are two-parted and have different PD species/metadata

rev_study<-filter(rev_study, Ref_Num != "18.2") #removing now that we're not calculating bird pubs (different bird species)
rev_study<-filter(rev_study, Ref_Num != "19.2") #removing now that we're not calculating bird pubs (different bird species)
rev_study<-filter(rev_study, Ref_Num != "48.2") #removing now that we're not calculating bird pubs (different bird species)
rev_study<-filter(rev_study, Ref_Num != "271.2") #removing now that we're not calculating bird pubs (different bird species)
rev_study<-filter(rev_study, Ref_Num != "271.3") #removing now that we're not calculating bird pubs (different bird species)

#updating studies by bird category now that we're not calculating bird pubs

buow_study<-rev_study[grep("BUOW",rev_study$Bird_Species),] 
mopl_study<-rev_study[grep("MOPL",rev_study$Bird_Species),] 
raptor_study<-rev_study[grep("TUVU|OSPR|GOEA|BAEA|FEHA|RTHA|RLHA|SWHA|NOHA|MIKI|AMKE|MERL|PRFA|APFA|PEFA",rev_study$Bird_Species),] 
gen_study<-filter(rev_study, Bird_Category == "General") 


#Hypothesis support breakdown

#Hypothesis 1
rev1<-rev %>% filter(rev$Hypothesis_1 != "NA")

levels(rev1$Bird_Category) <- c('Burrowing Owl','General','Mountain Plover','Raptor')

rev1<-filter(rev1, ! Bird_Category == 'Raptor') #dropping raptor from H1 based on reviewer feedback (now in H3 only)

summary(rev1$Hypothesis_1)

cbPalette <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#999999")

#Figure 1
ggplot(rev1) +
  geom_bar(aes(y=factor(Bird_Category,levels=c("General","Raptor","Burrowing Owl","Mountain Plover")), fill = Hypothesis_1), width = 0.7) +
  geom_text(stat='count', aes(y=Bird_Category, label=after_stat(count)),hjust=-0.5, size = 6) + 
  #facet_grid(~factor(rev1$Bird_Category, levels=c("MOPL","Raptor","BUOW","General"))) + #horizontal
  #facet_grid(~factor(rev1$Bird_Category, levels=c("Mountain Plover","Burrowing Owl","Raptor","General"))) + #horizontal
  labs(y = "", x = "# of Analyses",color="Hypothesis 1 Support") +
  #ggtitle("Hypothesis 1 Support by Bird Category") +
  scale_fill_manual("Conclusion", values = cbPalette) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 65)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 65)) +
  #xlim(0,65) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
  theme(#legend.position = "none",
    axis.text=element_text(size=18, color="black"),
    axis.title=element_text(size=20, color="black"),
    strip.text = element_text(size=14),
    legend.title=element_text(size=16, color="black"),
    legend.text=element_text(size=16, color="black")
  )

#^^^best plot

#Figure 2
#read in H1 dataset
setwd("/Users/andrewdreelin/Documents/R/phd_research")

hyp1<-read.csv("LR_H1_Table_Final.csv") 

str(hyp1)

#hyp1$Bird.Species<-as.factor(hyp1$Bird.Species)
hyp1$Bird_Population_Parameter<-as.factor(hyp1$Bird_Population_Parameter)
hyp1$Effect.Direction<-as.factor(hyp1$Effect.Direction)
#hyp1$Conclusion<-as.factor(hyp1$Conclusion)

hyp1<-filter(hyp1, ! Effect.Direction == "Not tested") 
hyp1<-filter(hyp1, Descriptive.only. == "No") 
hyp1<-filter(hyp1, Bird_Population_Parameter == "Occurrence") 

cbPalette <- c("#6699CC","#808080","#CC6677")

hyp1$Abundance_Change = factor(hyp1$Abundance_Effect, levels = c('Positive','Neutral','Negative'))

#wide bars & lower case names
new_fig2<-ggplot(hyp1) + #species in manual order of net support
  geom_col(aes(x = Freq, y=factor(Species, levels=c("Grasshopper Sparrow","Lark Bunting","Brewer's Sparrow","Western Meadowlark","Cassin's Sparrow","Vesper Sparrow","Baird's Sparrow","Sage Thrasher","American Pipit","Brown-headed Cowbird","Bullock's Oriole","Common Nighthawk","Eastern Kingbird","Lark Sparrow","Northern Bobwhite","Northern Mockingbird","Ring-necked Pheasant",'Rough-legged Hawk',"Western Kingbird","White-crowned Sparrow","American Crow","American Goldfinch","American Robin","Black-billed Magpie","Black-capped Chickadee","Blue Jay","Bobolink","Brewer's Blackbird","Canada Goose","Dickcissel","Spotted Towhee","Song Sparrow","Chipping Sparrow","Clay-colored Sparrow","Field Sparrow","Dark-eyed Junco","Red-winged Blackbird","Common Grackle","European Starling","House Finch","Lesser Goldfinch","Sprague's Pipit","Rock Pigeon (Feral)","Sharp-tailed Grouse","Loggerhead Shrike","House Sparrow","Cliff Swallow","Violet-green Swallow","Tree Swallow","NRWS","Say's Phoebe","Scissor-tailed Flycatcher","Chihuahuan Raven",'Great Horned Owl',"Turkey Vulture",'Bald Eagle',"Swainson's Hawk","Mallard","Scaled Quail","Cattle Egret","Northern Flicker","American Kestrel","Prairie Falcon",'Golden Eagle',"Red-tailed Hawk","Barn Swallow","Curve-billed Thrasher","Long-billed Curlew","CCLO/TBLO","Chestnut-collared Longspur","Thick-billed Longspur","Upland Sandpiper","WEME/EAME","Mourning Dove",'Northern Harrier','Ferruginous Hawk',"Killdeer","Horned Lark","Mountain Plover","Burrowing Owl")), fill = Abundance_Change), width = 1, color = "black") +
  labs(y = "", x = "# of Analyses") +
  scale_fill_manual("Analyses Affecting Occurrence/Abundance", values = cbPalette) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 18)) +
  scale_y_discrete(expand = c(0, 0), labels = c("grasshopper sparrow","lark bunting","brewer's sparrow","western meadowlark","cassin's sparrow","vesper sparrow","baird's sparrow","sage thrasher","american pipit","brown-headed cowbird","bullock's oriole","common nighthawk","eastern kingbird","lark sparrow","northern bobwhite","northern mockingbird","ring-necked pheasant",'rough-legged hawk',"western kingbird","white-crowned sparrow","american crow","american goldfinch","american robin","black-billed magpie","black-capped chickadee","blue jay","bobolink","brewer's blackbird","canada goose","dickcissel","spotted towhee","song sparrow","chipping sparrow","clay-colored sparrow","field sparrow","dark-eyed junco","red-winged blackbird","common grackle","european starling","house finch","lesser goldfinch","sprague's pipit","rock pigeon (feral)","sharp-tailed grouse","loggerhead shrike","house sparrow","cliff swallow","violet-green swallow","tree swallow","n. rough-winged swallow","say's phoebe","scissor-tailed flycatcher","chihuahuan raven","turkey vulture","swainson's hawk","mallard","scaled quail","cattle egret","northern flicker","american kestrel","prairie falcon",'golden eagle',"red-tailed hawk","barn swallow","curve-billed thrasher","long-billed curlew","cclo/tblo","chestnut-collared longspur","thick-billed longspur","upland sandpiper","weme/eame","mourning dove",'northern harrier','ferruginous hawk',"killdeer","horned lark","mountain plover","burrowing owl")) +
  theme_classic() +
  theme(legend.position = c(0.72, 0.85)) +
  theme(panel.grid.minor = element_blank()) + 
  theme(axis.title=element_text(size=16, color="black", vjust = 2),
        axis.text.y = element_text(size=22, color="black", hjust = 1),
        axis.text.x = element_text(size=22, color="black"),
        axis.ticks.length.y = unit(0.5, "cm"),
        axis.title.x = element_text(size=22),
        legend.text=element_text(size=22),
        legend.title=element_text(size=22)
  )

new_fig2

ggsave(new_fig2, filename = "~/Documents/R/phd_research/LR_Fig2_R2_test.pdf", height = 26, width = 16)


#Hypothesis 2
rev2<-rev %>% filter(rev$Hypothesis_2 != "NA")

summary(rev2$Hypothesis_2)

ggplot(rev2) +
  geom_bar(aes(x=Hypothesis_2, fill=Bird_Category))+
  #facet_grid(rev2$Bird_Category)+
  labs(x = "Hypothesis 2", y = "# of Analyses",color="Bird Category")+
  ggtitle("Hypothesis 2 Support by Bird Category") +
  scale_fill_manual("Bird Category", values = cbPalette)


#Hypothesis 3
raptor_rev<-raptor_rev %>% filter(raptor_rev$Hypothesis_3 != "NA")

cbPalette <- c("#CC79A7", "#E69F00","#56B4E9")
ggplot(raptor_rev) +
  geom_bar(aes(x=Hypothesis_3, fill=Bird_Category))+
  labs(x = "Hypothesis 3", y = "# of Analyses",color="Bird Category")+
  ggtitle("Hypothesis 3 Support") +
  scale_fill_manual("Bird Category", values = cbPalette)

summary(raptor_rev$Hypothesis_3)

#read in H3 dataset
hyp3<-read.csv("LR_H3_Table_Final.csv")

str(hyp3)

hyp3$Bird.Species<-as.factor(hyp3$Bird.Species)
hyp3$Bird_Population_Parameter<-as.factor(hyp3$Bird_Population_Parameter)
hyp3$Effect.Direction<-as.factor(hyp3$Effect.Direction)
hyp3$Conclusion<-as.factor(hyp3$Conclusion)

cbPalette <- c("#CC79A7", "#E69F00","#56B4E9")

#Figure 3
ggplot(hyp3) + #in order of support
  geom_bar(aes(y=factor(Bird.Species, levels=c("Swainson's Hawk","American Kestrel",'Great Horned Owl',"Turkey Vulture",'Northern Harrier','Rough-legged Hawk',"Red-tailed Hawk","Prairie Falcon",'Bald Eagle','Golden Eagle','Ferruginous Hawk')), fill = Conclusion)) + 
  geom_text(stat='count', aes(y=Bird.Species, label=after_stat(count)),hjust=-0.2, size = 6) + 
  labs(x = "# of Analyses", y = "") +
  scale_fill_manual("Conclusion", values = cbPalette) + 
  #ggtitle("Prairie Dog Effects on Raptors") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 35)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title=element_text(size=20,color="black"),
        axis.text = element_text(size=18,color="black"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16))

#^^^^best plot


#PD species breakdown (% of studies for each species, mean town size, town size range)

summary(rev_study$PD_Species)

MEPD_studies<-rev_study[grep("MEPD",rev_study$PD_Species),]
WTPD_studies<-rev_study[grep("WTPD",rev_study$PD_Species),]
UTPD_studies<-rev_study[grep("UTPD",rev_study$PD_Species),]
GUPD_studies<-rev_study[grep("GUPD",rev_study$PD_Species),]
BTPD_studies<-rev_study[grep("BTPD",rev_study$PD_Species),]

#summary statistics for BTPD colonies
summary(BTPD_studies$Mean_PD_Size)
summary(BTPD_studies$Total_PD_Size)
summary(BTPD_studies$Min_PD_Size)
summary(BTPD_studies$Max_PD_Size)


#habitat type breakdown
rev_study$Habitat_Type<-as.factor(rev_study$Habitat_Type)
summary(rev_study$Habitat_Type)

#number of studies from each habitat type
rev_study$Habitat_Type<-as.character(rev_study$Habitat_Type)

pubs_per_habitat<-table(unlist(strsplit(rev_study$Habitat_Type, ', ')))

view(pubs_per_habitat)


#grazing regime breakdown
rev_study$Grazing_Regime<-as.factor(rev_study$Grazing_Regime)
summary(rev_study$Grazing_Regime)


#fire regime breakdown
rev_study$Fire_Regime<-as.factor(rev_study$Fire_Regime)
summary(rev_study$Fire_Regime)

#country breakdown
summary(rev_study$Country)

#104 in USA
#9 in MX
#1 in CA

#BCR locations

cbPalette <- c("#0072B2", "#CC79A7", "#999999", "#F0E442", "#D55E00", "#E69F00", "#56B4E9", "#000000", "#009E73")

rev_study$BCRNAME = factor(rev_study$BCRNAME, levels = c('SHORTGRASS PRAIRIE','BADLANDS AND PRAIRIES','SOUTHERN ROCKIES/COLORADO PLATEAU','PRAIRIE POTHOLES','SIERRA MADRE OCCIDENTAL','CENTRAL MIXED GRASS PRAIRIE','CHIHUAHUAN DESERT','CONTINENTAL','NORTHERN ROCKIES'))

rev_study$BCRNAME<-as.factor(rev_study$BCRNAME)
summary(rev_study$BCRNAME)

ggplot(rev_study) +
  geom_bar(aes(x=reorder(BCRNAME,BCRNAME,function(x)-length(x)), fill=BCRNAME))+
  geom_text(stat='count', aes(x=BCRNAME, label=after_stat(count)),vjust=-1, size = 6) + 
  labs(x = "Bird Conservation Region", y = "# of Studies", color = "BCRNAME")+
  scale_fill_manual("BCRNAME", values = cbPalette)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 45)) +
  guides(fill=guide_legend((title="Bird Conservation Region"))) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(
    axis.title = element_text(size = 18,color="black"),
    axis.text = element_text(size=18,color="black"),
    legend.text=element_text(size=14,color="black"),
    legend.title=element_text(size=14,color="black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


#study site frequency breakdown
summary(rev_study$Loc_Name)
rev_study$Loc_Name<-as.character(rev_study$Loc_Name)
pubs_per_loc<-table(unlist(strsplit(rev_study$Loc_Name, ', ')))
view(pubs_per_loc) #works!

ggplot(rev_study) +
  geom_bar(aes(x=Loc_Name, fill=Loc_Name))

summary(mopl_study$Loc_Name)
ggplot(mopl_study) +
  geom_bar(aes(x=Loc_Name, fill=Loc_Name))

summary(buow_study$Loc_Name)
ggplot(buow_study) +
  geom_bar(aes(x=Loc_Name, fill=Loc_Name))

summary(raptor_study$Loc_Name)
ggplot(raptor_study) +
  geom_bar(aes(x=Loc_Name, fill=Loc_Name))

summary(gen_study$Loc_Name)
ggplot(gen_study) +
  geom_bar(aes(x=Loc_Name, fill=Loc_Name))

#Tribal representation 
summary(rev_study$Tribal_Land)

#Seasonality: (% of studies during breeding, wintering, and fall/spring migration)
rev_study$Sampling_Period<-as.factor(rev_study$Sampling_Period)
summary(rev_study$Sampling_Period)

summary(mopl_rev$Sampling_Period)
summary(buow_rev$Sampling_Period)
summary(raptor_rev$Sampling_Period)
summary(gen_rev$Sampling_Period)

#Study duration breakdown
summary(rev_study$Study_Duration)

#median study duration = 2 (report median since lots of outliers)
#mean = 5.262

ggplot(rev_study) +
  geom_bar(aes(x=Study_Duration))


#Mapping: 

#Figure 4: Map of study locations, including bird category (general, raptor, MOPL, BUOW) and seasonality to highlight geographic gaps and gaps in the annual cycle.

#making basemap
states<-map_data("state")
plains<-subset(states, region %in% c ("montana","washington","idaho","minnesota","wisconsin","oregon","california","nevada","iowa","louisiana","arkansas","missouri","north dakota","south dakota","wyoming","colorado","kansas","nebraska","utah","new mexico","arizona","texas","oklahoma")) 

NAm_map<-map_data("world", region = c("Mexico", "Canada"))

bm0<-ggplot(data = NAm_map, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = NAm_map, color = "black", fill = "white", size = 0.1) + 
  geom_polygon(data = plains, color = "black", fill = "white", size = 0.1) +
  coord_fixed(xlim = c(-116,-93), ylim = c(23,53), ratio = 1.2) +
  labs(x = "Longitude", y = "Latitude",color="Bird Category") +
  scale_x_longitude(breaks = c(-110, -100)) +
  scale_y_latitude(breaks = c(50, 40, 30)) +
  theme(legend.position="none",
        axis.title = element_text(size=18),
        axis.text = element_text(size = 14),
        panel.background = element_blank())

bm0 #works

#add in PD range shapefiles
BTPD_range<-st_read("./PD_Ranges/BTPD_shp/data_0.shp") #works
WTPD_range<-st_read("./PD_Ranges/WTPD_shp/data_0.shp")
MEPD_range<-st_read("./PD_Ranges/MEPD_shp/data_0.shp")
UTPD_range<-st_read("./PD_Ranges/UTPD_shp/data_0.shp")
GUPD_range<-st_read("./PD_Ranges/GUPD_shp/data_0.shp")

BTPD_geom<-st_geometry(BTPD_range) #works--> can view geometry now
WTPD_geom<-st_geometry(WTPD_range)
MEPD_geom<-st_geometry(MEPD_range)
UTPD_geom<-st_geometry(UTPD_range)
GUPD_geom<-st_geometry(GUPD_range)

plot(BTPD_geom[[1]])

#mapping PD range boundaries
PD_map<-ggplot() + 
  geom_sf(data = BTPD_geom, fill = NA, color = "#E69F00")+
  geom_sf(data = WTPD_geom, fill = NA, color = "#8452ba")+
  geom_sf(data = MEPD_geom, fill = NA, color = "#CC79A7")+
  geom_sf(data = UTPD_geom, fill = NA, color = "#8f810d")+
  geom_sf(data = GUPD_geom, fill = NA, color = "#009E73")

PD_map #works!

#putting both together
bm<-ggplot()+
  geom_polygon(data = NAm_map, aes(x = long, y = lat, group = group), color = "black", fill = "white", size = 0.1) + 
  geom_polygon(data = plains, aes(x = long, y = lat, group = group), color = "black", fill = "white", size = 0.1, inherit.aes = FALSE) +
  geom_sf(data = BTPD_geom, fill = NA, color = "#E69F00", linewidth = 0.8) +
  geom_sf(data = WTPD_geom, fill = NA, color = "#8452ba", linewidth = 0.8) +
  geom_sf(data = MEPD_geom, fill = NA, color = "#CC79A7", linewidth = 0.8) +
  geom_sf(data = UTPD_geom, fill = NA, color = "#8f810d", linewidth = 0.8) +
  geom_sf(data = GUPD_geom, fill = NA, color = "#009E73", linewidth = 0.8) + 
  scale_x_continuous(labels = ~ .x) +
  scale_y_continuous(labels = ~ .x) +
  coord_sf(xlim = c(-116,-93), ylim = c(23,53)) +
  labs(x = "Longitude", y = "Latitude",color="Bird Category")

bm #works!

#making a legend for PD species
bm_leg<-ggplot()+
  geom_polygon(data = NAm_map, aes(x = long, y = lat, group = group), color = "black", fill = "white", size = 0.1) + 
  geom_polygon(data = plains, aes(x = long, y = lat, group = group), color = "black", fill = "white", size = 0.1, inherit.aes = FALSE) +
  geom_sf(data = BTPD_geom, aes(color = "#E69F00", alpha = 0.0), linewidth = 0.8, fill = NA, show.legend = "line") +
  geom_sf(data = WTPD_geom, aes(color = "#8452ba", alpha = 0.0), linewidth = 0.8, fill = NA, show.legend = "line") +
  geom_sf(data = MEPD_geom, aes(color = "#CC79A7", alpha = 0.0), linewidth = 0.8, fill = NA, show.legend = "line") +
  geom_sf(data = UTPD_geom, aes(color = "#8f810d", alpha = 0.0), linewidth = 0.8, fill = NA, show.legend = "line") +
  geom_sf(data = GUPD_geom, aes(color = "#009E73", alpha = 0.0), linewidth = 0.8, fill = NA, show.legend = "line") + 
  coord_sf(xlim = c(-116,-93), ylim = c(23,53)) +
  #scale_x_continuous(labels = ~ .x) +
  #scale_y_continuous(labels = ~ .x) +
  scale_x_longitude(breaks = c(-110, -100)) +
  scale_y_latitude(breaks = c(50, 40, 30)) +
  scale_color_identity(guide="legend", labels = c('GUPD','WTPD','UTPD','MEPD','BTPD')) +
  guides(alpha = "none", color = guide_legend(override.aes = list(size = 14))) + 
  labs(x = "Longitude", y = "Latitude",color="Cynomys Species") +
  theme(legend.position = "bottom",
        legend.text=element_text(size=14),
        legend.title=element_text(size=16),
        axis.title=element_text(size=18),
        axis.text=element_text(size=14))

bm_leg #works!

#map of study locations overlaid on PD ranges
p_mopl<-bm +
  new_scale_color() +
  geom_count(data = mopl_study, aes(x = Long, y = Lat, color = "red", alpha=0), inherit.aes = FALSE)+
  #geom_jitter(height = 10, width = 10) +
  labs(x = "", y = "",color="Sampling Periods")+
  ggtitle("Mountain Plover") +
  #scale_size_continuous(range = c(3,13),guide = "none") + 
  #scale_alpha(guide = "none") + 
  scale_size_continuous(range = c(3,13)) + 
  scale_alpha(guide = "none") + 
  scale_x_longitude(breaks = c(-110, -100)) +
  scale_y_latitude(breaks = c(50, 40, 30)) +
  #scale_color_manual("red",guide = "none") + 
  #scale_color_manual(values = c("Breeding Season" = "red","Breeding, Postbreeding Migration" = "red","Nonbreeding Season" = "red", "Year-round" = "red", "Breeding, Nonbreeding" = "red", "Not applicable" = "red")) +
  theme(#legend.position="none",
        legend.text=element_text(size=11),
        legend.title=element_text(size=12),
        axis.title=element_text(size=18),
        axis.text = element_text(size = 14),
        plot.title=element_text(size=18),
        panel.background = element_blank(),
        #legend.position = 'none'
  )

p_mopl

p_raptor<-bm +
  new_scale_color() +
  geom_count(data = raptor_study, aes(x = Long, y = Lat, color = "red", alpha=0), inherit.aes = FALSE)+
  #geom_jitter(height = 10, width = 10) +
  labs(x = "", y = "",color="Sampling Periods")+
  ggtitle("Raptors") +
  scale_size_continuous(range = c(3,13),guide = "none") + 
  scale_alpha(guide = "none") + 
  scale_x_longitude(breaks = c(-110, -100)) +
  scale_y_latitude(breaks = c(50, 40, 30)) +
  #scale_color_manual(values = c("Breeding Season" = "#c25c4d","Breeding, Postbreeding Migration" = "#dda821","Nonbreeding Season" = "#337197", "Year-round" = "#775178", "Breeding, Nonbreeding" = "#7cad58", "Not applicable" = "gray")) +
  theme(legend.position="none",
        legend.text=element_text(size=11),
        legend.title=element_text(size=12),
        axis.title=element_text(size=18),
        axis.text = element_text(size = 14),
        plot.title=element_text(size=18),
        panel.background = element_blank(),
        #legend.position = 'none'
  )

p_raptor

p_gen<-bm +
  new_scale_color() +
  geom_count(data = gen_study, aes(x = Long, y = Lat, color = "red", alpha=0), inherit.aes = FALSE)+
  #geom_jitter(height = 10, width = 10) +
  labs(x = "", y = "",color="Sampling Periods")+
  ggtitle("General Birds") +
  scale_size_continuous(range = c(3,13),guide = "none") + 
  scale_alpha(guide = "none") + 
  scale_x_longitude(breaks = c(-110, -100)) +
  scale_y_latitude(breaks = c(50, 40, 30)) +
  #scale_color_manual(values = c("Breeding Season" = "#c25c4d","Breeding, Postbreeding Migration" = "#dda821","Nonbreeding Season" = "#337197", "Year-round" = "#775178", "Breeding, Nonbreeding" = "#7cad58", "Not applicable" = "gray")) +
  theme(legend.position="none",
        legend.text=element_text(size=11),
        legend.title=element_text(size=12),
        axis.title=element_text(size=18),
        axis.text = element_text(size = 14),
        plot.title=element_text(size=18),
        panel.background = element_blank(),
        #legend.position = 'none'
  )

p_gen

p_buow<-bm +
  new_scale_color() +
  geom_count(data = buow_study, aes(x = Long, y = Lat, color = "red", alpha=0), inherit.aes = FALSE)+
  #geom_jitter(height = 10, width = 10) +
  labs(x = "", y = "",color="Sampling Periods")+
  ggtitle("Burrowing Owl") +
  scale_size_continuous(range = c(3,13),guide = "none") + 
  scale_alpha(guide = "none") + 
  scale_x_longitude(breaks = c(-110, -100)) +
  scale_y_latitude(breaks = c(50, 40, 30)) +
  #scale_color_manual(values = c("Breeding Season" = "#c25c4d","Breeding, Postbreeding Migration" = "#dda821","Nonbreeding Season" = "#337197", "Year-round" = "#775178", "Breeding, Nonbreeding" = "#7cad58", "Not applicable" = "gray")) +
  theme(legend.position="none",
        legend.text=element_text(size=11),
        legend.title=element_text(size=12),
        axis.title=element_text(size=18),
        axis.text = element_text(size = 14),
        plot.title=element_text(size=18),
        panel.background = element_blank()
  )

p_buow

#multiplot(p_mopl,p_raptor,p_gen,p_buow,cols=2) 

grid.arrange(p_mopl,p_gen,p_raptor,p_buow,ncol=4)
###best arrangement so far. All plots in one row with no legend

leg<-get_legend(bm_leg)

ggarrange(p_mopl,p_gen,p_raptor,p_buow, nrow = 1, common.legend=TRUE,legend="bottom",legend.grob = leg)
#works!! Applies legend from basemap to entire plot



#map of study locations coded by seasonality
p_mopl<-bm0 +
  new_scale_color() +
  geom_count(data = mopl_study, aes(x = Long, y = Lat, color=factor(Sampling_Period), alpha=0), inherit.aes = FALSE)+
  #geom_jitter(height = 10, width = 10) +
  labs(x = "", y = "",color="Sampling Periods")+
  ggtitle("Mountain Plover") +
  scale_size_continuous(range = c(3,13),guide = "none") + 
  scale_alpha(guide = "none") + 
  scale_x_longitude(breaks = c(-110, -100)) +
  scale_y_latitude(breaks = c(50, 40, 30)) +
  scale_color_manual(values = c("Breeding Season" = "#c25c4d","Breeding, Postbreeding Migration" = "#dda821","Nonbreeding Season" = "#337197", "Year-round" = "#775178", "Breeding, Nonbreeding" = "#7cad58", "Not applicable" = "gray")) +
  theme(legend.position="none",
        legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.title=element_text(size=18),
        plot.title=element_text(size=20),
        panel.background = element_blank(),
        #legend.position = 'none'
  )

p_mopl

p_raptor<-bm0 +
  new_scale_color() +
  geom_count(data = raptor_study, aes(x = Long, y = Lat, color=factor(Sampling_Period), alpha=0), inherit.aes = FALSE)+
  #geom_jitter(height = 10, width = 10) +
  labs(x = "", y = "",color="Sampling Periods")+
  ggtitle("Raptors") +
  scale_size_continuous(range = c(3,13),guide = "none") + 
  scale_alpha(guide = "none") + 
  scale_x_longitude(breaks = c(-110, -100)) +
  scale_y_latitude(breaks = c(50, 40, 30)) +
  scale_color_manual(values = c("Breeding Season" = "#c25c4d","Breeding, Postbreeding Migration" = "#dda821","Nonbreeding Season" = "#337197", "Year-round" = "#775178", "Breeding, Nonbreeding" = "#7cad58", "Not applicable" = "gray")) +
  #scale_color_manual(values = c("Breeding Season" = "red","Breeding, Postbreeding Migration" = "yellow","Nonbreeding Season" = "blue", "Year-round" = "purple", "Breeding, Nonbreeding" = "green", "Not applicable" = "gray")) +
  theme(legend.position="none",
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.title=element_text(size=18),
        plot.title=element_text(size=18),
        panel.background = element_blank(),
        #legend.position = 'none'
  )

p_raptor

p_gen<-bm0 +
  new_scale_color() +
  geom_count(data = gen_study, aes(x = Long, y = Lat, color=factor(Sampling_Period), alpha=0), inherit.aes = FALSE)+
  #geom_jitter(height = 10, width = 10) +
  labs(x = "", y = "",color="Sampling Periods")+
  ggtitle("General Birds") +
  scale_size_continuous(range = c(3,13),guide = "none") + 
  scale_alpha(guide = "none") + 
  scale_x_longitude(breaks = c(-110, -100)) +
  scale_y_latitude(breaks = c(50, 40, 30)) +
  scale_color_manual(values = c("Breeding Season" = "#c25c4d","Breeding, Postbreeding Migration" = "#dda821","Nonbreeding Season" = "#337197", "Year-round" = "#775178", "Breeding, Nonbreeding" = "#7cad58", "Not applicable" = "gray")) +
  theme(legend.position="none",
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.title=element_text(size=18),
        plot.title=element_text(size=18),
        panel.background = element_blank(),
        #legend.position = 'none'
  )

p_gen

p_buow<-bm0 +
  new_scale_color() +
  geom_count(data = buow_study, aes(x = Long, y = Lat, color=factor(Sampling_Period), alpha=0), inherit.aes = FALSE)+
  #geom_jitter(height = 10, width = 10) +
  labs(x = "", y = "",color="Sampling Periods")+
  ggtitle("Burrowing Owl") +
  scale_size_continuous(range = c(3,13),guide = "none") + 
  scale_alpha(guide = "none") + 
  scale_x_longitude(breaks = c(-110, -100)) +
  scale_y_latitude(breaks = c(50, 40, 30)) +
  scale_color_manual(values = c("Breeding Season" = "#c25c4d","Breeding, Postbreeding Migration" = "#dda821","Nonbreeding Season" = "#337197", "Year-round" = "#775178", "Breeding, Nonbreeding" = "#7cad58", "Not applicable" = "gray")) +
  theme(legend.position="none",
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.title=element_text(size=18),
        plot.title=element_text(size=18),
        panel.background = element_blank()
  )

p_buow

p_dummy<-bm0 + #for legend
  new_scale_color() +
  geom_count(data = buow_study, aes(x = Long, y = Lat, color=factor(Sampling_Period), alpha=0), inherit.aes = FALSE)+
  #geom_jitter(height = 10, width = 10) +
  labs(x = "", y = "",color="Sampling Periods")+
  ggtitle("Burrowing Owl") +
  scale_size_continuous(range = c(3,13),guide = "none") + 
  scale_alpha(guide = "none") + 
  scale_x_longitude(breaks = c(-110, -100)) +
  scale_y_latitude(breaks = c(50, 40, 30)) +
  scale_color_manual(values = c("Breeding Season" = "#c25c4d","Breeding, Postbreeding Migration" = "#dda821","Nonbreeding Season" = "#337197", "Year-round" = "#775178", "Breeding, Nonbreeding" = "#7cad58", "Not applicable" = "gray")) +
  guides(color = guide_legend(override.aes = list(size = 8, shape = c(15,15,15,15,15)))) + 
  theme(legend.position="bottom",
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        axis.title=element_text(size=18),
        axis.text = element_text(size=10),
        plot.title=element_text(size=18),
        panel.background = element_blank()
  )

p_dummy

#seasonality looking good

grid.arrange(p_mopl,p_gen,p_raptor,p_buow,ncol=4)
###best arrangement so far. All plots in one row with no legend

leg<-get_legend(p_dummy)
#p_leg<-as_ggplot(leg)

ggarrange(p_mopl,p_gen,p_raptor,p_buow, nrow = 1, common.legend=TRUE,legend="bottom",legend.grob = leg)
#works!! Applies legend from BUOW to entire plot


#Effect type breakdown: % of studies on occurrence, nesting, diet, etc.

summary(rev$Effect_Type)

summary(mopl_rev$Effect_Type)
summary(buow_rev$Effect_Type)
summary(raptor_rev$Effect_Type)
summary(gen_rev$Effect_Type)

cbPalette <- c("#D55E00","#F0E442","#0072B2","#009E73","#999999")

levels(rev$Bird_Category) <- c('Burrowing Owl','General','Mountain Plover','Raptor')

#Figure 5
ggplot(rev) +
  geom_bar(aes(x=Effect_Type, fill=Effect_Type))+
  geom_text(stat='count', aes(x=Effect_Type, label=after_stat(count)),vjust=-1, size = 6) + 
  #facet_grid(~factor(rev$Bird_Category, levels=c("MOPL","BUOW","Raptor","General")))+
  facet_grid(~factor(rev$Bird_Category, levels=c("Mountain Plover","General","Raptor","Burrowing Owl")))+ #horizontal
  labs(x = "", y = "# of Analyses",color="Effect Type")+
  #ggtitle("Analyses by Bird Category") +
  scale_fill_manual("PD Effect Type", values = cbPalette) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title=element_text(size=20),
        legend.text=element_text(size=16),
        strip.text = element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.y = element_text(size=18,color="black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
  )

#^^^best plot

