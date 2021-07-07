############ R Script ##############

#load the required packages
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(ggplot2)

#retrieve the map of Africa from the cloud
africa = world %>% 
  filter(continent == "Africa", !is.na(iso_a2)) %>% 
  left_join(worldbank_df, by = "iso_a2") %>% 
  dplyr::select(name, subregion, gdpPercap, HDI, pop_growth) %>% 
  st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25")

# test
# check the default colour
ggplot(africa) +
  geom_sf(aes(geometry = geom, fill = HDI))

#change colour form a particular colour to another
ggplot(africa) +
  geom_sf(aes(geometry = geom, fill = HDI))+
  scale_fill_gradient(low = "blue", high = "green")

#Parse the map (africa) to another file
kelch13 <- africa

#check the kelch in the "Environment" to how you can concatenate your values

#concatenate all your parameters (and values) according to country list
Unique_SNP <- c(23,NA,17,1,40,19,NA,NA,NA,NA,NA,NA,37,25,NA,NA,9,20,3,4,47,1,2,NA,NA,2,NA,1,NA,2,14,1,NA,2,NA,5,NA,NA,1,NA,NA,6,NA,NA,NA,5,NA,24,20,NA)
A578S_p <- c(3,NA,9,NA,21,NA,NA,NA,NA,1,NA,NA,2,2,NA,NA,2,1,NA,5,13,NA,NA,NA,NA,3,NA,NA,NA,1,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,14,2,NA)
Total_SNP <- c(27,NA,25,1,116,26,NA,NA,NA,NA,NA,NA,116,28,NA,NA,10,25,3,9,107,1,2,NA,NA,6,NA,1,NA,2,21,1,NA,4,NA,8,NA,NA,1,NA,NA,93,NA,NA,NA,15,NA,50,41,NA)
Val_Ass_SNP <- c(2,NA,NA,NA,2,NA,NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,4,NA)
ATPase6_Unique_SNP <- c(3,NA,10,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,20,11,NA,NA,NA,NA,NA,NA,NA,NA,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA)
ATPase6_Total_SNP <- c(30,NA,23,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,72,34,NA,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,9,NA,NA,NA,NA)
Pfcoronin_Unique_SNP <- c(NA,NA,10,NA,1,NA,NA,NA,NA,NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,5,NA,NA,NA,NA,NA,NA,NA,NA,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
Pfcoronin_Total_SNP <- c(NA,NA,49,NA,4,NA,NA,NA,NA,NA,NA,NA,53,NA,NA,NA,NA,NA,NA,NA,16,NA,NA,NA,NA,NA,NA,NA,NA,12,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

#add columns to the kelch13 file (the columns are the parameters above)
kelch13$Unique_SNPs <- Unique_SNP
kelch13$A578S_prevalence <- A578S_p
kelch13$Total_SNP <- Total_SNP
kelch13$Val_and_Ass <- Val_Ass_SNP
kelch13$ATPase6_Unique_SNP <- ATPase6_Unique_SNP
kelch13$ATPase6_Total_SNP <- ATPase6_Total_SNP
kelch13$Pfcoronin_Unique_SNP <- Pfcoronin_Unique_SNP
kelch13$Pfcoronin_Total_SNP <- Pfcoronin_Total_SNP

#check the new kelch13 (kelch13 with new columns) in the "Environment"


#start plotting

#plot the "pfk13 Validated and associated SNPs
plot(kelch13["Val_and_Ass"])

#plot the pfk13 A578S prevalence
plot(kelch13["A578S_prevalence"])


#plot pfk13 Unique SNP   #### Figure 6a #######
tm_shape(kelch13) + tm_polygons("Unique_SNPs",
                                style = "fixed",
                                palette = colorRampPalette(c("yellow", "gold",
                                                             "darkorange", "pink", "deeppink", "magenta", "purple", "blue", "blue4"))(30),
                                breaks = c(1, 5, 10, 15, 20, 25, 30, 40, 50))

# plot pfk13 A578S prevalence
tm_shape(kelch13) + tm_polygons("A578S_prevalence",
                                style = "fixed",
                                palette = colorRampPalette(c("green", "yellow",
                                                             "pink", "magenta", "purple",
                                                             "blue"))(30),
                                breaks = c(1,2,3, 6, 9, 12, 15, 18, 21))


#plot pfk13 Total SNP         ######## Figure 6b ##########
tm_shape(kelch13) + tm_polygons("Total_SNP",
                                style = "fixed",
                                palette = colorRampPalette(c("aliceblue", "green", "greenyellow", "yellow",
                                                             "pink", "deeppink", "purple", "blue", "blue4"))(30),
                                breaks = c(1, 5, 10, 20, 30, 50, 70, 90, 110, 120 ))


#plot pfk13 val and Ass
tm_shape(kelch13) + tm_polygons("Val_and_Ass",
                                style = "fixed",
                                palette = colorRampPalette(c("yellow", "green", "blue"))(30),
                                breaks = c(1,2,3,4))

#ATPase6 plots

#ATPase6_Unique_SNP
a = tm_shape(kelch13) + tm_polygons("ATPase6_Unique_SNP",
                                    
                                    style = "fixed",
                                    palette = colorRampPalette(c("yellow", "green", "green", "blue"))(30),
                                    breaks = c(0, 10, 15, 20))

#view the plot
a

#ATPase6_Total_SNP    ####### Figure 8 ###########
b = tm_shape(kelch13) + tm_polygons("ATPase6_Total_SNP",
                                    style = "fixed",
                                    palette = colorRampPalette(c("yellow", "green","darkorange", "pink", "deeppink", "magenta", "purple", "blue"))(30),
                                    breaks = c(0, 10, 20, 30, 40, 70, 80))
#view the plot
b

#merge the two plots above (ATPase6_Unique_SNP and ATPase6_Total_SNP)
tmap_arrange(a,b)


#Pfcoronin plots

#Pfcoronin_Unique_SNP     ###### Figure 9 #########
c = tm_shape(kelch13) + tm_polygons("Pfcoronin_Unique_SNP",
                                    style = "fixed",
                                    palette = colorRampPalette(c("yellow", "green", "green", "blue"))(30),
                                    breaks = c(0,2,5,10))
#view the plot
c

#pfcoronin_Total_SNP
d = tm_shape(kelch13) + tm_polygons("Pfcoronin_Total_SNP",
                                    style = "fixed",
                                    palette = colorRampPalette(c("yellow", "green", "pink", "deeppink", "magenta", "purple", "blue"))(30),
                                    breaks = c(0, 10,15, 20, 30, 40, 50, 60))

#view the plot
d

#add c and d together
tmap_arrange(c, d)

############### The End############################


#Extra! #Etxtra!! #Extra!!!

tm_shape(kelch13) + tm_polygons("Unique_SNPs",
                                style = "fixed",
                                palette = colorRampPalette(c("yellow", "gold",
                                                             "darkorange", "pink", "deeppink", "magenta", "purple", "blue", "blue4"))(30),
                                breaks = c(1, 5, 10, 15, 20, 25, 30, 40 ))


tm_shape(kelch13) + tm_polygons("Unique_SNPs",
                                style = "fixed",
                                palette = colorRampPalette(c("yellow", "gold",
                                                             "darkorange", "pink", "deeppink", "magenta", "purple", "blue", "blue4"))(30),
                                breaks = c(1, 5, 10, 15, 20, 25, 30, 40 ))

plot(kelch13["Pfcoronin_Total_SNP"])
tm_shape(kelch13) + tm_polygons(col="Unique_SNPs", border.col = "white")

tm_shape(kelch13)+
  tm_polygons("Unique_SNPs", style = "quantile", n = 6, palette = "Spectral")
