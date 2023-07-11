#packages
library(readr)
library(dplyr)
library(plyr)
library(forcats)
library(ggplot2)
library(tidyr)
library(purrr)
library(faraway)
library(cowplot)
library(reshape2)
library(corrplot)
library(tibble)
library(scales)
library(RorBrewer)
library(ggthemes)
library(stringr)
library(gridExtra)
library(patchwork)
library(gghighlight)
library(ggdark)
library(viridis)
library(DT)
library(plotly)

#load data
site_079 <- (read_csv(file = "./data/site_079_summa_data.csv")) %>% 
  slice(2:5, 7:8) %>%
  select(1,7:67)

#pivot data for plots
p.analytes <- site_079 %>% 
  pivot_longer(- ID,
               names_to = "analyte",
               values_to = "conc.")
#VOC classes as objects
alc <- c("isopropanol", "butanol")
ald <- c("acetaldehyde")
aro <- c("cyclopentane", "cyclohexane", "methylcyclohexane")
btx <- c("benzene", "toluene", "ethylbenzene", "m+p-xylene", "o-xylene",
          "isopropylbenzene", "n-propylbenzene", "3-ethyltoluene",
          "4-ethyltoluene", "1,3,5-trimethylbenzene", "2-ethyltoluene",
          "1,2,4-trimethylbenzene", "1,2,3-trimethylbenzene",
          "1,3-diethylbenzene", "1,4-diethylbenzene")
chl <- c("C2HCl3", "C2Cl4")
kt <- c("acetone")
oth <- c("isoprene", "styrene", "acetonitrile", "methylethylketone",
           "a-pinene", "b-pinene", "limonene", "camphene", "methane")
stc <- c("ethane", "propane", "i-butane", "n-butane", "i-pentane",
                    "n-pentane", "n-hexane", "2,4 dimethylpentane", "n-heptane",
                    "2,3-dimethylpentane", "2-methylhexane", "3-methylhexane",
                    "2,2,4-trimethylpentane", "2,3,4-trimethylpentane",
                    "2-methylheptane", "3-methylheptane", "n-octane", "n-nonane",
                    "n-decane", "ethene", "propene", "t-2-butene", "1-butene",
                    "c-2-butene", "t-2-pentene", "1-pentene", "cis-2-pentene",
                    "ethyne")
#add voc categories to dfs
voc <- p.analytes %>% 
  mutate(category = case_when(p.analytes$analyte %in% alc ~ "Alc.",
                              p.analytes$analyte %in% ald ~ "Ald.",
                              p.analytes$analyte %in% stc ~ "Stc.",
                              p.analytes$analyte %in% aro ~ "Aro.",
                              p.analytes$analyte %in% btx ~ "Btx.",
                              p.analytes$analyte %in% chl ~ "Chl.",
                              p.analytes$analyte %in% kt ~ "Ktn.",
                              p.analytes$analyte %in% oth ~ "Oth."))

#create outdoor group
outdoor <- voc %>% 
  filter(ID =="outdoor")
#change conc. to numeric
voc$conc. <- as.numeric(as.character(voc$conc.))

outdoor$conc. <- as.numeric(as.character(outdoor$conc.))

head(voc)
head(outdoor)

#calculate ratios
voc <- voc %>%
  group_by(ID, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = round(as.numeric(voc$conc.)/as.numeric(outdoor$conc.), 2))

#subgroups,
#indoor
indoor <- voc %>% 
  filter(ID != "outdoor")
#food court all
food_court <- voc %>% 
  filter(ID == "fc south" | ID == "fc north")
#food court south
food_courtS <- voc %>% 
  filter(ID == "fc south")
#food court north
food_courtN <- voc %>% 
  filter(ID == "fc north")
#south balcony
south_balcony <- voc %>% 
  filter(ID == "south balcony")
#south entrance
south_entrance <- voc %>% 
  filter(ID == "south entrance")
#clinic waiting
clinic_waiting <- voc %>% 
  filter(ID == "clinic waiting")
#highlight methane
methane <- voc %>% 
  filter(analyte == "methane")

#data table
#for data table
voc2 <- p.analytes %>% 
  mutate(category = case_when(p.analytes$analyte %in% alc ~ "Alcohol",
                              p.analytes$analyte %in% ald ~ "Aldehyde",
                              p.analytes$analyte %in% stc ~ "Straight Chain",
                              p.analytes$analyte %in% aro ~ "Aromatic",
                              p.analytes$analyte %in% btx ~ "Btex",
                              p.analytes$analyte %in% chl ~ "Chlorinated",
                              p.analytes$analyte %in% kt ~ "Ketone",
                              p.analytes$analyte %in% oth ~ "Other"))
voc2 <- voc2 %>%
  group_by(ID, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = round(as.numeric(voc$conc.)/as.numeric(outdoor$conc.), 2))

datatable(voc2, colnames = c("Location", "Analyte", "Concentration",
                             "Category", "Outdoor Ratio"),
          options = list(pageLength = 10), rownames = FALSE)

#top 5 ratios 
# goofy <- clinic_waiting %>% 
#   group_by(analyte) %>% 
#   arrange(desc(od_ratio))
# 
# goofy <- top_n(ungroup(goofy), 5, od_ratio)
 

#plot all VOCs at site

voc_plot <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_calc() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  scale_colour_tableau() +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
voc_plot


#plotly output but y axis not formatted correctly and don't know how to fix
#WHEN ADDING THIS TO RMD MAKE SURE TO CHANGE THE DF TO INLCUDE LONG CAT NAMES
voc_plot2 <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID,
                            text = paste("Analyte: ", analyte,
                                         "<br> Conc. :", conc.,
                                         "<br> Class: ", category))) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_calc() + #try using different themes here
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  scale_colour_tableau() +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site 079: Mango House Summa Cannister Deployment")
ggplotly(voc_plot2, tooltip = "text")


#combo__plot 
# add after 'group = ID' if want the line thicker" , linewidth = 1, alpha = 0.5
# voc_combo_col <- ggplot(indoor, aes(fill = ID, x = analyte, y = conc.)) +
#   geom_col(position = position_dodge(1.5)) +
#   geom_line(data = outdoor, aes(x = analyte, y =  conc.,
#                                 group = ID)) +
#   #guides(linewidth = "none", alpha = "none") +
#   scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# voc_combo_col
#combo point plot
voc_combo_point <- ggplot(indoor, aes(color = ID, x = analyte, y = conc.)) +
  geom_point(data = indoor, aes(x = analyte, y = conc.),
             shape = 18, alpha = 0.5) +
  geom_line(data = outdoor, aes(x = analyte, y =  conc.,
                                group = ID, alpha = 0.5)) +
  guides(alpha = "none") +
  xlab("Analytes") +
  ylab(expression(atop("Concentration",paste("(VOC ppbv or methane ppmv)")))) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 3, angle = 45, hjust = 1)) +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
voc_combo_point
#facet wrap by location
loc_fctw <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc.)) +
  geom_point(color = "#50C878", shape = 18, alpha = 0.5) +
  geom_point(data = methane, aes(x = analyte, y = conc.,
                                 color ="red")) +
  guides(size = "none", color = "none") +
  xlab("Analytes") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  facet_wrap(~ID, scales = "free_y") +
  theme(axis.text.x = element_text(size = 3, angle = 45, hjust = 1)) +
  ggtitle("Site 079 Summa Cannister Deployment",
            "Grouped by Cannister location")
loc_fctw

#Two plots together not joined
voc_plot +
  loc_fctw +
  plot_layout(nrow = 2, heights = c(1, 2))


#facet grid by analyte
voc_fctg <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_point(shape = 18, alpha = 0.5) +
  geom_point(data = methane, aes(x = analyte, y = conc.)) +
  guides(size = "none") +
  facet_grid(~ category, scales = "free_x") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  facet_grid(.~ category, scales = "free", switch = "x", space = "free_x") +
  theme(strip.placement = "outside", strip.text = element_text(size  = 8),
        strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  labs(x = "Analytes Grouped in VOC Categories",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) 
voc_fctg
#facet grid
indoor_fctg <- ggplot(indoor, aes(color = ID, x = analyte, y = conc.)) +
  geom_point(data = indoor, aes(x = reorder(analyte, conc.), y = conc.),
             size = 3, shape = 18, alpha = 0.5) +
  facet_grid(.~ category, scales = "free", space = "free") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab(expression(atop("Concentration",paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Grouped by Analyte Class")
indoor_fctg
#plots for each room
#outdoor
od_plot <- outdoor %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
  geom_point(color = "#50C878", shape = 18, size = 3) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme(axis.text.x = element_text(size = 3, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Mango House Outdoor (rooftop)")
od_plot
#clinic waiting
cw_plot <- clinic_waiting %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
  geom_point(color = "#50C878", shape = 18, size = 3) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme(axis.text.x = element_text(size = 3, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Mango House Clinic Waiting Room")
cw_plot
#food court
fc_plot <- food_court %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
  geom_point(shape = 18, size = 1, alpha = 0.5) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme(axis.text.x = element_text(size = 3, angle = 45, hjust = 1)) +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Mango House Food Court",
          "N and S ends of space")
fc_plot
#south balcony
sb_plot <- south_balcony %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
  geom_point(color = "#50C878", shape = 18, size = 3) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme(axis.text.x = element_text(size = 3, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Mango House South Balcony",
          "2nd Floor above Food Court")
sb_plot
#south entrance
se_plot <- south_entrance %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
  geom_point(color = "#50C878", shape = 18, size = 3) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme(axis.text.x = element_text(size = 3, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Mango House Lobby")
se_plot

#indoor, outdoor ratio 
#without facet
ratio_plot <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                              y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 1, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Indoor to Outdoor Ratios") +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
ratio_plot

#plotly ratio plot
ratio_plotly <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                              y = od_ratio, color = ID),
                              text = paste("Analyte: ", voc$analyte,
                                           "<br> Conc. :", voc$od_ratio,
                                           "<br> Class: ", voc$category)) +
  geom_point(shape = 18, size = 1, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Indoor to Outdoor Ratios") +
  ggtitle("Site 079 Summa Cannister Deployment: Feb. 16 - Mar. 2, 2023. Aurora, CO")
ggplotly(ratio_plotly)

#with facet grid
fctg_ratio_plot <- ggplot(indoor) +
  geom_point(aes(x = reorder(analyte, od_ratio), y = od_ratio, color = ID),
             size = 1, shape = 18, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.,
  #                                 size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  facet_grid(.~ category, scales = "free", switch = "x", space = "free_x") +
  theme(strip.placement = "outside", strip.text = element_text(size  = 8),
        strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 3, angle = 45, hjust = 1)) +
  labs(x = "Analytes Grouped in VOC Categories", y = "outdoor ratio")
fctg_ratio_plot

#ratio combo plot
voc_nofacet <- voc %>% 
  ggplot() +
  geom_point(aes(x = reorder(analyte, od_ratio), y = od_ratio, color = ID),
             alpha = 0.5,
             subset(voc, ID != "outdoor")) +
  geom_point(data = methane, aes(x = analyte, y = conc.,
                                 size = 1.5)) +
  geom_line(aes(analyte, conc., group = ID, color = ID),
            subset(voc, ID == "outdoor")) +
  guides(size = "none", alpha = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Analytes Grouped in VOC Categories", y = "outdoor ratio")
voc_nofacet

#create object of the top 5 analytes for the indoor locations then plot
#clinic waiting
cw_top <- clinic_waiting %>% 
  group_by(analyte) %>% 
  arrange(desc(od_ratio))
cw_top <- top_n(ungroup(cw_top), 5, od_ratio) 

p_cw_top <- cw_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, od_ratio), y = od_ratio),
           stat = "identity", fill = "darkblue") +
  labs(x = "clinic waiting", y = "") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p_cw_top

#food court north
fcn_top <- food_courtN %>% 
  group_by(analyte) %>% 
  arrange(desc(od_ratio))
fcn_top <- top_n(ungroup(fcn_top), 5, od_ratio)

p_fcn_top <- fcn_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, od_ratio), y = od_ratio),
           stat = "identity", fill = "darkblue") +
  labs(x = "food court N", y = "") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p_fcn_top

#food court south
fcs_top <- food_courtS %>% 
  group_by(analyte) %>% 
  arrange(desc(od_ratio))
fcs_top <- top_n(ungroup(fcs_top), 5, od_ratio)

p_fcs_top <- fcs_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, od_ratio), y = od_ratio),
           stat = "identity", fill = "darkblue") +
  labs(x = "food court S", y = "") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p_fcs_top

#south balcony
sb_top <- south_balcony %>% 
  group_by(analyte) %>% 
  arrange(desc(od_ratio))
sb_top <- top_n(ungroup(sb_top), 5, od_ratio)

p_sb_top <- sb_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, od_ratio), y = od_ratio),
           stat = "identity", fill = "darkblue") +
  labs(x = "south balcony", y = "") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p_sb_top

#south entrance
se_top <- south_entrance %>% 
  group_by(analyte) %>% 
  arrange(desc(od_ratio))
se_top <-  top_n(ungroup(se_top), 5, od_ratio)

p_se_top <- se_top %>% 
  ggplot() +
  geom_col(aes(x = reorder(analyte, od_ratio), y = od_ratio),
           fill = "darkblue") +
  labs(x = "south entrance", y = "") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p_se_top

grid.arrange(p_cw_top, p_fcn_top, p_fcs_top, p_sb_top, p_se_top,
             ncol = 3, nrow = 2,
             top = "Top 5 Analytes in Each Location", left = "Outdoor Ratio")

#with facet wrap
ratio_fctw <-  ggplot(data = indoor, aes(x = reorder(analyte, od_ratio),
                                             y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 1, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.,
  #                 alpha = 0.5)) +
  # guides(size = "none", alpha = "none") +
  facet_wrap(~ category, scales = "free_x", labeller = label_parsed) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme(strip.placement = "outside", strip.text = element_text(size  = 8),
        strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 3, angle = 45, hjust = 1)) +
  labs(x = "Analytes Grouped in VOC Categories", y = "outdoor ratio")
ratio_fctw

#correlations
cor(outdoor$conc., clinic_waiting$conc., method = "spearman")
#create a new df for correlation
voc_cor <- site_079 %>% 
  slice(-3)
#convert select rows to numeric
voc_cor[2:62] <- lapply(voc_cor[2:62], as.numeric)
#run the correlation
vcor <- cor(voc_cor[, unlist(lapply(voc_cor, is.numeric))], method = "spearman")
#create the matrix
#if working woth smaller set cann values inside of matrix using:
#addCoef.col = "black"
corrplot(vcor, method = "color", tl.col = "black", tl.cex = 0.5)
#lower
corrplot(vcor, method = "color", type = "lower")
#upper w/ diag lables
corrplot(vcor, method = "color", type = "upper", tl.col = "black",
         tl.srt = 45, tl.cex = 0.5)
#red theme
corrplot(vcor, method = "color", tl.col = "black", tl.cex = 0.5,
         col=colorRampPalette(c("blue","white","red"))(200))
#another method for correlation
library(corrr)
cor2 <- correlate(voc_cor, method = "spearman", diagonal = 1)


#AES Test Lab
#add theme in line 408 to keep axis lable angle
#point
#theme minimal
voc_plot_min <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_point(shape = 18, size = 1.5, alpha = 0.5) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 4, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Concentration",paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
voc_plot_min + 
 scale_color_brewer(palette = "Dark2")
  #scale_color_manual(values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                              # "tomato2", "midnightblue"))

#bar/col, YUCK!
voct1 <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_col(position = "dodge", fill = "lightblue", color = "darkblue") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 4, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Concentration",paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
voct1

#clinic waiting vs outdoor
goofy <- ggplot() +
  geom_point(data = clinic_waiting, aes(x = analyte, y = conc., color = "clinic_waiting")) +
  geom_point(data = outdoor, aes(x = analyte, y = conc., color = "outdoor")) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 4, angle = 45, hjust = 1)) +
  scale_color_manual(name = "Location", 
                     values = c("#0041C2", "#FFDB58"),
                     labels = c("clinic waiting", "outdoor"))
goofy +theme(
  # Change legend background color
  legend.background = element_rect(fill = "darkgray"),
  legend.key = element_rect(fill = NA, color = NA))  

#working with custom legend
donald <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  dark_theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")

#dark theme with viridis colors manual
donald +
  scale_color_manual(values = c("#fde725", "#7ad151",
                                  "#22a884", "#2a788e", "#414487",
                                  "#440154"),
                       labels = c("Clinic Waiting", "Food Court\nNorth",
                                       "Food Court\nSouth", "Outdoor",
                                       "South Balcony", "South Entrance")) +
  theme(legend.title = element_text(color = "cyan", size = 10),
                 legend.text = element_text(color = "white"))

#dark w/ lava theme
donald +
  scale_color_manual(values = c("#c33b18", "#351d19",
                                "#f28525", "#fbd449", "#ae5c5f",
                                "#7f3029"),
                     labels = c("Clinic Waiting", "Food Court North",
                                "Food Court South", "Outdoor",
                                "South Balcony", "South Entrance")) +
  theme(legend.title = element_text(color = "#6c7476", size = 10),
        legend.text = element_text(color = "#f3d18c"))

#gray lava
gr_lava <- ggplot(voc, aes(x = reorder(analyte, conc.),
                y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
gr_lava +
  scale_color_manual(values = c("#bc2e16", "#7a2a1c",
                                "#f64009", "#af6f70", "#e66d1d",
                                "#fbc117"),
                     labels = c("Clinic Waiting", "Food Court\nNorth",
                                "Food Court\nSouth", "Outdoor",
                                "South Balcony", "South Entrance")) +
  theme(legend.title = element_text(color = "#d1644b", size = 10),
        legend.text = element_text(color = "#261e1b"))


#summer theme with
summer <- ggplot(voc, aes(x = reorder(analyte, conc.),
                          y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
summer +
  scale_color_manual(values = c("#065143", "#839b5d",
                                "#df5e21", "#3f88c5", "#aab6cb",
                                "#bc2e16"),
                     labels = c("Clinic Waiting", "Food Court\nNorth",
                                "Food Court\nSouth", "Outdoor",
                                "South Balcony", "South Entrance")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black"))

#alaska air theme
summer +
  scale_color_manual(values = c("#065143", "#b1d887",
                                "#00b2d6", "#007cba", "#01416e",
                                "#aaaaaa"),
                     labels = c("Clinic Waiting", "Food Court\nNorth",
                                "Food Court\nSouth", "Outdoor",
                                "South Balcony", "South Entrance")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black"))

#70s theme
summer +
  scale_color_manual(values = c("#FFE5B4", "#CD853F",
                                "#EB5406", "#007cba", "#191970",
                                "#659EC7"),
                     labels = c("Clinic Waiting", "Food Court\nNorth",
                                "Food Court\nSouth", "Outdoor",
                                "South Balcony", "South Entrance")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black"))

#summer colorful
summer +
  scale_color_manual(values = c("#3090C7", "#FFE87C",
                                "#A74AC7", "#12AD2B", "#C04000",
                                "#F98B88"),
                     labels = c("Clinic Waiting", "Food Court\nNorth",
                                "Food Court\nSouth", "Outdoor",
                                "South Balcony", "South Entrance")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black"))


#pink foam
pink_foam <- ggplot(voc, aes(x = reorder(analyte, conc.),
                           y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
pink_foam +
  scale_color_manual(values = c("#54bebe", "#006A4E",
                                "#e27c7c", "#503f3f", "#df979e",
                                "#c80064"),
                     labels = c("Clinic Waiting", "Food Court\nNorth",
                                "Food Court\nSouth", "Outdoor",
                                "South Balcony", "South Entrance")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "#261e1b"))

#ratio w/ yellowrainbow
yellowrainbow <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                                 y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  #theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Indoor to Outdoor Ratios"))) +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")

yellowrainbow + scale_color_manual(values = c("#EDDA74", "#ffc501",
                                              "#ff9801", "#037d50", "#024b30"),
                                   labels = c("Clinic Waiting", "Food Court\nNorth",
                                              "Food Court\nSouth", 
                                              "South Balcony", "South Entrance")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "#261e1b"))

#icecream
yellowrainbow + scale_color_manual(values = c("orchid", "chocolate4",
                                              "goldenrod2", "tomato2", "midnightblue"),
                                   labels = c("Clinic Waiting", "Food Court\nNorth",
                                              "Food Court\nSouth", 
                                              "South Balcony", "South Entrance")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "#261e1b"))

#viridis discrete
yellowrainbow + scale_colour_viridis(discrete = TRUE,
                                   labels = c("Clinic Waiting", "Food Court\nNorth",
                                              "Food Court\nSouth", 
                                              "South Balcony", "South Entrance")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "#261e1b"))

#viridis magama
viridismagma <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                                    y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  dark_theme_gray() +
  scale_color_viridis(option = "plasma", discrete = TRUE) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Indoor to Outdoor Ratios"))) +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
viridismagma

#google docs
googledocs <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                                   y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_gdocs() +
  scale_color_gdocs() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Indoor to Outdoor Ratios"))) +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
googledocs
#tableau
tableau <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                                 y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_igray() +
  scale_color_tableau() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Indoor to Outdoor Ratios"))) +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
tableau

#economist
eco <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                      y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_economist() +
  scale_color_economist() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 0)) +
  xlab("Analytes") +
  ylab(expression(atop("Indoor to Outdoor Ratios"))) +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")

eco + theme(legend.key.size = unit(2, "cm"))

#solarized
solarized <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                          y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_solarized() +
  scale_color_solarized() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Indoor to Outdoor Ratios") +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
solarized + theme(
  # Change legend background color
  legend.background = element_rect(fill = NA),
  legend.key = element_rect(fill = NA, color = NA))  

#solarized plotly
solarized2 <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                                y = od_ratio, color = ID,
                                text = paste("Analyte: ", analyte,
                                             "<br> Conc. :", conc.,
                                             "<br> Class: ", category))) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_solarized() +
  scale_color_solarized() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Indoor to Outdoor Ratios") +
  ggtitle("Site 079 Summa Cannister Deployment",
          "Feb. 16 - Mar. 2, 2023. Aurora, CO")
solarized2 + theme(
  # Change legend background color
  legend.background = element_rect(fill = NA),
  legend.key = element_rect(fill = NA, color = NA))
ggplotly(solarized2, tooltip = "text")
