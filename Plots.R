#GDT-
my_data<- read.csv("BehrlData.csv")
p <- ggplot(my_data, aes(x=Group, y=GDT, color=Group)) + 
  geom_boxplot()+theme_bw() + stat_summary(fun.y=mean, geom="point", shape=8, size=4) +   #Adding mean point
  geom_jitter(shape=16, position=position_jitter(0.2))+
  scale_color_manual(values=c("blue","red"))+
  ggplot2::labs(x = "Group", y = "GDT(ms)")+
  theme(legend.position="none")

#SPIN
#ClusteredBoxPlot#
my_data<- read.csv("BehrlData.csv")
p <- ggplot(my_data, aes(x=Group, y=SPIN, color=Group)) + 
  geom_boxplot()+theme_bw() + stat_summary(fun.y=mean, geom="point", shape=8, size=4) +   #Adding mean point
  geom_jitter(shape=16, position=position_jitter(0.2))+
  scale_color_manual(values=c("blue","red"))+
  ggplot2::labs(x = "Group", y = "SPeechInNoise Score (%)")+
  theme(legend.position="none")

#HalfViolinPlot#
my_data<- read.csv("BehrlData.csv")

ggplot2::ggplot(data = my_data, mapping = ggplot2::aes(x = Group, y = SPIN)) +
  geom_flat_violin() 
ggplot(mapping:aes)

# adding other components to the underlying flat violin
ggplot2::ggplot(data = my_data,
                mapping = ggplot2::aes(x = Group, y = SPIN, fill = Group)) +
  geom_flat_violin(scale = "count", trim = FALSE) +
  ggplot2::stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "pointrange",
    position = ggplot2::position_nudge(x = 0.05, y = 0)
  ) +
  ggplot2::geom_dotplot(
    binaxis = "y",
    dotsize = 0.5,
    stackdir = "down",
    binwidth = 1,
    position = ggplot2::position_nudge(x = -0.025, y = 0)
  ) +
  
  ggplot2::labs(x = "Group", y = "SPeech In Noise Score (%)") +
  ggstatsplot::theme_mprl() +
  ggplot2::theme(legend.position = "none") +
  scale_fill_manual(values=c("#56B4F9","#FF7777"))

###Options for color
scale_fill_manual(breaks = c("2", "1"), 
                  values=c("red", "blue"))
scale_fill_manual(breaks = c("2", "1"), 
                  values=c("red", "grey"))
scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1"))



#HalfViolin plot

#Example-
# trying out only the geom_flat_violin function
ggplot2::ggplot(data = iris, mapping = ggplot2::aes(x = Species, y = Sepal.Length)) +
  geom_flat_violin() 

# adding other components to the underlying flat violin
ggplot2::ggplot(data = iris,
                mapping = ggplot2::aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_flat_violin(scale = "count", trim = FALSE) +
  ggplot2::stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "pointrange",
    position = ggplot2::position_nudge(x = 0.05, y = 0)
  ) +
  ggplot2::geom_dotplot(
    binaxis = "y",
    dotsize = 0.5,
    stackdir = "down",
    binwidth = 0.1,
    position = ggplot2::position_nudge(x = -0.025, y = 0)
  ) +
  ggplot2::labs(x = "Species", y = "Sepal length (cm)") +
  ggstatsplot::theme_mprl() +
  ggplot2::theme(legend.position = "none")


###### Adapting it to my_data...... ######
my_data<- read.csv("LatencySlope.csv")

ggplot2::ggplot(data = my_data, mapping = ggplot2::aes(x = Group, y = A_Sl)) +
  geom_flat_violin() 


# adding other components to the underlying flat violin
ggplot2::ggplot(data = my_data,
                mapping = ggplot2::aes(x = Group, y = A_Sl, fill = Group)) +
  geom_flat_violin(scale = "count", trim = FALSE) +
  ggplot2::stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "pointrange",
    position = ggplot2::position_nudge(x = 0.05, y = 0)
  ) +
  ggplot2::geom_dotplot(
    binaxis = "y",
    dotsize = 7,
    stackdir = "down",
    binwidth = 0.0005,
    position = ggplot2::position_nudge(x = -0.025, y = 0)
  ) +
  
  ggplot2::labs(x = "Group", y = "Amplitude Slope (µs/dB)") +
  ggstatsplot::theme_mprl() +
  ggplot2::theme(legend.position = "none") +
  scale_fill_manual(values=c("#56B4F9","#FF7777"))

###Options for color
scale_fill_manual(breaks = c("2", "1"), 
                  values=c("red", "blue"))
scale_fill_manual(breaks = c("2", "1"), 
                  values=c("red", "grey"))
scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1"))


### Clustered Box plot###
my_data<- read.csv("LatencySlope.csv")

###Try and change x axis order!!!
cyl_table <- table(my_data$Condition)
cyl_levels <- names(cyl_table)[order(cyl_table)]
my_data$Condition2 <- factor(my_data$Condition, levels = cyl_levels)

my_data$Condition3 <- with(my_data, reorder(Condition, Condition, function(x) -length(x)))

scale_x_discrete(limits= my_data$Condition)


p <- ggplot(my_data, aes(x=Condition, y=TMTF, color=GroupB)) + 
  geom_boxplot()+theme_bw() + stat_summary(fun.y=mean, geom="point", shape=8, size=4) +   #Adding mean point
  geom_jitter(shape=16, position=position_jitter(0.1))+
  scale_color_manual(values=c("blue","red"))+
  ggplot2::labs(x = "ModulationFrequeny", y = "ModulationThreshold(20logm)")+
  theme(legend.position="top")

