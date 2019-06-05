library(ggplot2)
library(gtalibrary)

load("ggplot-panel.Rdata")

## The problem:
## I want to plot the variable 'target.status' in the top row, but not in the bottom row. 
## Instead, I want the bottom row to plot the variable 'mast'.
## At the same time, both variables need a legend.


## Trick #1: 
## Creating a joint variable is necessary so I can plot it all in a single ggplot command.
master$plotted.variable=master$target.status
master$plotted.variable[master$which.panel==2]=master$mast[master$which.panel==2]
master$plotted.variable=tolower(master$plotted.variable)

# sidenote: I use the first colour (navy blue) twice because the variables contain the same data, despite the different name.
plot.colours=c(gta_colour$qualitative[c(1,1,2,4,3,7)])


## Trick #2:
## I assign a redundant aes parameter ('size').
## It's redundant because as I mute it by setting identical values for all values of plotted.variable in scale_size_manual.
## However, this redundant aes parameter allows me to plot a second legend.
## Using 'breaks' in scale_colour_manual and scale_size_manual lets restrict the values shown in those legends.
## Thanks to 'override.aes', I can tailor the optics of both legends to my needs.

full.panel=
  ggplot(master, 
         aes(x=year, 
             y=value, 
             group=plotted.variable, 
             colour=plotted.variable, 
             size=plotted.variable)
  )+
  geom_line()+
  facet_grid(which.panel ~ importer)+
  gta_theme(x.bottom.angle =90)+
  theme(axis.text.x = element_text(vjust=.5),
        strip.background.x = element_rect(fill = gta_colour$panel.bg),
        strip.background.y = element_blank(),  ## This removes the strips from the vertical axes.
        strip.text.y = element_blank())+
  scale_y_continuous(limits=c(0,1), 
                     sec.axis = dup_axis())+
  scale_color_manual(values=plot.colours, 
                     breaks=c("any","targeted","untargeted"))+ ## 'breaks' specifies which values are displayed in the legend
  scale_size_manual(values=rep(1.2, length(unique(master$plotted.variable))), 
                    breaks=c("all impediments","subsidies to\nimport-competing firms","import tariffs"))+
  labs(y="Share of imports affected",
       x="", 
       colour="targeted or not\n(top row)", 
       size="instrument used\n(bottom row)")+
  guides(size=guide_legend(order=0, # I can set the order of the legend from left to right
                           title.position = "top",
                           nrow=3,
                           override.aes = list(size=1.2,
                                               colour=c(plot.colours[c(1,4,3)]))), ## allows me to change size of the symbol and its colours etc as I like
         color=guide_legend(order=1, 
                            title.position = "top",
                            nrow=3,
                            override.aes = list(size=1.2))
  )

full.panel

