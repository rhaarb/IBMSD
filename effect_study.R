### check effects of interviewer measurement correction on Observeds
### to run this script: data, fit1, corr_obs

require(tidyverse)
require(reshape2)

# NIDS
if (dataset.name=="nids_"){
  data <- data %>% mutate(subdistrict=cluster)
}

## add corrections to data
df <- data %>% 
  ungroup %>% 
  mutate(csbp = corr_obs(fit1, data)) 


## subdistrict
loc.df <- df %>% # create location-specific Observeds
  group_by(subdistrict) %>% 
  summarize(n = n(), Observed = mean(sbp > 140), Corrected = mean(csbp > 140)) %>% 
  mutate(change = abs(Observed - Corrected)) %>% 
  arrange(desc(Observed)) %>%
  filter(n > 20) 


  for (i in 1:4) {
      qtil_vec <- quantile(loc.df$change, c(.5,.7,.9,.99))
      loc.df.q <- loc.df %>% filter(change>qtil_vec[i])
    
      loc.df.q$subdistrict <- factor(loc.df.q$subdistrict, levels = loc.df.q$subdistrict)
      loc.df.q <- cbind(melt(loc.df.q[,-2]),loc.df.q[,2])
      loc.df.q <- loc.df.q %>% filter(variable!="change")
    
    # plot Observed per subdistrict 
  
      if (dataset.name!="nids_"){
        xaxis.label="Subdistrict"}
      
      if (dataset.name=="nids_"){
        xaxis.label="Cluster"}
  
      subdistp <- ggplot(loc.df.q, aes(x=subdistrict, y=value, group=variable, color=variable))+
                    geom_line(aes(group = subdistrict),size=.35,show.legend = FALSE)+
                    geom_point()+
                    theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=4),
                          axis.text.x=element_blank(),
                          panel.background = element_rect(fill = "white",
                                                          colour = "white",
                                                          size = 0.5, linetype = "solid"),
                          #panel.grid.major.y = element_blank(),    
                          panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                                          colour = "grey"), 
                          panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                                          colour = "grey"),
                          legend.position = "top")+
                    scale_color_manual(values=c("#999999", "#BA2100"))+
                    xlab(xaxis.label)+
                    ylab("")+
                    guides(color=guide_legend(title=paste0("Prevalence, ",names(qtil_vec[i]),
                                                           " percentile")))
    
    
    subdistp
    
    plot.name <- paste0("subdistrict_percentile_",substr(names(qtil_vec[i]),1,2))
    directory.and.file.name <- paste0(path.plots,dataset.name,plot.name,".pdf")
    pdf(directory.and.file.name,width=10,height=5)
    print(subdistp)
    dev.off()
  }

loc.df <- df %>% # create location-specific Observeds
  group_by(subdistrict) %>% 
  summarize(n = n(), Observed = mean(sbp > 140), Corrected = mean(csbp > 140)) %>% 
  mutate(change = abs(Observed - Corrected)) %>% 
  arrange(desc(Observed)) %>%
  filter(n > 20)

# print table
if (dataset.name!="lasiw1_"){
  loc.df %>% print(n = Inf) 
}

if (dataset.name!="lasiw1_"){
  loc <- loc.df[which.max(loc.df$change),]$subdistrict # largest effect 
}
if (dataset.name=="lasiw1_"){
  loc <- loc.df[which.max(loc.df$change),]$subdistrict # largest effect 
}

loc.df %>% filter(subdistrict == loc)

# examine subdistrict with largest effect
if (dataset.name=="iflsmerg_"){
  data.loc <- df %>% filter(subdistrict == loc) %>% select(`Observed (Subdistrict)`=sbp,
                                                           `Corrected (Subdistrict)`=csbp)
}
if (dataset.name=="nids_"){
  data.loc <- df %>% filter(subdistrict == loc) %>% select(`Observed (Cluster)`=sbp,
                                                           `Corrected (Cluster)`=csbp)
}
if (dataset.name=="lasiw1_"){
  capture.output(data.loc <- df %>% filter(subdistrict == loc) %>% select(`Observed (Subdistrict)`=sbp,
                                                                          `Corrected (Subdistrict)`=csbp),
                 file=nullfile())
}

densp <- data %>% ungroup %>% select(value=sbp)
densp[,"variable"] <- "Observed (Population)"

densp <- rbind(densp,melt(data.loc))
if (dataset.name=="iflsmerg_"){
  densp$variable <- factor(densp$variable, levels=c("Corrected (Subdistrict)", "Observed (Subdistrict)", "Observed (Population)"))
}
if (dataset.name=="nids_"){
  densp$variable <- factor(densp$variable, levels=c("Corrected (Cluster)", "Observed (Cluster)", "Observed (Population)"))
}


sdmep <- ggplot(data=densp, aes(x=value, group=factor(variable), color=factor(variable)))+
            geom_density(size=1.1)+
            geom_vline(xintercept = 140, linetype = "dashed")+
            theme(panel.background = element_rect(fill = "white",
                                                  colour = "white",
                                                  size = 0.5, linetype = "solid"),
                  #panel.grid.major.y = element_blank(),    
                  panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                                  colour = "grey"), 
                  panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                                  colour = "grey"),
                  legend.position = "top")+
            scale_color_manual(values=c("#BA2100", "#999999", "#000000"))+
            xlab("Systolic blood pressure")+
            ylab("Density")+
            guides(color=guide_legend(title="Data"))

sdmep

plot.name <- "subdistrict_max_effect_density"
directory.and.file.name <- paste0(path.plots,dataset.name,plot.name,".pdf")
pdf(directory.and.file.name,width=8,height=5)
print(sdmep)
dev.off()

## interviewer
int.df <- df %>% 
  group_by(int.id) %>% 
  summarize(n = n(), Observed = mean(sbp > 140), Corrected = mean(csbp > 140)) %>% 
  mutate(change = abs(Observed - Corrected)) %>% 
  arrange(desc(Observed)) %>%
  filter(n > 20) # lets raise the minimal sample size that we consider here

# keep subdistricts where change is above 5% percentile of changes
for (i in 1:4) {

  qtil_vec <- quantile(int.df$change, c(.5,.7,.9,.99))
  int.df.q <- int.df %>% filter(change>qtil_vec[i])

  # plot Observed per subdistrict 
  int.df.q$int.id <- factor(int.df.q$int.id, levels=int.df.q$int.id)
  int.df.q <- cbind(melt(int.df.q[,-2]),int.df.q[,2])
  int.df.q <- int.df.q %>% filter(variable!="change")
  
  int.effp <- ggplot(int.df.q, aes(x=int.id, y=value, group=variable, color=variable))+
    geom_line(aes(group = int.id),size=.35,show.legend = FALSE)+
    geom_point()+
    theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=4),
          axis.text.x=element_blank(),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          size = 0.5, linetype = "solid"),
          #panel.grid.major.y = element_blank(),    
          panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                          colour = "grey"), 
          panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                          colour = "grey"),
          legend.position = "top")+
    scale_color_manual(values=c("#999999", "#BA2100"))+
    xlab("Interviewer")+
    ylab("")+
    guides(color=guide_legend(title=paste0("Prevalence, ",names(qtil_vec[i]),
                                           " percentile")))
  
  int.effp
  
  plot.name <- paste0("int_eff_percentile_",substr(names(qtil_vec[i]),1,2))
  directory.and.file.name <- paste0(path.plots,dataset.name,plot.name,".pdf")
  pdf(directory.and.file.name,width=10,height=5)
  print(int.effp)
  dev.off()

}

int.df <- df %>% 
  group_by(int.id) %>% 
  summarize(n = n(), Observed = mean(sbp > 140), Corrected = mean(csbp > 140)) %>% 
  mutate(change = abs(Observed - Corrected)) %>% 
  arrange(desc(Observed)) %>%
  filter(n > 20)

# print table 
if (dataset.name!="lasiw1_"){
  int.df %>% print(n = Inf) 
}
int <- as.character(int.df[which.max(int.df$change),]$int.id) # largest effect 

# Suppressing SSU ids for LASI
if (dataset.name!="lasiw1_"){
  int.df %>% filter(int.id == int)
}
if (dataset.name=="lasiw1_"){
  capture.output(int.df %>% filter(int.id == int), file=nullfile())
}

# Examine interviewer with largest effect
data.int <- df %>% filter(int.id == int) %>% select(`Observed (Interviewer)`=sbp,
                                                    `Corrected (Interviewer)`=csbp)

densp <- data %>% ungroup %>% select(value=sbp)
densp[,"variable"] <- "Observed (Population)"

densp <- rbind(densp,melt(data.int))

imep <- ggplot(data=densp, aes(x=value, group=variable, color=variable))+
  geom_density(size=1.1)+
  geom_vline(xintercept = 140, linetype = "dashed")+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        #panel.grid.major.y = element_blank(),    
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "grey"),
        legend.position = "top")+
  scale_color_manual(values=c("#BA2100", "#999999", "#000000"))+
  xlab("Systolic blood pressure")+
  ylab("Density")+
  guides(color=guide_legend(title="Data"))

imep

plot.name <- "interviewer_max_effect_density"
directory.and.file.name <- paste0(path.plots,dataset.name,plot.name,".pdf")
pdf(directory.and.file.name,width=8,height=5)
print(imep)
dev.off()



