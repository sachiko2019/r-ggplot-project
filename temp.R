library(tidyverse)
interviews_plotting<-read.csv("data_output/interviews_plotting.csv")
head(interviews_plotting)

interviews_plot5<-ggplot(data=interviews_plotting,
       aes(x=no_membrs,y=number_items))+
  geom_jitter(alpha=.5,width=1.0,height=0.5,size=1)

ggsave("fig_output/membrs_item.png",interviews_plot,width=15,height=10,
       dpi=72)

ggplot(data=interviews_plotting,aes(x=no_membrs,y=number_items))+
  geom_jitter(aes(color=village),alpha=.5,width=0.2,height=0.2,size=4)+
  geom_smooth(method="lm")

interviews_plot<-ggplot(data=interviews_plotting,
  aes(x=no_membrs,y=number_items,color=village))+
  geom_jitter(,alpha=.5,width=0.2,height=0.2,size=4)+
  geom_smooth(method="lm")

ggsave("fig_output/membrs_item.png",interviews_plot,width=15,height=10,dpi=72)

ggplot(data=interviews_plotting,
                        aes(x=village,y=rooms,color=respondent_wall_type))+
  geom_jitter(alpha=.5,width=0.2,height=0.2,size=2)

ggplot(interviews_plotting,aes(x=no_membrs,fill=village))+
  geom_histogram(binwidth=1,color="white")

ggplot(interviews_plotting,aes(x=no_membrs,y=stat(density), color=village))+
  geom_freqpoly(binwidth=1)

ggplot(interviews_plotting,aes(x=no_membrs, color=village))+
  geom_density()

ggplot(data=interviews_plotting,
       aes(x=respondent_wall_type,fill=village))+
  geom_bar(position="dodge")

ggplot(data=interviews_plotting,
       aes(x=village,fill=village))+
  geom_bar(position="dodge")+
  facet_wrap(~respondent_wall_type)

counts<-table(interviews_plotting$village,interviews_plotting$respondent_wall_type)
counts

ggplot(data=interviews_plotting,
       aes(x=village,fill=village))+
  geom_bar(position="dodge")+
  facet_wrap(~respondent_wall_type)+
  scale_fill_brewer(palette="RdBu")

ggplot(data=interviews_plotting,
       aes(x=respondent_wall_type,fill=village))+
  geom_bar(position="fill")+
  ylab("proportion")

ggplot(data=interviews_plotting,
       aes(x=respondent_wall_type,fill=village))+
  geom_bar(position="fill")+
  ylab("proportion")+
  stat_count(geom="text",
             aes(label=stat(count)),
             position=position_fill(vjust=0.5),color="white")

ggplot(data=interviews_plotting,
       aes(x=village,fill=respondent_wall_type))+
  geom_bar(position="fill")+
  ylab("proportion")+
  stat_count(geom="text",
             aes(label=stat(count)),
             position=position_fill(vjust=0.5),color="white")

ggplot(data=interviews_plotting,
       aes(x=respondent_wall_type,y=rooms))+
         geom_boxplot(outlier.shape=NA)+
  geom_jitter(alpha=0.5,width=0.2,height=0.2,color="tomato")
       
ggplot(data=interviews_plotting,
       aes(x=respondent_wall_type,y=rooms))+
  geom_boxplot(alpha=0)+
  geom_jitter(alpha=0.5,width=0.2,height=0.2,color="tomato")

ggplot(data=interviews_plotting,
       aes(x=respondent_wall_type,y=rooms))+
  geom_boxplot(alpha=0)+
  geom_jitter(alpha=0.5,width=0.2,height=0.2,aes(color=village))

ggplot(data=interviews_plotting,
       aes(x=respondent_wall_type,y=liv_count))+
  geom_boxplot(alpha=0)+
  geom_jitter(alpha=0.5,width=0.2,height=0.2,aes(color=village))

ggplot(data=interviews_plotting,
       aes(x=respondent_wall_type,y=liv_count))+
  geom_boxplot(alpha=0)+
  geom_jitter(alpha=0.5,width=0.2,height=0.2,aes(color=memb_assoc))             

ggplot(data=interviews_plotting,
       aes(x=respondent_wall_type,y=liv_count,fill=memb_assoc,
           color=memb_assoc))+
  geom_boxplot(alpha=0.5)+
geom_point(alpha=0.5,position=position_jitterdodge(jitter.width=0.1,jitter.height=0.1))

ggplot(data=interviews_plotting,
       aes(x=respondent_wall_type,y=rooms))+
  geom_violin(alpha=0)+
  geom_jitter(alpha=0.5,width=0.2,height=0.2,color="tomato")

ggplot(data=interviews_plotting,aes(fill=respondent_wall_type,x=village))+
  geom_bar(position="fill")+
  stat_count(geom="text",aes(label=stat(count)),
             position=position_fill(vjust=0.5),color="white")+
  ylab("proportion")+xlab("village")+ggtitle("Proportion of wall by village")

ggplot(data=interviews_plotting,aes(fill=respondent_wall_type,x=village))+
  geom_bar(position="fill")+
  stat_count(geom="text",aes(label=stat(count)),
             position=position_fill(vjust=0.5),color="white")+
  labs(x="village",y = "Proporttion",title="Wall type by village")+
  scale_fill_discrete(labels=c("burnt brick","cement","mud daub","sun bricks"))+
  guides(fill=guide_legend(title="wall type"))

ggplot(data=interviews_plotting,aes(fill=memb_assoc,x=respondent_wall_type))+
  geom_bar(position="fill")+
  stat_count(geom="text",aes(label=stat(count)),
             position=position_fill(vjust=0.5),color="white")+
  scale_x_discrete(labels=c("burnt bricks","cement","mud daub","sum bricks"))+
  labs(x="Village",y = "Proporttion",title="Wall type by village")+
  facet_wrap(~village,nrow=2)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45,hjust=1))
  
ggplot(data=interviews_plotting,aes(fill=memb_assoc,x=respondent_wall_type))+
  geom_bar(position="fill")+
  stat_count(geom="text",aes(label=stat(count)),
             position=position_fill(vjust=0.5),color="white")+
  scale_x_discrete(labels=c("burnt bricks","cement","mud daub","sum bricks"))+
  labs(x="Village",y = "Proporttion",title="Wall type by village")+
  facet_wrap(~village,nrow=2)+
  theme_classic()+
  guides(fill=guide_legend(title="Member\nAssociation"))+
  theme(axis.text.x=element_text(angle=45,hjust=1,size=5),plot.title=element_text(hjust=.5))

mytheme<-  theme(axis.text.x=element_text(angle=45,hjust=1,size=5),plot.title=element_text(hjust=.5))
