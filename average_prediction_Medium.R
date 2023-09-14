# Create average line among factors=0 and factors=1
average_prediction_Medium<-rbind(
  tibble(ggpredict(mod_abund_Medium,type="zi_prob",terms=c("age[all]"),
                   condition=c(nutrient=1,fire=1,dry=1)))%>%
    select(-group)%>%mutate(type="a"),
  tibble(ggpredict(mod_abund_Medium,type="zi_prob",terms=c("age[all]"),
                   condition=c(nutrient=0,fire=0,dry=0)))%>%
    select(-group)%>%mutate(type="b"),
  tibble(ggpredict(mod_abund_Medium,type="zi_prob",terms=c("age[all]"),
                   condition=c(nutrient=0,fire=1,dry=1)))%>%
    select(-group)%>%mutate(type="c"),
  tibble(ggpredict(mod_abund_Medium,type="zi_prob",terms=c("age[all]"),
                   condition=c(nutrient=1,fire=0,dry=1)))%>%
    select(-group)%>%mutate(type="d"),
  tibble(ggpredict(mod_abund_Medium,type="zi_prob",terms=c("age[all]"),
                   condition=c(nutrient=1,fire=1,dry=0)))%>%
    select(-group)%>%mutate(type="e"),
  tibble(ggpredict(mod_abund_Medium,type="zi_prob",terms=c("age[all]"),
                   condition=c(nutrient=0,fire=0,dry=1)))%>%
    select(-group)%>%mutate(type="f"),
  tibble(ggpredict(mod_abund_Medium,type="zi_prob",terms=c("age[all]"),
                   condition=c(nutrient=0,fire=1,dry=0)))%>%
    select(-group)%>%mutate(type="g"),
  tibble(ggpredict(mod_abund_Medium,type="zi_prob",terms=c("age[all]"),
                   condition=c(nutrient=1,fire=0,dry=0)))%>%
    select(-group)%>%mutate(type="h")
)%>%
  group_by(x)%>%summarise(predicted=mean(predicted),std.error=mean(std.error),
                          conf.low=mean(conf.low),conf.high=mean(conf.high))

ggplot()+
  geom_point(data=data_peat,aes(x=age,y=Medium_prop),
             size=3,alpha=0.4,shape=16)+
  geom_ribbon(data=average_prediction_Medium,
              aes(x=x,y=1-predicted,ymin=1-conf.low,ymax=1-conf.high),
              color="grey",alpha=0.3)+
  geom_line(data=average_prediction_Medium,
            aes(x=x,y=1-predicted))+
  my_theme()+xlab("Age")+ylab("Probability of Medium presence")           
           