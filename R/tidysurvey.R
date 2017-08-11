# tidy up the survey data

d=read.table('../data/introsurvey.csv',header=TRUE,sep=',')
d$ID=seq(dim(d)[1])
varnames=names(d)
names(d)=seq(dim(d)[2])

d_melted=melt(d,value.name='value',id.vars=c(61))
names(d_melted)=c('ID','variable','value')
varcodes=array(NA,dim(d)[1])
varcodes[1]='timestamp'
varcodes[2:14]='demographics'
varcodes[15:21]='statstest'
varcodes[22:31]='personality'
varcodes[32:60]='statsattitude'

d_melted['varcode']=NA
for (i in 1:dim(d_melted)[1]){
  d_melted$varcode[i]=varcodes[d_melted$variable[i]]
}

stanford_year=d_melted %>% filter(variable == 2)  %>%
                          transform(value = as.numeric(value))

stanford_major=d_melted %>% filter(variable == 3)  %>%
                          transform(value = as.factor(value))

stats_past=d_melted %>% filter(variable == 4)  %>%
  transform(value = value == 'Yes')

stats_why=d_melted %>% filter(variable == 5)  %>%
  transform(value = as.factor(value))

stats_expect_like=d_melted %>% filter(variable == 6)  %>%
  transform(value = as.numeric(value))

stats_expect_grade=d_melted %>% filter(variable == 7)  %>%
  transform(value = as.factor(value))

programming_experience=d_melted %>% filter(variable == 8)  %>%
  transform(value = as.numeric(value))

programming_languages=d_melted %>% filter(variable == 9)

gender = d_melted %>% filter(variable == 10)  %>%
  transform(value = as.factor(value))

height = d_melted %>% filter(variable == 11)  %>%
  transform(value = as.numeric(value))

times_flown=d_melted %>% filter(variable == 12)  %>%
  transform(value = as.numeric(value))

favorite_food = d_melted %>% filter(variable == 13)

street_number=d_melted %>% filter(variable == 14)  %>%
  transform(value = as.numeric(value))


# statistics data
exampledata=c(6,8,8,9,12,13,15)

mean_correct=d_melted %>% filter(variable == 18)  %>%
  transform(value = as.numeric(value),correct=as.numeric(value)==mean(exampledata))

mode_correct=d_melted %>% filter(variable == 19)  %>%
  transform(value = as.numeric(value),correct=as.numeric(value)==mode(exampledata))

median_correct=d_melted %>% filter(variable == 20)  %>%
  transform(value = as.numeric(value),correct=as.numeric(value)==median(exampledata))

sd_correct=d_melted %>% filter(variable == 21)  %>%
  transform(value = as.numeric(value),
            correct=abs(as.numeric(value)-sd(exampledata))<0.2)

correct=(1-pnorm(130,mean=100,sd=15))*1000
stats_percentile=d_melted %>% filter(variable == 15)  %>%
  transform(value = as.numeric(value),
            correct=abs(as.numeric(value)-correct)<3)

correct='If there is truly no difference between vegetarians and meat-eaters, then one would find a difference of 8% or more about 1% of the time.'
stats_pval=d_melted %>% filter(variable == 16)  %>%
  transform(value = as.factor(value),correct=value==correct)

correct='Vegetarians are different from meat-eaters'
stats_causality=d_melted %>% filter(variable == 17)  %>%
  transform(value = as.factor(value),correct=value==correct)


# statistics attitude data
# from https://www.stat.auckland.ac.nz/~iase/cblumberg/wise2.pdf
# The 29-item ATS has two subscales. The Attitudes Toward Field subscale consists of the
#following 20 items, with reverse-keyed items indicated by an “(R)”:
#  1, 3, 5, 6(R), 9, 10(R), 11, 13, 14(R), 16(R), 17, 19, 20(R), 21, 22, 23, 24, 26, 28(R), 29
#The Attitudes Toward Course subscale consists of the following 9 items:
#  2(R), 4(R), 7(R), 8, 12(R), 15(R), 18(R), 25(R), 27(R)

statsatt=d_melted %>%
  filter(varcode == 'statsattitude') %>%
  transform(value=as.numeric(value),variable=as.numeric(variable)-31)

reverse_score_attitude=c(6,10,14,16,20,28,2,4,7,12,15,18,25,27)
statsatt$reverse=0
statsatt$reverse[statsatt$variable %in% reverse_score_attitude]=1
statsatt$value_fixed=statsatt$value
statsatt$value_fixed[statsatt$reverse==1] = 8 - statsatt$value_fixed[statsatt$reverse==1]

# personality data
# TIPI scale scoring (“R” denotes reverse-scored items):
#Extraversion: 1, 6R; Agreeableness: 2R, 7; Conscientiousness; 3, 8R;
# Emotional Stability: 4R, 9; Openness to Experiences: 5, 10R.

personality=d_melted %>%
  filter(varcode == 'personality') %>%
  transform(value=as.numeric(value),variable=as.numeric(variable)-21)

reverse_score_tips=c(6,2,8,4,10)
personality$reverse=0
personality$reverse[personality$variable %in% reverse_score_tips]=1
personality$value_fixed=personality$value
personality$value_fixed[personality$reverse==1] = 8 - personality$value_fixed[personality$reverse==1]

