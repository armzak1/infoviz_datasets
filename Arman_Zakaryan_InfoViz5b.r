library('ggplot2')
df <- read.csv('https://raw.githubusercontent.com/armzak1/infoviz_datasets/master/countries.csv')

#cleaning up column names
head(df)
colnames(df) <- lapply(colnames(df), function(x){gsub('\\.', "", x)})

#cleaning up data types
to_double <- function(x){as.double(gsub(',', '.', x))}

df$Netmigration <- unlist(lapply(list(df$Netmigration), to_double))
df$Literacy <- unlist(lapply(list(df$Literacy), to_double))
df$Phonesper1000 <- unlist(lapply(list(df$Phonesper1000), to_double))
df$Infantmortalityper1000births <- unlist(lapply(list(df$Infantmortalityper1000births), to_double))
df$Birthrate <- unlist(lapply(list(df$Birthrate), to_double))
df$Crops <- as.double(unlist(lapply(list(df$Crops), to_double)))
df$Region <-  unlist(lapply(df$Region, trimws))

#converting percentage for each economy sector to a categorical feature that indicates 
#the sector with the highest percentage
df$Service <- unlist(lapply(list(df$Service), to_double))
df$Industry <- unlist(lapply(list(df$Industry), to_double))
df$Agriculture <- unlist(lapply(list(df$Agriculture), to_double))
df$LeadingService <- as.numeric((df$Service > df$Agriculture) & (df$Service > df$Industry))*3
df$LeadingAgriculture <- as.numeric((df$Service < df$Agriculture) & (df$Agriculture > df$Industry))
df$LeadingIndustry <- as.numeric((df$Industry > df$Agriculture) & (df$Service < df$Industry))*2
df$LeadingField <- df$LeadingService + df$LeadingIndustry + df$LeadingAgriculture
df$LeadingField <- as.factor(df$LeadingField)

#dropping N/As
df <- df[!(df$LeadingField == 0),]
df$LeadingField <- factor(df$LeadingField, levels = c(0,1,2,3), 
       labels = c('N/A', 'Industry', 'Agriculture', 'Service'))

df <- na.omit(df)

#custom theme 
theme_arj <- theme(panel.background = element_rect(fill='#E8EDF1'), 
                   panel.grid.major = element_line(),
                   panel.grid.minor = element_blank(),
                   strip.background = element_rect(fill='#9FCCC7'),
                   strip.text = element_text(color = "#36454f"),
                   text = element_text(colour = "#36454f", family='sans'),
                   plot.caption = element_text(hjust = 0),
                   plot.title = element_text(hjust = 0.5))

#The plota are meant to be viewed in the full screen mode.

ggplot(df, aes(Netmigration, GDPpercapita, color=Literacy)) +
  scale_color_gradient(low='#F04849', high = '#59B69B') +
  geom_jitter(alpha=1) + 
  coord_fixed(ratio = 0.0004) +
  ylim(0, 40000) + 
  geom_smooth(se=FALSE, method='lm', size=1.2, color='#F48C37')+
  facet_grid(. ~ LeadingField) + 
  theme_arj + 
  labs(title = "Relationship of Countries' Net Migration and Socioeconomic Conditions \n\n", 
       subtitle = "Visualization of countries' net migration vs GDP-per capita, divided by the leading sector of the countries' economy.",
       x='Net Migration', y='GDP-per capita', 
       caption = '


We notice a positive relationship between countries GDP per capita and Net Migration. 
This can be attributed to the fact that countries with higher GDP per capitas are more attractive to migrate to.
The graph also shows that countries with high GDP per capita have in general a higher level of literacy.
We can also observe that in countries where the leading sector of economy is Industry GDP per capita 
and Net Migration are pretty low, compared to countries where the leading sector is either Service or Agriculture.
Most of the coutries that have Service as their economic frontier have higher GDP per capita, Net Migration and Level of literacy than the rest.
       
       
Data Source: The World Factbook')

ggplot(df) + geom_boxplot(aes(df$LeadingField, df$Phonesper1000), fill='#F48C37', width=0.2) +
  scale_y_log10() +
  theme_arj +
  labs(title = "Relationship of Countries' Leading Field of Economy and Socioeconomic Conditions \n\n", 
       subtitle = "Boxplots of phone possession per 1000 people by the leading field of economy",
       x='Leading Field of Economy', y='Phones per 1000 person', 
       caption = '

As previous plot suggested, overall socioeconomic condiitons in countries where the leading field is Industry is way worse,
compared to the rest. Phone possession is also an indicator of socioeconomic conditions in a country, and this plot indicates
that in countries where Industry is the economic frontier, possession of phones is even less than 1% (~10 people in 1000).
Whereas in countries where the frontier is Service, the phone possession is closer to 40-50%, implying better socioeconomic conditions.

       
Data Source: The World Factbook')


ggplot(df, aes(df$LeadingField, df$Birthrate)) +
  geom_boxplot( fill='#F48C37', width = 0.2) + 
  theme_arj +
  labs(title = "Relationship of Countries' Leading Field of Economy and Birthrate \n\n", 
       subtitle = "Boxplots of birthrates by the leading field of economy",
       x='Leading Field of Economy', y='Birthrate', 
       caption = '

This suggests that the less developed countries tend to have higher birthrate than the ones that are more developed.
The leading field of economy is here to indicate how developed a country is (based on the relationships found in previous plots).
       
Data Source: The World Factbook')
  
ggplot(df, aes(df$LeadingField, df$Infantmortalityper1000births)) +
  geom_boxplot( fill='#F48C37', width = 0.2) + 
  theme_arj +
  labs(title = "Relationship of Countries' Leading Field of Economy and Infant Mortality \n\n", 
       subtitle = "Boxplots of infant mortality rate by the leading field of economy",
       x='Leading Field of Economy', y='Infant mortality rate (per 1000)', 
       caption = '

This furthers the point that the previous graphs were making, by indicating once again that in countries where Industry is 
the economic frontier, the conditions are worse, than in rest of the world. Infant mortality rate is a good indicator of those conditions.

       
Data Source: The World Factbook')

ggplot(df, aes(x=reorder(Region, table(Region)[Region]), fill=LeadingField)) +
  geom_bar(position='stack') +
  coord_flip() +
  theme_arj +
  scale_fill_manual(name ='Leading Field', values = c('#F5D55C', '#59B69B', '#20639A')) + 
  labs(title = "Leading Field of Economy by Region \n\n", 
       subtitle = "Barplot showing the distribution of the leading field of economy in each region",
       x='Region', y='Count', 
       caption = '

And here we observe the distribution of leading economy fields in each region. Based on the previous observations,
if Industry is not present in the leading fields for any of the countries in the Region, than that Region is relatively
in better socioeconomic conditions. As we see, the countries that are in bad socioeconomic conditions are either in Africa
or in Near East. 
       
Data Source: The World Factbook')

       