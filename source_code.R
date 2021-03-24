# Packages used
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(stringr)

# The H-1B visa category allows for "dual intent" where the foreign national will be coming to work in a professional position temporarily while also intending to immigrate to the United States.
# The H1B1 visa applicant, however, has to demonstrate that he/she does not intend to immigrate to the United States.

# Our analysis is primarily focused on H1B and H1B1 Visa Applicants, so removing E3 Australian applicants
Visa <- filter(H_1B_H_1B1_E_3_FY2020_Q3, VISA_CLASS != "E-3 Australian") 

### Throughout the analysis, I will attempt to answer the following questions:

#  What are the top occupations America needs the most?
#  Who are the top employers that submit the most applications?
#  Which employers pay the most for what job?
#  Which states and cities hire the most H-1B visa workers?

Visa <- lapply(Visa, toupper)

Visa$PREVAILING_WAGE <- as.numeric(Visa$PREVAILING_WAGE)
occu_group <- group_by(Visa , SOC_TITLE)
visa_by_occu <- dplyr::summarise(occu_group,
                                 count=n(),
                                 mean = mean(PREVAILING_WAGE))
visa_by_occu <- visa_by_occu[with(visa_by_occu, order(-mean)),]

# there are a total of 866 Job titles, thus considering only the relevant job titles
visa_by_occu<-visa_by_occu[1:20,]
visa_by_occu <- visa_by_occu[-20,] # NA values omission

## What are the top occupations?

ggplot(aes(x = reorder(SOC_TITLE, count), y=count), data = visa_by_occu) +
  geom_bar(stat = 'identity') + coord_flip() +
  xlab('Occupantions') +
  ylab('Number of Applications') +
  theme(axis.text = element_text(size = 8), 
        plot.title = element_text(size = 12)) +
  ggtitle('Top H-1B/H-1B1 Visa Occupations 2019-20')

# Technology related professions are the most in demnad occupations.

## Who are the top employers that submit the most applications ?

employer_group <- group_by(Visa , EMPLOYER_NAME)
visa_by_employer <- dplyr::summarise(employer_group,
                                     count=n(),
                                     mean=mean(PREVAILING_WAGE))
visa_by_employer <- visa_by_employer[with(visa_by_employer, order(-count)), ]
visa_by_employer<-visa_by_employer[1:20,]

ggplot(aes(x = reorder(EMPLOYER_NAME, count), y=count), data = visa_by_employer) +
  geom_bar(stat = 'identity') + coord_flip() +
  xlab('Employers') +
  ylab('Number of Applications') +
  theme(axis.text = element_text(size = 8),
        plot.title = element_text(size = 12)) +
  ggtitle('Top Employers for H-1B/H-1B1 Visa Application 2019-20')

# Cognizant Technology Solutions US Corp leads by a large margin and submitted over 20000 applications last year. 

employer_job_group <- group_by(Visa, JOB_TITLE, EMPLOYER_NAME)
visa_by_employer_job <- dplyr::summarise(employer_job_group,
                                         count=n(),
                                         mean = mean(PREVAILING_WAGE))

visa_by_employer_job <- visa_by_employer_job[with(visa_by_employer_job, order(-count)),]
visa_by_employer_job <- visa_by_employer_job[1:20,]

ggplot(aes(x = reorder(JOB_TITLE, count), y=count, fill=EMPLOYER_NAME), data = visa_by_employer_job) +
  geom_bar(stat = 'identity', position = position_dodge()) + 
  coord_flip() +
  ylab('Number of Applications') +
  xlab('Job Title') +
  theme(axis.text = element_text(size = 8), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4),
        legend.justification=c(1,0), legend.position=c(1,0), legend.title=element_text(size=8), 
        legend.text=element_text(size=7), 
        plot.title = element_text(size = 10)) +
  ggtitle('Top Job Titles for H-1B/H-1B1 Visa Application 2019-20')

# Senior System Analysts, Manager , System Analyst are in huge demand at Cogniaznt. Delloite is interested in Senior Consultants, Manager and Consultants.

### What occupations make the most money ?

Visa[which.min(Visa$PREVAILING_WAGE),]
# Minimum wage was $ 7.25 which was however paid on an hourly basis, to the Job title of Teacher/Administrator. The application was denied.

Visa[which.max(Visa$PREVAILING_WAGE),]
# Maximum Wage was observed to be $760,202 to a POSTDOCTORAL FELLOW (CHEMICAL ENGINEERING) employed by LAWRENCE BERKELEY NATIONAL LABORATORY. 

ggplot(Visa, aes(x = PREVAILING_WAGE)) + 
  scale_x_continuous(limits = c(0, 300000)) +
  geom_histogram(aes(y = ..density..),bins=100) + geom_density(color='blue') +
  xlab('Prevailing Wage(USD)') +
  ggtitle('Prevailing Wage Distribution 2019-20')

# Majority of the wages are between 60K to 120K USD

ggplot(aes(x = reorder(SOC_TITLE, mean), y=mean), data = visa_by_occu) +
  geom_bar(stat = 'identity') + coord_flip() +
  xlab('Occupantions') +
  ylab('Average Prevailing Wage(USD)') +
  theme(axis.text = element_text(size = 8), 
        plot.title = element_text(size = 10)) +
  ggtitle('Top Wage H-1B/H1B1 Visa Occupations 2019-20') 

# Physicians, Surgeons and Health and Medical Mangers enjoy the highest Prevailing Wages that is way over 20K USD.

### Which employers pay the most ?

ggplot(aes(x = reorder(JOB_TITLE, mean), y=mean, fill=EMPLOYER_NAME), data = visa_by_employer_job) +
  geom_bar(stat = 'identity', position = position_dodge()) + 
  coord_flip() +
  ylab('Average Prevailing Wage(USD)') +
  xlab('Job Title') +
  theme(axis.text = element_text(size = 8), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4),
        legend.justification=c(1,0), legend.position=c(1,-1), legend.title=element_text(size=), legend.text=element_text(size=7), 
        plot.title = element_text(size = 9)) +
  ggtitle('Top Job Titles and Average Wages for H-1B/H-1B1 Visa Application 2019-20')

# Looking at the Job Title and Employer, consultants/ Managers hired by Deloitte enjoy the highest average prevailing wage, Software Engineers at Microsoft are paid way more than similar roles offered at different companies.

top_employer_df <- filter(visa, EMPLOYER_NAME %in% visa_by_employer[['EMPLOYER_NAME']])
ggplot(aes(x = EMPLOYER_NAME, y = PREVAILING_WAGE), data = top_employer_df) +
  geom_boxplot() +
  coord_flip(ylim=c(0,150000)) +
  xlab('Employers') +
  ylab('Prevailing Wage(USD)') + 
  ggtitle('Wage by Top 20 Employers')
by(top_employer_df$PREVAILING_WAGE, top_employer_df$EMPLOYER_NAME, summary)

# Max. median wage is provided by Facebook Inc., Min. median wage are observed to be of Tata Consultancy and INFOSYS Ltd.
# It's observed that J.P. Morgan Chase and Co. & Google LLC has one outliers each exceeding 250K USD Prevsiling Wage.

### Which states cities apply the most H-1B/H-1B1 Visas?
Visa$CITY <- str_replace(Visa$WORKSITE_CITY, '(.+),.+', '\\1')
Visa$STATE <- str_replace(Visa$WORKSITE_STATE, '.+,(.+)', '\\1')
state_group <- group_by(Visa, STATE)
visa_by_state <- dplyr::summarize(state_group, 
                                  count = n(),
                                  mean = mean(PREVAILING_WAGE))
visa_by_state <- visa_by_state[with(visa_by_state, order(-count)), ]
visa_by_state <- visa_by_state[1:20, ]

ggplot(aes(x = reorder(STATE, count), y = count), data = visa_by_state) +
  geom_bar(stat = 'identity') + coord_flip() +
  xlab('State') +
  ylab('Number of Applications') +
  ggtitle('Top States Apply the Most H-1B/H-1B1 Visas')

# As expected, State of California hires the most workers on H-1B/H-1B1 visas, followed by TEXAS, NY, Washington.

city_group <- group_by(Visa, CITY)
visa_by_city <- dplyr::summarize(city_group, 
                                 count = n(),
                                 mean = mean(PREVAILING_WAGE))
visa_by_city <- visa_by_city[with(visa_by_city, order(-count)), ]
visa_by_city <- visa_by_city[1:20, ]
ggplot(aes(x = reorder(CITY, count), y = count), data = visa_by_city) +
  geom_bar(stat = 'identity') + coord_flip() +
  xlab('City') +
  ylab('Number of Applications') +
  ggtitle('Top Cities Apply the Most H-1B Visas')

# NY City takes the lead by a large margin in the number of H-1B Visa applications.

### THE END
