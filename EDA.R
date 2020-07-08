#Bank marketing campaign for Exploratory data analysis

#Data Extraction
bank = read.csv("E:/1. Data Science NextGen/Data sets/bank.csv")
View(bank)
nrow(bank)

#Data inspection
summary(bank)
str(bank)          #glimpse(bank)
class(bank)
dim(bank)

#Identifying uniques
bank %>% distinct(job)
bank %>% distinct(marital)
bank %>% distinct(education)
bank %>% distinct(contact)
bank %>% distinct(poutcome)

bank %>% group_by(age) %>% summarise(client = n()) %>% View()

bank %>% group_by(job) %>% summarise(client = n()) %>% View()

bank %>% group_by(marital) %>% summarise(client = n()) %>% View()

bank %>% group_by(education) %>% summarise(client = n()) %>% View()

#has credit in default ?
bank %>% group_by(default) %>% summarise(client = n()) %>% View()

bank %>% group_by(balance) %>% summarise(client = n()) %>% View()

#has housing loan ?
bank %>% group_by(housing) %>% summarise(client = n()) %>% View()

#has personal loan?
bank %>% group_by(loan) %>% summarise(client = n()) %>% View()

#contact communication type
bank %>% group_by(contact) %>% summarise(client = n()) %>% View()

#last contact day
bank %>% group_by(month,day) %>% summarise(client = n()) %>% View()

#last contact duration
bank %>% group_by(duration) %>% summarise(client = n()) %>% View()

#number of contacts performed during this campaign
bank %>% group_by(contactsPerformed = campaign) %>% summarise(client = n()) %>% View()

#number of days that passed by after the client was last contacted from a previous campaign
bank %>% group_by(pdays) %>% summarise(client = n()) %>% View()

#number of contacts performed before this campaign and for this client
bank %>% group_by(previous) %>% summarise(client = n()) %>% View()

#outcome of the previous marketing campaign
bank %>% group_by(poutcome) %>% summarise(client = n()) %>% View()

#Output whether client subscribed a term deposit
bank %>% group_by(deposit) %>% summarise(client = n()) %>% View()

#Observing null values
colSums(is.na(bank))

#Removing duplicate rows
bank %>% distinct() -> bank

#Visualization

#Visualizing categorical features

bank %>% ggplot() + geom_bar(aes(job, fill=bank$deposit),position = "dodge") + ylab("Number of clients") + xlab("Job") + ggtitle("Distribution of job and deposit") + theme_classic()

bank %>% ggplot() + geom_bar(aes(marital, fill=bank$deposit),position = "dodge") + ylab("Number of clients") + xlab("Marital Status") + ggtitle("Distribution of Marital Status and deposit") + theme_classic()

bank %>% ggplot() + geom_bar(aes(education, fill=bank$deposit), position = "dodge") + ylab("Number of clients") + xlab("Education") + ggtitle("Distribution of Education and deposit") + theme_classic()

bank %>% ggplot() + geom_bar(aes(contact, fill=bank$deposit), position = "dodge") + ylab("Number of clients") + xlab("Contact") + ggtitle("Distribution of Contact and deposit") + theme_classic()

bank %>% ggplot() + geom_bar(aes(loan, fill=bank$deposit), position = "dodge") + ylab("Number of clients") + xlab("Loan") + ggtitle("Distribution of Loan and deposit") + theme_classic()

bank %>% ggplot() + geom_bar(aes(housing, fill=bank$deposit), position = "dodge") + ylab("Number of clients") + xlab("Housing Loan") + ggtitle("Distribution of Housing Loan and deposit") + theme_classic()

#Visualizing numerical features

