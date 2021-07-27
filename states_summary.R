#load in libraries
library(usmap)

#get the 4 most common states
tail(names(sort(table(loan$addr_state))), 3)
head(names(sort(table(loan$addr_state))), 4)

#state populations obtained from https://www.kaggle.com/lucasvictor/us-state-populations-2018
state_pops = read.csv("State Populations.csv")

#make state names into abbreviations
state_pops$State = state.abb[match(state_pops$State,state.name)]
state_pops[49,1] = "DC"
row.names(state_pops) <- state_pops$State
state_pops$State <- NULL

#get the counts for each state in a dataframe
summary_state = as.data.frame(summary(as.factor(loan$addr_state)))


states <- merge(state_pops, summary_state, by=0, all=TRUE)
states$per_capita = states$`summary(as.factor(loan$addr_state))`/states$X2018.Population*1000
states = states %>% 
  rename(
    state = Row.names,
  )

#write.csv(states,"states.csv")

#per capita map
plot_usmap("states", data = states, values="per_capita", color="black") +
  scale_fill_continuous(
    low = "yellow", high = "red", name = "Per Capita*1,000", label = scales::comma, na.value = "white"
  ) + theme(legend.position = "right") + labs(title = "Borrower Breakdown Per Capita") + theme(plot.title = element_text(hjust = 0.5))


#overall map
loan = loan %>% 
  rename(
    state = addr_state,
  )

plot_usmap(data = loan %>% count(state), values = "n", color = "black") +
  scale_fill_continuous(
    low = "yellow", high = "red", name = "# of Borrowers", label = scales::comma, na.value = "white"
  ) + theme(legend.position = "right") + labs(title = "Borrower Breakdown Per State") +
  theme(plot.title = element_text(hjust = 0.5))

#number of loans over the years
loan[["issue_d"]] = format(as.Date(loan[["issue_d"]], origin="1960-01-01"),"%Y-%m-%d")
loan[[paste("issue_d", "_year", sep="")]] = as.factor(year(loan[["issue_d"]]))
loan[[paste("issue_d", "_month", sep="")]] = as.factor(month(loan[["issue_d"]]))

#bar chart of frequencies of loans in each year
library(scales)
options(scipen=10000)
loan$purpose <- factor(loan$purpose, levels = c("car", "credit_card", "debt_consolidation","educational","home_improvement","house","major_purchase","medical","moving","small_business","renewable_energy","vacation","wedding","other"))
loan_filtered = loan %>% filter(issue_d_year %in% c("2011","2012","2013","2014","2015"))
frequencies <- ggplot(loan_filtered,aes(x=factor(issue_d_year), fill = purpose)) + 
  geom_bar() +
  scale_fill_manual("legend", values = c("car" = "#8dd3c7", "credit_card" = "#ffffb3", 
                                         "debt_consolidation" = "#bebada",  "educational" = "#fb8072", 
                                         "home_improvement" = "#80b1d3",  "house" = "#fdb462",  
                                         "major_purchase" = "#b3de69",  "medical" = "#fccde5",  
                                         "moving" = "#d9d9d9", "small_business" = "#bc80bd", 
                                         "renewable_energy" = "#ccebc5", "vacation" = "#ffed6f", 
                                         "wedding" = "cyan4", "other" = "aquamarine")) +
  xlab("Issue Year") + ylab("Frequency") + ggtitle("Frequency of Loans Issued from 2011-2016, Grouped by Purpose") + scale_y_continuous(labels=comma) + guides(fill=guide_legend(title='Purpose'))
frequencies


#bar chart with overall loan frequencies
loan_filtered = loan %>% filter(issue_d_year != 2007)
updated_frequencies <- ggplot(loan_filtered,aes(x=factor(issue_d_year))) + 
  geom_bar(fill="steelblue4") +
  #xlab("Issue Year") + ylab("Number of Loans") + 
  ggtitle("Number of Loans Issued from 2008 to 2015") + 
  scale_y_continuous(labels=comma) + theme(plot.title = element_text(hjust = 0.5))
updated_frequencies





#loan amount box plot (do something with grade)
box <- ggplot(loan,aes(x=loan_amnt,fill=grade)) + geom_boxplot() +
  xlab("Loan Amount")  + scale_y_continuous(labels=comma) + guides(fill=guide_legend(title='Purpose'))
box














#making money states
fp_loans <- loan %>%
  dplyr::filter(loan_status=="Fully Paid") %>%
  select(issue_d, last_pymnt_d, total_rec_prncp, funded_amnt, term, installment, state) %>%
  mutate(term_len = difftime(last_pymnt_d, issue_d, units="weeks"))

fp_loans$term_len = as.numeric(gsub("([0-9]+).*$", "\\1", fp_loans$term_len))
summary(as.factor(fp_loans$term_len))

fp_loans$term = substr(fp_loans$term, 1,2)
fp_loans$term = as.numeric(fp_loans$term)

fp_loans$term_len = round(as.numeric(difftime(fp_loans$last_pymnt_d, fp_loans$issue_d, units ="days"))/(365.25/12))
fp_loans$LC_Revenue = (fp_loans$term_len*fp_loans$installment)*.01
fp_loans$expected_LC_Rev = (as.numeric(fp_loans$term)*fp_loans$installment)*.01
fp_loans$LC_rev_loss = fp_loans$expected_LC_Rev - fp_loans$LC_Revenue

fp_loans_rev = fp_loans %>% select(state, LC_Revenue)
money = as.data.frame(aggregate(x = fp_loans_rev$LC_Revenue, by = list(fp_loans_rev$state), FUN = sum))
money = money %>% 
  rename(
    state = Group.1,
    money = x
  )

row.names(money) <- money$state
money <- merge(money, state_pops, by=0, all=TRUE)
money$per_capita = money$money/states$X2018.Population*1000

plot_usmap("states", data = money, values="money", color="black") +
  scale_fill_continuous(
    low = "white", high = "steelblue4", name = "Money ($)", label = scales::comma, na.value = "white"
  ) + theme(legend.position = "right") + labs(title = "Total Money Breakdown, 2006-2016")

#Per-Capita Money
plot_usmap("states", data = money, values="per_capita", color="black") +
  scale_fill_continuous(
    low = "white", high = "steelblue4", name = "Money ($) / Per-Capita*1,000", label = scales::comma, na.value = "white"
  ) + theme(legend.position = "right") + labs(title = "Total Money Breakdown Per-Capita from 2006 to 2016")



