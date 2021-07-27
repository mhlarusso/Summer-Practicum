#cleaning up emp_title
library(stringr)
loan$emp_title = loan$emp_title %>% tolower() %>% trimws()
loan$emp_title = as.factor(loan$emp_title)
loan = loan %>% filter(application_type != "JOINT")
loan_60 = loan %>%
  group_by(emp_title) %>%
  filter(n()>60)

sort(table(loan_60$emp_title),decreasing=TRUE)
loan$emp_title[loan$emp_title == 'rn'] <- 'nurse'
loan$emp_title[loan$emp_title == 'registered nurse'] <- 'nurse'
loan$emp_title[loan$emp_title == 'ibm'] <- ''
loan$emp_title[loan$emp_title == 'jp morgan chase'] <- ''
loan$emp_title[loan$emp_title == 'verizon'] <- ''
loan$emp_title[loan$emp_title == 'geico'] <- ''
loan$emp_title[loan$emp_title == 'union bank'] <- ''
loan$emp_title[loan$emp_title == 'whole foods market'] <- ''
loan$emp_title[loan$emp_title == 'oracle'] <- 
loan$emp_title[loan$emp_title == 'e-7'] <- ''
loan$emp_title[loan$emp_title == 'adp'] <- ''
loan$emp_title[loan$emp_title == 'merrill lynch'] <- ''
loan$emp_title[loan$emp_title == 'lowes'] <- ''
loan$emp_title[loan$emp_title == 'us army'] <- 'army'
loan$emp_title[loan$emp_title == 'government'] <- ''
loan$emp_title[loan$emp_title == 'costco wholesale'] <- ''
loan$emp_title[loan$emp_title == 'soilder'] <- 'army'
loan$emp_title[loan$emp_title == 'hha'] <- 'home health aide'

loan$emp_title[loan$emp_title == 'saic'] <- ''
loan$emp_title[loan$emp_title == 'usmc'] <- 'marine corps'
loan$emp_title[loan$emp_title == 'asst. manager'] <- ''
loan$emp_title[loan$emp_title == 'target'] <- ''


loan$emp_title[loan$emp_title == 'wal-mart'] <- ''
loan$emp_title[loan$emp_title == 'walmart'] <- ''

loan$emp_title[loan$emp_title == 'vp'] <- 'vice president'
loan$emp_title[loan$emp_title == 'at&t'] <- ''
loan$emp_title[loan$emp_title == 'usps'] <- ''
loan$emp_title[loan$emp_title == 'ups'] <- ''
loan$emp_title[loan$emp_title == 'usaf'] <- 'air force'
loan$emp_title[loan$emp_title == 'us air force'] <- 'air force'
loan$emp_title[loan$emp_title == 'wells fargo'] <- ''
loan$emp_title[loan$emp_title == 'cpa'] <- 'accountant'
loan$emp_title[loan$emp_title == 'wells fargo bank'] <- ''
loan$emp_title[loan$emp_title == 'lockheed martin'] <- ''
loan$emp_title[loan$emp_title == 'walgreens'] <- ''
loan$emp_title[loan$emp_title == 'emt'] <- 'paramedic'
loan$emp_title[loan$emp_title == 'u.s. army'] <- 'army'
loan$emp_title[loan$emp_title == 'director of it'] <- 'it director'
loan$emp_title[loan$emp_title == 'national sales manager'] <- 'sales manager'
loan$emp_title[loan$emp_title == 'mgr'] <- 'manager'
loan$emp_title[loan$emp_title == 'office mgr'] <- ''
loan$emp_title[loan$emp_title == 'department of veterans affairs'] <- ''
loan$emp_title[loan$emp_title == 'fidelity investments'] <- ''
loan$emp_title[loan$emp_title == 'sprint'] <- ''
loan$emp_title[loan$emp_title == 'citibank'] <- ''
loan$emp_title[loan$emp_title == 'kaiser permanente'] <- ''
loan$emp_title[loan$emp_title == 'general motors'] <- ''
loan$emp_title[loan$emp_title == 'federal aviation administration'] <- ''
loan$emp_title[loan$emp_title == 'northrop grumman'] <- ''
loan$emp_title[loan$emp_title == 'general electric'] <- ''
loan$emp_title[loan$emp_title == 'united airlines'] <- ''
loan$emp_title[loan$emp_title == 'military'] <- ''
loan$emp_title[loan$emp_title == 'president'] <- ''
loan$emp_title[loan$emp_title == 'lausd'] <- ''
loan$emp_title[loan$emp_title == 'american express'] <- ''
loan$emp_title[loan$emp_title == 'thomson reuters'] <- ''
loan$emp_title[loan$emp_title == 'united states army'] <- 'army'
loan$emp_title[loan$emp_title == 'hr'] <- 'human resources'
loan$emp_title[loan$emp_title == 'general dynamics'] <- 'human resources'
loan$emp_title[loan$emp_title == 'qa manager'] <- 'quality assurance manager'
loan$emp_title[loan$emp_title == 'care giver'] <- 'caregiver'
loan$emp_title[loan$emp_title == 'e-6'] <- ''
loan$emp_title[loan$emp_title == 'qa analyst'] <- 'quality assurance analyst'
loan$emp_title[loan$emp_title == 'crna'] <- 'nurse anesthetist'
loan$emp_title[loan$emp_title == 'boeing'] <- ''
loan$emp_title[loan$emp_title == 'sr manager'] <- 'senior manager'
loan$emp_title[loan$emp_title == 'sr. financial analyst'] <- 'senior financial analyst'
loan$emp_title[loan$emp_title == 'sr. systems engineer'] <- 'senior systems engineer'
loan$emp_title[loan$emp_title == 'raytheon'] <- ''
loan$emp_title[loan$emp_title == 'pca'] <- 'personal care assistant'
loan$emp_title[loan$emp_title == 'ceo'] <- 'chief executive officer'
loan$emp_title[loan$emp_title == 'cio'] <- 'chief information officer'
loan$emp_title[loan$emp_title == 'csa'] <- ''
loan$emp_title[loan$emp_title == 'the boeing company'] <- ''
loan$emp_title[loan$emp_title == 'fedex'] <- ''
loan$emp_title[loan$emp_title == 'cma'] <- 'certified medical assistant'
loan$emp_title[loan$emp_title == 'sales mgr'] <- 'sales manager'
loan$emp_title[loan$emp_title == 'costco'] <- ''
loan$emp_title[loan$emp_title == 'fire fighter'] <- 'firefighter'
loan$emp_title[loan$emp_title == 'cvs pharmacy'] <- ''
loan$emp_title[loan$emp_title == 'president/ceo'] <- 'chief executive officer'
loan$emp_title[loan$emp_title == 'self'] <- 'self employed'
loan$emp_title[loan$emp_title == 'sr. analyst'] <- 'senior analyst'
loan$emp_title[loan$emp_title == 'federal government'] <- ''
loan$emp_title[loan$emp_title == 'citi bank'] <- ''
loan$emp_title[loan$emp_title == 'pnc bank'] <- ''
loan$emp_title[loan$emp_title == 'td bank'] <- ''
loan$emp_title[loan$emp_title == 'nypd'] <- 'police'
loan$emp_title[loan$emp_title == 'sr. accountant'] <- 'senior accountant'
loan$emp_title[loan$emp_title == 'citigroup'] <- ''
loan$emp_title[loan$emp_title == 'irs'] <- ''
loan$emp_title[loan$emp_title == 'staff'] <- ''
loan$emp_title[loan$emp_title == "macy's"] <- ''
loan$emp_title[loan$emp_title == 'frito lay'] <- ''
loan$emp_title[loan$emp_title == 'r.n.'] <- 'nurse'
loan$emp_title[loan$emp_title == 'vice-president'] <- 'vice president'
loan$emp_title[loan$emp_title == 'state farm insurance'] <- ''
loan$emp_title[loan$emp_title == 'e6'] <- ''
loan$emp_title[loan$emp_title == 'united states navy'] <- 'navy'
loan$emp_title[loan$emp_title == 'sr. software engineer'] <- ''
loan$emp_title[loan$emp_title == 'morgan stanley'] <- ''
loan$emp_title[loan$emp_title == 'time warner cable'] <- ''
loan$emp_title[loan$emp_title == 'sr. business analyst'] <- ''
loan$emp_title[loan$emp_title == 'nordstrom'] <- ''
loan$emp_title[loan$emp_title == 'att'] <- ''
loan$emp_title[loan$emp_title == 'microsoft'] <- ''
loan$emp_title[loan$emp_title == 'sr. consultant'] <- 'senior consultant'
loan$emp_title[loan$emp_title == 'pct'] <- ''
loan$emp_title[loan$emp_title == 'sr. director'] <- 'senior director'
loan$emp_title[loan$emp_title == 'sr. engineer'] <- 'senior engineer'
loan$emp_title[loan$emp_title == 'thompson reuters'] <- ''
loan$emp_title[loan$emp_title == 'sr accountant'] <- 'senior accountant'
loan$emp_title[loan$emp_title == 'bae systems'] <- ''
loan$emp_title[loan$emp_title == 'sr. director'] <- ''
loan$emp_title[loan$emp_title == 'staff rn'] <- 'nurse'
loan$emp_title[loan$emp_title == 'millwright'] <- ''
loan$emp_title[loan$emp_title == 'department manager'] <- ''
loan$emp_title[loan$emp_title == 'best buy'] <- ''
loan$emp_title[loan$emp_title == 'home depot'] <- ''
loan$emp_title[loan$emp_title == 'bank of america'] <- ''
loan$emp_title[loan$emp_title == 'verizon wireless'] <- ''
loan$emp_title[loan$emp_title == 'us government'] <- ''
loan$emp_title[loan$emp_title == 'capital one'] <- ''
loan$emp_title[loan$emp_title == 'the home depot'] <- ''


loan$emp_title[loan$emp_title == 'united states air force'] <- 'air force'
loan$emp_title[loan$emp_title == 'united parcel service'] <- ''
loan$emp_title[loan$emp_title == 'comcast'] <- ''
loan$emp_title[loan$emp_title == 'citi bank'] <- ''
loan$emp_title[loan$emp_title == 'us navy'] <- 'navy'
loan$emp_title[loan$emp_title == 'state of california'] <- ''
loan$emp_title[loan$emp_title == 'hewlett packard'] <- ''

loan$emp_title[loan$emp_title == 'cfo'] <- 'chief financial officer'
loan$emp_title[loan$emp_title == 'department of homeland security'] <- ''
loan$emp_title[loan$emp_title == 'us bank'] <- ''
loan$emp_title[loan$emp_title == 'u.s. postal service'] <- ''
loan$emp_title[loan$emp_title == 'chase'] <- ''
loan$emp_title[loan$emp_title == 'svp'] <- 'senior vice president'
loan$emp_title[loan$emp_title == 'cto'] <- 'chief technical officer'
loan$emp_title[loan$emp_title == 'gm'] <- 'manager'
loan$emp_title[loan$emp_title == 'general manager'] <- 'manager'
loan$emp_title[loan$emp_title == 'general mgr'] <- 'manager'
loan$emp_title[loan$emp_title == 'utility'] <- ''
loan$emp_title[loan$emp_title == 'director of engineering'] <- 'engineering director'
loan$emp_title[loan$emp_title == 'director of sales'] <- 'sales director'
loan$emp_title[loan$emp_title == 'lpn'] <- 'nurse'
loan$emp_title[loan$emp_title == 'cna'] <- 'nurse assistant'
loan$emp_title[loan$emp_title == 'construction'] <- 'construction worker'
loan$emp_title[loan$emp_title == 'avp'] <- ''
loan$emp_title[loan$emp_title == 'assistant general manager'] <- 'assistant manager'
loan$emp_title[loan$emp_title == 'sr. administrative assistant'] <- 'senior administrative assistant'
loan$emp_title[loan$emp_title == 'coo'] <- 'chief operations officer'
loan$emp_title[loan$emp_title == 'lvn'] <- 'nurse'
loan$emp_title[loan$emp_title == 'director of nursing'] <- 'nursing director'
loan$emp_title[loan$emp_title == 'csr'] <- ''
loan$emp_title[loan$emp_title == 'rn manager'] <- 'nursing director'
loan$emp_title[loan$emp_title == 'sr business analyst'] <- 'senior business analyst'
loan$emp_title[loan$emp_title == 'home health aid'] <- 'home health aide'
loan$emp_title[loan$emp_title == 'sr. manager'] <- 'senior manager'
loan$emp_title[loan$emp_title == 'rn supervisor'] <- 'nursing director'
loan$emp_title[loan$emp_title == 'hairstylist'] <- 'hair stylist'
loan$emp_title[loan$emp_title == 'pepsico'] <- ''
loan$emp_title[loan$emp_title == 'sr analyst'] <- 'senior analyst'
loan$emp_title[loan$emp_title == 'kroger'] <- ''
loan$emp_title[loan$emp_title == 'fifth third bank'] <- ''
loan$emp_title[loan$emp_title == 'director of accounting'] <- 'accounting director'
loan$emp_title[loan$emp_title == 'director of admissions'] <- 'admissions director'
loan$emp_title[loan$emp_title == 'director of facilities'] <- 'facilities director'
loan$emp_title[loan$emp_title == 'bb&t'] <- ''
loan$emp_title[loan$emp_title == 'state of michigan'] <- ''
loan$emp_title[loan$emp_title == 'safeway'] <- ''
loan$emp_title[loan$emp_title == 'dod'] <- ''
loan$emp_title[loan$emp_title == 'csc'] <- ''
loan$emp_title[loan$emp_title == 'metlife'] <- ''
loan$emp_title[loan$emp_title == 'state of florida'] <- ''
loan$emp_title[loan$emp_title == 'ernst & young'] <- ''
loan$emp_title[loan$emp_title == 'centurylink'] <- ''
loan$emp_title[loan$emp_title == 'clark county school district'] <- ''
loan$emp_title[loan$emp_title == 'senior vp'] <- 'senior vice president'
loan$emp_title[loan$emp_title == 'dsp'] <- 'direct support professional'
loan$emp_title[loan$emp_title == 'sgt'] <- 'sergeant'
loan$emp_title[loan$emp_title == 'admin. assistant'] <- 'admin assistant'
loan$emp_title[loan$emp_title == 'american airlines'] <- ''
loan$emp_title[loan$emp_title == 'registered dental assistant'] <- 'dental assistant'
loan$emp_title[loan$emp_title == 'booz allen hamilton'] <- ''
loan$emp_title[loan$emp_title == 'social security administration'] <- ''
loan$emp_title[loan$emp_title == 'sales'] <- ''
loan$emp_title[loan$emp_title == 'jpmorgan chase'] <- ''
loan$emp_title[loan$emp_title == 'united states postal service'] <- ''
loan$emp_title[loan$emp_title == 'southwest airlines'] <- ''
loan$emp_title[loan$emp_title == 'los angeles unified school district'] <- ''
loan$emp_title[loan$emp_title == 'va medical center'] <- ''
loan$emp_title[loan$emp_title == 'department of defense'] <- ''
loan$emp_title[loan$emp_title == 'chase bank'] <- ''
loan$emp_title[loan$emp_title == 'staff accountant'] <- 'accountant'
loan$emp_title[loan$emp_title == 'union pacific railroad'] <- ''
loan$emp_title[loan$emp_title == 'department of justice'] <- ''
loan$emp_title[loan$emp_title == 'director of marketing'] <- 'marketing director'
loan$emp_title[loan$emp_title == 'office'] <- ''
loan$emp_title[loan$emp_title == 'staff'] <- ''
loan$emp_title[loan$emp_title == 'faculty'] <- ''
loan$emp_title[loan$emp_title == 'director of finance'] <- 'finance director'
loan$emp_title[loan$emp_title == 'us postal service'] <- ''
loan$emp_title[loan$emp_title == 'internal revenue service'] <- ''
loan$emp_title[loan$emp_title == 'president/owner'] <- 'owner'
loan$emp_title[loan$emp_title == 'finance'] <- ''
loan$emp_title[loan$emp_title == 'engineering'] <- 'engineer'
loan$emp_title[loan$emp_title == 'manufacturing'] <- 'manufacturer'
loan$emp_title[loan$emp_title == 'soldier'] <- 'army'
loan$emp_title[loan$emp_title == 'owner/operator'] <- 'owner'
loan$emp_title[loan$emp_title == 'vp marketing'] <- 'marketing vp'
loan$emp_title[loan$emp_title == 'manger'] <- 'manager'
loan$emp_title[loan$emp_title == 'english teacher'] <- 'teacher'
loan$emp_title[loan$emp_title == 'group leader'] <- 'team lead'
loan$emp_title[loan$emp_title == 'sr financial analyst'] <- 'senior financial analyst'
loan$emp_title[loan$emp_title == 'operations'] <- ''
loan$emp_title[loan$emp_title == 'register nurse'] <- ''
loan$emp_title[loan$emp_title == 'service'] <- ''
loan$emp_title[loan$emp_title == 'vp of operations'] <- 'operations vp'
loan$emp_title[loan$emp_title == 'waitress'] <- 'server'
loan$emp_title[loan$emp_title == 'waiter'] <- 'server'
loan$emp_title[loan$emp_title == 'owner/manager'] <- 'owner'
loan$emp_title[loan$emp_title == 'staff nurse'] <- 'nurse'
loan$emp_title[loan$emp_title == 'science teacher'] <- 'teacher'
loan$emp_title[loan$emp_title == 'math teacher'] <- 'teacher'
loan$emp_title[loan$emp_title == 'lead teacher'] <- 'teacher'
loan$emp_title[loan$emp_title == 'elementary teacher'] <- 'teacher'
loan$emp_title[loan$emp_title == 'lead teacher'] <- 'teacher'
loan$emp_title[loan$emp_title == 'accounts manager'] <- 'account manager'
loan$emp_title[loan$emp_title == 'asst manager'] <- 'assistant manager'
loan$emp_title[loan$emp_title == 'business office manager'] <- 'manager'
loan$emp_title[loan$emp_title == 'business manager'] <- 'manager'
loan$emp_title[loan$emp_title == 'client service manager'] <- 'client services manager'
loan$emp_title[loan$emp_title == 'financial manager'] <- 'finance manager'
loan$emp_title[loan$emp_title == 'hr manager'] <- 'human resources manager'
loan$emp_title[loan$emp_title == 'human resource manager'] <- 'human resources manager'
loan$emp_title[loan$emp_title == 'asst manager'] <- 'assistant manager'
loan$emp_title[loan$emp_title == 'key account manager'] <- 'account manager'
loan$emp_title[loan$emp_title == 'asst manager'] <- 'assistant manager'
loan$emp_title[loan$emp_title == 'market manager'] <- 'marketing manager'
loan$emp_title[loan$emp_title == 'nurse case manager'] <- 'nurse manager'
loan$emp_title[loan$emp_title == 'office manager'] <- 'manager'
loan$emp_title[loan$emp_title == 'operation manager'] <- 'operations manager'
loan$emp_title[loan$emp_title == 'ops manager'] <- 'operations manager'
loan$emp_title[loan$emp_title == 'program manager'] <- 'manager'
loan$emp_title[loan$emp_title == 'product manager'] <- 'manager'
loan$emp_title[loan$emp_title == 'quality manager'] <- 'quality assurance manager'
loan$emp_title[loan$emp_title == 'rn case manager'] <- 'nurse manager'




























































#creating dataset with only the popular occupations, cleaned up
loan_emp_title = loan %>%
  group_by(emp_title) %>%
  filter(n()>200) %>% select(id, emp_title, annual_inc, verification_status) %>% filter(emp_title != '')

loan_emp_title = loan_emp_title %>% filter(!is.na(emp_title))
loan_emp_title$verification_status = as.factor(loan_emp_title$verification_status)
loan_emp_title$verification_status[loan_emp_title$verification_status == 'Source Verified'] <- 'Verified'


#income verification, ANOVA Way
#trying = lm(annual_inc ~ verification_status*emp_title, data = loan_emp_title)

#testing assumptions, normality, does not appear to be normal
#anova(trying)
#plot(trying,2)
#shapiro.test(trying)

#constant variance assumption is not met
#leveneTest(trying)

#two-sample t-test route
#loan_emp_title_samp = loan_emp_title %>% sample_frac(.03)
#shapiro.test(loan_emp_title_samp$annual_inc[loan_emp_title_samp$verification_status=="Not Verified"])
#still not normal

#var.test(annual_inc ~ verification_status, data = loan_emp_title_samp)
#variances are still different

#going to non-parametric, kurskal-wallis test for distributional dominance
summary_df <- loan_emp_title %>% group_by(emp_title) %>% filter(length(unique(as.factor(loan_emp_title$verification_status))) == 2) 
summary = summary_df %>% group_by(emp_title) %>% count(sort=TRUE) %>% filter(emp_title != "")

#kw_test<-as.data.frame(matrix(nrow=dim(summary)[1],ncol=2))
#colnames(kw_test)<-c("title","pval")

#creating dataset
#j = 1
#for (i in summary$emp_title){
  #print(i)
  #kw_test$title[j] = i
  #df = summary_df %>% filter(emp_title == i)
  #kw_test$pval[j] = kruskal.test(log(annual_inc) ~ verification_status, data = df)$p.value
  #print(kruskal.test(log(annual_inc) ~ verification_status, data = df)$p.value)
  #j = j + 1
#}

#hist(kw_test$pval, breaks = 30)

#kw_test %>% filter(pval <= .000001)



#two-sample t-test way starting here
wilc_test<-as.data.frame(matrix(nrow=dim(summary)[1],ncol=2))
colnames(wilc_test)<-c("title","pval")
j = 1
for (i in summary$emp_title){
  print(i)
  wilc_test$title[j] = i
  df = summary_df %>% filter(emp_title == i)
  wilc_test$pval[j] = wilcox.test(annual_inc ~ verification_status, data = df)$p.value
  print(wilcox.test(annual_inc ~ verification_status, data = df)$p.value)
  j = j + 1
}


hist(wilc_test$pval, breaks = 30)

wilc_test %>% filter(pval <= .000001)


#looking at the outliers and see results
#manager
summary_manager = summary_df %>% filter(emp_title == "manager") %>% filter(annual_inc > 200000 & annual_inc < 1000000)

ggplot(summary_manager,aes(x=annual_inc)) +
  geom_density(data=subset(summary_manager,verification_status == 'Verified'),aes(fill=verification_status), alpha = 0.2) +
  geom_density(data=subset(summary_manager,verification_status == 'Not Verified'),aes(fill=verification_status), alpha = 0.2) +
  labs(x = "Annual Income")

#teacher
#overall graph, teachers
summary_teacher = summary_df %>% filter(emp_title == "teacher") %>% filter(annual_inc < 150000)
ggplot(summary_teacher,aes(x=annual_inc/1000)) +
  geom_density(data=subset(summary_teacher,verification_status == 'Verified'),aes(fill=verification_status), alpha = 0.2) +
  geom_density(data=subset(summary_teacher,verification_status == 'Not Verified'),aes(fill=verification_status), alpha = 0.2) +
  labs(x = "Annual Income (Thousands $)",y="Density") + ggtitle("Teacher Annual Income Outlier Investigation") + 
  guides(fill=guide_legend(title='Verification Status')) + theme(plot.title = element_text(hjust = 0.5))


#outlier graph
summary_teacher = summary_df %>% filter(emp_title == "teacher") #%>% filter(annual_inc > 150000 & annual_inc < 500000)

ggplot(summary_teacher,aes(x=annual_inc/1000)) +
  geom_density(data=subset(summary_teacher,verification_status == 'Verified'),aes(fill=verification_status), alpha = 0.2) +
  geom_density(data=subset(summary_teacher,verification_status == 'Not Verified'),aes(fill=verification_status), alpha = 0.2) +
  labs(x = "Annual Income (Thousands $)",y="Density") + ggtitle("Teacher Annual Income Outlier Investigation") +
  guides(fill=guide_legend(title='Verification Status')) + theme(plot.title = element_text(hjust = 0.5))

#0.01478353 unverified defaulted
#344/11827 =  0.03203285 verified defaulted
combined = rbind(train_master_processed,test_master_processed) %>% select(id, default)
merged = merge(summary_teacher, combined, by='id')
merged %>% filter(default == 1 & verification_status == "Not Verified") %>% nrow()/merged %>% filter(verification_status == "Not Verified") %>% nrow()
merged %>% filter(default == 1 & verification_status == "Verified") %>% nrow()/merged %>% filter(verification_status == "Verified") %>% nrow()


#pilot summary
summary_pilot = summary_df %>% filter(emp_title == "pilot")
ggplot(summary_pilot,aes(x=annual_inc/1000)) +
  geom_density(data=subset(summary_pilot,verification_status == 'Verified'),aes(fill=verification_status), alpha = 0.2) +
  geom_density(data=subset(summary_pilot,verification_status == 'Not Verified'),aes(fill=verification_status), alpha = 0.2) +
  labs(x = "Annual Income (Thousands $)",y="Density") + ggtitle("Pilot Annual Income") + 
  guides(fill=guide_legend(title='Verification Status')) + theme(plot.title = element_text(hjust = 0.5))

#outlier investigation, no unverified defaulted
merged = merge(summary_pilot, combined, by='id')



#physician 
summary_physician = summary_df %>% filter(emp_title == "physician")
ggplot(summary_physician,aes(x=annual_inc/1000)) +
  geom_density(data=subset(summary_physician,verification_status == 'Verified'),aes(fill=verification_status), alpha = 0.2) +
  geom_density(data=subset(summary_physician,verification_status == 'Not Verified'),aes(fill=verification_status), alpha = 0.2) +
  labs(x = "Annual Income (Thousands $)",y="Density") + ggtitle("Physician Annual Income") + 
  guides(fill=guide_legend(title='Verification Status')) + theme(plot.title = element_text(hjust = 0.5))

#outlier investigation, 2 unverified defaulted
merged = merge(summary_physician, combined, by='id')
merged %>% filter(default == 1 & verification_status == "Not Verified") %>% nrow()/merged %>% filter(verification_status == "Not Verified") %>% nrow()
merged %>% filter(default == 1 & verification_status == "Verified") %>% nrow()/merged %>% filter(verification_status == "Verified") %>% nrow()


#engineer 
summary_engineer = summary_df %>% filter(emp_title == "engineer")
ggplot(summary_physician,aes(x=annual_inc/1000)) +
  geom_density(data=subset(summary_engineer,verification_status == 'Verified'),aes(fill=verification_status), alpha = 0.2) +
  geom_density(data=subset(summary_engineer,verification_status == 'Not Verified'),aes(fill=verification_status), alpha = 0.2) +
  labs(x = "Annual Income (Thousands $)",y="Density") + ggtitle("Engineer Annual Income") + 
  guides(fill=guide_legend(title='Verification Status')) + theme(plot.title = element_text(hjust = 0.5))

#unverified default rate: 0.01478353
#verified default rate: 0.03203285
merged = merge(summary_engineer, combined, by='id')
merged %>% filter(default == 1 & verification_status == "Not Verified") %>% nrow()/merged %>% filter(verification_status == "Not Verified") %>% nrow()
merged %>% filter(default == 1 & verification_status == "Verified") %>% nrow()/merged %>% filter(verification_status == "Verified") %>% nrow()




#driver 
summary_driver = summary_df %>% filter(emp_title == "driver")
ggplot(summary_driver,aes(x=annual_inc/1000)) +
  geom_density(data=subset(summary_driver,verification_status == 'Verified'),aes(fill=verification_status), alpha = 0.2) +
  geom_density(data=subset(summary_driver,verification_status == 'Not Verified'),aes(fill=verification_status), alpha = 0.2) +
  labs(x = "Annual Income (Thousands $)",y="Density") + ggtitle("Driver Annual Income") + 
  guides(fill=guide_legend(title='Verification Status')) + theme(plot.title = element_text(hjust = 0.5))

#unverified default rate: 0.01478353
#verified default rate: 0.03203285
merged = merge(summary_driver, combined, by='id')
merged %>% filter(default == 1 & verification_status == "Not Verified") %>% nrow()/merged %>% filter(verification_status == "Not Verified") %>% nrow()
merged %>% filter(default == 1 & verification_status == "Verified") %>% nrow()/merged %>% filter(verification_status == "Verified") %>% nrow()


#nurse 
summary_nurse = summary_df %>% filter(emp_title == "nurse") %>% filter(annual_inc < 100000)
ggplot(summary_nurse,aes(x=annual_inc/1000)) +
  geom_density(data=subset(summary_nurse,verification_status == 'Verified'),aes(fill=verification_status), alpha = 0.2) +
  geom_density(data=subset(summary_nurse,verification_status == 'Not Verified'),aes(fill=verification_status), alpha = 0.2) +
  labs(x = "Annual Income (Thousands $)",y="Density") + ggtitle("Nurse Annual Income") + 
  guides(fill=guide_legend(title='Verification Status')) + theme(plot.title = element_text(hjust = 0.5))

#unverified default rate: 0.01478353
#verified default rate: 0.03203285
merged = merge(summary_nurse, combined, by='id')
merged %>% filter(default == 1 & verification_status == "Not Verified") %>% nrow()/merged %>% filter(verification_status == "Not Verified") %>% nrow()
merged %>% filter(default == 1 & verification_status == "Verified") %>% nrow()/merged %>% filter(verification_status == "Verified") %>% nrow()


#accountant
summary_accountant = summary_df %>% filter(emp_title == "accountant")
ggplot(summary_accountant,aes(x=annual_inc/1000)) +
  geom_density(data=subset(summary_accountant,verification_status == 'Verified'),aes(fill=verification_status), alpha = 0.2) +
  geom_density(data=subset(summary_accountant,verification_status == 'Not Verified'),aes(fill=verification_status), alpha = 0.2) +
  labs(x = "Annual Income (Thousands $)",y="Density") + ggtitle("Accountant Annual Income") + 
  guides(fill=guide_legend(title='Verification Status')) + theme(plot.title = element_text(hjust = 0.5))

#unverified default rate: 0.01478353
#verified default rate: 0.03203285
merged = merge(summary_accountant, combined, by='id')
merged %>% filter(default == 1 & verification_status == "Not Verified") %>% nrow()/merged %>% filter(verification_status == "Not Verified") %>% nrow()
merged %>% filter(default == 1 & verification_status == "Verified") %>% nrow()/merged %>% filter(verification_status == "Verified") %>% nrow()


#financial advisor
summary_fa = summary_df %>% filter(emp_title == "financial advisor")
ggplot(summary_fa,aes(x=annual_inc/1000)) +
  geom_density(data=subset(summary_fa,verification_status == 'Verified'),aes(fill=verification_status), alpha = 0.2) +
  geom_density(data=subset(summary_fa,verification_status == 'Not Verified'),aes(fill=verification_status), alpha = 0.2) +
  labs(x = "Annual Income (Thousands $)",y="Density") + ggtitle("Financial Advisor Annual Income") + 
  guides(fill=guide_legend(title='Verification Status')) + theme(plot.title = element_text(hjust = 0.5))

#unverified default rate: 0.01932367
#verified default rate: 0.01251739
merged = merge(summary_fa, combined, by='id')
merged %>% filter(default == 1 & verification_status == "Not Verified") %>% nrow()/merged %>% filter(verification_status == "Not Verified") %>% nrow()
merged %>% filter(default == 1 & verification_status == "Verified") %>% nrow()/merged %>% filter(verification_status == "Verified") %>% nrow()








#occupations graph
summary_filtered = summary[1:5,]
top_5_occ <- ggplot(summary_filtered,aes(fct_rev(fct_reorder(emp_title,
                                                             n)),
                                         n)) + 
  geom_bar(stat='identity',fill='steelblue4') +
   ylab("# of Borrowers") + scale_y_continuous(labels=comma)
top_5_occ










# BAD CODE starting here
#getting train and test data
train <- loan_emp_title %>% sample_frac(0.7)
test <- anti_join(loan_emp_title, train, by='id')
train_sample <- train %>% sample_frac(0.05) %>% select(verification_status,annual_inc,emp_title)

#sliced ANOVA - cannot do because assumptions are not met
loan_emp_title_aov <- loan_emp_title %>% 
  group_by(emp_title) %>%
  nest() %>%
  mutate(aov = map(data, ~summary(aov(annual_inc ~ verification_status, data = .x))))







#word cloud
summary_emp_title = loan_emp_title %>% group_by(emp_title) %>% count(sort=TRUE) %>% filter(emp_title != "")
summary_emp_title_samp = summary_emp_title[1:20,]
#install.packages("wordcloud")
#library(wordcloud)
#install.packages('Rcpp')
#library(Rcpp)
#set.seed(1234) # for reproducibility 
#wordcloud(words = summary_emp_title_samp$emp_title, freq = summary_emp_title_samp$n)


#summary_emp_title_samp %>%
  #head() %>%
  #with(wordcloud(emp_title, n))

#income verification












#all testing code
a = data.frame(unique(loan_emp_title$emp_title))

sort(table(loan_emp_title$emp_title),decreasing=TRUE)
summary(loan_emp_title$emp_title)

write.csv(loan_emp_title,"loan_employee_information.csv")

set.seed(42)
loan_emp_title %>% mutate(id = row_number())
train <- loan_emp_title %>% sample_frac(0.7)
test <- anti_join(loan_emp_title, train, by='id')
train_sample <- train %>% sample_frac(0.05) %>% select(emp_length,annual_inc,emp_title,addr_state)

#testing relationship with income and emp_title, years of employment, and state
aov <- aov(annual_inc ~ factor(emp_title) + factor(emp_length) + factor(addr_state), data = train_sample)
summary(aov)

#delete addr_states, not significant
aov <- aov(annual_inc ~ emp_title*emp_length, data = train_sample)
summary(aov)


tukey.aov <- TukeyHSD(aov)
print(aov)




loan_emp_title_lm <- aov(money ~ emp_title*addr_state, data=train) #at least one group is statistically significant than the rest
anova(loan_emp_title_lm)
summary(loan_emp_title_lm)

#which group is different?
tukey.loan_emp_title_lm <- TukeyHSD(loan_emp_title_lm)

#checking only the top 5 occupations
loan_emp_title_topfive = loan_emp_title[loan_emp_title$emp_title %in% c('manager','nurse','teacher','owner','supervisor'), ]
loan_emp_title_topfive %>% mutate(id=row_number())
train <- loan_emp_title_topfive %>% sample_frac(0.7)
test <- anti_join(loan_emp_title_topfive, train, by='id')

topfive_lm <- aov(money ~ emp_title, data=train)
anova(topfive_lm)
summary(topfive_lm)

#trying year to predict loan amount
loan %>% mutate(id=row_number())
loan$issue_d_day <- NULL
loan$last_credit_pull_d_day <- NULL
loan$next_pymnt_d_day <- NULL
loan$last_pymnt_d_day <- NULL
loan$earliest_cr_line_day <- NULL
loan$policy_code <- NULL
loan$id <- NULL
loan$member_id <- NULL
train <- loan %>% sample_frac(0.7)
test <- anti_join(loan, train, by='id')
year_lm <- lm(loan_amnt ~ issue_d_month, data = train)
anova(year_lm)
summary(year_lm)

#state predict loan amount?
state_lm <- lm(int_rate ~ addr_state, data = train)
anova(state_lm)
summary(state_lm)

install.packages("car")
library(car)
model = lm(loan_amnt~.,train)
vif(model)





