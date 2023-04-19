names(cera)

cer <- CERA_Survey_PD22_Graves_9225_Data_Key
dim(cer)
dim(cera)
cera <- CERA_Survey_PD22_Graves_9225_Data_Key
names(cer) <-  c("Q1_1","res_Q1","residency_Q1.2",
                  "state_Q2_4","size_community_Q3","num_resident_Q4",
                  "med_degree_Q5","current_num_year_director_Q6","total_num_year_director_Q7",
                  "gender_Q10","gender_other_Q_11","native_american_Q9.1",
                  "asian_Q9.2","black_Q9.3","hispanicQ9.4",
                  "middle_east_Q9.5","hawaiian_Q9.6","white_Q9.7",
                  "wont_disclose_Q9.8","self_identify_med_Q10","children_ASD_Q1",
                  "adults_ASD_Q2","resident_ASD_Q3","Used_ASD_Never_Q4.1",
                  "used_ASD_sometimes_Q4.2","used_ASD_often_Q4.3","used_ASD_no_curri_Q4.4",
                  "used_ASD_not_aware_Q4.5","education_ASD_Q5","family_med_ASD_Q6",
                  "patient_ASD_Q7","clinical_ASD_Q8","area_ASD_Q9",
                  "area_adult_Q10")




cera_new <- cer %>% mutate(
  res_Q1 = case_when(
    res_Q1 == 1 ~ "University Based",
    res_Q1 == 2 ~ "Community Based, University-Affiliated",
    res_Q1 == 3 ~ "Community Based Non_affiliated",
    res_Q1 == 4 ~ "Military",
    res_Q1 == 0 ~ "Other",
    TRUE ~ as.character(NA)
  ),
  state_Q2_4 = case_when(
    state_Q2_4 == 1 ~ "New England",
    state_Q2_4 == 2 ~ "Middle Atlantic",
    state_Q2_4 == 3 ~ "South Atlantic",
    state_Q2_4 == 4 ~ "East South Central",
    state_Q2_4 == 5 ~ "East North Central",
    state_Q2_4 == 6 ~ "West South Crntral",
    state_Q2_4 == 7 ~ "West North Central",
    state_Q2_4 == 8 ~ "Mountain",
    state_Q2_4 == 9 ~ "Pacific",
    TRUE ~ as.character(NA)
  ),
  size_community_Q3 = case_when(
    size_community_Q3 == 1 ~ "Less than 30,000",
    size_community_Q3 == 2 ~ "30,000 to 74,999",
    size_community_Q3 == 3 ~ "75,000 to 149,999",
    size_community_Q3 == 4 ~ "150,000 to 499,999",
    size_community_Q3 == 5 ~ "500,000 to 1 million",
    size_community_Q3 == 6 ~ "More than 1 million",
    TRUE ~ as.character(NA)
    
  ),
  num_resident_Q4 = case_when(
    num_resident_Q4 == 1 ~ "< 19",
    num_resident_Q4 == 2 ~ "19-31",
    num_resident_Q4 == 3 ~ ">31",
    TRUE ~ as.character(NA)
  ),
  med_degree_Q5 = case_when(
    med_degree_Q5 == 1 ~ "MD",
    med_degree_Q5 == 2 ~ "DO",
    TRUE ~ as.character(NA)
  ),
  #  current_num_year_director_Q6 = case_when(),
  #  total_num_year_director_Q7 = case_when(),
  gender_Q10 = case_when(
    gender_Q10 == 1 ~ "Female/Women" , 
    gender_Q10 == 2 ~ "Male/Men", 
    gender_Q10 == 3 ~ "Genderqueer/Gender Non-conforming", 
    gender_Q10 == 4 ~ "Non-binary", 
    gender_Q10 == 5 ~ "Other",
    TRUE ~ as.character(NA)
  ),
  #  gender_other_Q_11 = case_when(),
  native_american_Q9.1 = ifelse(native_american_Q9.1 == 1, "Yes", "No"),
  asian_Q9.2 = ifelse(asian_Q9.2 == 2, "Yes", "No"),
  black_Q9.3 = ifelse(black_Q9.3== 3, "Yes", "No"),
  hispanicQ9.4 = ifelse(hispanicQ9.4 == 4, "Yes", "No"),
  middle_east_Q9.5 = ifelse(middle_east_Q9.5 == 5, "Yes", "No"),
  hawaiian_Q9.6 = ifelse(hawaiian_Q9.6 == 6, "Yes","No"),
  white_Q9.7 = ifelse(white_Q9.7 == 7, "Yes", "No"),
  wont_disclose_Q9.8 = ifelse(wont_disclose_Q9.8 == 8, "Yes", "No"),
  
  race = case_when(
    native_american_Q9.1 == "Yes" ~ "Native American",
    asian_Q9.2 == "Yes" ~ "Asian",
    black_Q9.3 == "Yes" ~ "Black",
    hispanicQ9.4 == "Yes" ~ "Hispanic",
    middle_east_Q9.5 == "Yes" ~ "Middle Eastern",
    hawaiian_Q9.6 == "Yes" ~ "Hawaiian",
    white_Q9.7 == "Yes" ~ "White",
    TRUE ~ as.character(NA)
  ),
  
  self_identify_med_Q10 = case_when(
    self_identify_med_Q10 == 1 ~ "No",
    self_identify_med_Q10 == 2 ~ "Yes",
    TRUE ~ as.character(NA)
  ),
  children_ASD_Q1 = factor(children_ASD_Q1, ordered = TRUE),
  #case_when(
  #  children_ASD_Q1 == 1 ~ "Strongly disagree",
  #  children_ASD_Q1 == 2 ~ "Moderately disagree",
  #  children_ASD_Q1 == 3 ~ "Slightly disagree",
  #  children_ASD_Q1 == 4 ~ "Slightly agree",
  #  children_ASD_Q1 == 5 ~ "Moderately agree",
  #  children_ASD_Q1 == 6 ~ "Strongly agree",
  #  TRUE ~ as.character(NA)
  #),
  adults_ASD_Q2 = factor(adults_ASD_Q2, ordered = TRUE),
  #case_when(
  #  adults_ASD_Q2 == 1 ~ "Strongly disagree",
  #  adults_ASD_Q2 == 2 ~ "Moderately disagree",
  #  adults_ASD_Q2 == 3 ~ "Slightly disagree",
  #  adults_ASD_Q2 == 4 ~ "Slightly agree",
  #  adults_ASD_Q2 == 5 ~ "Moderately agree",
  #  adults_ASD_Q2 == 6 ~ "Strongly agree",
  #  TRUE ~ as.character(NA)
  #),
  resident_ASD_Q3 = factor(resident_ASD_Q3, ordered = TRUE),
  #case_when(
  #  resident_ASD_Q3 == 1 ~ "Strongly disagree",
  #  resident_ASD_Q3 == 2 ~ "Moderately disagree",
  #  resident_ASD_Q3 == 3 ~ "Slightly disagree",
  #  resident_ASD_Q3 == 4 ~ "Slightly agree",
  #  resident_ASD_Q3 == 5 ~ "Moderately agree",
  #  resident_ASD_Q3 == 6 ~ "Strongly Agree",
  #  TRUE ~ as.character(NA)
  #),
  Used_ASD_Never_Q4.1 = ifelse(Used_ASD_Never_Q4.1 == 1, "Yes", "No"),
  used_ASD_sometimes_Q4.2 = ifelse(used_ASD_sometimes_Q4.2 == 2, "Yes", "No"),
  used_ASD_often_Q4.3 = ifelse(used_ASD_often_Q4.3 == 3, "Yes", "No"),
  used_ASD_no_curri_Q4.4 = ifelse(used_ASD_no_curri_Q4.4 == 4, "Yes", "No"),
  used_ASD_not_aware_Q4.5 = ifelse(used_ASD_not_aware_Q4.5 == 5, "Yes", "No" ),
  education_ASD_Q5 = case_when(
    education_ASD_Q5 == 1 ~ "No access to ASD specialists",
    education_ASD_Q5 == 2 ~ "No",
    education_ASD_Q5 == 3 ~ "Yes",
    education_ASD_Q5 == 4 ~ "I don't Know",
    TRUE ~ as.character(NA)
    
  ),
  family_med_ASD_Q6 = ifelse(family_med_ASD_Q6 == 1, "No", "Yes"),
  patient_ASD_Q7 = factor(patient_ASD_Q7, ordered =  TRUE),
  #case_when(
  #  patient_ASD_Q7 == 1 ~ "Strongly disagree",
  #  patient_ASD_Q7 == 2 ~ "Moderately disagree",
  #  patient_ASD_Q7 == 3 ~ "Slightly disagree",
  #  patient_ASD_Q7 == 4 ~ "Slightly agree",
  #  patient_ASD_Q7 == 5 ~ "Moderately agree",
  #  patient_ASD_Q7 == 6 ~ "Strongly agree",
  #  TRUE ~ as.character(NA)
  
  #  ),
  clinical_ASD_Q8 = factor(clinical_ASD_Q8, ordered = TRUE),
  #case_when(
  #clinical_ASD_Q8 == 1 ~ "Strongly disagree",
  #clinical_ASD_Q8 == 2 ~ "Moderately disagree",
  #clinical_ASD_Q8 == 3 ~ "Slightly disagree",
  #clinical_ASD_Q8 == 4 ~ "Slightly agree",
  #clinical_ASD_Q8 == 5 ~ "Moderately agree",
  #clinical_ASD_Q8 == 6 ~ "Strongly agree",
  #TRUE ~ as.character(NA)
  #  ),
  area_ASD_Q9 = case_when(
    area_ASD_Q9 == 1 ~ "Screening",
    area_ASD_Q9 == 2 ~ "Diagnosis",
    area_ASD_Q9 == 3 ~ "Medical work-up and management",
    area_ASD_Q9 == 4 ~ "Community resources and family education",
    area_ASD_Q9 == 5 ~ "We have significant gaps in this curriculum",
    area_ASD_Q9 == 6 ~ "We have no significant gaps in this curriculum",
    area_ASD_Q9 == 7 ~ "ASD care is not part of FM scope in our community",
    TRUE ~ as.character(NA)
    
  ),
  area_adult_Q10 = case_when(
    area_adult_Q10 == 1 ~ "Screening",
    area_adult_Q10 == 2 ~ "Diagnosis",
    area_adult_Q10 == 3 ~ "Medical work-up and management",
    area_adult_Q10 == 4 ~ "Community resources and family education",
    area_adult_Q10 == 5 ~ "We have significant gaps in this curriculum",
    area_adult_Q10 == 6 ~ "We have no significant gaps in this curriculum",
    area_adult_Q10 == 7 ~ "ASD care is not part of FM scope in our community",
    TRUE ~ as.character(NA)
    
  )
  
  
) %>% mutate_if(is.character, as.factor)
cera_new <- cera_new %>% mutate(
  r1_child_ASD = case_when(
    children_ASD_Q1 == "1" | children_ASD_Q1 == "2" | children_ASD_Q1 == "3" ~ "Disagree",
    children_ASD_Q1 == "4" | children_ASD_Q1 == "5" | children_ASD_Q1 == "6" ~ "Agree",
    TRUE ~ as.character(NA)
  ),
  r2_adult_ASD = case_when(
    adults_ASD_Q2 == "1" | adults_ASD_Q2 =="2" | adults_ASD_Q2 =="3" ~ "Disagree",
    adults_ASD_Q2 == "4" | adults_ASD_Q2 =="5" | adults_ASD_Q2 =="6" ~ "Agree",
    TRUE ~ as.character(NA)
  ),
  r3_resid_ASD = case_when( 
    resident_ASD_Q3 == "1" | resident_ASD_Q3 == "2" | resident_ASD_Q3 == "3" ~ "Disagree",
    resident_ASD_Q3 == "4" | resident_ASD_Q3 == "5" | resident_ASD_Q3 == "6" ~ "Agree",
    TRUE ~ as.character(NA)
  ),
  ASD_use = case_when(
    Used_ASD_Never_Q4.1 == "Yes" ~ "Never",
    used_ASD_sometimes_Q4.2 == "Yes" ~ "Sometimes",
    used_ASD_often_Q4.3 == "Yes" ~ "Often",
    used_ASD_no_curri_Q4.4 == "Yes" ~ "Do not have a curriculum",
    used_ASD_not_aware_Q4.5 == "Yes" ~ "Not aware of these materials"
  )
)





apply(cera_new, 2, table)



# Table 1

library(gtsummary)

tab1_data <- cera_new %>% select(-c(r2_adult_ASD, r3_resid_ASD,
                                    Q1_1, residency_Q1.2,
                                    gender_other_Q_11,native_american_Q9.1,
                                    asian_Q9.2, black_Q9.3, hispanicQ9.4, middle_east_Q9.5,
                                    native_american_Q9.1, white_Q9.7, wont_disclose_Q9.8,
                                    children_ASD_Q1, adults_ASD_Q2, resident_ASD_Q3,
                                    Used_ASD_Never_Q4.1, used_ASD_sometimes_Q4.2, 
                                    used_ASD_often_Q4.3,used_ASD_no_curri_Q4.4,
                                    used_ASD_not_aware_Q4.5
)) %>% mutate_if(is.factor, as.character)
tab1_data %>% tbl_summary(
  by = r1_child_ASD
) %>% add_p()


# Table 1



tab2_data <- cera_new %>% select(-c(r1_child_ASD, r3_resid_ASD,
                                    Q1_1, residency_Q1.2,
                                    gender_other_Q_11,native_american_Q9.1,
                                    asian_Q9.2, black_Q9.3, hispanicQ9.4, middle_east_Q9.5,
                                    native_american_Q9.1, white_Q9.7, wont_disclose_Q9.8,
                                    children_ASD_Q1, adults_ASD_Q2, resident_ASD_Q3,
                                    Used_ASD_Never_Q4.1, used_ASD_sometimes_Q4.2, 
                                    used_ASD_often_Q4.3,used_ASD_no_curri_Q4.4,
                                    used_ASD_not_aware_Q4.5
)) %>% mutate_if(is.factor, as.character)
tab2_data %>% tbl_summary(
  by = r2_adult_ASD
) %>% add_p()



# Table 1



tab3_data <- cera_new %>% select(-c(r2_adult_ASD, r1_child_ASD,
                                    Q1_1, residency_Q1.2,
                                    gender_other_Q_11,native_american_Q9.1,
                                    asian_Q9.2, black_Q9.3, hispanicQ9.4, middle_east_Q9.5,
                                    native_american_Q9.1, white_Q9.7, wont_disclose_Q9.8,
                                    children_ASD_Q1, adults_ASD_Q2, resident_ASD_Q3,
                                    Used_ASD_Never_Q4.1, used_ASD_sometimes_Q4.2, 
                                    used_ASD_often_Q4.3,used_ASD_no_curri_Q4.4,
                                    used_ASD_not_aware_Q4.5
)) %>% mutate_if(is.factor, as.character)
tab3_data %>% tbl_summary(
  by = r3_resid_ASD
) %>% add_p()



# P-value of some test

#Q7
fisher.test( table(cera_new$patient_ASD_Q7, cera_new$r1_child_ASD) ,
             simulate.p.value = TRUE, B=1e7)
fisher.test( table(cera_new$patient_ASD_Q7, cera_new$r2_adult_ASD) ,
             simulate.p.value = TRUE, B=1e7)
# State
fisher.test( table(cera_new$state_Q2_4, cera_new$r1_child_ASD) ,
             simulate.p.value = TRUE, B=1e7)

fisher.test( table(cera_new$state_Q2_4, cera_new$r2_adult_ASD) ,
             simulate.p.value = TRUE, B=1e7)

fisher.test( table(cera_new$state_Q2_4, cera_new$r3_resid_ASD) ,
             simulate.p.value = TRUE, B=1e7)

#Q9
fisher.test( table(cera_new$area_ASD_Q9, cera_new$r1_child_ASD) ,
             simulate.p.value = TRUE, B=1e7)

fisher.test( table(cera_new$area_ASD_Q9, cera_new$r2_adult_ASD) ,
             simulate.p.value = TRUE, B=1e7)

fisher.test( table(cera_new$area_ASD_Q9, cera_new$r3_resid_ASD) ,
             simulate.p.value = TRUE, B=1e7)

#Q10
fisher.test( table(cera_new$area_adult_Q10, cera_new$r1_child_ASD) ,
             simulate.p.value = TRUE, B=1e7)

fisher.test( table(cera_new$area_adult_Q10, cera_new$r2_adult_ASD) ,
             simulate.p.value = TRUE, B=1e7)

fisher.test( table(cera_new$area_adult_Q10, cera_new$r3_resid_ASD) ,
             simulate.p.value = TRUE, B=1e7)





# Standard deviation and mean and t-test


cera_new %>% group_by(r1_child_ASD) %>% summarise(ME =mean(current_num_year_director_Q6, na.rm = TRUE) ,
                                                  SD = sd(current_num_year_director_Q6, na.rm = TRUE))

cera_new %>% group_by(r2_adult_ASD) %>% summarise(ME =mean(current_num_year_director_Q6, na.rm = TRUE) ,
                                                  SD = sd(current_num_year_director_Q6, na.rm = TRUE))
cera_new %>% group_by(r1_child_ASD) %>% summarise(ME =mean(current_num_year_director_Q6, na.rm = TRUE) ,
                                                  SD = sd(current_num_year_director_Q6, na.rm = TRUE))


cera_new %>% group_by(r1_child_ASD) %>% summarise(ME =mean(total_num_year_director_Q7, na.rm = TRUE) ,
                                                  SD = sd(total_num_year_director_Q7, na.rm = TRUE))

cera_new %>% group_by(r2_adult_ASD) %>% summarise(ME =mean(total_num_year_director_Q7, na.rm = TRUE) ,
                                                  SD = sd(total_num_year_director_Q7, na.rm = TRUE))
cera_new %>% group_by(r1_child_ASD) %>% summarise(ME =mean(total_num_year_director_Q7, na.rm = TRUE) ,
                                                  SD = sd(total_num_year_director_Q7, na.rm = TRUE))

t.test(cera_new$current_num_year_director_Q6~cera_new$r1_child_ASD)
t.test(cera_new$current_num_year_director_Q6~cera_new$r2_adult_ASD)
t.test(cera_new$current_num_year_director_Q6~cera_new$r3_resid_ASD)

t.test(cera_new$total_num_year_director_Q7~ cera_new$r1_child_ASD)
t.test(cera_new$total_num_year_director_Q7~ cera_new$r2_adult_ASD)
t.test(cera_new$total_num_year_director_Q7~ cera_new$r3_resid_ASD)


"res_Q1"                      



new_data <- cera_new %>% select(-c(Q1_1, residency_Q1.2,
                                   gender_other_Q_11,native_american_Q9.1,
                                   asian_Q9.2, black_Q9.3, hispanicQ9.4, middle_east_Q9.5,
                                   native_american_Q9.1, white_Q9.7, wont_disclose_Q9.8,
                                   children_ASD_Q1, adults_ASD_Q2, resident_ASD_Q3,
                                   Used_ASD_Never_Q4.1, used_ASD_sometimes_Q4.2, 
                                   used_ASD_often_Q4.3,used_ASD_no_curri_Q4.4,
                                   used_ASD_not_aware_Q4.5
)) %>% mutate_if(is.factor, as.character)

plot_data <- gather(new_data, condition, measurement,r1_child_ASD:r3_resid_ASD, factor_key = TRUE )



swr = function(string, nwrap=50) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)

#plot_data$condition <- swr(plot_data$condition)

#"res_Q1"                      

plot_data %>% group_by(res_Q1, measurement, condition) %>% count() %>% na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.008)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.017)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.042)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>% 
  ggplot(aes(x =res_Q1, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("Type of residency program")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name = "",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))





#[2] "state_Q2_4"                  

plot_data %>% group_by(state_Q2_4, measurement, condition) %>% count() %>% na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.61)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.76)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.33)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%  
  ggplot(aes(x =state_Q2_4, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("State of the residency program")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[3] "size_community_Q3"           

plot_data %>% group_by(size_community_Q3, measurement, condition) %>% count() %>% na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.6)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.7)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.6)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%  
  ggplot(aes(x =size_community_Q3, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("Size of the community")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[4] "num_resident_Q4"            

plot_data %>% group_by(num_resident_Q4, measurement, condition) %>% count() %>% na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.7)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.4)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.9)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%    
  ggplot(aes(x =num_resident_Q4, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("Number of resident (total complement)")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[5] "med_degree_Q5"               

plot_data %>% group_by(med_degree_Q5, measurement, condition) %>% count() %>% na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.057)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.036)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.4)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%  
  ggplot(aes(x =med_degree_Q5, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("Medical Degree")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[6] "current_num_year_director_Q6"
plot_data %>% select(current_num_year_director_Q6, measurement, condition) %>% 
  na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.5)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.67)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.51)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%  
  ggplot(aes(y =current_num_year_director_Q6,x = measurement,  fill = measurement ))+
  geom_boxplot()+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("How many years been current program director?")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))
#[7] "total_num_year_director_Q7"  
plot_data %>% select(total_num_year_director_Q7, measurement, condition) %>% 
  na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.76)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.87)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.83)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%   
  ggplot(aes(y =total_num_year_director_Q7,x = measurement, fill = measurement ))+
  geom_boxplot()+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("How many total years have been served as a program director?")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[8] "gender_Q10"                  
plot_data %>% group_by(gender_Q10, measurement, condition) %>% count() %>% na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.9)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.9)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.83)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%  
  ggplot(aes(x =gender_Q10, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("Gender")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))
#[10] "self_identify_med_Q10"       
plot_data %>% group_by(self_identify_med_Q10, measurement, condition) %>% count() %>% na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.8)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.8)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.9)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%  
  ggplot(aes(x =self_identify_med_Q10, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("self-identify as under-represented in medicine")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[11] "education_ASD_Q5"            
plot_data %>% group_by(education_ASD_Q5, measurement, condition) %>% count() %>% na.omit() %>%
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.016)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.008)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0,001)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%   
  ggplot(aes(x =education_ASD_Q5, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("Program engages interprofessional or interdisciplinary faculty  who deliver educational content on ASD to FM residents")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[12] "family_med_ASD_Q6"           
plot_data %>% group_by(family_med_ASD_Q6, measurement, condition) %>% count() %>% na.omit() %>%
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.023)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.006)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.001)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%  
  ggplot(aes(x =family_med_ASD_Q6, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("Program has a Family Medicine faculty champion who delivers educational content on ASD to FM residents")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[13] "patient_ASD_Q7"              
plot_data %>% group_by(patient_ASD_Q7, measurement, condition) %>% count() %>% na.omit() %>%
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.<0.001)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value <0.001)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value <0.001)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%   
  ggplot(aes(x =patient_ASD_Q7, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("Program has sufficient patients living with ASD to provide excellent clinical teaching about ASD")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[14] "clinical_ASD_Q8"             
plot_data %>% group_by(clinical_ASD_Q8, measurement, condition) %>% count() %>% na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value <0.001)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.001)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.001)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%  
  ggplot(aes(x =clinical_ASD_Q8, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("Family medicine faculty role model the healthcare of persons living with ASD in our clinical setting")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[15] "area_ASD_Q9"                 
plot_data %>% group_by(area_ASD_Q9, measurement, condition) %>% count() %>% na.omit() %>%
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value <0.001)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value <0.001)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value <0.001)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%   
  ggplot(aes(x =area_ASD_Q9, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("Significant GAP in curricular materials related to PEDIATRIC AND ADOLESCENT patients living with ASD?")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[16] "area_adult_Q10"              
plot_data %>% group_by(area_adult_Q10, measurement, condition) %>% count() %>% na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.001)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.001)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.004)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%  
  ggplot(aes(x =area_adult_Q10, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("State of the program director")+
  xlab("Significant GAP in curricular materials related to ADULT patients living with ASD?")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[17] "race"                        
plot_data %>% group_by(race, measurement, condition) %>% count() %>% na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.5)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.086)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.13)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%  
  ggplot(aes(x =race, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("Race/Ethnicity")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))

#[19] "ASD_use"
plot_data %>% group_by(ASD_use, measurement, condition) %>% count() %>% na.omit() %>% 
  mutate(
    condition = case_when(
      condition  == "r1_child_ASD" ~ "My program does an excellent job of preparing residents to deliver care to CHILDREN and ADOLESCENTS with ASD and their families
      (p-value 0.001)",
      condition  == "r2_adult_ASD" ~ "My program does an excellent job of preparing residents to deliver care to ADULTS with ASD and their families
      (p-value 0.002)",
      condition  == "r3_resid_ASD" ~ "My program does an excellent job of preparing residents to accept transitions of care of young ADULTS with ASD and their families from pediatric specialty care
      (p-value 0.001)",
      TRUE~ as.character(NA)
    ),
    condition = swr(condition)
  ) %>%  
  ggplot(aes(x =ASD_use, y = n, fill = measurement ))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~condition)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("Frequency")+
  ggtitle("Program uses published standardized curricular materials for educating FM residents on ASD program or Autism Case Training (ACT)")+
  xlab("")+
  scale_fill_brewer(
    palette = "Set1",
    name ="",
    direction = -1
  )+
  theme(axis.text.x = element_text(angle = 90))








