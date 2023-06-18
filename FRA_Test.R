library(readr)
raw = read_csv("https://raw.githubusercontent.com/Xinhao-Liu/FRA_Visualizer/main/All_year_FRA_1996_2022_4.2.2023.csv")

raw %>% 
  filter(!is.na(TYPE_clean), !is.na(TotalDerail), TotalDerail >= 0) %>% 
  mutate(Accident_type = ifelse(TYPE_clean == "01", "Derailments",
                                ifelse(TYPE_clean %in% c("02","03","04","05","06","08"), "Collisions",
                                       ifelse(TYPE_clean == "07", "Grade Crossing", "Other")))) %>% 
  mutate(Date = as.Date(as.character(Date),format = "%Y%m%d")) %>% 
  filter(`class 1` %in% c("non-1"),
         TrainType %in% c("O"),
         Accident_type %in% c("Other"),
         ACCTRK %in% c(1),
         Category %in% c("H"),
         Date <= "2020-01-01",
         Date >= "2010-01-01") %>% 
  filter(!is.na(Group)) %>% 
  group_by(Group) %>% 
  mutate(Frequency = n(), total_derail = sum(TotalDerail)) %>% 
  ungroup() %>% 
  mutate(frequency_ratio = Frequency/length(SUMS), severity_ratio = total_derail/sum(TotalDerail)) %>%
  group_by(Group) %>% 
  mutate(`Average Number of Cars Derailed` = round(total_derail/Frequency,1)) %>% 
  select(`Group Name`,Group, `Average Number of Cars Derailed`, Frequency, total_derail, severity_ratio) %>% 
  unique() %>% 
  arrange(desc(total_derail)) %>% 
  ungroup() %>% 
  mutate(`Cumulative Percentage` = paste0(round(cumsum(severity_ratio),3)*100,"%"),`Total Number of Cars` = total_derail) %>% 
  select(`Group Name`,Group, Frequency, `Total Number of Cars`, `Cumulative Percentage`)


traffic = read_csv("https://raw.githubusercontent.com/Xinhao-Liu/FRA_Visualizer/main/All%20Traffic%20Data_1996-2022_All.csv")

row_names <- row.names(t(traffic %>% 
                           select(-`1996`) %>% 
                           filter(...1=="All_Freight") %>% 
                           select(`2010`:`2019`)))

test = data.frame(t(traffic %>% 
                      select(-`1996`) %>% 
                      filter(...1=="All_Freight") %>% 
                      select(`2010`:`2019`))) %>% 
  mutate(Year = as.numeric(row_names)) %>% 
  rename(traffic = t.traffic.....select...1996.......filter....1.....All_Freight.......)


raw %>% 
  filter(!is.na(TYPE_clean), !is.na(TotalDerail), TotalDerail >= 0) %>% 
  mutate(Accident_type = ifelse(TYPE_clean == "01", "Derailments",
                                ifelse(TYPE_clean %in% c("02","03","04","05","06","08"), "Collisions",
                                       ifelse(TYPE_clean == "07", "Grade Crossing", "Other")))) %>% 
  mutate(Date = as.Date(as.character(Date),format = "%Y%m%d")) %>% 
  filter(`class 1` %in% c("non-1"),
         TrainType %in% c("O"),
         Accident_type %in% c("Other","Derailments"),
         ACCTRK %in% c(1),
         Category %in% c("H"),
         Date <= "2020-01-01",
         Date >= "2010-01-01") %>% 
  group_by(Category,Year) %>% 
  
  summarise(total = n()) %>% 
  left_join(test,by="Year") %>% 
  mutate(rate = total/traffic)
  






