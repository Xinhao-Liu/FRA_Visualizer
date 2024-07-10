library(readr)
raw = read_csv("https://raw.githubusercontent.com/Xinhao-Liu/FRA_Visualizer/main/All_year_FRA.csv")

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


traffic = read_csv("https://raw.githubusercontent.com/Xinhao-Liu/FRA_Visualizer/main/All%20Traffic%20Data_1996-2023_class1_ONE.csv")

row_names <- row.names(t(traffic %>% 
                           select(-`1996`) %>% 
                           filter(...1=="All_Freight") %>% 
                           select(`2010`:`2019`)))

test = data.frame(t(traffic %>% 
                      select(-`1996`) %>% 
                      select(`2010`:`2019`))) %>% 
  mutate(Year = as.numeric(row_names)) %>% 
  rename(traffic = t.traffic.....select...1996.......filter....1.....All_Freight.......)

raw_traffic = read_csv("https://raw.githubusercontent.com/Xinhao-Liu/FRA_Visualizer/main/All%20Traffic%20Data_1996-2023_class1_ONE.csv")

aaa = raw %>% 
  # filter(!is.na(TYPE_clean), !is.na(TotalDerail), TotalDerail >= 0) %>% 
  mutate(Accident_type = ifelse(TYPE_clean == "01", "Derailments",
                                ifelse(TYPE_clean %in% c("02","03","04","05","06","08"), "Collisions",
                                       ifelse(TYPE_clean == "07", "Grade Crossing", "Other")))) %>% 
  mutate(Date = as.Date(as.character(Date),format = "%Y%m%d")) %>% 
  filter(#TrainType %in% c("F"),
         Accident_type %in% c("Derailments","Collisions"),
         `class 1`=="class1",
         `Railroad Successor` %in% c("BNSF","KCS","UP","CSX","NS","CNGT","CP(US)"),
         ACCTRK %in% c(2,4),
         Date <= "2022-12-31",
         Date >= "2013-01-01") %>%
  mutate(`Railroad Successor` = ifelse(`Railroad Successor` == "CNGT", "CN",
                                ifelse(`Railroad Successor` == "CP(US)", "CP", `Railroad Successor`))) %>% 
  mutate(traffic_name = paste(ifelse(`class 1`=="class1", `Railroad Successor`, "Non"),
                              "ClassI",
                              "Freight",
                              ifelse(ACCTRK%in%c(1,3),"Both_Mainline","Non_Mainline"),sep = "_")) %>% 
  select(`Railroad Successor`,`class 1`,ACCTRK,Accident_type,Category,traffic_name,Year) %>% 
  left_join(raw_traffic,by=c("traffic_name"="...1")) %>% 
  mutate(index = Year - 1996 + 8, traffic_value = 0)

num_vec = seq(1:nrow(aaa))
index_vec = as.vector(aaa %>% select(index))[[1]]

final = aaa

traffic_val = mapply(function(i, j) {
  final[i, ncol(aaa)] = aaa[i, j][[1]]
}, i = num_vec, j = index_vec)


final = 
  
  final %>% 
  mutate(traffic_value = traffic_val) %>% 
  mutate(ACCTRK = as.factor(ACCTRK)) %>% 
  select(`Railroad Successor`,`class 1`,ACCTRK,Accident_type,Category,traffic_value,Year) %>% 
  group_by(Category,Year) %>% 
  mutate(count=n(),final_traffic = sum(unique(traffic_value))) %>% 
  ungroup() %>% 
  group_by(Year) %>%
  mutate(final_traffic = max(final_traffic)) %>% 
  mutate(rate=count/final_traffic) %>% 
  unique() %>% 
  select(Accident_type,Year,rate) %>% 
  unique()
  

name = colnames(final)[1]

ggplot(final,aes(x=as.character(Year),y=rate,group=name,color=name))+
  geom_line(size = 2)+
  xlab("Year")+
  ylab("Rate (per million miles)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "top",
        axis.text.x = element_text(color = "#000000", size = 16,
                                   margin = margin(t = 0, r = 0, b = 5, l = 0)),
        axis.text.y = element_text(color = "#000000", size = 16,
                                   margin = margin(t = 0, r = 0, b = 0, l = 10)),
        axis.title.y.right = element_text(color = "#000000", size = 20, face = "bold",
                                          margin = margin(t = 0, r = 0, b = 0, l = 10)),
        axis.title.x = element_text(color = "#000000", size = 20, face="bold"),
        axis.title.y = element_text(color = "#000000", size = 20, face="bold",
                                    margin = margin(t = 0, r = 0, b = 0, l = 10)),
        legend.title = element_text(color = "#000000", size = 16),
        legend.text = element_text(color = "#000000", size = 16))

#### 7.10.2024
raw = read_csv("https://raw.githubusercontent.com/Xinhao-Liu/FRA_Visualizer/main/All_year_FRA.csv")

raw %>% 
  filter(Year > 1996) %>% 
  filter(!is.na(TYPE_clean), !is.na(TotalDerail), TotalDerail >= 0) %>% 
  mutate(Accident_type = ifelse(TYPE_clean == "01", "Derailments",
                                ifelse(TYPE_clean %in% c("02","03","04","05","06","08"), "Collisions",
                                       ifelse(TYPE_clean == "07", "Grade Crossing", "Other")))) %>% 
  mutate(Date = as.Date(as.character(Date),format = "%Y%m%d")) %>% 
  mutate(HIGHSPD = as.numeric(HIGHSPD)) %>% 
  mutate(TotalConsist = as.numeric(TotalConsist)) %>% 
  filter(`class 1` %in% c("non-1"),
         TrainType %in% c("O"),
         Accident_type %in% c("Other"),
         ACCTRK %in% c(1),
         Category %in% c("H"),
         Date <= "2020-01-01",
         Date >= "2010-01-01")






