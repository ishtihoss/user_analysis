# create overall rank before sorting into user category

# load libraries 

library(lubridate)
library(tidyverse)
library(readr)
library(scales)
library(tidyr)

# load user data

pre_spike_user_mv <- read_csv("D:/BigPay data and scripts/24th Feb tier based quality assessment/pre_spike_user_mv.csv")
post_spike_user_mv <- read_csv("D:/BigPay data and scripts/24th Feb tier based quality assessment/post_spike_user_mv.csv")

# load comprehensive merchant data

merchant_comprehensive <- read_csv("D:/BigPay data and scripts/24th Feb tier based quality assessment/merchant_comprehensive.csv")

# add month

merchant_comprehensive_w_month_column <- merchant_comprehensive %>% mutate(month=month(date_txn), year=year(date_txn), date=date(date_txn))

# create overall user rank before separating into user categories

df1 <- merchant_comprehensive %>% group_by(user_id) %>% summarise(total_spend=sum(billing_amount)) %>%
  mutate(rank=ntile(desc(total_spend),3)) %>% select(user_id, rank)


# merge rank column

merch_comp <- merge(merchant_comprehensive,df1, on="user_id")


# create if-else logic based column for user_category

df2 <- merch_comp %>% 
  mutate(user_category= ifelse(user_id %in% pre_spike_user_mv$user_id, "pre_spike","post_spike"))

# run viz by ranked groups

df2 %>% group_by(user_category,mcc_category,rank) %>% filter(rank==3) %>% 
  summarise(total_spend=sum(billing_amount)) %>%  arrange(desc(total_spend)) %>% head(n=50) %>% 
  ggplot(aes(reorder(mcc_category,total_spend),total_spend)) + 
  geom_bar(stat="identity") + coord_flip() + facet_grid(rank~user_category) + 
  labs(x="",y="") + scale_y_continuous(labels = comma)

# fair comparison by tier

df2 %>% group_by(user_category,mcc_category,rank) %>% filter(rank==2) %>% 
  summarise(total_spend=sum(billing_amount)) %>% 
  mutate(norm_spend=ifelse(user_category=="pre_spike",total_spend/8,total_spend/6)) %>%  
  arrange(desc(norm_spend)) %>% head(n=50) %>% 
  ggplot(aes(reorder(mcc_category,norm_spend),norm_spend)) + 
  geom_bar(stat="identity") + coord_flip() + facet_grid(rank~user_category) + 
  labs(x="",y="") + scale_y_continuous(labels = comma)

# fair comparison overall

df2 %>% group_by(user_category,mcc_group,rank) %>%  filter(rank==3) %>%
  summarise(total_spend=sum(billing_amount)) %>% 
  mutate(norm_spend=ifelse(user_category=="pre_spike",total_spend/8,total_spend/6)) %>%  
  arrange(desc(norm_spend)) %>% head(n=50) %>% 
  ggplot(aes(reorder(mcc_group,norm_spend),norm_spend)) + 
  geom_bar(stat="identity") + coord_flip() + facet_grid(rank~user_category) + 
  labs(x="",y="") + scale_y_continuous(labels = comma)

# add rank to all users for correlation analysis

all_users <- rbind(pre_spike_user_mv,post_spike_user_mv)

ranked_users <- merge(all_users,df1,on="user_id", all.x=TRUE)

cor_df <- merchant_comprehensive %>% group_by(user_id) %>% 
  summarise(total_spend=sum(billing_amount)) %>% select(user_id, total_spend)

ranked_users_w_total_spend <- merge(ranked_users,cor_df,on="user_id")

# User composition by state

ranked_users %>% 
  mutate(user_category= ifelse(user_id %in% pre_spike_user_mv$user_id, "pre_spike","post_spike")) %>%
  group_by(occupation,user_category) %>% summarise(count=length(unique(user_id))) %>%
  arrange(desc(count)) %>%
  ggplot(aes(reorder(occupation,count),count)) + geom_bar(stat="identity") + coord_flip() + 
  facet_wrap(~user_category) + labs(x="",y="") + scale_y_continuous(labels = comma)

# create new unified df with left join that includes users with no spend

all_users_incl_no_spend <- merge(all_users,merchant_comprehensive,on="user_id",all.x=TRUE)
all_users_incl_no_spend$billing_amount[is.na(all_users_incl_no_spend$billing_amount)] <- 0
filled_na_w_totalspend <- all_users_incl_no_spend %>% 
  group_by(user_id,lag_to_card_activation,lag_to_first_topup) %>% summarise(total_spend=sum(billing_amount))

# mean monthly spend by occupation

occupation_tag <- all_users %>% select(user_id,occupation)

df3 <- merge(df2,occupation_tag, on="user_id", all.x = TRUE)

df3 %>% group_by(occupation,user_category) %>% 
    summarise(count=length(unique(user_id)),total_spend=sum(billing_amount)) %>% 
    mutate(monthly_average_spend=ifelse(user_category=="pre_spike",total_spend/8,total_spend/6),
    mas_per_user=monthly_average_spend/count) %>% 
    arrange(desc(mas_per_user)) %>% 
    ggplot(aes(reorder(occupation,mas_per_user),mas_per_user,fill=total_spend)) + 
    geom_bar(stat="identity") + coord_flip(y=c(0,2000)) + facet_grid(~user_category) + 
    labs(x="",y="") + scale_y_continuous(labels = comma) +
    scale_fill_distiller("Total spend by each occupation",palette = "Spectral",labels=comma) +
  geom_text(aes(label=round(mas_per_user,2)),hjust=-.2,size=3) + 
  labs(title="Monthly Average Spend per User by Occupation",caption="Between Jan 2021 and Feb 2022")


# monthly change

merchant_comprehensive_w_month_column <- merchant_comprehensive %>% 
  mutate(month=month(date_txn), year=year(date_txn), day=day(date_txn))

dfm_2022 <- merchant_comprehensive_w_month_column %>% filter(month %in% c(1,2) & year==2022 & day<=28)

dfm_2022 <- dfm_2022 %>% group_by(mapped_merchant_name, month) %>% summarise(total_spend=sum(billing_amount))

tib <- dfm_2022 %>% pivot_wider(names_from = month, values_from = total_spend) %>% 
  mutate(change_percentage = ((`2`-`1`)/`1`) * 100,total=`1`+`2`)  %>% arrange(desc(total)) %>% head(50) 

tib %>%
  ggplot(aes(reorder(mapped_merchant_name,total),change_percentage,fill=total)) + 
  geom_bar(stat="identity") + labs(x="",y="") + scale_y_continuous(labels = comma) + 
  scale_fill_distiller("% Change of top 50 merchants",palette = "Spectral",labels=comma) + 
  theme(axis.text.x = element_text(angle = 90)) + geom_label(aes(label=mapped_merchant_name),hjust=-.2,size=3) + 
  coord_flip(y=c(-100,100)) + geom_text(aes(label=round(tib$change_percentage,2)),vjust=1.8,size=3)


# Spend Breakdown by Individual Occupation

df3 <- merge(df2,occupation_tag, on="user_id", all.x = TRUE)

df3 %>% .$Occupation %>% unique()

df3 %>% filter(occupation=='SELF-EMPLOYED') %>% group_by(user_category,mcc_category) %>%  
  summarise(count=length(unique(user_id)),total_spend=sum(billing_amount)) %>% 
  mutate(monthly_average_spend=ifelse(user_category=="pre_spike",total_spend/8,total_spend/6),
         mas_per_user=monthly_average_spend/count) %>%  
  arrange(desc(total_spend)) %>% head(50) %>%
  ggplot(aes(reorder(mcc_category,total_spend),total_spend,fill=total_spend)) + 
  geom_bar(stat="identity") + coord_flip(y=c(0,40000000)) + facet_grid(~user_category) + 
  labs(x="",y="") + scale_y_continuous(labels = comma) +
  scale_fill_distiller("Total spend",palette = "Spectral",labels=comma) +
  geom_text(aes(label=round(total_spend,2)),hjust=-.2,size=3) + 
  labs(title="Total Spend by Self-Employed by MCC Categories",caption="Between Jan 2021 and Feb 2022")