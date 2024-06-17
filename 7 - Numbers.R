# Numbers mentioned in the text -----------------
# Number of stocks by the end of 2020:
data[eom==as.Date("2020-12-31"), .N]
# Dollar volume of Walmart and Xerox
chars[id %in% c(22111, 27983),.(id, eom, dolvol=dolvol/1e6)][eom==as.Date("2020-12-31")]
# Auto correlation of ret_1_0 and be_me
ar1_ss[char %in% c("be_me", "ret_1_0")]
cluster_labels[ar1_ss, on = .(characteristic=char)][cluster=="quality", median(ar1)]
cluster_labels[ar1_ss, on = .(characteristic=char)][cluster=="momentum"]

data_tc[valid==T & !is.na(be_me) & !is.na(ret_12_1) & be_me != 0.5 & ret_12_1 != 0.5, cor(be_me, ret_12_1), by = eom][, summary(V1)]
data_tc[valid==T & !is.na(be_me) & !is.na(ret_12_1) & be_me != 0.5 & ret_12_1 != 0.5, cor(be_me, ret_12_1)]

# Realized utility without second layer of portfolio tuning
validation_m1[k==1 & g==0 & u==1, .(obj = (mean(r)-0.5*var(r)*pf_set$gamma_rel-mean(tc))*12, sr = mean(r-tc)/sd(r)*sqrt(12))]
validation_static[k==1 & g==0 & u==1, .(obj = (mean(r)-0.5*var(r)*pf_set$gamma_rel-mean(tc))*12, sr = mean(r-tc)/sd(r)*sqrt(12))]
validation_static[k==0.2 & g==0 & u==1, .(obj = (mean(r)-0.5*var(r)*pf_set$gamma_rel-mean(tc))*12, sr = mean(r-tc)/sd(r)*sqrt(12))]

# Move investors from 10b to 1b at a relative risk aversion of 10
large <- ef_ss[wealth_end==1e10 & gamma_rel == 10]
# Large investors for a vol of 14% can get 
large[, r_tc]
# Small investors for a vol of 14% can get 
(small_er <- ef_ss[wealth_end==1e9, r_tc[gamma_rel==100]+(large$sd-sd[gamma_rel==100])*(r_tc[gamma_rel==20]-r_tc[gamma_rel==100])/(sd[gamma_rel==20]-sd[gamma_rel==100])])
output$ef + annotate("point", x = large$sd, y = small_er)

# Median dollar volume by end of sample (for simulations)
chars[valid==T, median(dolvol/1e6), by = eom][eom==max(eom)]


# Shorting fees ------------------------
short_fees[date==max(date)][,.(n=.N,fee=mean(indicative_fee)),by=dcbs][order(dcbs)][, pct := n/sum(n)][]
short_sub <- short_fees[date==as.Date("2020-12-31")][chars[valid==T & eom==max(eom), .(id, eom, size_grp)], on = .(permno=id, date=eom)]
short_sub[, mean(!is.na(indicative_fee))]
short_sub$indicative_fee |> quantile(na.rm=T, probs = seq(0, 1, 0.01))
short_sub[,.(n=.N,fee=mean(indicative_fee)),by=dcbs][order(dcbs)][, pct := n/sum(n)][]
