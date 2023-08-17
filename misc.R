# correlation matrices
agg = data.frame(read_excel(path="data.xlsx", sheet = "Sheet20"))
agg <- agg[,-1]
agg.cor <- cor(agg)
agg.summ <- summary(agg)

# DEA vs Energy Productivity
dea_ep = data.frame(read_excel(path="data.xlsx", sheet = "dea_ep"))
dea_ep <- dea_ep[,-1]
rownames(dea_ep) <- rownames(df_enviro)
plot(dea_ep$EP, dea_ep$DEA, main="DEA Scores vs Energy Productivity",
     xlab="Energy Productivity", ylab="DEA Scores")
text(dea_ep$EP, dea_ep$DEA-0.003, labels=rownames(dea_ep), cex = 0.5)

# scatter plot of static model scores
static_avg = data.frame(read_excel(path="data.xlsx", sheet = "static_avg"))
static_avg = static_avg[,-1]
rownames(static_avg) <- rownames(df_enviro)
par (mfrow= c(1,1))
plot(static_avg$nrg, static_avg$env,
     xlab="Energy and Economic Efficiency", ylab="Environmental Efficiency")
text(static_avg$nrg, static_avg$env-0.007, labels=rownames(dea_ep), cex = 0.5)
plot(static_avg$nrg, static_avg$env,
     xlab="Energy and Economic Efficiency", ylab="Environmental Efficiency", 
     xlim = c(0.969,1), ylim = c(0.87,1))
text(static_avg$nrg, static_avg$env-0.007, labels=rownames(dea_ep), cex = 0.5)

# scatter plot of dynamic model scores
dynamic_ovr = data.frame(read_excel(path="data.xlsx", sheet = "dynamic_ovr"))
rownames(dynamic_ovr) = rownames(df_2016)
plot(dynamic_ovr$nrg, dynamic_ovr$env, xlab="Energy and Economic Efficiency", 
     ylab="Environmental Efficiency")
text(dynamic_ovr$nrg, dynamic_ovr$env-0.02, labels=rownames(dynamic_ovr), 
     cex = 0.5)

# Mann-Whitney U tests

# Impaccts of climate policies
mwu_s2 = data.frame(read_excel(path="data.xlsx", sheet = "mwu_s2"))
s_a = c(mwu_s2$s_a)
s_b = c(mwu_s2[1:36,2])
wilcox.test(s_a,s_b, alternative = "less")
help("wilcox.test")

# old vs new member states

# static
mwu_s1_ON = data.frame(read_excel(path="data.xlsx", sheet = "mwu_s1_ON"))
s_a = c(mwu_s1_ON$s_a)
s_b = c(mwu_s1_ON[1:65,2])
wilcox.test(s_a,s_b, alternative = "greater")
help("wilcox.test")

mwu_s2_ON = data.frame(read_excel(path="data.xlsx", sheet = "mwu_s2_ON"))
s_a = c(mwu_s2_ON$s_a)
s_b = c(mwu_s2_ON[1:65,2])
wilcox.test(s_a,s_b, alternative = "greater")

# dynamic
mwu_d1_ON = data.frame(read_excel(path="data.xlsx", sheet = "mwu_d1_ON"))
s_a = c(mwu_d1_ON$s_a)
s_b = c(mwu_d1_ON[1:65,2])
wilcox.test(s_a,s_b, alternative = "greater")

mwu_d2_ON = data.frame(read_excel(path="data.xlsx", sheet = "mwu_d2_ON"))
s_a = c(mwu_d2_ON$s_a)
s_b = c(mwu_d2_ON[1:65,2])
wilcox.test(s_a,s_b, alternative = "greater")


