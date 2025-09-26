# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
setwd("~/simulation_result")   # Save the results to the simulation_result file (or specify your own directory)
set.seed(123)  # For reproducibility

# Parameters
p <- 100  # total number of coefficients
s <- 10
h <- 5

# Generate random Rademacher vector zeta
zeta <- sample(c(-1, 1), size = p, replace = TRUE)

# Random subsets
S <- sort(sample((2*s+1):p, size = s, replace = FALSE))
H <- sort(sample((2*s+1):p, size = s, replace = FALSE))

# Initialize delta vectors
delta_i <- rep(0, p)
delta_ii <- rep(0, p)
delta_iii <- rep(0, p)

# (i) Transferable
delta_i[1:(2*s)] <- h * zeta[1:(2*s)] / (3*s)
delta_i[S] <- h * zeta[S] / (3*s)

# (ii) Partial-transferable
H <- sort(sample((2*s+1):p, size = s, replace = FALSE))
bar_delta_ii <- rep(0, p)
bar_delta_ii[1:(2*s)] <- h * zeta[1:(2*s)] / (3*s)
bar_delta_ii[S] <- h * zeta[S] / (3*s)
delta_ii <- bar_delta_ii
delta_ii[(s+1):(2*s)] <- delta_ii[(s+1):(2*s)] + 1
delta_ii[H] <- delta_ii[H] + 1

# (iii) Non-transferable
H <- sort(sample((2*s+1):p, size = s, replace = FALSE))
bar_delta_iii <- rep(0, p)
bar_delta_iii[1:(2*s)] <- h * zeta[1:(2*s)] / (3*s)
bar_delta_iii[S] <- h * zeta[S] / (3*s)
beta <- sample(c(-1, 1), size = s, replace = TRUE)  # example beta vector
delta_iii <- bar_delta_iii
delta_iii[1:s] <- delta_iii[1:s] + beta
delta_iii[(s+1):(2*s)] <- delta_iii[(s+1):(2*s)] + 1
delta_iii[H] <- delta_iii[H] + 1

library(ggplot2)
library(tidyr)
library(dplyr)

# 假设 delta_i, delta_ii, delta_iii 已经在环境中
# 构造数据框
df <- data.frame(
  j = 1:length(delta_i),
  Transferable = delta_i,
  Partial = delta_ii,
  NonTransferable = delta_iii
)

# 将数据转成长格式便于 ggplot
df_long <- df %>%
  pivot_longer(cols = -j, names_to = "Type", values_to = "Delta") %>%
  mutate(Type = factor(Type, levels = c("Transferable", "Partial", "NonTransferable"),
                       labels = c("(i) Transferable source", 
                                  "(ii) Partial-transferable source",
                                  "(iii) Non-transferable source")))

# 设置蓝色调
blue_colors <- c("#1f77b4", "#1f78b4", "#2196f3")

# 绘图
x.name <- bquote("Index of " ~ delta^(k))
# 绘图
p <- ggplot(df_long, aes(x=j, y=Delta)) +
  geom_col(fill = "#2196f3") +  # 蓝色柱状
  geom_vline(xintercept = c(10,20), linetype="dashed", color="red", size=0.8) +
  facet_wrap(~Type, ncol=1, scales="fixed") +  # y轴固定
  coord_cartesian(ylim = c(-1,1)) +            # 设置y轴范围
  labs(x=x.name, y="") +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face="bold", size=14),
    panel.spacing = unit(1, "lines")
  )

p

# 保存为 PNG 文件
ggsave(filename = "delta_plot.png", plot = p, width = 8, height = 6, bg = "white")
