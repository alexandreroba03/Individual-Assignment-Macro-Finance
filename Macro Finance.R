
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("scales")
install.packages("grid")
library(ggplot2)
library(gridExtra)
library(scales)
library(grid)
library(knitr)
library(kableExtra)
library(dplyr)
library(zoo)
library(lubridate)
library(purrr)
library(flextable)
# ──────────────────────────────────────────────────────────────
# (c.i) - Conditional Mean Forecast (AR(1) — Mean Reversion)
# ──────────────────────────────────────────────────────────────
rho <- 0.95
x1 <- 1
T <- 20

t <- 1:T
E_xt <- rho^(t - 1) * x1

par(mar = c(5, 5, 4, 4))
matplot(t, E_xt, type = "l", pch = 16, col = "darkblue", lwd = 2, lty = 1,
        xlab = "Time t", ylab = expression(E[1](x[t])),
        main = "Conditional Mean Forecast under AR(1) — Mean Reversion Effect",
        xlim = c(1, 20), ylim = c(0, 1.05),
        cex.main = 2,  
        cex.lab = 1.6,   
        cex.axis = 1.4)  
abline(h = 0, col = "red", lty = 2, lwd = 2.5)
legend("topright",
       legend = c(expression(E[1](x[t])), "Long-term mean"),
       col = c("darkblue", "red"),
       lty = c(1, 2), lwd = c(2, 2.5), pch = c(16, NA),
       bty = "n", cex = 1.2)
grid()

# ──────────────────────────────────────────────────────────────
# (c.ii) - Forecast + 1 Std Dev Band (AR(1))
# ──────────────────────────────────────────────────────────────
rho <- 0.95
sigma_eps <- 0.25
x1 <- 1
T <- 20

t <- 1:T
E_xt <- rho^(t - 1) * x1
Var_xt <- sigma_eps^2 * (1 - rho^(2 * (t - 1))) / (1 - rho^2)
SD_xt <- sqrt(Var_xt)

upper_band <- E_xt + SD_xt
lower_band <- E_xt - SD_xt

par(mar = c(5, 5, 4, 4))  
matplot(t, E_xt, type = "l", pch = 16, col = "darkblue", lwd = 2, lty = 1,
        xlab = "Time t",
        ylab = expression(E[1](x[t])),
        main = "Forecast with ±1 Conditional Standard Deviation Band (AR(1))",
        xlim = c(1, 20), ylim = range(c(lower_band, upper_band)),
        cex.main = 2, cex.lab = 1.6, cex.axis = 1.4)
lines(t, upper_band, col = "blue", lty = 2, lwd = 2)
lines(t, lower_band, col = "blue", lty = 2, lwd = 2)
abline(h = 0, col = "red", lty = 3, lwd = 2.5)
legend("topright",
       legend = c(expression(E[1](x[t])), expression(E[1](x[t]) %+-% sigma[x]), "Long-term mean"),
       col = c("darkblue", "blue", "red"),
       lty = c(1, 2, 3), lwd = c(2, 2, 2.5), pch = c(16, NA, NA),
       bty = "n", cex = 1.1)
grid()

# ──────────────────────────────────────────────────────────────
# (c.iii) - Chebyshev vs Normal Cones
# ──────────────────────────────────────────────────────────────
rho <- 0.95
sigma_eps <- 0.25
x1 <- 1
T <- 20
t <- 1:T

E_xt <- rho^(t - 1) * x1
Var_xt <- sigma_eps^2 * (1 - rho^(2 * (t - 1))) / (1 - rho^2)
SD_xt <- sqrt(Var_xt)

# Chebychev’s coefficients
k_90_cheb <- sqrt(10)   
k_95_cheb <- sqrt(20)   

upper_cheb_90 <- E_xt + k_90_cheb * SD_xt
lower_cheb_90 <- E_xt - k_90_cheb * SD_xt

upper_cheb_95 <- E_xt + k_95_cheb * SD_xt
lower_cheb_95 <- E_xt - k_95_cheb * SD_xt

# Coefficients of normal distribution
z_90 <- 1.645
z_95 <- 1.96

upper_norm_90 <- E_xt + z_90 * SD_xt
lower_norm_90 <- E_xt - z_90 * SD_xt

upper_norm_95 <- E_xt + z_95 * SD_xt
lower_norm_95 <- E_xt - z_95 * SD_xt

par(mar = c(5, 5, 4, 4))
plot(t, E_xt, type = "l", col = "black", lwd = 2,
     ylim = range(c(lower_cheb_95, upper_cheb_95)),
     xlab = "Time t", ylab = expression(E[1](x[t])),
     main = "90% and 95% Prediction Cones: Chebychev vs Normal",
     cex.main = 2, cex.lab = 1.6, cex.axis = 1.4)
lines(t, E_xt, col = "black", lwd = 2)
lines(t, upper_cheb_90, col = "#1f78b4", lty = 2, lwd = 2)
lines(t, lower_cheb_90, col = "#1f78b4", lty = 2, lwd = 2)
lines(t, upper_cheb_95, col = "blue", lty = 3, lwd = 2)
lines(t, lower_cheb_95, col = "blue", lty = 3, lwd = 2)
lines(t, upper_norm_90, col = "#ff7f00", lty = 2, lwd = 2)
lines(t, lower_norm_90, col = "#ff7f00", lty = 2, lwd = 2)
lines(t, upper_norm_95, col = "darkred", lty = 3, lwd = 2)
lines(t, lower_norm_95, col = "darkred", lty = 3, lwd = 2)
abline(h = 0, col = "red", lty = 3, lwd = 2.5)
legend("topright",
       legend = c(expression(E[1](x[t])),
                  "Chebychev 90%", "Chebychev 95%",
                  "Normal 90%", "Normal 95%"),
       col = c("black", "#1f78b4", "blue", "#ff7f00", "darkred"),
       lty = c(1, 2, 3, 2, 3), lwd = 2,
       bty = "o", cex = 1.1)
grid()

# ──────────────────────────────────────────────────────────────
# (c.iv) - Simulations AR(1) + cones Chebychev & Normal
# ──────────────────────────────────────────────────────────────
set.seed(42)
rho <- 0.95
sigma_eps <- 0.25
x1 <- 1
T <- 20
t <- 1:T
n_sim <- 100

E_xt <- rho^(t - 1) * x1
Var_xt <- sigma_eps^2 * (1 - rho^(2 * (t - 1))) / (1 - rho^2)
SD_xt <- sqrt(Var_xt)

# Cônes Chebychev
k_90_cheb <- sqrt(10)
k_95_cheb <- sqrt(20)
upper_cheb_90 <- E_xt + k_90_cheb * SD_xt
lower_cheb_90 <- E_xt - k_90_cheb * SD_xt
upper_cheb_95 <- E_xt + k_95_cheb * SD_xt
lower_cheb_95 <- E_xt - k_95_cheb * SD_xt

# Cônes Normaux
z_90 <- 1.645
z_95 <- 1.96
upper_norm_90 <- E_xt + z_90 * SD_xt
lower_norm_90 <- E_xt - z_90 * SD_xt
upper_norm_95 <- E_xt + z_95 * SD_xt
lower_norm_95 <- E_xt - z_95 * SD_xt

# Simulations AR(1)
sim_matrix <- matrix(NA, nrow = T, ncol = n_sim)
for (s in 1:n_sim) {
  x <- numeric(T)
  x[1] <- x1
  eps <- rnorm(T - 1, mean = 0, sd = sigma_eps)
  for (i in 2:T) {
    x[i] <- rho * x[i - 1] + eps[i - 1]
  }
  sim_matrix[, s] <- x
}

par(mar = c(5, 5, 4, 4))
plot(t, E_xt, type = "n",
     ylim = range(c(lower_cheb_95, upper_cheb_95, sim_matrix)),
     xlab = "Time t", ylab = expression(E[1](x[t])),
     main = "Simulated Paths with 90%-95% Cones (Chebychev vs Normal)",
     cex.main = 2, cex.lab = 1.6, cex.axis = 1.4)
for (s in 1:n_sim) {
  lines(t, sim_matrix[, s], col = "grey", lwd = 1)
}
lines(t, E_xt, col = "black", lwd = 2)
lines(t, upper_cheb_90, col = "#1f78b4", lty = 2, lwd = 2)
lines(t, lower_cheb_90, col = "#1f78b4", lty = 2, lwd = 2)
lines(t, upper_cheb_95, col = "blue", lty = 3, lwd = 2)
lines(t, lower_cheb_95, col = "blue", lty = 3, lwd = 2)
lines(t, upper_norm_90, col = "#ff7f00", lty = 2, lwd = 2)
lines(t, lower_norm_90, col = "#ff7f00", lty = 2, lwd = 2)
lines(t, upper_norm_95, col = "darkred", lty = 3, lwd = 2)
lines(t, lower_norm_95, col = "darkred", lty = 3, lwd = 2)
abline(h = 0, col = "red", lty = 3, lwd = 3)
legend("topright",
       legend = c(expression(E[1](x[t])),
                  "Chebyshev 90%", "Chebyshev 95%",
                  "Normal 90%", "Normal 95%",
                  "Long-term mean"),
       col = c("black", "#1f78b4", "blue", "#ff7f00", "darkred", "darkgray"),
       lty = c(1, 2, 3, 2, 3, 3), lwd = 2, bty = "o", cex = 1.1)

grid()

# ──────────────────────────────────────────────────────────────
# (c.iv) - Simulations AR(1) + cones Chebychev & Normal + normal
# ──────────────────────────────────────────────────────────────
set.seed(42)
rho <- 0.95; sigma_eps <- 0.25; x1 <- 1
T <- 20; t <- 1:T; n_sim <- 100

E_xt <- rho^(t - 1) * x1
Var_xt <- sigma_eps^2 * (1 - rho^(2 * (t - 1))) / (1 - rho^2)
SD_xt <- sqrt(Var_xt)
k_90_cheb <- sqrt(10); k_95_cheb <- sqrt(20); z_90 <- 1.645; z_95 <- 1.96

upper_cheb_90 <- E_xt + k_90_cheb * SD_xt
lower_cheb_90 <- E_xt - k_90_cheb * SD_xt
upper_cheb_95 <- E_xt + k_95_cheb * SD_xt
lower_cheb_95 <- E_xt - k_95_cheb * SD_xt
upper_norm_90 <- E_xt + z_90 * SD_xt
lower_norm_90 <- E_xt - z_90 * SD_xt
upper_norm_95 <- E_xt + z_95 * SD_xt
lower_norm_95 <- E_xt - z_95 * SD_xt

sim_matrix <- matrix(NA, nrow = T, ncol = n_sim)
for (s in 1:n_sim) {
  x <- numeric(T); x[1] <- x1
  eps <- rnorm(T - 1, 0, sigma_eps)
  for (i in 2:T) x[i] <- rho * x[i - 1] + eps[i - 1]
  sim_matrix[, s] <- x
}
df_sim <- data.frame(time = rep(t, n_sim), value = as.vector(sim_matrix), sim = rep(1:n_sim, each = T))

x20 <- sim_matrix[T, ]
dens_emp <- density(x20)
mean_20 <- E_xt[T]
df_dens_colored <- data.frame(x = dens_emp$x, density = dens_emp$y)

p1 <- ggplot() +
  geom_line(data = df_sim, aes(x = time, y = value, group = sim), color = "grey70", alpha = 0.6, linewidth = 0.5) +
  geom_line(data = data.frame(t, E_xt), aes(x = t, y = E_xt, color = "Long-term mean"), linewidth = 1.3) +
  geom_line(data = data.frame(t, upper_cheb_90), aes(x = t, y = upper_cheb_90, color = "Chebyshev 90%"), linetype = "dashed", linewidth = 1) +
  geom_line(data = data.frame(t, lower_cheb_90), aes(x = t, y = lower_cheb_90, color = "Chebyshev 90%"), linetype = "dashed", linewidth = 1) +
  geom_line(data = data.frame(t, upper_cheb_95), aes(x = t, y = upper_cheb_95, color = "Chebyshev 95%"), linetype = "dotted", linewidth = 1) +
  geom_line(data = data.frame(t, lower_cheb_95), aes(x = t, y = lower_cheb_95, color = "Chebyshev 95%"), linetype = "dotted", linewidth = 1) +
  geom_line(data = data.frame(t, upper_norm_90), aes(x = t, y = upper_norm_90, color = "Normal 90%"), linetype = "dashed", linewidth = 1) +
  geom_line(data = data.frame(t, lower_norm_90), aes(x = t, y = lower_norm_90, color = "Normal 90%"), linetype = "dashed", linewidth = 1) +
  geom_line(data = data.frame(t, upper_norm_95), aes(x = t, y = upper_norm_95, color = "Normal 95%"), linetype = "dotted", linewidth = 1) +
  geom_line(data = data.frame(t, lower_norm_95), aes(x = t, y = lower_norm_95, color = "Normal 95%"), linetype = "dotted", linewidth = 1) +
  geom_hline(yintercept = 0, color = "red", lty = 3, linewidth = 1) +
  scale_color_manual(values = c(
    "Long-term mean" = "black",
    "Chebyshev 90%" = "#1f78b4",
    "Chebyshev 95%" = "blue",
    "Normal 90%" = "#ff7f00",
    "Normal 95%" = "darkred"
  )) +
  labs(x = "Time t", y = expression(x[t]), color = NULL) +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = c(0.05, 0.05),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = alpha("white", 0.6), color = "black"),
    panel.border = element_rect(color = "black", fill = NA)
  )

p2 <- ggplot(df_dens_colored) +
  geom_path(aes(x = density, y = x, color = density), linewidth = 2) +
  geom_hline(yintercept = mean_20, color = "black", linewidth = 1.4) +
  geom_segment(data = data.frame(x20 = x20),
               aes(x = 0, xend = 0.015, y = x20, yend = x20),
               color = "grey50", alpha = 0.5, linewidth = 0.4) +
  geom_hline(yintercept = c(upper_cheb_90[T], lower_cheb_90[T]), color = "#1f78b4", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = c(upper_cheb_95[T], lower_cheb_95[T]), color = "blue", linetype = "dotted", linewidth = 1) +
  geom_hline(yintercept = c(upper_norm_90[T], lower_norm_90[T]), color = "#ff7f00", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = c(upper_norm_95[T], lower_norm_95[T]), color = "darkred", linetype = "dotted", linewidth = 1) +
  scale_color_gradient(low = "grey40", high = "gold") +
  labs(x = "Density", y = expression(x[20]), color = "Frequency") +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = c(0.6, 0.025),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
    panel.border = element_rect(color = "black", fill = NA)
  )

grid.arrange(
  arrangeGrob(p1, p2, ncol = 2, widths = c(3.5, 1.5)),
  top = textGrob("Simulated AR(1) Paths and Density of x", gp = gpar(fontsize = 16, fontface = "bold"))
)
prop_out_cheb90 <- mean(x20 < lower_cheb_90[T] | x20 > upper_cheb_90[T])
prop_out_cheb95 <- mean(x20 < lower_cheb_95[T] | x20 > upper_cheb_95[T])
prop_out_norm90 <- mean(x20 < lower_norm_90[T] | x20 > upper_norm_90[T])
prop_out_norm95 <- mean(x20 < lower_norm_95[T] | x20 > upper_norm_95[T])
prop_out_cheb90
prop_out_cheb95
prop_out_norm90
prop_out_norm95

table_props <- data.frame(
  Interval = c("Chebyshev 90%", "Chebyshev 95%", "Normal 90%", "Normal 95%"),
  `Proportion outside interval` = c(
    round(prop_out_cheb90, 3),
    round(prop_out_cheb95, 3),
    round(prop_out_norm90, 3),
    round(prop_out_norm95, 3)
  )
)

# Renommer les colonnes avec espaces
colnames(table_props) <- c("Interval", "Proportion outside interval")

# Affichage avec flextable
flextable(table_props) %>%
  set_caption("Proportion of simulations outside the confidence intervals") %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  theme_booktabs()


# ──────────────────────────────────────────────────────────────
# (h.i) - Impulse Response Function of r[t] — MA(1)
# ──────────────────────────────────────────────────────────────
T <- 10
eps <- rep(0, T + 1)
eps[2] <- 1  
mu <- 0

# Valeurs de delta
deltas <- c(0.5, 0, -0.5)
colors <- c("#1f78b4", "darkgreen", "darkred")
names(colors) <- paste0("delta = ", deltas)

# Plot
plot(NULL, xlim = c(0, T), ylim = c(-1, 1.2), 
     xlab = "Time t", ylab = expression(r[t]), 
     main = "Impulse Response Function of r[t] — MA(1)",cex.main = 2, cex.lab = 1.6, cex.axis = 1.4)


# Tracer les lignes pour chaque delta
for (i in seq_along(deltas)) {
  delta <- deltas[i]
  rt <- rep(0, T + 1)
  for (t in 2:(T + 1)) {
    rt[t] <- mu + eps[t] + delta * eps[t - 1]
  }
  lines(0:T, rt, col = colors[i], lwd = 2)
}

legend("topright", legend = names(colors), col = colors, lwd = 2, bty = "n")
grid()

# ──────────────────────────────────────────────────────────────
# (h.ii) - Impulse Response of Price p[t] under MA(1)
# ──────────────────────────────────────────────────────────────
T <- 10
epsilon <- rep(0, T + 1)
epsilon[2] <- 1  

deltas <- c(0.5, 0, -0.5)
colors <- c("orange", "forestgreen", "firebrick")

pt_all <- matrix(0, nrow = length(deltas), ncol = T + 1)

for (j in 1:length(deltas)) {
  delta <- deltas[j]
  rt <- rep(0, T + 1)
  pt <- rep(0, T + 1)
  
  for (t in 2:(T + 1)) {
    rt[t] <- epsilon[t] + delta * epsilon[t - 1]
    pt[t] <- pt[t - 1] + rt[t]
  }
  
  pt_all[j, ] <- pt
}

matplot(0:T, t(pt_all), type = "l", lwd = 2, col = colors, lty = 1,
        xlab = "Time t", ylab = expression(p[t]), 
        main = "Impulse Response Function of p[t] — MA(1)",cex.main = 2, cex.lab = 1.6, cex.axis = 1.4)

legend("bottomright", legend = c("delta = 0.5", "delta = 0", "delta = -0.5"),
       col = colors, bty = "n", lwd = 2)
abline(h = 0, col = "black", lty = 3)
grid()


# ──────────────────────────────────────────────────────────────
# 3.2 - Stocks Market: Stock returns and strategies
# ──────────────────────────────────────────────────────────────
library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(flextable)
library(ggplot2)

ind30_raw <- read_csv("~/Downloads/30_Industry_Portfolios.CSV", skip = 11, n_max = 1182)
ff_raw <- read.csv("~/Downloads/F-F_Research_Data_Factors.CSV", skip = 3)

colnames(ff_raw)[1] <- "Date"

ff_clean <- ff_raw %>%
  filter(!apply(., 1, function(row) all(is.na(row) | row == ""))) %>%
  mutate(Date = as.yearmon(as.character(Date), format = "%Y%m"),
         Date = as.Date(Date)) %>%
  mutate(across(-1, ~na_if(na_if(as.numeric(.x), -99.99), -999)))  # RF inclus

ind30_clean <- ind30_raw %>%
  rename(Date = 1) %>%
  mutate(Date = as.yearmon(as.character(Date), format = "%Y%m"),
         Date = as.Date(Date)) %>%
  mutate(across(-Date, ~na_if(na_if(.x, -99.99), -999)))

ind30_excess <- ind30_clean %>%
  left_join(dplyr::select(ff_clean, Date, RF), by = "Date") %>%
  mutate(across(where(is.numeric) & !any_of(c("RF")), ~ . - RF)) %>%
  dplyr::select(-RF)

rolling_avg <- ind30_excess %>%
  arrange(Date) %>%
  mutate(across(-Date, ~ lag(rollapplyr(.x, width = 12, FUN = mean, fill = NA, na.rm = TRUE))))

long_avg <- rolling_avg %>%
  pivot_longer(-Date, names_to = "Industry", values_to = "RollingAvg") %>%
  filter(Date >= as.Date("1927-07-01"))  

ranked <- long_avg %>%
  group_by(Date) %>%
  mutate(Rank = rank(RollingAvg, ties.method = "average")) %>%
  ungroup()

mean_ranks <- ranked %>%
  group_by(Industry) %>%
  summarise(Average_Rank = mean(Rank, na.rm = TRUE)) %>%
  arrange(Average_Rank)

lowest_industry <- mean_ranks %>% slice(1)
highest_industry <- mean_ranks %>% slice(n())

cat("Lowest average rank industry:", lowest_industry$Industry, 
    "with avg rank =", lowest_industry$Average_Rank, "\n")
cat("Highest average rank industry:", highest_industry$Industry, 
    "with avg rank =", highest_industry$Average_Rank, "\n")

ggplot(filter(ranked, Industry == lowest_industry$Industry), aes(x = Date, y = Rank)) +
  geom_line(color = "red", linewidth = 0.7) +
  labs(
    title = paste("Monthly Rank Over Time —", lowest_industry$Industry),
    x = "Date",
    y = "Rank (1 = lowest past return)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 20, color = "#2d2d2d"
    ),
    axis.title = element_text(face = "bold", size = 15, color = "#2d2d2d"),
    axis.text = element_text(color = "#3a3a3a"),
    panel.grid.major = element_line(color = "#d0d0d0", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

flextable(mean_ranks) %>%
  set_header_labels(Industry = "Industry", Average_Rank = "Average Rank") %>%
  autofit() %>%
  bold(part = "header") %>%
  color(i = 1, color = "darkred") %>%
  color(i = nrow(mean_ranks), color = "darkgreen") %>%
  align(align = "center", part = "all")

top_industries_by_month <- ranked %>%
  filter(Rank == max(Rank, na.rm = TRUE)) %>%
  dplyr::select(Date, Industry) %>%
  arrange(Date)

top_1_each_month <- top_industries_by_month %>%
  count(Industry, name = "TopCount") %>%
  arrange(desc(TopCount))

ggplot(top_1_each_month, aes(x = reorder(Industry, TopCount), y = TopCount)) +
  geom_col(fill = "#2c3e50", color = "white", width = 0.7) +  
  coord_flip() +
  labs(
    title = "Frequency of Top Rank per Industry (1927–2024)",
    x = "Industry",
    y = "Number of Times Ranked #1"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 20,
      color = "black"
    ),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#d0d0d0", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.8),
    plot.background = element_rect(fill = "white", color = NA)
  )

top_list <- top_industries_by_month %>%
  group_by(Date) %>%
  summarise(Industries = list(Industry), .groups = "drop")

top_turnover <- top_list %>%
  mutate(Previous = lag(Industries)) %>%
  mutate(
    Turnover = map2_dbl(Industries, Previous, ~ {
      if (is.null(.x) || is.null(.y)) return(NA_real_)
      length(setdiff(.x, .y)) + length(setdiff(.y, .x))
    }),
    TurnoverRate = Turnover / length(unique(ranked$Industry))
  )

avg_turnover <- mean(top_turnover$Turnover, na.rm = TRUE)
med_turnover <- median(top_turnover$Turnover, na.rm = TRUE)

cat("Average monthly turnover:", avg_turnover, "\n")
cat("Median monthly turnover:", med_turnover, "\n")

if (avg_turnover >= 10) {
  cat("\n Interpretation:\nThe industry momentum strategy is highly dynamic.\nThere is frequent rotation in top-ranked industries, suggesting high turnover and low persistence.\n")
} else {
  cat("\n Interpretation:\nThe top industries are relatively stable over time.\nIndustry momentum seems concentrated on a few persistent leaders, suggesting a low-turnover strategy.\n")
}

# ─────────────────────────────────────────────────────
# WINNER PORTFOLIO ANALYSIS (Top 15 Ranked Industries)
# ─────────────────────────────────────────────────────
ret_long_excess <- ind30_excess %>%
  pivot_longer(-Date, names_to = "Industry", values_to = "ExcessReturn")

ranked_returns <- ranked %>%
  left_join(ret_long_excess, by = c("Date", "Industry")) %>%
  filter(!is.na(RollingAvg) & !is.na(ExcessReturn))

winners_each_month <- ranked_returns %>%
  group_by(Date) %>%
  filter(Rank >= 16) %>%
  summarise(WinnerReturn = mean(ExcessReturn, na.rm = TRUE), .groups = "drop")

winner_mean <- mean(winners_each_month$WinnerReturn, na.rm = TRUE)
winner_sd <- sd(winners_each_month$WinnerReturn, na.rm = TRUE)
winner_sharpe_monthly <- winner_mean / winner_sd
winner_sharpe_annual <- sqrt(12) * winner_sharpe_monthly

winner_composition <- ranked_returns %>%
  group_by(Date) %>%
  filter(Rank >= 16) %>%
  ungroup() %>%
  count(Industry, name = "Appearances") %>%
  mutate(Frequency = Appearances / length(unique(ranked_returns$Date))) %>%
  arrange(desc(Frequency)) %>%
  filter(Frequency > 0)

summary_table <- data.frame(
  Metric = c("Mean Monthly Excess Return", "Monthly Standard Deviation",
             "Monthly Sharpe Ratio", "Annualized Sharpe Ratio"),
  Value = c(winner_mean, winner_sd, winner_sharpe_monthly, winner_sharpe_annual)
)

flextable(summary_table) %>%
  set_header_labels(Metric = "Statistic Winner Portfolio", Value = "Value") %>%
  colformat_num(j = "Value", digits = 4) %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  theme_booktabs()

ggplot(winner_composition, aes(x = Appearances, y = reorder(Industry, Appearances))) +
  geom_col(fill = "#2c3e50", width = 0.7) +
  geom_text(aes(label = scales::percent(Frequency, accuracy = 0.1)),
            hjust = -0.1, color = "black", size = 5) +
  labs(
    title = "Top Industries by Frequency in Winner Portfolio (1927–2024)",
    x = "Number of Appearances in Top 15",
    y = "Industry"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#d0d0d0", size = 0.4),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.8)
  ) +
  coord_cartesian(clip = "off")

# ─────────────────────────────────────────────────────
# LOSER PORTFOLIO ANALYSIS (Bottom 15 Ranked Industries)
# ─────────────────────────────────────────────────────
losers_each_month <- ranked_returns %>%
  group_by(Date) %>%
  filter(Rank <= 15) %>%
  summarise(LoserReturn = mean(ExcessReturn, na.rm = TRUE), .groups = "drop")

loser_mean <- mean(losers_each_month$LoserReturn, na.rm = TRUE)
loser_sd <- sd(losers_each_month$LoserReturn, na.rm = TRUE)
loser_sharpe_monthly <- loser_mean / loser_sd
loser_sharpe_annual <- sqrt(12) * loser_sharpe_monthly

loser_composition <- ranked_returns %>%
  group_by(Date) %>%
  filter(Rank <= 15) %>%
  ungroup() %>%
  count(Industry, name = "Appearances") %>%
  mutate(Frequency = Appearances / length(unique(ranked_returns$Date))) %>%
  arrange(desc(Frequency)) %>%
  filter(Frequency > 0)

market_returns <- ff_clean %>%
  filter(Date %in% ranked_returns$Date) %>%
  dplyr::select(Date, Mkt.RF) %>%
  mutate(Mkt.RF = as.numeric(Mkt.RF))

market_sd <- sd(market_returns$Mkt.RF, na.rm = TRUE)
market_mean <- mean(market_returns$Mkt.RF, na.rm = TRUE)
market_sharpe_monthly <- market_mean / market_sd
market_sharpe_annual <- sqrt(12) * market_sharpe_monthly

summary_loser <- data.frame(
  Metric = c("Mean Monthly Excess Return", "Monthly Standard Deviation",
             "Monthly Sharpe Ratio", "Annualized Sharpe Ratio",
             "Annualized Sharpe Ratio of Market"),
  Value = c(loser_mean, loser_sd, loser_sharpe_monthly, loser_sharpe_annual, market_sharpe_annual)
)

flextable(summary_loser) %>%
  set_header_labels(Metric = "Statistic Loser Portfolio", Value = "Value") %>%
  colformat_num(j = "Value", digits = 4) %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  theme_booktabs()

ggplot(loser_composition, aes(x = Appearances, y = reorder(Industry, Appearances))) +
  geom_col(fill = "#2c3e50", width = 0.7) +
  geom_text(aes(label = scales::percent(Frequency, accuracy = 0.1)),
            hjust = -0.1, color = "black", size = 5) +
  labs(
    title = "Most Frequent Industries in Loser Portfolio (1927–2024)",
    x = "Number of Appearances in Bottom 15",
    y = "Industry"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#d0d0d0", size = 0.4),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.8)
  ) +
  coord_cartesian(clip = "off")

sharpe_comparison <- data.frame(
  Portfolio = c("Winner", "Loser", "Market"),
  Sharpe_Annual = c(winner_sharpe_annual, loser_sharpe_annual, market_sharpe_annual)
)

flextable(sharpe_comparison) %>%
  colformat_num(j = "Sharpe_Annual", digits = 3) %>%
  set_header_labels(Portfolio = "Portfolio", Sharpe_Annual = "Annualized Sharpe Ratio") %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  theme_booktabs()

# ────────────────────────────────────────────────────────────────
# 4. CUMULATIVE RETURNS — Winner, Loser, Long-Short, Market
# ────────────────────────────────────────────────────────────────
ret_long_total <- ret_long_excess %>%
  left_join(ff_clean %>% dplyr::select(Date, RF), by = "Date") %>%
  mutate(TotalReturn = ExcessReturn + RF)

ranked_total_returns <- ranked %>%
  left_join(ret_long_total, by = c("Date", "Industry")) %>%
  filter(!is.na(RollingAvg) & !is.na(TotalReturn))

winner_returns_total <- ranked_total_returns %>%
  group_by(Date) %>%
  filter(Rank >= 16) %>%
  summarise(Winner = mean(TotalReturn, na.rm = TRUE), .groups = "drop")

loser_returns_total <- ranked_total_returns %>%
  group_by(Date) %>%
  filter(Rank <= 15) %>%
  summarise(Loser = mean(TotalReturn, na.rm = TRUE), .groups = "drop")

long_short_returns <- winner_returns_total %>%
  inner_join(loser_returns_total, by = "Date") %>%
  mutate(LongShort = Winner - Loser) %>%
  dplyr::select(Date, LongShort)

market_total <- ff_clean %>%
  filter(Date >= as.Date("1927-07-01")) %>%
  mutate(Market = Mkt.RF + RF) %>%
  dplyr::select(Date, Market)

returns_all <- winner_returns_total %>%
  inner_join(loser_returns_total, by = "Date") %>%
  inner_join(long_short_returns, by = "Date") %>%
  inner_join(market_total, by = "Date")

cumulative_returns <- returns_all %>%
  mutate(
    cum_Winner    = cumprod(1 + Winner / 100),
    cum_Loser     = cumprod(1 + Loser / 100),
    cum_LongShort = cumprod(1 + LongShort / 100),
    cum_Market    = cumprod(1 + Market / 100)
  ) %>%
  dplyr::select(Date, cum_Winner, cum_Loser, cum_LongShort, cum_Market) %>%
  pivot_longer(-Date, names_to = "Portfolio", values_to = "CumulativeReturn") %>%
  mutate(Portfolio = case_when(
    Portfolio == "cum_Winner" ~ "Winner",
    Portfolio == "cum_Loser" ~ "Loser",
    Portfolio == "cum_LongShort" ~ "Long/Short",
    Portfolio == "cum_Market" ~ "Market"
  ))

ggplot(cumulative_returns, aes(x = Date, y = CumulativeReturn, color = Portfolio)) +
  geom_line(linewidth = 1.1) +
  scale_y_log10(labels = scales::label_number()) +
  scale_color_manual(
    values = c("Winner" = "#1f78b4", "Loser" = "#e31a1c",
               "Long/Short" = "#33a02c", "Market" = "#444444")
  ) +
  labs(
    title = "Cumulative Return — Winner vs Loser vs Long/Short vs Market",
    subtitle = "Log scale, total returns including risk-free rate",
    x = "Date", y = "Cumulative Return (log scale)", color = "Portfolio"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20, color = "black"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 15, color = "#555555"),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "#dddddd", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    plot.background = element_rect(fill = "white", color = NA)
  )

cumulative_summary <- cumulative_returns %>%
  group_by(Portfolio) %>%
  summarise(Final_Cumulative_Return = last(CumulativeReturn), .groups = "drop")

flextable(cumulative_summary) %>%
  set_header_labels(
    Portfolio = "Portfolio",
    Final_Cumulative_Return = "Final Cumulative Return"
  ) %>%
  colformat_num(j = "Final_Cumulative_Return", digits = 2) %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  theme_booktabs()

# ────────────────────────────────────────────────────────────────
# 5. INDUSTRY MOMENTUM LOSS — Using CUMULATIVE RETURNS
# ────────────────────────────────────────────────────────────────
library(dplyr)
library(zoo)
library(lubridate)
library(purrr)
library(flextable)

loss_df <- long_short_returns %>%
  left_join(market_total, by = "Date") %>%
  arrange(Date) %>%
  mutate(
    LS_3m_cum = rollapply(1 + LongShort / 100, 3, prod, fill = NA, align = "right") - 1,
    Market_3m_cum = rollapply(1 + Market / 100, 3, prod, fill = NA, align = "right") - 1,
    All3Negative = rollapply(LongShort < 0, 3, all, fill = NA, align = "right")
  )

raw_crash_episodes <- loss_df %>%
  filter(LS_3m_cum < -0.12, All3Negative == TRUE)

clean_crashes <- raw_crash_episodes %>%
  mutate(NewEpisode = c(TRUE, diff(as.yearmon(Date)) > 0.25)) %>%
  filter(NewEpisode)

crash_table <- clean_crashes %>%
  transmute(
    `Episode End` = format(Date, "%Y-%m"),
    `Ind-Mom 3M Cum. Return` = LS_3m_cum,
    `Market 3M Cum. Return` = Market_3m_cum,
    `Months Involved` = paste(
      format(Date %m-% months(2), "%Y-%m"),
      format(Date %m-% months(1), "%Y-%m"),
      format(Date, "%Y-%m"),
      sep = ", "
    )
  )

flextable(crash_table) %>%
  set_caption("Major Industry Momentum Crash Episodes (3-Month Sequences)") %>%
  colformat_num(j = c("Ind-Mom 3M Cum. Return", "Market 3M Cum. Return"), digits = 4) %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  theme_booktabs()
#Plot
long_short_cum <- long_short_returns %>%
  arrange(Date) %>%
  mutate(CumReturn = cumprod(1 + LongShort / 100))

episodes_to_plot <- crash_table %>%
  mutate(
    Start = as.Date(as.yearmon(substr(`Months Involved`, 1, 7))),
    End = as.Date(as.yearmon(substr(`Months Involved`, nchar(`Months Involved`) - 6, nchar(`Months Involved`)))) + months(1) - 1,
    Label = `Months Involved`,
    Group = factor(row_number())
  )

color_strong <- c("#e41a1c", "#ff7f00", "#33a02c", "#984ea3", "#377eb8")[1:nrow(episodes_to_plot)]

ggplot(long_short_cum, aes(x = Date, y = CumReturn)) +
  geom_rect(data = episodes_to_plot,
            aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf, fill = Label),
            alpha = 5, inherit.aes = FALSE) +  
  geom_line(color = "black", linewidth = 0.6) +
  scale_fill_manual(values = color_strong, name = "Crash Episodes\n(Months Involved)") +
  labs(
    title = "Cumulative Return of Industry Momentum Strategy",
    subtitle = "Shaded areas highlight 3-month periods with major momentum crashes",
    x = "Date", y = "Cumulative Return"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20, color = "black"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 15, color = "#666666"),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 15),
    panel.grid.major = element_line(color = "#cccccc"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

# ────────────────────────────────────────────────────────────────
# Part 4. Bond Market
# ────────────────────────────────────────────────────────────────
# ────────────────────────────────────────────────────────────────
# Test 1 — Forward yield change over the life of the short bond
# ────────────────────────────────────────────────────────────────
library(readxl); library(zoo); library(dplyr); library(tidyr)
library(purrr); library(broom); library(flextable); library(stringr)

# 1. Import and clean data
lw_raw <- read_excel("~/Downloads/LW_monthly.xlsx", skip = 8)
names(lw_raw)[1] <- "Date"

lw <- lw_raw %>%
  mutate(Date = as.Date(as.yearmon(as.character(Date), format = "%Y%m"))) %>%
  filter(Date >= as.Date("1961-01-01") & Date <= as.Date("2023-12-01")) %>%
  arrange(Date)

# 2. Sélection dynamique de toutes les colonnes de taux en mois
rate_cols <- names(lw)[grepl("^\\d+ m$", names(lw))]  # toutes les colonnes "x m"
rates <- lw %>%
  dplyr::select(Date, all_of(rate_cols))

# 3. Fonction de régression Test 1
run_EH_test1 <- function(data, short_rate_col = "1 m") {
  n_vals <- c(2, 3, 4, 6, 9, 12, 24, 36, 48, 60, 120)  # mois = 2Y, 3Y, 4Y, 5Y, 10Y
  
  results <- tibble(
    n = integer(),
    alpha0 = numeric(),
    se_alpha0 = numeric(),
    alpha1 = numeric(),
    se_alpha1 = numeric(),
    t_alpha1 = numeric(),
    signif_alpha1 = character()
  )
  
  for (n in n_vals) {
    yn  <- paste0(n, " m")
    yn1 <- paste0(n - 1, " m")
    
    # check if columns exist
    if (!(yn %in% names(data)) || !(yn1 %in% names(data))) {
      message("Missing columns: ", yn, " or ", yn1, " — skipped")
      next
    }
    
    y_short <- data[[short_rate_col]]
    y_n     <- data[[yn]]
    y_n1_lead <- dplyr::lead(data[[yn1]], 1)
    
    lhs <- y_n1_lead - y_n
    rhs <- (y_n - y_short) / (n - 1)
    
    df <- tibble(lhs = lhs, rhs = rhs) %>%
      filter(!is.na(lhs) & !is.na(rhs))
    
    model <- lm(lhs ~ rhs, data = df)
    coef <- summary(model)$coefficients
    
    alpha0 <- coef["(Intercept)", "Estimate"]
    se_alpha0 <- coef["(Intercept)", "Std. Error"]
    alpha1 <- coef["rhs", "Estimate"]
    se_alpha1 <- coef["rhs", "Std. Error"]
    t_alpha1 <- alpha1 / se_alpha1
    signif_alpha1 <- ifelse(abs(t_alpha1) > 1.96, "Yes", "No")
    
    results <- add_row(results,
                       n = n,
                       alpha0 = alpha0,
                       se_alpha0 = se_alpha0,
                       alpha1 = alpha1,
                       se_alpha1 = se_alpha1,
                       t_alpha1 = t_alpha1,
                       signif_alpha1 = signif_alpha1)
  }
  
  results %>% arrange(n)  
}

# 4. Application et affichage
results_test1 <- run_EH_test1(rates)

flextable(results_test1) %>%
  set_header_labels(
    n = "Maturity (n)",
    alpha0 = "Intercept",
    se_alpha0 = "SE(Intercept)",
    alpha1 = "Slope Coefficient",
    se_alpha1 = "Standard Error",
    t_alpha1 = "t-stat",
    signif_alpha1 = "Signif. at 5%"
  ) %>%
  add_header_lines("Table 1a — Regression of forward yield changes on predicted spread (Campbell & Shiller, 1991)") %>%
  align(align = "center", part = "header") %>%
  align(align = "center", j = names(results_test1), part = "body") %>%
  autofit() %>%
  theme_booktabs()

# ────────────────────────────────────────────────────────────────
# Test 2 : Regression of future average short rate change on spread
# ────────────────────────────────────────────────────────────────
run_EH_test2 <- function(bond_data, short_rate_col = "1 m") {
  n_values <- c(2, 3, 4, 6, 9, 12, 24, 36, 48, 60, 120)  # maturities en mois
  
  results <- tibble(
    n = integer(),
    beta0 = numeric(),
    se_beta0 = numeric(),
    beta1 = numeric(),
    se_beta1 = numeric(),
    t_beta1 = numeric(),
    signif_beta1 = character()
  )
  
  for (n in n_values) {
    yn_name <- paste0(n, " m")
    if (!(yn_name %in% colnames(bond_data))) next
    
    y_short <- bond_data[[short_rate_col]]
    y_long <- bond_data[[yn_name]]
    
    rhs <- y_long - y_short
    future_rates <- sapply(0:(n - 1), function(i) dplyr::lead(y_short, i))
    avg_future_short <- rowMeans(future_rates, na.rm = TRUE)
    lhs <- avg_future_short - y_short
    
    df <- tibble(lhs = lhs, rhs = rhs) %>%
      filter(!is.na(lhs) & !is.na(rhs))
    
    model <- lm(lhs ~ rhs, data = df)
    coef_summary <- summary(model)$coefficients
    
    beta0 <- coef_summary["(Intercept)", "Estimate"]
    se_beta0 <- coef_summary["(Intercept)", "Std. Error"]
    beta1 <- coef_summary["rhs", "Estimate"]
    se_beta1 <- coef_summary["rhs", "Std. Error"]
    t_beta1 <- beta1 / se_beta1
    signif_beta1 <- ifelse(abs(t_beta1) > 1.96, "Yes", "No")
    
    results <- add_row(results, n = n, beta0, se_beta0, beta1, se_beta1, t_beta1, signif_beta1)
  }
  
  results
}

# ────────────────────────────────────────────────────────────────
# Appliquer le test sur tes données : `rates`
# ────────────────────────────────────────────────────────────────
# Reformater les colonnes avec renommage explicite AVANT le select
results_test2_fmt <- results_test2 %>%
  mutate(
    `Maturity (n)`      = n,
    `Intercept`         = beta0,
    `SE(Intercept)`     = se_beta0,
    `Slope Coefficient` = beta1,
    `Standard Error`    = se_beta1,
    `t-stat`            = t_beta1,
    `Signif. at 5%`     = signif_beta1
  )

# Utiliser `select()` avec `any_of()` pour gérer noms non standards
results_test2_fmt <- results_test2_fmt %>%
  dplyr::select(any_of(c(
    "Maturity (n)", "Intercept", "SE(Intercept)",
    "Slope Coefficient", "Standard Error", "t-stat", "Signif. at 5%"
  )))

flextable(results_test2_fmt) %>%
  add_header_lines("Table 2 — Regression of perfect foresight change in short rate on current spread (Campbell & Shiller, 1991)") %>%
  align(align = "center", part = "header") %>%
  align(align = "center", j = names(results_test2_fmt), part = "body") %>%
  autofit() %>%
  theme_booktabs()


#Tables for report
returns_no_short <- data.frame(
  Ticker = c("VBLIX", "VEUSX"),
  "Mean Return" = c(0.0010, 0.0061)
)

cov_no_short <- data.frame(
  Ticker = c("VBLIX", "VEUSX"),
  VBLIX = c(0.0011, 0.0007),
  VEUSX = c(0.0007, 0.0025)
)

summary_no_short <- data.frame(
  Portfolio = c("Global Min Var", "Tangency"),
  VBLIX = c(0.8136, 0),
  VEUSX = c(0.1864, 1),
  Return = c(0.002, 0.0061),
  Volatility = c(0.0323, 0.0504),
  "Sharpe Ratio" = c(0.0541, 0.1166)
)

returns_short <- returns_no_short
cov_short <- cov_no_short

summary_short <- data.frame(
  Portfolio = c("Global Min Var", "Tangency"),
  VBLIX = c(0.8136, -0.4962),
  VEUSX = c(0.1864, 1.4962),
  Return = c(0.002, 0.0086),
  Volatility = c(0.0323, 0.0703),
  "Sharpe Ratio" = c(0.0541, 0.1194)
)

cat("\n────────────── No Short Selling ──────────────\n\n")

flextable(returns_no_short) %>%
  set_caption("Sample Mean Monthly Returns") %>%
  autofit()

flextable(cov_no_short) %>%
  set_caption("Covariance Matrix") %>%
  autofit()

flextable(summary_no_short) %>%
  set_caption("Portfolio Summary — No Short Selling") %>%
  autofit()

cat("\n────────────── With Short Selling ──────────────\n\n")

flextable(returns_short) %>%
  set_caption("Sample Mean Monthly Returns") %>%
  autofit()

flextable(cov_short) %>%
  set_caption("Covariance Matrix") %>%
  autofit()

flextable(summary_short) %>%
  set_caption("Portfolio Summary — With Short Selling") %>%
  autofit()

returns_no_short <- data.frame(
  Ticker = c("AAPL", "AMZN", "CSCO", "GOOG", "LOGI"),
  `Mean Return` = c(0.0205, 0.0253, 0.0108, 0.0184, 0.0217)
)

cov_no_short <- data.frame(
  Ticker = c("AAPL", "AMZN", "CSCO", "GOOG", "LOGI"),
  AAPL = c(0.0067, 0.0040, 0.0025, 0.0028, 0.0026),
  AMZN = c(0.0040, 0.0082, 0.0025, 0.0041, 0.0032),
  CSCO = c(0.0025, 0.0025, 0.0048, 0.0020, 0.0021),
  GOOG = c(0.0028, 0.0041, 0.0020, 0.0049, 0.0025),
  LOGI = c(0.0026, 0.0032, 0.0021, 0.0025, 0.0075)
)

summary_no_short <- data.frame(
  Portfolio = c("Global Min Var", "Tangency"),
  AAPL = c(0.1217, 0.2215),
  AMZN = c(0, 0.2540),
  CSCO = c(0.3792, 0),
  GOOG = c(0.3376, 0.2284),
  LOGI = c(0.1615, 0.2960),
  Return = c(0.0163, 0.0216),
  Volatility = c(0.0562, 0.0643),
  `Sharpe Ratio` = c(0.2853, 0.3315)
)

returns_short <- returns_no_short
cov_short <- cov_no_short

summary_short <- data.frame(
  Portfolio = c("Global Min Var", "Tangency"),
  AAPL = c(0.1324, 0.2449),
  AMZN = c(-0.0476, 0.2687),
  CSCO = c(0.3836, -0.0735),
  GOOG = c(0.3644, 0.2457),
  LOGI = c(0.1669, 0.3143),
  Return = c(0.0160, 0.0223),
  Volatility = c(0.0561, 0.0665),
  `Sharpe Ratio` = c(0.2800, 0.3322)
)

print("────────────── No Short Selling ──────────────")
flextable(returns_no_short) %>%
  set_caption("Sample Mean Monthly Returns") %>%
  autofit()

flextable(cov_no_short) %>%
  set_caption("Covariance Matrix") %>%
  autofit()

flextable(summary_no_short) %>%
  set_caption("Portfolio Summary — No Short Selling") %>%
  autofit()

print("────────────── With Short Selling ──────────────")
flextable(returns_short) %>%
  set_caption("Sample Mean Monthly Returns") %>%
  autofit()

flextable(cov_short) %>%
  set_caption("Covariance Matrix") %>%
  autofit()

flextable(summary_short) %>%
  set_caption("Portfolio Summary — With Short Selling") %>%
  autofit()
