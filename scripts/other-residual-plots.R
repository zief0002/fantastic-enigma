wine = read_csv("~/Documents/github/benevolent-anteater/data/wine.csv")
wine


lm.1 = lm(rating ~ 1 + price, data = wine)
aug_wine = augment(lm.1)

# Density plot of the residuals
p1 = ggplot(data = aug_wine, aes(x = .std.resid)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized Residual") +
  ylab("Probability density")

# Scatterplot of the residuals versus X
p2 = ggplot(data = aug_wine, aes(x = price, y = .std.resid)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Wine price") +
  ylab("Standardized Residual")


# Layout plots (need patchwork library loaded)
p1 | p2




slid = read_csv("~/Documents/github/urban-memory/data/slid.csv")
lm.2 = lm(wages ~ 1 + education, data = slid)
aug_slid = augment(lm.2)

# Density plot of the residuals
p1 = ggplot(data = aug_slid, aes(x = .std.resid)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized Residual") +
  ylab("Probability density")

# Scatterplot of the residuals versus X
p2 = ggplot(data = aug_slid, aes(x = education, y = .std.resid)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Education") +
  ylab("Standardized Residual")


# Layout plots (need patchwork library loaded)
p1 | p2


fl = read_csv("data/florida-vote-2000.csv")
lm.3 = lm(buchanan ~ 1 + reg_reform, data = fl)
aug_fl = augment(lm.3)

# Density plot of the residuals
p1 = ggplot(data = aug_fl, aes(x = .std.resid)) +
  stat_density(geom = "line") +
  theme_bw() +
  xlab("Standardized Residual") +
  ylab("Probability density")

# Scatterplot of the residuals versus X
p2 = ggplot(data = aug_fl, aes(x = reg_reform, y = .std.resid)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Registered Reform Party members") +
  ylab("Standardized Residual")


# Layout plots (need patchwork library loaded)
p1 | p2




aug_fl = aug_fl |> 
  mutate(cty = fl$county)

aug_fl

extreme = aug_fl |> 
  filter(.std.resid <= -2 | .std.resid >= 2)

nonextreme = aug_fl |> 
  filter(.std.resid > -2 & .std.resid < 2)

# Plot using text for the extreme observations and points for the non-extreme
ggplot(data = extreme, aes(x = .fitted, y = .std.resid)) +
  geom_text(aes(label = cty), size = 4, color = "red") +
  geom_point(data = nonextreme) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab("Fitted values") +
  ylab("Standardized residual")




