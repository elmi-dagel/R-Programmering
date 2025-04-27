

# --------------------------------------
# Ladda paket
# --------------------------------------
library(tidyverse)
library(readxl)
library(GGally)
library(car)
library(rsample)
library(Metrics)

# --------------------------------------
# Läs in datan
# --------------------------------------
data <- read_excel("C:/Users/Mahad/Desktop/data_insamling_volvo.xlsx")

# --------------------------------------
# Dataförberedelse
# --------------------------------------
glimpse(data)
colSums(is.na(data))

data_clean <- data

# Hantera saknade värden
na_vars <- c("Biltyp", "Drivning", "Färg", "Modell", "Växellåda")
for (v in na_vars) {
  data_clean[[v]][is.na(data_clean[[v]])] <- "Okänd"
}

data_clean$Hästkrafter[is.na(data_clean$Hästkrafter)] <- median(data_clean$Hästkrafter, na.rm = TRUE)

# Ta bort irrelevanta kolumner
data_clean <- data_clean %>% select(-Motorstorlek, -Datum_i_trafik)

# Faktorisera
kat_var <- c("Säljare", "Bränsle", "Växellåda", "Biltyp", "Drivning", "Färg", "Märke", "Modell")
data_clean[kat_var] <- lapply(data_clean[kat_var], as.factor)

# Skapa nya variabler
data_clean$Bilens_ålder <- 2025 - data_clean$Modellår
data_clean$LogPris <- log(data_clean$Försäljningspris)

# --------------------------------------
# Explorativ dataanalys (EDA)
# --------------------------------------

# Histogram
histogram_vars <- c("Försäljningspris", "Miltal", "Modellår", "Hästkrafter")

for (v in histogram_vars) {
  p <- ggplot(data_clean, aes(x = .data[[v]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(title = paste("Histogram för", v))
  
  print(p)  
}

# Stapeldiagram
bar_vars <- c("Säljare", "Växellåda", "Bränsle", "Biltyp", "Drivning")

for (v in bar_vars) {
  p <- ggplot(data_clean, aes(x = .data[[v]])) +
    geom_bar(fill = "coral") +
    labs(title = paste("Fördelning av", v)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)  
}


# Samband ålder och pris

ggplot(data_clean, aes(x = Bilens_ålder, y = Försäljningspris)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue")

ggplot(data_clean, aes(x = Bilens_ålder, y = LogPris)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue")

# Boxplots
box_vars <- c("Försäljningspris", "Miltal", "Bilens_ålder", "Hästkrafter")

for (v in box_vars) {
  p <- ggplot(data_clean, aes(y = .data[[v]])) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Boxplot av", v))
  
  print(p)  
}


# --------------------------------------
# Outlierhantering
# --------------------------------------
remove_outliers <- function(df, column) {
  q1 <- quantile(df[[column]], 0.25)
  q3 <- quantile(df[[column]], 0.75)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  df %>% filter(df[[column]] >= lower & df[[column]] <= upper)
}

data_no_outliers <- data_clean
outlier_cols <- c("Försäljningspris", "Miltal", "Bilens_ålder", "Hästkrafter")
for (v in outlier_cols) {
  data_no_outliers <- remove_outliers(data_no_outliers, v)
}

# --------------------------------------
# Korrelation och multikollinearitet
# --------------------------------------
library(GGally)
num_data_no_outliers <- data_no_outliers %>% select(where(is.numeric))
ggpairs(num_data_no_outliers)

mod_vif <- lm(LogPris ~ Miltal + Hästkrafter + Bilens_ålder, data = data_no_outliers)
vif(mod_vif)

# --------------------------------------
# Dela upp datan i träning och test
# --------------------------------------
set.seed(123)
split <- initial_split(data_no_outliers, prop = 0.7)
train_data <- training(split)
test_data <- testing(split)

# --------------------------------------
# Träna regressionsmodell
# --------------------------------------
modell_full <- lm(LogPris ~ Miltal + Hästkrafter + Bilens_ålder +
                    Säljare + Växellåda + Bränsle + Biltyp + Drivning,
                  data = train_data)

summary(modell_full)

# --------------------------------------
# Diagnostik av modellen
# --------------------------------------
qqnorm(residuals(modell_full))
qqline(residuals(modell_full), col = "red")

hist(residuals(modell_full), breaks = 30, col = "lightblue")

plot(fitted(modell_full), residuals(modell_full))
abline(h = 0, col = "red")

plot(cooks.distance(modell_full), type = "h")
abline(h = 4/length(train_data$LogPris), col = "red", lty = 2)

# --------------------------------------
# Prediktion och utvärdering
# --------------------------------------
test_data <- test_data %>%
  filter(Växellåda != "Okänd", Biltyp %in% levels(train_data$Biltyp))
test_data <- test_data %>%
  filter(Biltyp != "Coupé")

prediktioner <- predict(modell_full, newdata = test_data)
faktiska <- test_data$LogPris

ss_res <- sum((faktiska - prediktioner)^2)
ss_tot <- sum((faktiska - mean(faktiska))^2)
r_squared <- 1 - (ss_res / ss_tot)

rmse_varde <- rmse(faktiska, prediktioner)
mae_varde <- mae(faktiska, prediktioner)

cat("Resultat på testdatan:\n")
cat("R²:", round(r_squared, 3), "\n")
cat("RMSE:", round(rmse_varde, 3), "\n")
cat("MAE:", round(mae_varde, 3), "\n")
# Extra plot

ggplot(data.frame(Prediktion = prediktioner, Faktiskt = faktiska), aes(x = Prediktion, y = Faktiskt)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue")

# --------------------------------------

