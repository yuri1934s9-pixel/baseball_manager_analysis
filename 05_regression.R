# =============================================================================
# パッケージの読み込み
# =============================================================================
library("dplyr")
library("tibble")
library("estimatr")
library("readr")
library("modelsummary")
library("broom")
library("stats")
library("MASS")
library("lmtest")
library("sandwich")
library("marginaleffects")
library("AER")
library("ggplot2")
library("patchwork")

# =============================================================================
# データ読み込みと結合
# =============================================================================
# ファイルパスを変数に格納
FILE_PATH_1 <- "監督効果_coach1.csv"
FILE_PATH_2 <- "年俸・年数.csv"

# データ読み込み
input_dir <- "監督効果"
df1 <- read_csv(file.path(input_dir, FILE_PATH_1))
df2 <- read_csv(file.path(input_dir, FILE_PATH_2))

# データ結合
df_merged <- merge(df1, df2)

# =============================================================================
# 回帰分析の実行
# =============================================================================
#「ポジション」をファクター型に変換
df_merged$ポジション <- relevel(factor(df_merged$ポジション), ref = "外野手")

# 球団ダミーをまとめる
team_vars <- c("阪神", "中日", "ヤクルト", "広島", "横浜",
               "西武", "日本ハム", "ロッテ", "ソフトバンク", "オリックス", "楽天", "近鉄")

team_formula <- paste(team_vars, collapse = " + ")

# 監督効果の要因
#モデル1-1: 監督効果 ~ ポジション(OLS)
f11 <- as.formula(paste("監督効果 ~ ポジション"))
result_11 <- lm_robust(f11, data = df_merged, clusters = 球団, se_type = "CR2")

#モデル1-2: 監督効果 ~ ポジション + 球団効果 (OLS)
f12 <- as.formula(paste("監督効果 ~ ポジション +", team_formula))
result_12 <- lm_robust(f12, data = df_merged, clusters = 球団, se_type = "CR2")

#モデル1-3: 監督効果 ~ ポジション + 監督経験 + 外国人 (OLS)
f13 <- as.formula(paste("監督効果 ~ ポジション + 監督経験D + 外国人D"))
result_13 <- lm_robust(f13, data = df_merged, clusters = 球団, se_type = "CR2")

#モデル1-4: 監督効果 ~ ポジション + 監督経験 + 外国人 + 球団効果 (OLS)
f14 <- as.formula(paste("監督効果 ~ ポジション +", team_formula, "+ 監督経験D + 外国人D"))
result_14 <- lm_robust(f14, data = df_merged, clusters = 球団, se_type = "CR2")

## 残差プロット
model_plot <- lm(f14, data = df_merged)
png("監督効果の要因_残差.png", width = 1200, height = 1200, res = 150)

# 画面を2x2に分割して表示
par(mfrow = c(2, 2))
plot(model_plot)
dev.off()
par(mfrow = c(1, 1)) # 設定を戻す

# 初期年俸
# モデル2-1: 初期年俸(対数) ~ 監督効果(OLS)
f21 <- as.formula(paste("log(初期年俸) ~ 監督効果"))
result_21 <- lm_robust(f21, data = df_merged, clusters = 球団, se_type = "CR2")

# モデル2-2: 初期年俸(対数) ~ 監督効果 + 球団効果 (OLS)
f22 <- as.formula(paste("log(初期年俸) ~ 監督効果 +", team_formula))
result_22 <- lm_robust(f22, data = df_merged, clusters = 球団, se_type = "CR2")

# モデル2-3: 初期年俸(対数) ~ 監督効果 + 監督経験 + 外国人 (OLS)
f23 <- as.formula(paste("log(初期年俸) ~ 監督効果 + 監督経験D + 外国人D"))
result_23 <- lm_robust(f23, data = df_merged, clusters = 球団, se_type = "CR2")

# モデル2-4: 初期年俸(対数) ~ 監督効果 + 球団効果 + 監督経験 + 外国人 (OLS)
f24 <- as.formula(paste("log(初期年俸) ~ 監督効果 +", team_formula, "+ 監督経験D + 外国人D"))
result_24 <- lm_robust(f24, data = df_merged, clusters = 球団, se_type = "CR2")

## 残差プロット
model_plot <- lm(f24, data = df_merged)
png("初期年俸_残差.png", width = 1200, height = 1200, res = 150)

# 画面を2x2に分割して表示
par(mfrow = c(2, 2))
plot(model_plot)
dev.off()
par(mfrow = c(1, 1)) # 設定を戻す

# 契約年数
# モデル3-1: 契約年数 ~ 監督効果(PO)
f31 <- as.formula(paste("契約年数 ~ 監督効果"))
result_31 <- glm(f31, data = df_merged, family = "poisson")
## 分散差し替え
vcov_31 <- vcovCL(result_31, cluster = ~球団)

# モデル3-2: 契約年数 ~ 監督効果(NB)
f32 <- as.formula(paste("契約年数 ~ 監督効果"))
result_32 <- glm.nb(f32, data = df_merged)
vcov_32 <- vcovCL(result_32, cluster = ~球団)

## 限界効果
result_32_ame <- avg_comparisons(result_32, vcov = ~球団)

# モデル3-3: 契約年数 ~ 監督効果 + 球団効果 (NB)
f33 <- as.formula(paste("契約年数 ~ 監督効果 +", team_formula))
result_33 <- glm.nb(f33, data = df_merged)
vcov_33 <- vcovCL(result_33, cluster = ~球団)

## 限界効果
result_33_ame <- avg_comparisons(result_33, vcov = ~球団)

# モデル3-4: 契約年数 ~ 監督効果 + 監督経験 + 外国人 (NB)
f34 <- as.formula(paste("契約年数 ~ 監督効果 + 監督経験D + 外国人D"))
result_34 <- glm.nb(f34, data = df_merged)
vcov_34 <- vcovCL(result_34, cluster = ~球団)

## 限界効果
result_34_ame <- avg_comparisons(result_34, vcov = ~球団)

# モデル3-5: 契約年数 ~ 監督効果 + 球団効果 + 監督経験 + 外国人 (NB)
f35 <- as.formula(paste("契約年数 ~ 監督効果 +", team_formula, "+ 監督経験D + 外国人D"))
result_35 <- glm.nb(f35, data = df_merged)
vcov_35 <- vcovCL(result_35, cluster = ~球団)

## 限界効果
result_35_ame <- avg_comparisons(result_35, vcov = ~球団)

# =============================================================================
# 結果の出力
# =============================================================================
## 保存先ファイルを指定
output_dir <- "推定結果"

# 表示オプションの設定
MSUMMARY_STARS <- c("*" = .1, "**" = .05, "***" = .01)

# --- カスタム関数の定義 ---
# 1. GLMモデルの擬似R2を計算するための関数
glance_custom.glm <- function(x, ...) {
  # モデルの対数尤度
  ll_model <- as.numeric(logLik(x))
  
  # Nullモデル（切片のみ）の対数尤度
  ll_null <- as.numeric(logLik(update(x, . ~ 1)))
  
  # McFaddenの擬似決定係数（Pseudo R2）の計算
  pseudo_r2 <- 1 - (ll_model / ll_null)
  
  # 分散比を計算 (ピアソン残差ベース)
  disp <- sum(residuals(x, type = "pearson")^2) / x$df.residual
  
  tibble(pseudo_r2 = pseudo_r2, dispersion_ratio = disp)
}

# 表示する適合度指標のマッピングを定義
gof_map_custom_1 <- tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "観測数", 0,
  "adj.r.squared", "調整済み R2", 3, 
  "pseudo_r2", "擬似 R2", 3
  )

gof_map_custom_2 <- tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "観測数", 0,
  "aic", "AIC", 3
)

gof_map_custom_3 <- tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "観測数", 0,
  "dispersion_ratio", "分散比", 3,
)

# 変数の表示順序
map_vars <- c(
  "(Intercept)" = "定数項",
  "ポジション投手" = "投手",
  "ポジション捕手" = "捕手",
  "ポジション一塁手" = "一塁手",
  "ポジション二塁手" = "二塁手",
  "ポジション三塁手" = "三塁手",
  "ポジション遊撃手" = "遊撃手",
  "監督効果" = "監督効果",
  "阪神" = "阪神",
  "中日" = "中日",
  "ヤクルト" = "ヤクルト",
  "広島" = "広島",
  "横浜" = "横浜",
  "西武" = "西武",
  "日本ハム" = "日本ハム",
  "ロッテ" = "ロッテ",
  "ソフトバンク" = "ソフトバンク",
  "オリックス" = "オリックス",
  "楽天" = "楽天",
  "近鉄" = "近鉄",
  "監督経験D" = "監督経験",
  "外国人D" = "外国人"
)

map_vars_1 <- c(
  "(Intercept)" = "定数項",
  "ポジション捕手" = "捕手",
  "ポジション二塁手" = "二塁手",
  "監督効果" = "監督効果",
  "横浜" = "横浜",
  "西武" = "西武",
  "日本ハム" = "日本ハム",
  "ロッテ" = "ロッテ",
  "ソフトバンク" = "ソフトバンク",
  "オリックス" = "オリックス",
  "楽天" = "楽天",
  "近鉄" = "近鉄",
  "監督経験D" = "監督経験",
  "外国人D" = "外国人"
)

# --- 表1: 監督効果の決定要因(OLS) ---
model_list_factor <- list(
  "(a)" = result_11, 
  "(b)" = result_12, 
  "(c)" = result_13,
  "(d)" = result_14
)

rows_factor <- tribble(
  ~term, ~"(a)", ~"(b)", ~"(c)", ~"(d)",
  "省略されたポジションダミー", "外野手", "外野手", "外野手", "外野手",
  "省略された球団ダミー", "", "巨人", "",  "巨人"
)

msummary(
  model_list_factor, 
  stars = MSUMMARY_STARS, 
  coef_map = map_vars_1,
  gof_map = gof_map_custom_1,
  add_rows = rows_factor,
  output = file.path(output_dir, "監督効果の要因.png")
)

msummary(
  model_list_factor, 
  stars = MSUMMARY_STARS, 
  coef_map = map_vars,
  gof_map = gof_map_custom_1,
  add_rows = rows_factor,
  output = file.path(output_dir, "監督効果の要因(省略なし).png")
)

# --- 表2: 初期年俸への影響 (OLS) ---
model_list_salary <- list(
  "(a)" = result_21,
  "(b)" = result_22,
  "(c)" = result_23,
  "(d)" = result_24
)

rows_salary <- tribble(
  ~term, ~"(a)", ~"(b)", ~"(c)", ~"(d)",
  "省略された球団ダミー", "", "巨人", "", "巨人"
)

msummary(
  model_list_salary,
  stars = MSUMMARY_STARS,
  coef_map = map_vars,
  gof_map = gof_map_custom_1,
  add_rows = rows_salary,
  output = file.path(output_dir, "初期年俸への影響.png")
)

# --- 表3: 契約年数への影響 (PO) ---
## 過分散検定
sink(file.path(output_dir, "過分散検定.txt"))
print(dispersiontest(result_31, trafo = 2))
sink()

## ポアソン回帰・負の二項回帰比較
model_list_compare_2 <- list(
  "ポアソン回帰" = result_31,
  "負の二項回帰" = result_32
)

msummary(
  model_list_compare_2,
  vcov = list(vcov_31, vcov_32),
  exponentiate = TRUE,
  stars = MSUMMARY_STARS,
  coef_map = map_vars,
  gof_map = gof_map_custom_3,
  output = file.path(output_dir, "ポアソン・負の二項回帰の比較.png")
)

## 負の二項回帰
model_list_year <- list(
  "(a)" = result_32,
  "(b)" = result_33,
  "(c)" = result_34,
  "(d)" = result_35
)

rows_year <- tribble(
  ~term, ~"(a)", ~"(b)", ~"(c)", ~"(d)",
  "省略された球団ダミー", "", "巨人", "", "巨人"
)

msummary(
  model_list_year,
  vcov = list(vcov_32, vcov_33, vcov_34, vcov_35),
  exponentiate = TRUE,
  stars = MSUMMARY_STARS,
  coef_map = map_vars,
  gof_map = gof_map_custom_1,
  add_rows = rows_year,
  output = file.path(output_dir, "契約年数への影響.png")
)

model_list_ame <- list(
  "(a)の限界効果" = result_32_ame,
  "(b)の限界効果" = result_33_ame,
  "(c)の限界効果" = result_34_ame,
  "(d)の限界効果" = result_35_ame
)

rows_ame <- tribble(
  ~term, ~"(a)の限界効果", ~"(b)の限界効果", ~"(c)の限界効果", ~"(d)の限界効果",
  "省略された球団ダミー", "", "巨人", "","巨人"
)

msummary(
  model_list_ame,
  exponentiate = FALSE,
  stars = MSUMMARY_STARS,
  coef_map = map_vars,
  gof_map = gof_map_custom_1,
  add_rows = rows_ame,
  output = file.path(output_dir, "契約年数への影響(限界効果あり).png")
)

# 実行完了メッセージ
cat("✅ 表（監督効果要因、初期年俸、年俸上昇率、契約年数）の出力が完了しました。\n")
