
# =============================================================================
# Project 6: Gaming Addiction and Academic Performance (GPA)
# Analysis: Ordinal Logistic Regression using Cumulative Link Models (CLM)
# Outcome: GPA (ordinal, 4 levels)
# Instrument: IGD-20 Gaming Disorder Scale (20 items, 6 subscales)
# =============================================================================

# ── Libraries ────────────────────────────────────────────────────────────────
library(tidyverse)
library(readxl)

# ── Data Import ───────────────────────────────────────────────────────────────
gaming_data <- read_excel(path = "Project 6 data.xlsx")

# ── Variable Renaming ─────────────────────────────────────────────────────────
# Translate Arabic column names to English for downstream analysis
gaming_data <- gaming_data %>% rename("Age" = `العمر:`, "Social Status" = `الحالة الاجتماعية :`,
                       "College" = `إلى أي كلية تنتمي؟`, "Living with" = `مع من تعيش؟`,
                       "Ever been Diagnosed with a mental illness" = `هل سبق أن تم تشخيصك بمرض نفسي؟`,
                       "Smoking" = `هل أنت مدخن؟`
                       ) %>% drop_na()



# ── Factor Encoding ───────────────────────────────────────────────────────────
# Recode all numeric-coded variables into labeled factors
# GPA is set as an ordered factor (required for ordinal regression via clm())
gaming_data <- gaming_data %>% 
  mutate(Age = factor(Age, levels = c("1", "2", "3"),
                      labels = c("18-20", ">20-23", ">23")),
         `Social Status` = factor(`Social Status`, levels = c("1", "2"),
                                  labels = c("Single", "Married")),
         `Living with` = factor(`Living with`,
                                         levels = c("1", "2", "3"),
                                         labels = c("Alone", "Family", "Friends")),
         College = factor(College, levels = c("1", "2", "3", "4", "5", "6"),
                          labels = c("Nursing", "Public Health and Tropical Meidicne",
                                     "Pharmacy", "Applied Medical Sciences",
                                     "Dentistry", "Medicine")),
         `Ever been Diagnosed with a mental illness` = factor(
           `Ever been Diagnosed with a mental illness`, levels = c("0", "1"),
           labels = c("No", "Yes")),
         Smoking = factor(Smoking, levels = c("0", "1", "2"),
                          labels = c("Never Smoker", "Previous Smoker", "Current Smoker")),
         Addiction = factor(Addiction, levels = c("0", "1"), labels = c("No", "Yes")),
         GPA = factor(GPA, levels = c("1", "2", "3", "4"), labels = c("<2.5", "2.5-3.74",
                                                                      "3.75-4.49",
                                                                      "4.5-5"),
                      ordered = T))

# ── IGD-20 Item Renaming ──────────────────────────────────────────────────────
# Rename the 20 IGD scale items from Arabic to short English identifiers
# Items marked with "r" (e.g., item2r, item19r) are reverse-scored in the scale
gaming_data <- gaming_data %>% rename(
  "item1" = `1.  غالباً , لا أنام بسبب جلسات اللعب الطويلة.`,
  "item2r" = `2R.  عندما لا ألعب أبداً, لا أشعر بتحسن.`,
  "item3" = `3.  زدت بشكل ملحوظ الوقت الذي أخصصه للعب خلال السنة الماضية.`,
  "item4" = `4.  عندما لا ألعب، أشعر بأنني سريع الانفعال.`,
  "item5" = `5. فقدت الاهتمام بهواياتي الأخرى بسبب لعبي.`,
  "item6" = `6. أود تقليص الوقت الذي أكرّسه لكن ذلك صعب علي.`,
  "item7" = `7. أنشغل عادة في التفكير في جلسة اللعب المقبلة عندما ألعب`,
  "item8" = `8.  الجأ إلى الألعاب لمساعدتي على التغلب على أي مشاعر سيئة قد تراودني.`,
  "item9" = `9.  أحتاج إلى قضاء فترات متزايدة من الوقت في اللعب.`,
  "item10" = `10.  عندما لا ألعب ، أشعر بالحزن.`,
  "item11" = `11. كذبُت على أفراد عائلتي بسبب إسرافي في اللعب.`,
  "item12" = `12.   لا أظن أنني أستطيع الكفّ عن اللعب.`,
  "item13" = `13. أظن ان اللعب أصبح النشاط الأكثر استهلاكا لوقتي.`,
  "item14" = `14. ألعب لكي أنسى كل ما يزعجني.`,
  "item15" = `15. غالبا" ما أفكر ان نهارا بكامله لا يكفي لأفعل كل ما احتاج فعله في اللعبة.`,
  "item16" = `16. أميل إلى الشعور بالقلق إن كنت لا أستطيع اللعب لأي سبب من الأسباب .`,
  "item17" = `17. أظن أن لَعِبي يهدد علاقتي بشريكي/شريكتي.`,
  "item18" = `18. غالبا" ما أحاول أن العب أقل لكنني أجد أنني غير قادر على ذلك.`,
  "item19r" = `19R. أعرف أن نشاطي اليومي الرئيسي (أي المهنة، الدراسة، العمل المنزلي، الخ ) لا تتأثر سلبا بسبب لعبي.`,
  "item20" = `20. اعتقد أن لَعِبي يؤثّر سلبًا على نواحٍ هامةٍ في حياتي.`
)

# ── Analysis Subset ───────────────────────────────────────────────────────────
# Retain only demographic/outcome columns (cols 1–9), excluding IGD item responses
# This dataset is used for all regression and descriptive analyses
gaming_data_without_Q <- gaming_data %>% select(1:9)




