# Load packages
require(lmerTest)
library(xtable)
require(MuMIn)
require(devtools)

# Load data
diff_w <- read.csv("R_tz_no_aggregates_w.csv", header = TRUE, as.is = TRUE)
str(diff_w)

# Same for weekday dataframe
diff_w $useruuid <- as.factor(diff_w$useruuid)
diff_w $east_west <- as.factor(diff_w$east_west)
diff_w $east_west <- relevel(diff_w$east_west,ref="west")
diff_w $bmi_cat <- as.factor(diff_w$bmi_cat)
diff_w $bmi_cat <- relevel(diff_w $bmi_cat,ref="1")
diff_w $generation <- as.factor(diff_w$generations)
diff_w $generation <- relevel(diff_w$generation,ref="gen x")
diff_w $bmi_cat <- as.factor(diff_w$bmi_cat)
diff_w $bmi_cat <- relevel(diff_w $bmi_cat,ref=1)
diff_w $gender <- as.factor(diff_w$gender)
diff_w $gender <- relevel(diff_w $gender,ref="MALE")
diff_w $tz_diff_hour <- as.factor(diff_w$tz_diff_hour)
diff_w $tz_diff_hour <- relevel(diff_w$tz_diff_hour,ref="0")

diff_w $duration_diff <- as.numeric(diff_w$duration_diff)
diff_w $dur_C <- as.numeric(diff_w$duration_median_C)
diff_w $log_distance_c <- as.numeric(diff_w$log_distance_c)

# Full model + slope covariates 
diff_travel_full_slope_w <- lmer(duration_diff ~ 1 + dur_C*east_west  + dur_C*generation + dur_C*bmi_cat + gender + tz_diff_hour + log_distance_c + (1|useruuid), diff_w, na.action='na.omit', REML=TRUE)
anova(diff_travel_full_slope_w)
drop1(diff_travel_full_slope_w,test="F")
summary(diff_travel_full_slope_w)


