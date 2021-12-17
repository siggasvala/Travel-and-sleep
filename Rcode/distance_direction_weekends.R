# Load packages
require(lmerTest)
library(xtable)
require(MuMIn)
require(devtools)

# Load data
diff_f <- read.csv("R_dist_eastwest_tz_f.csv", header = TRUE, as.is = TRUE)
str(diff_f)

# Same for weekday dataframe
diff_f $useruuid <- as.factor(diff_f$useruuid)
diff_f $east_west <- as.factor(diff_f$east_west)
diff_f $east_west <- relevel(diff_f$east_west,ref="west")
diff_f $generation <- as.factor(diff_f$generations)
diff_f $generation <- relevel(diff_f$generation,ref="gen x")
diff_f $bmi_cat <- as.factor(diff_f$bmi_cat)
diff_f $bmi_cat <- relevel(diff_f $bmi_cat,ref=1)
diff_f $gender <- as.factor(diff_f$gender)
diff_f $gender <- relevel(diff_f $gender,ref="MALE")
diff_f $east_west_journey <- as.factor(diff_f$east_west_journey)
diff_f $east_west_journey <- relevel(diff_f$east_west_journey,ref="west_journey")
diff_f $tz_diff_cat <- as.factor(diff_f$tz_diff_cat)
diff_f $tz_diff_cat <- relevel(diff_f$tz_diff_cat,ref="0")

diff_f $duration_diff <- as.numeric(diff_f$duration_diff)
diff_f $dur_C <- as.numeric(diff_f$duration_median_C)
diff_f $log_distance_c <- as.numeric(diff_f$log_distance_c)

# Full model + slope covariates 
diff_travel_full_slope_f <- lmer(duration_diff ~ 1 + dur_C*east_west  + dur_C*generation + dur_C*bmi_cat + dur_C*gender + tz_diff_cat*east_west_journey + log_distance_c*dur_C + (1|useruuid), diff_f, na.action='na.omit', REML=TRUE)
anova(diff_travel_full_slope_f)
drop1(diff_travel_full_slope_f,test="F")

# Redefine
diff_travel_full_slope_f <- lmer(duration_diff ~ 1 + dur_C*east_west  + dur_C*generation + dur_C*bmi_cat + dur_C*gender + tz_diff_cat + east_west_journey + log_distance_c*dur_C + (1|useruuid), diff_f, na.action='na.omit', REML=TRUE)
anova(diff_travel_full_slope_f)
drop1(diff_travel_full_slope_f,test="F")
summary(diff_travel_full_slope_f)

