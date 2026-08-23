# Teaching Parenting Parent Survey
# 02_analysis_public.R
#
# Code created May 2026
# Last updated August 21, 2026

library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(purrr)
library(broom)
library(readr)

# ==============================================================================
# 1. IMPORT CLEAN PUBLIC DATA
# ==============================================================================

dat <- read_csv(
  "Parent_Survey_Clean_Analytic.csv",
  show_col_types = FALSE
)

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================

yes_no_to_binary <- function(x) {
  case_when(
    str_to_lower(str_squish(x)) == "yes" ~ 1L,
    str_to_lower(str_squish(x)) == "no" ~ 0L,
    TRUE ~ NA_integer_
  )
}

make_indicator <- function(x, pattern) {
  case_when(
    is.na(x) | str_squish(x) == "" ~ NA_integer_,
    str_detect(x, fixed(pattern, ignore_case = TRUE)) ~ 1L,
    TRUE ~ 0L
  )
}

likert_levels <- c(
  "Strongly disagree",
  "Disagree",
  "Slightly disagree",
  "Slightly agree",
  "Agree",
  "Strongly agree"
)

# ==============================================================================
# 3. ANALYTIC RECODING
# ==============================================================================

dat <- dat %>%
  mutate(
    
    # --------------------------------------------------------------------------
    # Binary coding for prior coursework and primary outcomes
    # Original survey responses are "Yes"/"No".
    # Recode to 1 = Yes, 0 = No for analysis.
    # --------------------------------------------------------------------------
    
    parenting_course = yes_no_to_binary(parenting_course),
    childdev_course = yes_no_to_binary(childdev_course),
    
    hs_parenting_support = yes_no_to_binary(hs_parenting_support),
    hs_childdev_support = yes_no_to_binary(hs_childdev_support),
    
    
    # --------------------------------------------------------------------------
    # High-school type
    # hs_type is a multiple-response variable.
    # Create a binary indicator for whether the respondent selected homeschool.
    # --------------------------------------------------------------------------
    
    hs_type_homeschool =
      make_indicator(hs_type, "Home School"),
    
    
    # Race/ethnicity was a multiple-response survey item.
    # Create indicators for categories needed to construct the analytic variable.
    
    parent_ethnicity_caucasian =
      make_indicator(parent_ethnicity, "Caucasian"),
    
    parent_ethnicity_african_american =
      make_indicator(parent_ethnicity, "African-American"),
    
    parent_ethnicity_hispanic_latino =
      make_indicator(parent_ethnicity, "Hispanic/Latino"),
    
    parent_ethnicity_asian_pacific_islander =
      make_indicator(parent_ethnicity, "Asian/Pacific Islander"),
    
    parent_ethnicity_native_american =
      make_indicator(parent_ethnicity, "Native American"),
    
    parent_ethnicity_middle_eastern =
      make_indicator(parent_ethnicity, "Middle Eastern"),
    
    parent_ethnicity_other_ind =
      make_indicator(parent_ethnicity, "Other"),
    
    # Collapse race/ethnicity into three mutually exclusive categories for
    # regression analyses because of small cell sizes.
    #
    # 1. Caucasian, non-Hispanic/Latino
    # 2. African American
    # 3. Other race/ethnicity
    #
    # Respondents identifying as Hispanic/Latino are included in "Other,"
    # including those who also selected Caucasian.
    
    ethnicity_3cat = case_when(
      is.na(parent_ethnicity) ~ NA_character_,
      
      # Anyone who selected African American is classified as African American,
      # regardless of any additional race/ethnicity selections
      parent_ethnicity_african_american == 1L ~
        "African American",
      
      # Caucasian respondents who did not identify as Hispanic/Latino
      parent_ethnicity_caucasian == 1L &
        parent_ethnicity_hispanic_latino == 0L ~
        "Caucasian, non-Hispanic/Latino",
      
      # All remaining respondents
      TRUE ~ "Other"
    ),
    
    ethnicity_3cat = factor(
      ethnicity_3cat,
      levels = c(
        "Caucasian, non-Hispanic/Latino",
        "African American",
        "Other"
      )
    ),
    
    
    # --------------------------------------------------------------------------
    # Parenting / child-development attitude items
    # Original responses use a six-point Likert scale.
    # First preserve the ordered response categories, then create numeric
    # versions scored from 1 = Strongly disagree to 6 = Strongly agree.
    # --------------------------------------------------------------------------
    
    across(
      parenting_attitude1:parenting_attitude19,
      ~ factor(
        .x,
        levels = likert_levels,
        ordered = TRUE
      )
    ),
    
    across(
      parenting_attitude1:parenting_attitude19,
      ~ as.integer(.x),
      .names = "{.col}_num"
    ),
    
    
    # --------------------------------------------------------------------------
    # Education category used in regression models
    # Original education categories are collapsed into three groups because
    # some original categories have small cell sizes.
    # --------------------------------------------------------------------------
    
    education_3cat = case_when(
      parent_education %in% c(
        "Some high school",
        "High school diploma/GED",
        "Associate degree"
      ) ~ "Associate's or less",
      
      parent_education == "Bachelor’s degree" ~ "Bachelor's degree",
      
      parent_education %in% c(
        "Master’s degree",
        "Doctoral degree (e.g. MD, PhD, EdD) or Professional degree (e.g. JD)"
      ) ~ "Graduate degree",
      
      TRUE ~ NA_character_
    ),
    
    # Set "Associate's or less" as the reference category in regression models.
    education_3cat = factor(
      education_3cat,
      levels = c(
        "Associate's or less",
        "Bachelor's degree",
        "Graduate degree"
      )
    ),
    
    
  
    # --------------------------------------------------------------------------
    # Household-income category used in regression models
    # Collapse the original six income categories into three groups because of
    # small cell sizes in the original categories.
    # --------------------------------------------------------------------------
    
    hh_income_model = case_when(
      hh_income %in% c(
        "0 - 35,000 USD",
        "35,000 – 65,000 USD"
      ) ~ "<65k",
      
      hh_income %in% c(
        "65,001 – 105,000 USD",
        "105,001 – 175,000 USD"
      ) ~ "65k–175k",
      
      hh_income %in% c(
        "175,001 – 250,000 USD",
        "More than 250,000 USD"
      ) ~ ">175k",
      
      TRUE ~ NA_character_
    ),
    
    # Set the lowest-income group as the reference category.
    hh_income_model = factor(
      hh_income_model,
      levels = c(
        "<65k",
        "65k–175k",
        ">175k"
      )
    ),
    
    
    # --------------------------------------------------------------------------
    # Parenting-information sources
    # parenting_source is a multiple-response variable.
    # Create binary indicators for the five sources examined in the analyses.
    # 1 = source selected; 0 = source not selected.
    # --------------------------------------------------------------------------
    
    use_parents =
      make_indicator(parenting_source, "My parents"),
    
    use_friends =
      make_indicator(parenting_source, "Friends"),
    
    use_internet =
      make_indicator(parenting_source, "Internet search"),
    
    use_pediatrician =
      make_indicator(parenting_source, "Pediatrician"),
    
    use_ai =
      make_indicator(parenting_source, "AI (e.g. ChatGPT)")
  )

# ==============================================================================
# 4. PARTICIPANT CHARACTERISTICS
# ==============================================================================

n_total <- nrow(dat)

# Number of states represented in the sample
n_states <- n_distinct(dat$state)
n_states

state_summary <- dat %>%
  count(state, sort = TRUE) %>%
  mutate(percent = round(100 * n / n_total, 1))

state_summary


# ------------------------------------------------------------------------------
# Table 1: participant demographic characteristics
# ------------------------------------------------------------------------------

# Single-response variables
single_vars <- c(
  urbanicity = "Area type",
  hh_income = "Household income",
  parent_education = "Parent education",
  parent_relationship = "Relationship to child"
)

single_demo <- map_dfr(
  names(single_vars),
  function(var) {
    dat %>%
      filter(!is.na(.data[[var]])) %>%
      count(category = .data[[var]]) %>%
      mutate(
        characteristic = single_vars[[var]],
        percent = round(100 * n / n_total, 1)
      ) %>%
      select(characteristic, category, n, percent)
  }
)

# Multiple-response variables
# Percentages use total sample N as the denominator, so they may sum to >100%.
multi_vars <- c(
  hs_type = "High school type",
  parent_employment = "Parent employment",
  parent_ethnicity = "Parent ethnicity"
)

multi_demo <- map_dfr(
  names(multi_vars),
  function(var) {
    dat %>%
      select(all_of(var)) %>%
      filter(!is.na(.data[[var]])) %>%
      separate_rows(all_of(var), sep = ",") %>%
      mutate(category = str_trim(.data[[var]])) %>%
      count(category) %>%
      mutate(
        characteristic = multi_vars[[var]],
        percent = round(100 * n / n_total, 1)
      ) %>%
      select(characteristic, category, n, percent)
  }
)

# Prior parenting / child-development coursework
course_demo <- tibble(
  characteristic = c(
    "Previously took a parenting course",
    "Previously took child development course"
  ),
  category = "Yes",
  n = c(
    sum(dat$parenting_course == 1, na.rm = TRUE),
    sum(dat$childdev_course == 1, na.rm = TRUE)
  )
) %>%
  mutate(percent = round(100 * n / n_total, 1))

table1 <- bind_rows(
  single_demo,
  multi_demo,
  course_demo
)

table1


# ------------------------------------------------------------------------------
# Parenting-style descriptive statistics
# ------------------------------------------------------------------------------

parenting_style_summary <- dat %>%
  select(
    parent_style_strict,
    parent_style_warmth,
    parent_style_negotiate,
    parent_style_permissive,
    parent_style_connection,
    parent_style_explainrules
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "score"
  ) %>%
  group_by(variable) %>%
  summarise(
    n = sum(!is.na(score)),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    min = min(score, na.rm = TRUE),
    max = max(score, na.rm = TRUE),
    .groups = "drop"
  )

parenting_style_summary


# ==============================================================================
# RESEARCH QUESTION 1
# WHERE DO PARENTS GO FOR PARENTING INFORMATION, AND DOES SOURCE USE VARY
# BY PARENT EDUCATION AND RACE/ETHNICITY?
# ==============================================================================

# ------------------------------------------------------------------------------
# Descriptive parenting-information source use
# ------------------------------------------------------------------------------

parenting_source_summary <- dat %>%
  select(parenting_source) %>%
  filter(!is.na(parenting_source)) %>%
  separate_rows(parenting_source, sep = ",") %>%
  mutate(parenting_source = str_trim(parenting_source)) %>%
  count(parenting_source, sort = TRUE) %>%
  mutate(percent = round(100 * n / n_total, 1))

parenting_source_summary

trusted_source_summary <- dat %>%
  filter(!is.na(source_trusted)) %>%
  count(source_trusted, sort = TRUE) %>%
  mutate(percent = round(100 * n / sum(n), 1))

trusted_source_summary

frequent_source_summary <- dat %>%
  filter(!is.na(source_freq)) %>%
  count(source_freq, sort = TRUE) %>%
  mutate(percent = round(100 * n / sum(n), 1))

frequent_source_summary


# ------------------------------------------------------------------------------
# Parenting-information source use by education
# Supplementary Table S1
# ------------------------------------------------------------------------------

sources <- c(
  "use_parents",
  "use_friends",
  "use_internet",
  "use_pediatrician",
  "use_ai"
)

source_by_education <- map_dfr(
  sources,
  function(source) {

    p_value <- chisq.test(
      table(
        dat$education_3cat,
        dat[[source]]
      )
    )$p.value

    dat %>%
      filter(!is.na(education_3cat)) %>%
      group_by(education_3cat) %>%
      summarise(
        n_using = sum(.data[[source]] == 1, na.rm = TRUE),
        n_total = n(),
        percent = round(100 * n_using / n_total, 1),
        .groups = "drop"
      ) %>%
      mutate(
        source = source,
        p_value = p_value
      )
  }
)

source_by_education


# ------------------------------------------------------------------------------
# Parenting-information source use by race/ethnicity
# Supplementary Table S2
# ------------------------------------------------------------------------------

source_by_ethnicity <- map_dfr(
  sources,
  function(source) {

    p_value <- chisq.test(
      table(
        dat$ethnicity_3cat,
        dat[[source]]
      )
    )$p.value

    dat %>%
      filter(!is.na(ethnicity_3cat)) %>%
      group_by(ethnicity_3cat) %>%
      summarise(
        n_using = sum(.data[[source]] == 1, na.rm = TRUE),
        n_total = n(),
        percent = round(100 * n_using / n_total, 1),
        .groups = "drop"
      ) %>%
      mutate(
        source = source,
        p_value = p_value
      )
  }
)

source_by_ethnicity


# ==============================================================================
# DESCRIPTIVE CONTEXT: OVERALL ATTITUDES TOWARD PARENTING / CHILD DEVELOPMENT
# ==============================================================================

# Full six-category distribution supports Figure 3.

# Labels for parenting attitude items
attitude_labels <- c(
  parenting_attitude1 = "People can learn to become better parents",
  parenting_attitude2 = "Knowledge about child development helps people be good parents",
  parenting_attitude3 = "Parenting is easy",
  parenting_attitude4 = "Parenting makes a difference in children's lives",
  parenting_attitude5 = "Parenting has little influence on child development",
  parenting_attitude6 = "I felt adequately prepared when I first became a parent",
  parenting_attitude7 = "Learning about parenting and child development before becoming a parent would have been helpful"
)


# Full six-category distribution
attitude_full_summary <- dat %>%
  select(parenting_attitude1:parenting_attitude7) %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "response"
  ) %>%
  filter(!is.na(response)) %>%
  mutate(
    item_label = recode(item, !!!attitude_labels)
  ) %>%
  count(item, item_label, response) %>%
  group_by(item, item_label) %>%
  mutate(
    percent = round(100 * n / sum(n), 1)
  ) %>%
  ungroup()

attitude_full_summary


# Collapse to Agree vs Disagree
attitude_collapsed <- dat %>%
  select(parenting_attitude1:parenting_attitude7) %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "response"
  ) %>%
  filter(!is.na(response)) %>%
  mutate(
    item_label = recode(item, !!!attitude_labels),
    
    category = case_when(
      response %in% c(
        "Strongly disagree",
        "Disagree",
        "Slightly disagree"
      ) ~ "Disagree",
      
      response %in% c(
        "Slightly agree",
        "Agree",
        "Strongly agree"
      ) ~ "Agree"
    )
  ) %>%
  count(item, item_label, category) %>%
  group_by(item, item_label) %>%
  mutate(
    percent = round(100 * n / sum(n), 1)
  ) %>%
  ungroup()

attitude_collapsed


# ==============================================================================
# RESEARCH QUESTION 2
# DO PARENTS SUPPORT TEACHING PARENTING / CHILD DEVELOPMENT IN HIGH SCHOOL,
# AND WHAT FACTORS PREDICT SUPPORT?
# ==============================================================================

# ------------------------------------------------------------------------------
# Overall support
# ------------------------------------------------------------------------------

parenting_support <- dat %>%
  count(hs_parenting_support) %>%
  mutate(percent = round(100 * n / sum(n), 1))

parenting_support

childdev_support <- dat %>%
  count(hs_childdev_support) %>%
  mutate(percent = round(100 * n / sum(n), 1))

childdev_support


# ------------------------------------------------------------------------------
# Closed-ended reasons for SUPPORTING parenting education
# Denominator = parents who supported teaching parenting (n = 171)
# ------------------------------------------------------------------------------

parenting_support_reason_summary <- tibble(
  reason = c(
    "Help adolescents become more responsible future parents",
    "Help adolescents understand their own parents",
    "Many high schoolers are already caring for children",
    "High school students are mature enough to discuss parenting"
  ),
  pattern = c(
    "Everybody should know about parenting because it will help them become more responsible parents",
    "Understanding parenting helps high school students understand their own parents better",
    "Many high schoolers are already taking care of kids",
    "High school students are mature enough to discuss parenting topics"
  )
) %>%
  mutate(
    n = map_int(
      pattern,
      ~ sum(
        str_detect(
          dat$hs_parenting_support_reason,
          fixed(.x, ignore_case = TRUE)
        ),
        na.rm = TRUE
      )
    ),
    denominator = sum(!is.na(dat$hs_parenting_support_reason)),
    percent = round(100 * n / denominator, 1)
  ) %>%
  select(reason, n, denominator, percent)

parenting_support_reason_summary


# ------------------------------------------------------------------------------
# Closed-ended reasons for SUPPORTING child-development education
# Denominator = parents who supported teaching child development (n = 182)
# ------------------------------------------------------------------------------

childdev_support_reason_summary <- tibble(
  reason = c(
    "Prepare students to care for young children in the future",
    "Everyone should understand how children grow and learn",
    "Many high schoolers are already caring for children"
  ),
  pattern = c(
    "Understanding how children learn and grow prepares high school students for interactions",
    "Everybody should know about how children grow and learn",
    "Many high schoolers are already taking care of kids"
  )
) %>%
  mutate(
    n = map_int(
      pattern,
      ~ sum(
        str_detect(
          dat$hs_childdev_support_reason,
          fixed(.x, ignore_case = TRUE)
        ),
        na.rm = TRUE
      )
    ),
    denominator = sum(!is.na(dat$hs_childdev_support_reason)),
    percent = round(100 * n / denominator, 1)
  ) %>%
  select(reason, n, denominator, percent)

childdev_support_reason_summary

# ------------------------------------------------------------------------------
# Closed-ended reasons for OPPOSING parenting education
# Denominator = parents who did not support teaching parenting (n = 30)
# ------------------------------------------------------------------------------

parenting_anti_reason_summary <- tibble(
  reason = c(
    "High school students are too young to be thinking about parenting",
    "High school is not the right place to learn about parenting",
    "Learning about parenting might encourage adolescents to have children",
    "Do not want anyone else teaching their child about parenting"
  ),
  pattern = c(
    "They are too young to be thinking about parenting",
    "High school is not the right place to learn about parenting",
    "Learning about parenting might encourage adolescents to have children before they would otherwise",
    "I don't want anyone else to teach my child about parenting"
  )
) %>%
  mutate(
    n = map_int(
      pattern,
      ~ sum(
        str_detect(
          dat$hs_parenting_anti_reason,
          fixed(.x, ignore_case = TRUE)
        ),
        na.rm = TRUE
      )
    ),
    denominator = sum(!is.na(dat$hs_parenting_anti_reason)),
    percent = round(100 * n / denominator, 1)
  ) %>%
  select(reason, n, denominator, percent)

parenting_anti_reason_summary


# ------------------------------------------------------------------------------
# Closed-ended reasons for OPPOSING child-development education
# Denominator = parents who did not support teaching child development (n = 19)
# ------------------------------------------------------------------------------

childdev_anti_reason_summary <- tibble(
  reason = c(
    "High school students do not need to know about child development at this age",
    "High school is not the right place to learn about child development",
    "Learning about child development might encourage adolescents to have children"
  ),
  pattern = c(
    "They don't need to know about child development at this age",
    "High school is not the right place to learn about child development",
    "Learning about child development might encourage adolescents to have children before they would otherwise"
  )
) %>%
  mutate(
    n = map_int(
      pattern,
      ~ sum(
        str_detect(
          dat$hs_childdev_anti_reason,
          fixed(.x, ignore_case = TRUE)
        ),
        na.rm = TRUE
      )
    ),
    denominator = sum(!is.na(dat$hs_childdev_anti_reason)),
    percent = round(100 * n / denominator, 1)
  ) %>%
  select(reason, n, denominator, percent)

childdev_anti_reason_summary

# ------------------------------------------------------------------------------
# Support by demographic characteristics / prior coursework
# Supplementary Table S3
# ------------------------------------------------------------------------------

support_predictors <- c(
  "education_3cat",
  "ethnicity_3cat",
  "hh_income_model",
  "hs_type_homeschool",
  "parenting_course",
  "childdev_course"
)

support_by_predictor <- map_dfr(
  support_predictors,
  function(var) {

    dat_sub <- dat %>%
      filter(!is.na(.data[[var]]))

    parenting_p <- chisq.test(
      table(dat_sub[[var]], dat_sub$hs_parenting_support)
    )$p.value

    childdev_p <- chisq.test(
      table(dat_sub[[var]], dat_sub$hs_childdev_support)
    )$p.value

    dat_sub %>%
      group_by(category = .data[[var]]) %>%
      summarise(
        category_n = n(),

        parenting_support_n =
          sum(hs_parenting_support == 1, na.rm = TRUE),

        parenting_support_percent =
          round(
            100 * parenting_support_n /
              sum(!is.na(hs_parenting_support)),
            1
          ),

        childdev_support_n =
          sum(hs_childdev_support == 1, na.rm = TRUE),

        childdev_support_percent =
          round(
            100 * childdev_support_n /
              sum(!is.na(hs_childdev_support)),
            1
          ),

        .groups = "drop"
      ) %>%
      mutate(
        category = as.character(category),
        predictor = var,
        parenting_p = parenting_p,
        childdev_p = childdev_p
      ) %>%
      select(
        predictor,
        category,
        category_n,
        parenting_support_n,
        parenting_support_percent,
        parenting_p,
        childdev_support_n,
        childdev_support_percent,
        childdev_p
      )
  }
)

support_by_predictor


# ------------------------------------------------------------------------------
# Multivariable logistic regression:
# demographic / experiential predictors of support
# Table 2
# ------------------------------------------------------------------------------

model_parenting <- glm(
  hs_parenting_support ~
    education_3cat +
    ethnicity_3cat +
    hh_income_model +
    hs_type_homeschool +
    parenting_course +
    childdev_course,
  family = binomial,
  data = dat
)

model_childdev <- glm(
  hs_childdev_support ~
    education_3cat +
    ethnicity_3cat +
    hh_income_model +
    hs_type_homeschool +
    parenting_course +
    childdev_course,
  family = binomial,
  data = dat
)

support_logit_results <- bind_rows(
  tidy(
    model_parenting,
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    mutate(outcome = "Parenting support"),

  tidy(
    model_childdev,
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    mutate(outcome = "Child development support")
) %>%
  filter(term != "(Intercept)")

support_logit_results


# ------------------------------------------------------------------------------
# Exploratory adjusted logistic regressions:
# parenting styles as predictors of support
# Table 2
# ------------------------------------------------------------------------------

style_vars <- c(
  "parent_style_strict",
  "parent_style_warmth",
  "parent_style_negotiate",
  "parent_style_permissive",
  "parent_style_connection",
  "parent_style_explainrules"
)

run_style_model <- function(outcome, predictor) {

  glm(
    reformulate(
      c(
        predictor,
        "education_3cat",
        "ethnicity_3cat",
        "hh_income_model",
        "hs_type_homeschool",
        "parenting_course",
        "childdev_course"
      ),
      response = outcome
    ),
    family = binomial,
    data = dat
  )
}

style_model_results <- expand_grid(
  outcome = c(
    "hs_parenting_support",
    "hs_childdev_support"
  ),
  predictor = style_vars
) %>%
  mutate(
    model = map2(
      outcome,
      predictor,
      run_style_model
    )
  ) %>%
  mutate(
    result = map2(
      model,
      predictor,
      ~ tidy(
        .x,
        exponentiate = TRUE,
        conf.int = TRUE
      ) %>%
        filter(term == .y)
    )
  ) %>%
  select(outcome, predictor, result) %>%
  unnest(result)

style_model_results


# ------------------------------------------------------------------------------
# Exploratory adjusted logistic regressions:
# parenting-related attitudes as predictors of support
# Supplementary Table S4
# ------------------------------------------------------------------------------

attitude_vars <- paste0(
  "parenting_attitude",
  1:7,
  "_num"
)

run_attitude_model <- function(outcome, predictor) {

  glm(
    reformulate(
      c(
        predictor,
        "education_3cat",
        "ethnicity_3cat",
        "hh_income_model",
        "hs_type_homeschool",
        "parenting_course",
        "childdev_course"
      ),
      response = outcome
    ),
    family = binomial,
    data = dat
  )
}

attitude_model_results <- expand_grid(
  outcome = c(
    "hs_parenting_support",
    "hs_childdev_support"
  ),
  predictor = attitude_vars
) %>%
  mutate(
    model = map2(
      outcome,
      predictor,
      run_attitude_model
    )
  ) %>%
  mutate(
    result = map2(
      model,
      predictor,
      ~ tidy(
        .x,
        exponentiate = TRUE,
        conf.int = TRUE
      ) %>%
        filter(term == .y)
    )
  ) %>%
  select(outcome, predictor, result) %>%
  unnest(result)

attitude_model_results


# ------------------------------------------------------------------------------
# Homeschool descriptive statistic referenced in the Discussion
# ------------------------------------------------------------------------------

homeschool_overall <- dat %>%
  summarise(
    homeschool_n = sum(hs_type_homeschool == 1, na.rm = TRUE),
    homeschool_percent =
      round(100 * mean(hs_type_homeschool == 1, na.rm = TRUE), 1)
  )

homeschool_overall

homeschool_non_support <- dat %>%
  filter(hs_parenting_support == 0) %>%
  summarise(
    non_supporters_n = n(),
    homeschool_n = sum(hs_type_homeschool == 1, na.rm = TRUE),
    homeschool_percent =
      round(100 * mean(hs_type_homeschool == 1, na.rm = TRUE), 1)
  )

homeschool_non_support


# ==============================================================================
# RESEARCH QUESTION 3
# WHAT TOPICS DO PARENTS THINK ARE MOST / LEAST IMPORTANT TO TEACH IN HS?
# ==============================================================================

topic_labels <- c(
  parenting_attitude10 = "Physical development",
  parenting_attitude11 = "Language/cognitive development",
  parenting_attitude12 = "Social/emotional development",
  parenting_attitude13 = "Parenting readiness skills",
  parenting_attitude14 = "Parenting behaviors/approaches",
  parenting_attitude15 = "Parent-child relationships",
  parenting_attitude16 = "Parent/caregiver well-being",
  parenting_attitude17 = "Child and family nutrition",
  parenting_attitude18 = "Identifying health challenges",
  parenting_attitude19 = "Parenting resources"
)

# Full six-category response distribution (supports Figure 4)
topic_attitude_table <- dat %>%
  select(parenting_attitude10:parenting_attitude19) %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "response"
  ) %>%
  filter(!is.na(response)) %>%
  mutate(
    topic = recode(item, !!!topic_labels)
  ) %>%
  count(topic, response) %>%
  group_by(topic) %>%
  mutate(
    percent = round(100 * n / sum(n), 1)
  ) %>%
  ungroup()

topic_attitude_table

# Collapsed Agree / Disagree summary for interpretation
topic_agreement_table <- dat %>%
  select(parenting_attitude10:parenting_attitude19) %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "response"
  ) %>%
  filter(!is.na(response)) %>%
  mutate(
    topic = recode(item, !!!topic_labels),
    category = case_when(
      response %in% c(
        "Strongly disagree",
        "Disagree",
        "Slightly disagree"
      ) ~ "Disagree",
      response %in% c(
        "Slightly agree",
        "Agree",
        "Strongly agree"
      ) ~ "Agree"
    )
  ) %>%
  count(topic, category) %>%
  group_by(topic) %>%
  mutate(
    percent = round(100 * n / sum(n), 1)
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = category,
    values_from = c(n, percent)
  ) %>%
  arrange(desc(percent_Agree))

topic_agreement_table

# Single topic selected as most important
most_important_topics <- dat %>%
  filter(!is.na(topics_important)) %>%
  count(topics_important, sort = TRUE) %>%
  mutate(
    percent = round(100 * n / sum(n), 1)
  )

most_important_topics

# Single topic selected as least important
least_important_topics <- dat %>%
  filter(!is.na(topics_unimportant)) %>%
  count(topics_unimportant, sort = TRUE) %>%
  mutate(
    percent = round(100 * n / sum(n), 1)
  )

least_important_topics
