School Expenses
================
Jeff Huang
4/27/2019

``` r
# Cleaning data for most efficient schools: aggregate to the district averages

carmel_tsal <- carmel %>% 
  rename(District = agency) %>% 
  group_by(District) %>% 
  summarize(total = mean(total_pay_benefits), base = mean(base_pay))

mend_tsal <- mend %>% 
  rename(District = agency) %>% 
  group_by(District) %>% 
  summarize(total = mean(total_pay_benefits), base = mean(base_pay))

pc_tsal <- pc %>% 
  rename(District = agency) %>% 
  group_by(District) %>% 
  summarize(total = mean(total_pay_benefits), base = mean(base_pay))

addtl_schools <- bind_rows(carmel_tsal, mend_tsal, pc_tsal)


# free and reduced lunchdata cleaning: calculate free and reduced lunch percentages

frlunch_rate <- frlunch %>% 
  mutate(district_code = paste(county_code, district_code, sep="")) %>% 
  group_by(district_code) %>% 
  summarize(free_count = sum(free_meal_count_k_12), frpm_count = sum(frpm_count_k_12), enrollment = sum(enrollment_k_12)) %>% 
  mutate(free_rate = free_count/enrollment, frpm_rate = frpm_count/enrollment) %>% 
  select(district_code, enrollment, free_rate, frpm_rate)
  
# staff demographic info - split total number of staff members
# staff education currently arranged as one column, need it to spread, and contain counts

total <- staff %>% 
  group_by(district_code) %>% 
  mutate(total = n()) %>% 
  select(1:5, 17)

staff_edu <- staff %>% 
  group_by(district_code, education_level) %>% 
  summarize(count = n()) %>% 
  spread(key=education_level, value=count) %>% 
  ungroup()
  
new_staff <- left_join(total, staff_edu, by="district_code") %>% 
  distinct(district_code, .keep_all=TRUE) 

# calculate percentages of each degree

n_edu_staff <- new_staff %>% 
  mutate(bachelor = B+C, doctor = D, master = M + V, bach_per = bachelor / total, doc_per = D / total, mast_per = master/total) %>% 
  select(district_code, county_name, district_name, bachelor, doctor, master, bach_per, doc_per, mast_per) 

# staff demographic characteristics
# teachers defined as greater than 60% of time spent teaching (looking for teachers that do not do admin work)
# admin defined as greater than 40% as administrative work  (looking for admins that do not teach)
# aggregate across districts

count_teach <- staff %>% 
  filter(fte_teaching > 60) %>% 
  group_by(district_code) %>% 
  summarize(teacher = n())

count_admin <- staff %>% 
  filter(fte_administrative > 40) %>% 
  group_by(district_code) %>% 
  summarize(admin = n())

# summarize statistics of demographics for each district, join other tables

add_staff <- staff %>% 
  select(district_code, county_name, 
       district_name, age, education_level, 
       years_teaching, years_in_district, fte_teaching, fte_administrative, fte_pupil_services) %>% 
  group_by(district_code) %>% 
  summarize(avg_t_age = mean(age), t_exp = mean(years_teaching), dist_exp = mean(years_in_district), num_staff = n()) %>% 
  full_join(count_admin, by="district_code") %>% 
  full_join(count_teach, by="district_code")


# full staff addtl info

full_staff <- full_join(n_edu_staff, add_staff, by="district_code") %>% 
  full_join(frlunch_rate, b="district_code")


# merge/clean SAT dataset - numeric handling of data instead of character

full_sat <- bind_rows(sat15, sat16, .id="year")
full_sat[, 7:11] <- sapply(full_sat[, 7:11], as.numeric)
```

    ## Warning in lapply(X = X, FUN = FUN, ...): NAs introduced by coercion

    ## Warning in lapply(X = X, FUN = FUN, ...): NAs introduced by coercion

    ## Warning in lapply(X = X, FUN = FUN, ...): NAs introduced by coercion

``` r
# compute average composite scores, as well as prepares for data merging

full_sat <- full_sat %>%
  mutate(sat_comp = avg_scr_read + avg_scr_math,
         sat_score = num_tst_takr * sat_comp,
         district_code = substr(cds, 1, 7)) %>% 
  select(1:8, sat_comp, sat_score, district_code, -rtype) %>% 
  drop_na(sat_comp, sat_score)

# merge AP dataset
# also adding numbers of tests taken, as well as average AP score

full_ap <- bind_rows(ap15, ap16, ap17, ap18, .id="year") %>%
  mutate(num_tst = num_scr1 + num_scr2 + num_scr3 + num_scr4 + num_scr5,
         score = num_scr1 * 1 + num_scr2 * 2 + num_scr3 * 3 + num_scr4 * 4 + num_scr5 * 5,
         district_code = substr(cds, 1, 7)) %>% 
  select(1:8, num_tst, score, district_code, -rtype) %>% 
  drop_na(score, num_tst)


# create composite scores by averaging each of the different scores, and create a "score" variable representing total score (to be added and divided at the district level later)

full_act <- bind_rows(act15, act16, act17, act18, .id="year") %>% 
  drop_na(sname, enroll12) %>% 
  mutate(comp = (avg_scr_eng + avg_scr_math + avg_scr_read + avg_scr_sci)/4, 
         score = comp * num_tst_takr,
         district_code = substr(cds, 1, 7)) %>% 
  mutate(test_per = num_tst_takr / enroll12) %>%
  select(1:8, comp, score, test_per, district_code, -rtype)



# district ranks. make sure that there are sufficient test takers (to prevent large outliers). Create a participation rate by dividing num of ACT test takers by 12th grade enrollment. not an meaningful rate by itself (all grades take the test), but it is a proxy for how many students are in a school, and how many take the test. used a percentile rank to avoid minor errors in this methodology

# same thing for the SAT and AP tests, although AP tests are divided by 10-12th grade enrollment because AP tests are much more commonly taken across grades than are SAT and ACT tests.
# create ranks for participation and composite score for merging later

district_act <- full_act %>% 
  drop_na(test_per, comp) %>% 
  group_by(district_code, dname) %>% 
  summarize(act_test_per = mean(num_tst_takr)/mean(enroll12), act_avg_comp = sum(score)/sum(num_tst_takr)) %>%
  ungroup() %>% 
  mutate(act_part_rank = ntile(act_test_per, 100), act_comp_rank = ntile(act_avg_comp, 100))

district_sat <- full_sat %>% 
  group_by(district_code, dname) %>% 
  summarize(sat_test_per = mean(num_tst_takr)/mean(enroll12), sat_avg_comp = sum(sat_score)/sum(num_tst_takr)) %>%
  ungroup() %>% 
  mutate(sat_part_rank = ntile(sat_test_per, 100), sat_comp_rank = ntile(sat_avg_comp, 100))

district_ap <- full_ap %>%
  group_by(district_code, dname) %>% 
  summarize(ap_test_per = mean(num_tst)/mean(enroll1012), ap_avg_score = sum(score)/sum(num_tst)) %>%
  ungroup() %>% 
  mutate(ap_part_rank = ntile(ap_test_per, 100), ap_score_rank = ntile(ap_avg_score, 100))



## expense data. binding 4 year averages of expenses. I wanted to use average daily attendance (current_expense_ada) to come up with a number of dollars spent per pupil according to days attended in school.

full_expense <- bind_rows(expense15, expense16, expense17, expense18, .id="year") %>% 
  filter(! district %in% c("Statewide", "Statewide Totals")) %>%
  filter(lea_type == "Unified") %>% 
  mutate(district_code = paste(co, cds, sep="")) %>% 
  group_by(district_code, district) %>% 
  summarize(d_spend = sum(current_expense_ada)) %>% 
  ungroup() %>% 
  mutate(expense_rank = ntile(desc(d_spend), 100))

# merged all test data

district <- full_join(district_sat, district_act, by="district_code") %>% 
  full_join(district_ap, by="district_code")

# merged the test and expense to perform more analysis

district <- district %>% 
  inner_join(full_expense, by="district_code") %>% 
  select(-'dname.x', -'dname.y', -district)
```

``` r
# create indexes for sorting later

index <- district %>% 
  mutate(comp_index = ((sat_comp_rank + act_comp_rank + ap_score_rank) / 3),
         comp_rank = ntile(comp_index, 100),
         part_index = ((sat_part_rank + act_part_rank + ap_part_rank) / 3),
         part_rank = ntile(part_index, 100))
  
# create table with entire dataset, cleaned

full_district_demo <- full_join(index, full_staff, by=c("district_code", "dname" = "district_name")) %>% 
  select(-county_name) %>% 
  drop_na(dname) %>% 
  distinct(dname, district_code, .keep_all=TRUE)


# creating tables for later - pick interesting ones - chose ACT because it is typically the more correlative with schooling (not income)

expense <- full_district_demo %>% 
  arrange(desc(d_spend)) %>% 
  head(10)

comp <- full_district_demo %>% 
  arrange(desc(act_avg_comp)) %>% 
  head(10)


# metrics - creating the efficiency ranking, reorganizing for the table

efficiency <- full_district_demo %>% 
  drop_na(sat_avg_comp, act_avg_comp, sat_test_per, act_test_per) %>% 
  mutate(comp_rank = ntile(((sat_avg_comp +  act_avg_comp / 2) +  ap_avg_score)/ 2, 100),
         part_rank = ntile(((sat_test_per +  act_test_per / 2) +  ap_test_per)/ 2, 100)) %>% 
  
# creates the efficiency ranking using the averages computed above  
  
  mutate(efficiency_rank = (4/12) * comp_rank + (3/12) * part_rank + (5/12) * expense_rank) %>% 
  arrange(desc(efficiency_rank)) %>% 
  mutate(act_test_per = act_test_per * 100,
         sat_test_per = sat_test_per * 100,
         ap_test_per = ap_test_per * 100) %>% 
  rename(District = dname, "ACT Participation Rate"=act_test_per,
         "SAT Participation Rate" = sat_test_per,
         "AP Participation Rate" = ap_test_per,
         "Avg. SAT Composite Score" = sat_avg_comp, 
         "Avg. ACT Composite Score" = act_avg_comp,
         "Avg. AP Score" = ap_avg_score,
         "Participation Percentile Rank"=part_rank, 
         "Composite Percentile Rank" = comp_rank, 
         "Per Pupil Spend" = d_spend,
         "Spend Percentile Rank" = expense_rank,
         "Efficiency Index" = efficiency_rank) %>% 
  
# adds additional stats for the three most efficient schools
  
  left_join(addtl_schools, by="District") %>% 
  mutate(admin_ratio_per100 = admin * 100 / enrollment,
         admin_teach_per100 = admin * 100 / teacher)


# adds averages for all the school districts in the data to use as comparison in bar graphs

average <- efficiency %>% 
  summarize_all(mean, na.rm=TRUE) 
```

    ## Warning in mean.default(district_code, na.rm = TRUE): argument is not
    ## numeric or logical: returning NA

    ## Warning in mean.default(District, na.rm = TRUE): argument is not numeric or
    ## logical: returning NA

``` r
average$District <- "Average Values"

efficiency <- bind_rows(efficiency, average)
```

``` r
# clean data for display

most_eff <- efficiency %>% 
  select(-sat_part_rank, -act_part_rank, -ap_part_rank, -ap_score_rank, -act_comp_rank, -sat_comp_rank, -`Spend Percentile Rank`, -bachelor, -doctor, -master, -`Efficiency Index`, -comp_index, -part_index, -`Composite Percentile Rank`, -`Participation Percentile Rank`) %>% 
  select(1:25)

top3 <- efficiency %>% 
  filter(District %in% c("Carmel Unified", "Mendocino Unified", "Piedmont City Unified", "Average Values"))

top10 <- efficiency %>% 
  arrange(desc(`Efficiency Index`))


# editing for later graphs

eff_tsal <- top3 %>%
  select(District, total, base) %>%
  rename(`Base Salary` = base,
              `Total Salary (incl. Benefits)` = total) %>% 
  melt(id.vars = 'District')

eff_ratio <- top3 %>%
  mutate(admin_ratio_per100 = admin * 1000 / enrollment,
         admin_teach_per100 = admin * 100 / teacher) %>%
  select(District, admin_ratio_per100, admin_teach_per100) %>%
  rename(`Admin to Student Ratio (per 1,000)` = admin_ratio_per100,
              `Admin to Teacher Ratio (per 100)` = admin_teach_per100) %>% 
  melt(id.vars='District')

eff_adv <- top3 %>%
  select(District, bach_per, mast_per, doc_per) %>%
  rename(
    `Percent of Teachers with Bachelors'` = bach_per,
    `Percent of Teachers with Masters'` = mast_per,
    `Percent of Teachers with Doctorates'` = doc_per
  ) %>% 
  melt(id.vars='District')

eff_frpm <- top3 %>%
  select(District, free_rate, frpm_rate) %>%
  rename(`Percent of Students on Free Lunch` = free_rate,
         `Percent of Students on Free or Reduced Lunch` = frpm_rate) %>% 
  melt(id.vars="District")

eff_addtl <- top3 %>%
  select(District, avg_t_age, t_exp, dist_exp) %>%
  rename(     `Average Teacher Age` = avg_t_age,
              `Years of Teaching Experience` = t_exp,
              `Years in District` = dist_exp) %>% 
  melt(id.vars="District")

select_eff <- most_eff %>% arrange(District)



library(gt)
top10
```

    ## # A tibble: 283 x 40
    ##    district_code `SAT Participat… `Avg. SAT Compo… sat_part_rank
    ##    <chr>                    <dbl>            <dbl>         <dbl>
    ##  1 2365581                   57.4            1113.            86
    ##  2 0161275                   64.2            1258.            91
    ##  3 1964964                   73.4            1251.            97
    ##  4 2765987                   23.8            1143.             9
    ##  5 2766134                   66.7            1104.            93
    ##  6 1964659                   71.1            1232.            95
    ##  7 3768031                   77.7            1102.            98
    ##  8 0161127                   62.2            1182.            91
    ##  9 2866290                   45.5            1024.            62
    ## 10 1964311                   52.1            1159.            78
    ## # … with 273 more rows, and 36 more variables: sat_comp_rank <dbl>, `ACT
    ## #   Participation Rate` <dbl>, `Avg. ACT Composite Score` <dbl>,
    ## #   act_part_rank <dbl>, act_comp_rank <dbl>, District <chr>, `AP
    ## #   Participation Rate` <dbl>, `Avg. AP Score` <dbl>, ap_part_rank <dbl>,
    ## #   ap_score_rank <dbl>, `Per Pupil Spend` <dbl>, `Spend Percentile
    ## #   Rank` <dbl>, comp_index <dbl>, `Composite Percentile Rank` <dbl>,
    ## #   part_index <dbl>, `Participation Percentile Rank` <dbl>,
    ## #   bachelor <dbl>, doctor <dbl>, master <dbl>, bach_per <dbl>,
    ## #   doc_per <dbl>, mast_per <dbl>, avg_t_age <dbl>, t_exp <dbl>,
    ## #   dist_exp <dbl>, num_staff <dbl>, admin <dbl>, teacher <dbl>,
    ## #   enrollment <dbl>, free_rate <dbl>, frpm_rate <dbl>, `Efficiency
    ## #   Index` <dbl>, total <dbl>, base <dbl>, admin_ratio_per100 <dbl>,
    ## #   admin_teach_per100 <dbl>

``` r
top10 <- top10 %>% 
  select(District, `Spend Percentile Rank`, `Composite Percentile Rank`, `Participation Percentile Rank`, `Efficiency Index`) %>% 
  head(10)
  
gt(top10) %>% 
  tab_header(
    title= "Most Efficient CA School Districts") %>% 
  fmt_number(columns = c(2:5),
             decimals = 1)
```

<!--html_preserve-->
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#fqatbwtfpa .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #000000;
  font-size: 16px;
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
}

#fqatbwtfpa .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
}

#fqatbwtfpa .gt_title {
  color: #000000;
  font-size: 125%;
  /* heading.title.font.size */
  padding-top: 4px;
  /* heading.top.padding */
  padding-bottom: 1px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fqatbwtfpa .gt_subtitle {
  color: #000000;
  font-size: 85%;
  /* heading.subtitle.font.size */
  padding-top: 1px;
  padding-bottom: 4px;
  /* heading.bottom.padding */
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fqatbwtfpa .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* heading.border.bottom.color */
}

#fqatbwtfpa .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  padding-top: 4px;
  padding-bottom: 4px;
}

#fqatbwtfpa .gt_col_heading {
  color: #000000;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 16px;
  /* column_labels.font.size */
  font-weight: initial;
  /* column_labels.font.weight */
  vertical-align: middle;
  padding: 10px;
  margin: 10px;
}

#fqatbwtfpa .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#fqatbwtfpa .gt_group_heading {
  padding: 8px;
  color: #000000;
  background-color: #FFFFFF;
  /* stub_group.background.color */
  font-size: 16px;
  /* stub_group.font.size */
  font-weight: initial;
  /* stub_group.font.weight */
  border-top-style: solid;
  /* stub_group.border.top.style */
  border-top-width: 2px;
  /* stub_group.border.top.width */
  border-top-color: #A8A8A8;
  /* stub_group.border.top.color */
  border-bottom-style: solid;
  /* stub_group.border.bottom.style */
  border-bottom-width: 2px;
  /* stub_group.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* stub_group.border.bottom.color */
  vertical-align: middle;
}

#fqatbwtfpa .gt_empty_group_heading {
  padding: 0.5px;
  color: #000000;
  background-color: #FFFFFF;
  /* stub_group.background.color */
  font-size: 16px;
  /* stub_group.font.size */
  font-weight: initial;
  /* stub_group.font.weight */
  border-top-style: solid;
  /* stub_group.border.top.style */
  border-top-width: 2px;
  /* stub_group.border.top.width */
  border-top-color: #A8A8A8;
  /* stub_group.border.top.color */
  border-bottom-style: solid;
  /* stub_group.border.bottom.style */
  border-bottom-width: 2px;
  /* stub_group.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* stub_group.border.bottom.color */
  vertical-align: middle;
}

#fqatbwtfpa .gt_striped {
  background-color: #f2f2f2;
}

#fqatbwtfpa .gt_row {
  padding: 10px;
  /* row.padding */
  margin: 10px;
  vertical-align: middle;
}

#fqatbwtfpa .gt_stub {
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #A8A8A8;
  padding-left: 12px;
}

#fqatbwtfpa .gt_stub.gt_row {
  background-color: #FFFFFF;
}

#fqatbwtfpa .gt_summary_row {
  background-color: #FFFFFF;
  /* summary_row.background.color */
  padding: 6px;
  /* summary_row.padding */
  text-transform: inherit;
  /* summary_row.text_transform */
}

#fqatbwtfpa .gt_first_summary_row {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
}

#fqatbwtfpa .gt_table_body {
  border-top-style: solid;
  /* field.border.top.style */
  border-top-width: 2px;
  /* field.border.top.width */
  border-top-color: #A8A8A8;
  /* field.border.top.color */
  border-bottom-style: solid;
  /* field.border.bottom.style */
  border-bottom-width: 2px;
  /* field.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* field.border.bottom.color */
}

#fqatbwtfpa .gt_footnote {
  font-size: 90%;
  /* footnote.font.size */
  padding: 4px;
  /* footnote.padding */
}

#fqatbwtfpa .gt_sourcenote {
  font-size: 90%;
  /* sourcenote.font.size */
  padding: 4px;
  /* sourcenote.padding */
}

#fqatbwtfpa .gt_center {
  text-align: center;
}

#fqatbwtfpa .gt_left {
  text-align: left;
}

#fqatbwtfpa .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fqatbwtfpa .gt_font_normal {
  font-weight: normal;
}

#fqatbwtfpa .gt_font_bold {
  font-weight: bold;
}

#fqatbwtfpa .gt_font_italic {
  font-style: italic;
}

#fqatbwtfpa .gt_super {
  font-size: 65%;
}

#fqatbwtfpa .gt_footnote_glyph {
  font-style: italic;
  font-size: 65%;
}
</style>
<!--gt table start-->
<table class="gt_table">
<thead>
<tr>
<th colspan="5" class="gt_heading gt_title gt_font_normal gt_center">
Most Efficient CA School Districts
</th>
</tr>
<tr>
<th colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_center gt_bottom_border">
</th>
</tr>
</thead>
<tr>
<th class="gt_col_heading gt_left" rowspan="1" colspan="1">
District
</th>
<th class="gt_col_heading gt_right" rowspan="1" colspan="1">
Spend Percentile Rank
</th>
<th class="gt_col_heading gt_right" rowspan="1" colspan="1">
Composite Percentile Rank
</th>
<th class="gt_col_heading gt_right" rowspan="1" colspan="1">
Participation Percentile Rank
</th>
<th class="gt_col_heading gt_right" rowspan="1" colspan="1">
Efficiency Index
</th>
</tr>
<tbody class="gt_table_body">
<tr>
<td class="gt_row gt_left">
Mendocino Unified
</td>
<td class="gt_row gt_right">
89.0
</td>
<td class="gt_row gt_right">
91.0
</td>
<td class="gt_row gt_right">
85.0
</td>
<td class="gt_row gt_right">
88.7
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_striped">
Piedmont City Unified
</td>
<td class="gt_row gt_right gt_striped">
61.0
</td>
<td class="gt_row gt_right gt_striped">
100.0
</td>
<td class="gt_row gt_right gt_striped">
96.0
</td>
<td class="gt_row gt_right gt_striped">
82.8
</td>
</tr>
<tr>
<td class="gt_row gt_left">
San Marino Unified
</td>
<td class="gt_row gt_right">
59.0
</td>
<td class="gt_row gt_right">
99.0
</td>
<td class="gt_row gt_right">
99.0
</td>
<td class="gt_row gt_right">
82.3
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_striped">
Carmel Unified
</td>
<td class="gt_row gt_right gt_striped">
65.0
</td>
<td class="gt_row gt_right gt_striped">
93.0
</td>
<td class="gt_row gt_right gt_striped">
97.0
</td>
<td class="gt_row gt_right gt_striped">
82.3
</td>
</tr>
<tr>
<td class="gt_row gt_left">
Pacific Grove Unified
</td>
<td class="gt_row gt_right">
70.0
</td>
<td class="gt_row gt_right">
90.0
</td>
<td class="gt_row gt_right">
89.0
</td>
<td class="gt_row gt_right">
81.4
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_striped">
La Canada Unified
</td>
<td class="gt_row gt_right gt_striped">
51.0
</td>
<td class="gt_row gt_right gt_striped">
99.0
</td>
<td class="gt_row gt_right gt_striped">
100.0
</td>
<td class="gt_row gt_right gt_striped">
79.2
</td>
</tr>
<tr>
<td class="gt_row gt_left">
Coronado Unified
</td>
<td class="gt_row gt_right">
59.0
</td>
<td class="gt_row gt_right">
89.0
</td>
<td class="gt_row gt_right">
99.0
</td>
<td class="gt_row gt_right">
79.0
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_striped">
Albany City Unified
</td>
<td class="gt_row gt_right gt_striped">
54.0
</td>
<td class="gt_row gt_right gt_striped">
96.0
</td>
<td class="gt_row gt_right gt_striped">
91.0
</td>
<td class="gt_row gt_right gt_striped">
77.2
</td>
</tr>
<tr>
<td class="gt_row gt_left">
Saint Helena Unified
</td>
<td class="gt_row gt_right">
87.0
</td>
<td class="gt_row gt_right">
69.0
</td>
<td class="gt_row gt_right">
70.0
</td>
<td class="gt_row gt_right">
76.8
</td>
</tr>
<tr>
<td class="gt_row gt_left gt_striped">
Beverly Hills Unified
</td>
<td class="gt_row gt_right gt_striped">
53.0
</td>
<td class="gt_row gt_right gt_striped">
95.0
</td>
<td class="gt_row gt_right gt_striped">
91.0
</td>
<td class="gt_row gt_right gt_striped">
76.5
</td>
</tr>
</tbody>
</table>
<!--gt table end-->

<!--/html_preserve-->
``` r
# writing files out

write_rds(expense, "CA_EDUC_Data/expense.rds")
write_rds(comp, "CA_EDUC_Data/comp.rds")
write_rds(most_eff, "CA_EDUC_Data/efficiency.rds")
write_rds(top3, "CA_EDUC_Data/top3.rds")
write_rds(top10, "CA_EDUC_Data/top10.rds")
write_rds(eff_tsal, "CA_EDUC_Data/eff_tsal.rds")
write_rds(eff_ratio, "CA_EDUC_Data/eff_ratio.rds")
write_rds(eff_adv, "CA_EDUC_Data/eff_adv.rds")
write_rds(eff_frpm, "CA_EDUC_Data/eff_frpm.rds")
write_rds(eff_addtl, "CA_EDUC_Data/eff_addtl.rds")
write_rds(select_eff, "CA_EDUC_DATA/select_eff.rds")
```
