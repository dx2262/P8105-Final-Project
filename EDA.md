EDA
================
Nicole Criscuolo
2025-11-19

``` r
parks_df =
  read.csv("data/Parks_Properties_20251104.csv") |> 
  janitor::clean_names() |> 
  select(acquisitiondate, acres, communityboard, eapply, gispropnum, location, pip_ratable, subcategory, typecategory, zipcode, multipolygon) |> 
  rename(
    "id" = "communityboard",
    "park_name"= "eapply",
    "unique_id" = "gispropnum",
    "inpsected" = "pip_ratable"
  ) |> 
  mutate(
    acquisitiondate = format(as.Date(acquisitiondate, format = "%Y %b %d %I:%M:%S %p"), "%m/%d/%Y")
  )
```

``` r
demo_df =
  read_excel("data/2022-chp-pud.xlsx", sheet = 4, range = cell_rows(2:67)) |> 
  janitor::clean_names() |> 
  filter(row_number() > 6) |> 
  select(-starts_with(c("nyc_comparison_", "lower_95cl_", "upper_95cl_"))) |> 
  select(id:rent_burden, air_pollution, child_obesity:unmet_med_care_reliability_note,
         obesity:binge_drink_reliability_note, premature_mort_number:life_expectancy) |> 
  rename("neighbourhood" = "name")|> 
  mutate(id = as.character(id))
```

    ## New names:
    ## • `lower_95CL` -> `lower_95CL...16`
    ## • `upper_95CL` -> `upper_95CL...17`
    ## • `NYC_Comparison` -> `NYC_Comparison...18`
    ## • `lower_95CL` -> `lower_95CL...20`
    ## • `upper_95CL` -> `upper_95CL...21`
    ## • `NYC_Comparison` -> `NYC_Comparison...22`
    ## • `NYC_comparison` -> `NYC_comparison...24`
    ## • `NYC_Comparison` -> `NYC_Comparison...26`
    ## • `lower_95CL` -> `lower_95CL...28`
    ## • `upper_95CL` -> `upper_95CL...29`
    ## • `NYC_Comparison` -> `NYC_Comparison...30`
    ## • `lower_95CL` -> `lower_95CL...32`
    ## • `upper_95CL` -> `upper_95CL...33`
    ## • `NYC_Comparison` -> `NYC_Comparison...34`
    ## • `lower_95CL` -> `lower_95CL...36`
    ## • `upper_95CL` -> `upper_95CL...37`
    ## • `NYC_Comparison` -> `NYC_Comparison...38`
    ## • `lower_95CL` -> `lower_95CL...41`
    ## • `upper_95CL` -> `upper_95CL...42`
    ## • `NYC_Comparison` -> `NYC_Comparison...43`
    ## • `lower_95CL` -> `lower_95CL...45`
    ## • `upper_95CL` -> `upper_95CL...46`
    ## • `NYC_Comparison` -> `NYC_Comparison...47`
    ## • `lower_95CL` -> `lower_95CL...50`
    ## • `upper_95CL` -> `upper_95CL...51`
    ## • `NYC_Comparison` -> `NYC_Comparison...52`
    ## • `lower_95CL` -> `lower_95CL...55`
    ## • `upper_95CL` -> `upper_95CL...56`
    ## • `NYC_Comparison` -> `NYC_Comparison...57`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...58`
    ## • `lower_95CL` -> `lower_95CL...63`
    ## • `upper_95CL` -> `upper_95CL...64`
    ## • `NYC_Comparison` -> `NYC_Comparison...65`
    ## • `lower_95CL` -> `lower_95CL...68`
    ## • `upper_95CL` -> `upper_95CL...69`
    ## • `NYC_Comparison` -> `NYC_Comparison...70`
    ## • `NYC_comparison` -> `NYC_comparison...73`
    ## • `lower_95CL` -> `lower_95CL...75`
    ## • `upper_95CL` -> `upper_95CL...76`
    ## • `NYC_Comparison` -> `NYC_Comparison...77`
    ## • `NYC_Comparison` -> `NYC_Comparison...81`
    ## • `NYC_Comparison` -> `NYC_Comparison...83`
    ## • `NYC_Comparison` -> `NYC_Comparison...86`
    ## • `lower_95CL` -> `lower_95CL...88`
    ## • `upper_95CL` -> `upper_95CL...89`
    ## • `NYC_Comparison` -> `NYC_Comparison...90`
    ## • `lower_95CL` -> `lower_95CL...93`
    ## • `upper_95CL` -> `upper_95CL...94`
    ## • `NYC_Comparison` -> `NYC_Comparison...95`
    ## • `lower_95CL` -> `lower_95CL...98`
    ## • `upper_95CL` -> `upper_95CL...99`
    ## • `NYC_Comparison` -> `NYC_Comparison...100`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...101`
    ## • `lower_95CL` -> `lower_95CL...104`
    ## • `upper_95CL` -> `upper_95CL...105`
    ## • `NYC_Comparison` -> `NYC_Comparison...106`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...107`
    ## • `lower_95CL` -> `lower_95CL...110`
    ## • `upper_95CL` -> `upper_95CL...111`
    ## • `NYC_Comparison` -> `NYC_Comparison...112`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...113`
    ## • `lower_95CL` -> `lower_95CL...116`
    ## • `upper_95CL` -> `upper_95CL...117`
    ## • `NYC_Comparison` -> `NYC_Comparison...118`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...119`
    ## • `lower_95CL` -> `lower_95CL...122`
    ## • `upper_95CL` -> `upper_95CL...123`
    ## • `NYC_Comparison` -> `NYC_Comparison...124`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...125`
    ## • `lower_95CL` -> `lower_95CL...128`
    ## • `upper_95CL` -> `upper_95CL...129`
    ## • `NYC_Comparison` -> `NYC_Comparison...130`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...131`
    ## • `lower_95CL` -> `lower_95CL...134`
    ## • `upper_95CL` -> `upper_95CL...135`
    ## • `NYC_Comparison` -> `NYC_Comparison...136`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...137`
    ## • `lower_95CL` -> `lower_95CL...139`
    ## • `upper_95CL` -> `upper_95CL...140`
    ## • `NYC_Comparison` -> `NYC_Comparison...141`
    ## • `lower_95CL` -> `lower_95CL...143`
    ## • `upper_95CL` -> `upper_95CL...144`
    ## • `NYC_Comparison` -> `NYC_Comparison...145`
    ## • `lower_95CL` -> `lower_95CL...150`
    ## • `upper_95CL` -> `upper_95CL...151`
    ## • `NYC_Comparison` -> `NYC_Comparison...152`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...153`
    ## • `lower_95CL` -> `lower_95CL...156`
    ## • `upper_95CL` -> `upper_95CL...157`
    ## • `NYC_Comparison` -> `NYC_Comparison...158`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...159`
    ## • `lower_95CL` -> `lower_95CL...162`
    ## • `upper_95CL` -> `upper_95CL...163`
    ## • `NYC_Comparison` -> `NYC_Comparison...164`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...165`
    ## • `lower_95CL` -> `lower_95CL...168`
    ## • `upper_95CL` -> `upper_95CL...169`
    ## • `NYC_Comparison` -> `NYC_Comparison...170`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...171`
    ## • `lower_95CL` -> `lower_95CL...177`
    ## • `upper_95CL` -> `upper_95CL...178`
    ## • `NYC_Comparison` -> `NYC_Comparison...179`
    ## • `NYC_Comparison_Pvalues` -> `NYC_Comparison_Pvalues...180`
    ## • `lower_95CL` -> `lower_95CL...182`
    ## • `upper_95CL` -> `upper_95CL...183`
    ## • `NYC_comparison` -> `NYC_comparison...184`
    ## • `lower_95CL` -> `lower_95CL...187`
    ## • `upper_95CL` -> `upper_95CL...188`
    ## • `NYC_comparison` -> `NYC_comparison...189`
    ## • `NYC_Comparison` -> `NYC_Comparison...192`
    ## • `lower_95CL` -> `lower_95CL...194`
    ## • `upper_95CL` -> `upper_95CL...195`
    ## • `NYC_Comparison` -> `NYC_Comparison...196`
