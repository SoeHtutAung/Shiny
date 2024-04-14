README
================

## Sample dashboard by Soe

This is the repository of a simple dashboard exploring Humanitarian
crisis in Myanmar, which was developed by R programming. The necessary
data are extracted from latest available sources by web scrapping with
R, and they were saved as CSV files in ‘data folder’ after manipulating.
In this app, Shiny app will use those saved CSV files to run the
dashboard as web scrapping will not be done as part of the app. So that,
we can avoid unnecessary bugs due to missing website links from ongiong
updates by organizations. The dashboard can be accessible at
<https://www.shinyapps.io/>.

(**Note**: *Being a free cloud server account, the provided link may
expire after limited time. This is my very first interactive dashboard
with primary focus on programming with R Shiny, rather than
visualization techniques. I used simple instructions as learned from
[Shiny](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html)
and Youtube.*)

## 1. Data sources

Most recent information are extracted by web-scrapping with R:

| Source | Description                                             | URL                                                                                                                                                                                                                    | Extracted CSV file      |
|--------|---------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------|
| UNDP   | Nation-wide survey in 2023 on household income          | <https://www.undp.org/sites/g/files/zskgke326/files/2024-04/undp_poverty_and_the_household_economy_of_myanmar_-_a_disappearing_middle_class_april_2024.pdf>                                                            | undp_data.csv           |
| UNHCR  | Updated information about IDPs                          | <https://data.unhcr.org/en/country/mmr>                                                                                                                                                                                | idp_data.csv            |
| UNFPA  | Migration data from 2019 inter-census                   | <https://www.dop.gov.mm/sites/dop.gov.mm/files/datamap-documents/ics_rp_migration_en.xlsx>                                                                                                                             | idp_data.csv (appended) |
| HARP   | Vulnerability assessment in 2019                        | <https://themimu.info/sites/themimu.info/files/documents/Datasets_Vulnerability_Analysis_in_Myanmar_09Jul2018.xlsx>                                                                                                    | harp_data.csv           |
| ACLED  | Monitored data on civilian Fatalities from 2014 to date | <https://data.humdata.org/dataset/1dbe32d3-4432-4ff4-9192-dc310663428b/resource/ede52625-30f4-4e92-ab56-4a31281e8a4e/download/myanmar_hrp_civilian_targeting_events_and_fatalities_by_month-year_as-of-12apr2024.xlsx> | acled_civ.csv           |

Data sources and extracted CSV files

## 2. Dashboard overview

There are three pages in the dashboard for different administrative
levels to display relevant information. A menu on the right side is
links to my LinkedIn and GitHub profiles.

![](images/Screenshot%202024-04-14%20125414.png)

### 2.1 Regional situation

Using data from inter-census results of UNFPA, IDP information from
UNHCR and recent income assessment from UNDP, we organized this page. In
this page, you can customize regions, total and IDP population size to
view following graphs:

1.  A scatter plot displaying IDP population size and changes in poverty
    rate from 2017 to 2024

2.  A choreograph map displaying changes in poverty rate

3.  A bar plot comparing total and IDP population size

4.  A summary table and two boxes with overall regional situation

<img src="images/Screenshot%202024-04-14%20125809-01.png" width="557" />

### 2.2 District situation

[ACLED](https://acleddata.com/) regularly produces data to monitor
conflicts, and its coverage for Myanmar spans from 2014 to the present
day. You can select the period of interest to see the trend and
highlighted areas of civil fatalities due to conflicts in Myanmar.

(**Tips**: *Fatalities beyond 2021 are mostly due to armed conflict
after coup d’état*)

<img src="images/Screenshot%202024-04-14%20130618-01.png" width="556" />

### 2.3 Township situation

A comprehensive assessment conducted by
[HARP](https://www.crownagents.com/project/humanitarian-assistance-and-resilience-programme-facility-harp-f/)
in 2019 made vulnerability scores available at township level. You can
customize this full page map by selecting region of interest and
adjusting parameters, such as ‘Vulnerability band’ and ‘Vulnerable
population’.

<img src="images/Screenshot%202024-04-14%20131547.png" width="554" />
