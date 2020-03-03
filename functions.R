
GetData <- function() {
  
  # Import historical prison population data
  Prison.Data <- RSocrata::read.socrata("https://data.ct.gov/resource/4tr4-7mju.csv")
  
  # Now that we have the prison data, let's format it into a usable dataset for analysis
  Prison.Data <- Prison.Data %>% 
    dplyr::mutate_at(vars(date), as.Date) %>% 
    dplyr::filter(date >= as.Date("2010-01-01", "%Y-%m-%d")) %>% 
    dplyr::filter(date <= as.Date("2019-12-31", "%Y-%m-%d")) %>% 
    dplyr::filter(lubridate::day(date) == 1) %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarise(total.state.prison.pop = sum(total_facility_population_count))
  
  ##                    ##
  ## PAUSE TO VISUALIZE ##
  ##                    ##
  
  # Import historical employment data
  Employment.Data <- RSocrata::read.socrata("https://data.ct.gov/resource/8zbs-9atu.csv")
  
  # Now that we have the employment data, let's format it into a usable dataset for analysis
  Employment.Data <- Employment.Data %>% 
    dplyr::filter(industry_title == "Total Nonfarm", 
                  area == 0) %>% 
    dplyr::select(jan,
                  feb,
                  mar,
                  apr,
                  may, 
                  jun, 
                  jul, 
                  aug,
                  sep,
                  oct,
                  nov,
                  dec,
                  year) %>% 
    reshape2::melt(id.vars = "year", 
                   measure.vars = 1:12) %>% 
    dplyr::rename(month = variable, 
                  total.nonfarm.employed = value) %>%
    dplyr::mutate(date = as.Date(paste0(month, "-1-", year), "%b-%d-%Y")) %>%
    dplyr::select(date, total.nonfarm.employed) %>% 
    dplyr::filter(date >= as.Date("2010-01-01", "%Y-%m-%d")) %>%
    dplyr::arrange(date)
  
  # Merge the two datasets
  Merged.Data <- Prison.Data %>% 
    dplyr::left_join(Employment.Data, 
                     by = "date")
  
  return(Merged.Data)
  
}

