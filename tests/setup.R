# 
# 
# 
library(haven)

nsch_stata_import <-
	function( this_url ){
		
		this_tf <- tempfile()
		
		download.file( this_url , this_tf , mode = 'wb' )
		
		unzipped_files <- unzip( this_tf , exdir = tempdir() )
		
		this_stata <- grep( '\\.dta$' , unzipped_files , value = TRUE )
		
		this_tbl <- read_stata( this_stata )
		
		this_df <- data.frame( this_tbl )
		
		file.remove( c( this_tf , unzipped_files ) )
		
		names( this_df ) <- tolower( names( this_df ) )
		
		this_df
	}

nsch_screener_url <-
	"https://www2.census.gov/programs-surveys/nsch/datasets/2021/nsch_2021_screener_Stata.zip"

nsch_topical_url <-
	"https://www2.census.gov/programs-surveys/nsch/datasets/2021/nsch_2021_topical_Stata.zip" 

nsch_screener_df <- nsch_stata_import( nsch_screener_url )

nsch_df <- nsch_stata_import( nsch_topical_url )
# nsch_fn <- file.path( path.expand( "~" ) , "NSCH" , "this_file.rds" )
# saveRDS( nsch_df , file = nsch_fn , compress = FALSE )
# nsch_df <- readRDS( nsch_fn )
fpl_columns <- grep( '^fpl_i[0-9]' , names( nsch_df ) , value = TRUE )

fpl_wide_df <- nsch_df[ c( 'hhid' , fpl_columns ) ]

nsch_df[ fpl_columns ] <- NULL
fpl_long_df <- 
	reshape( 
		fpl_wide_df , 
		varying = list( fpl_columns ) , 
		direction = 'long' , 
		timevar = 'implicate' , 
		idvar = 'hhid' 
	)
	
names( fpl_long_df )[ ncol( fpl_long_df ) ] <- 'fpl'
nsch_long_df <- merge( nsch_df , fpl_long_df )

stopifnot( nrow( nsch_long_df ) == nrow( fpl_long_df ) )

stopifnot( nrow( nsch_long_df ) / length( fpl_columns ) == nrow( nsch_df ) )
nsch_list <- split( nsch_long_df , nsch_long_df[ , 'implicate' ] )
library(survey)
library(mitools)

nsch_design <- 
	svydesign( 
		id = ~ 1 , 
		strata = ~ fipsst + stratum , 
		weights = ~ fwc , 
		data = imputationList( nsch_list ) ,
		nest = TRUE
	)
nsch_design <-
	update(
		nsch_design ,
		
		one = 1 ,
		
		state_name =
			factor(
				fipsst ,
				levels = 
					c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 
					11L, 12L, 13L, 15L, 16L, 17L, 18L, 
					19L, 20L, 21L, 22L, 23L, 24L, 25L, 
					26L, 27L, 28L, 29L, 30L, 31L, 32L, 
					33L, 34L, 35L, 36L, 37L, 38L, 39L, 
					40L, 41L, 42L, 44L, 45L, 46L, 47L, 
					48L, 49L, 50L, 51L, 53L, 54L, 55L, 
					56L, 72L) ,
				labels =
					c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
					"Colorado", "Connecticut", "Delaware", "District of Columbia", 
					"Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
					"Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
					"Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
					"Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
					"New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
					"Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
					"South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
					"Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")
			) ,

		
		overall_health =
			factor( 
				c( 1 , 1 , 2 , 3 , 3 )[ k2q01 ] , 
				levels = 1:3 , 
				labels = c( 'excellent or very good' , 'good' , 'fair or poor' ) 
			) ,
		
		indicator_1_3 = ifelse( k6q40 > 1 , NA , k6q40 ) ,

# 		indicator_5_2 =
# 			ifelse( k7q05r %in% 1:5 , 1 ,
# 			ifelse( k7q05r %in% 0 , 0 , NA ) ) ,
			
		indicator_5_3 =
			ifelse( k7q30 == 1 | k7q31 == 1 | k7q32 == 1 , 1 ,
			ifelse( k7q30 == 0 | k7q31 == 0 | k7q32 == 0 , 0 , NA ) ) ,
			
		poverty_categories = 
			factor( 
				1 + findInterval( fpl , c( 100 , 200 , 400 ) ) ,
				labels = 
					c( "below poverty" , "100-199% fpl" , "200-399% fpl" , "400%+ fpl" )
			) ,
		
		sc_sex =
			factor( ifelse( sc_sex %in% 1:2 , sc_sex , NA ) , labels = c( "male" , "female" ) )
		
	)
MIcombine( with( nsch_design , svyby( ~ one , ~ one , unwtd.count ) ) )

MIcombine( with( nsch_design , svyby( ~ one , ~ state , unwtd.count ) ) )
MIcombine( with( nsch_design , svytotal( ~ one ) ) )

MIcombine( with( nsch_design ,
	svyby( ~ one , ~ state , svytotal )
) )
MIcombine( with( nsch_design , svymean( ~ ageyr_child ) ) )

MIcombine( with( nsch_design ,
	svyby( ~ ageyr_child , ~ state , svymean )
) )
MIcombine( with( nsch_design , svymean( ~ poverty_categories ) ) )

MIcombine( with( nsch_design ,
	svyby( ~ poverty_categories , ~ state , svymean )
) )
MIcombine( with( nsch_design , svytotal( ~ ageyr_child ) ) )

MIcombine( with( nsch_design ,
	svyby( ~ ageyr_child , ~ state , svytotal )
) )
MIcombine( with( nsch_design , svytotal( ~ poverty_categories ) ) )

MIcombine( with( nsch_design ,
	svyby( ~ poverty_categories , ~ state , svytotal )
) )
MIcombine( with( nsch_design ,
	svyquantile(
		~ ageyr_child ,
		0.5 , se = TRUE 
) ) )

MIcombine( with( nsch_design ,
	svyby(
		~ ageyr_child , ~ state , svyquantile ,
		0.5 , se = TRUE ,
		ci = TRUE 
) ) )
MIcombine( with( nsch_design ,
	svyratio( numerator = ~ k6q63 , denominator = ~ totkids4 )
) )
sub_nsch_design <- subset( nsch_design , agepos4 == 1 )
MIcombine( with( sub_nsch_design , svymean( ~ ageyr_child ) ) )
this_result <-
	MIcombine( with( nsch_design ,
		svymean( ~ ageyr_child )
	) )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	MIcombine( with( nsch_design ,
		svyby( ~ ageyr_child , ~ state , svymean )
	) )

coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nsch_design$designs[[1]] )
MIcombine( with( nsch_design , svyvar( ~ ageyr_child ) ) )
# SRS without replacement
MIcombine( with( nsch_design ,
	svymean( ~ ageyr_child , deff = TRUE )
) )

# SRS with replacement
MIcombine( with( nsch_design ,
	svymean( ~ ageyr_child , deff = "replace" )
) )
# MIsvyciprop( ~ indicator_5_2 , nsch_design ,
# 	method = "likelihood" )
# MIsvyttest( ageyr_child ~ indicator_5_2 , nsch_design )
# MIsvychisq( ~ indicator_5_2 + poverty_categories , nsch_design )
glm_result <- 
	MIcombine( with( nsch_design ,
		svyglm( ageyr_child ~ indicator_5_2 + poverty_categories )
	) )
	
summary( glm_result )
results <-
	svyby( 
		~ as.numeric( overall_health == 'excellent or very good' ) ,
		~ poverty_categories , 
		nsch_design$designs[[1]] , 
		svymean , 
		na.rm = TRUE 
	)

published_proportions <- c( 0.833 , 0.859 , 0.907 , 0.955 )

published_lb <- c( 0.810 , 0.838 , 0.894 , 0.949 )

published_ub <- c( 0.854 , 0.878 , 0.919 , 0.961 )

stopifnot( all( round( coef( results ) , 3 ) == published_proportions ) )

( ci_results <- confint( results ) )

stopifnot( all( abs( ci_results[ , 1 ] - published_lb ) < 0.0015 ) )

stopifnot( all( abs( ci_results[ , 2 ] - published_ub ) < 0.0015 ) )
