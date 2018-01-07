if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
lodown( "nsch" , output_dir = file.path( getwd() ) )
library(lodown)
# examine all available NSCH microdata files
nsch_cat <-
	get_catalog( "nsch" ,
		output_dir = file.path( getwd() ) )

# 2012 only
nsch_cat <- subset( nsch_cat , year == 2012 )
# download the microdata to your local computer
lodown( "nsch" , nsch_cat )

library(survey)
library(mitools)

nsch_imp <- readRDS( file.path( getwd() , "2012 main.rds" ) )

nsch_design <- 
	svydesign( 
		id = ~ 1 , 
		strata = ~ state + sample , 
		weights = ~ nschwt , 
		data = imputationList( nsch_imp )
	)
nsch_design <-
	update(
		nsch_design ,
		
		indicator_1_3 = ifelse( k6q40 > 1 , NA , k6q40 ) ,

		indicator_5_2 =
			ifelse( k7q05r %in% 1:5 , 1 ,
			ifelse( k7q05r %in% 0 , 0 , NA ) ) ,
			
		indicator_5_3 =
			ifelse( k7q30 == 1 | k7q31 == 1 | k7q32 == 1 , 1 ,
			ifelse( k7q30 == 0 | k7q31 == 0 | k7q32 == 0 , 0 , NA ) ) ,
			
		povcat = 
			factor( 
				findInterval( povlevel_i , c( 1 , 2 , 6 , 8 ) ) ,
				labels = 
					c( "below poverty" , "100-199% fpl" , "200-399% fpl" , "400%+ fpl" )
			) ,
		
		sex = factor( ifelse( sex %in% 1:2 , sex , NA ) , labels = c( "male" , "female" ) )
		
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
MIcombine( with( nsch_design , svymean( ~ povcat ) ) )

MIcombine( with( nsch_design ,
	svyby( ~ povcat , ~ state , svymean )
) )
MIcombine( with( nsch_design , svytotal( ~ ageyr_child ) ) )

MIcombine( with( nsch_design ,
	svyby( ~ ageyr_child , ~ state , svytotal )
) )
MIcombine( with( nsch_design , svytotal( ~ povcat ) ) )

MIcombine( with( nsch_design ,
	svyby( ~ povcat , ~ state , svytotal )
) )
MIcombine( with( nsch_design , svyquantile( ~ ageyr_child , 0.5 , se = TRUE ) ) )

MIcombine( with( nsch_design ,
	svyby( 
		~ ageyr_child , ~ state , svyquantile , 0.5 ,
		se = TRUE , keep.var = TRUE , ci = TRUE 
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
lodown:::MIsvyciprop( ~ indicator_5_2 , nsch_design ,
	method = "likelihood" )
lodown:::MIsvyttest( ageyr_child ~ indicator_5_2 , nsch_design )
lodown:::MIsvychisq( ~ indicator_5_2 + povcat , nsch_design )
glm_result <- 
	MIcombine( with( nsch_design ,
		svyglm( ageyr_child ~ indicator_5_2 + povcat )
	) )
	
summary( glm_result )

