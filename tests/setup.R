if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

nsduh_cat <-
	get_catalog( "nsduh" ,
		output_dir = file.path( getwd() ) )

nsduh_cat <- nsduh_cat[ split( seq( nrow( nsduh_cat ) ) , 1 + sort( seq( nrow( nsduh_cat ) ) %% 5 ) )[[ this_sample_break ]] , ]

nsduh_cat <- lodown( "nsduh" , nsduh_cat )
if( any( nsduh_cat$year == 2016 ) ){











library(survey)

nsduh_df <- 
	readRDS( file.path( getwd() , "2016 main.rds" ) )

variables_to_keep <-
	c( 'verep' , 'vestr' , 'analwt_c' , 'health' , 'cigtry' , 'cocage' ,
		'mjever' , 'coutyp4' , 'preg' )
	
nsduh_df <- nsduh_df[ variables_to_keep ] ; gc()
	
nsduh_design <- 
	svydesign( 
		id = ~ verep , 
		strata = ~ vestr , 
		data = nsduh_df , 
		weights = ~ analwt_c , 
		nest = TRUE 
	)
nsduh_design <- 
	update( 
		nsduh_design , 
		
		one = 1 ,
		
		health = 
			factor( 
				health , 
				levels = 1:5 , 
				labels = c( "excellent" , "very good" , "good" ,
					"fair" , "poor" )
			) ,
			
		age_tried_first_cigarette = ifelse( cigtry > 99 , NA , cigtry ) ,
		
		age_tried_cocaine = ifelse( cocage > 99 , NA , cocage ) ,

		ever_used_marijuana = as.numeric( mjever == 1 ) ,
		
		county_type =
			factor(
				coutyp4 ,
				levels = 1:3 ,
				labels = c( "large metro" , "small metro" , "nonmetro" )
			)
			
	)
sum( weights( nsduh_design , "sampling" ) != 0 )

svyby( ~ one , ~ county_type , nsduh_design , unwtd.count )
svytotal( ~ one , nsduh_design )

svyby( ~ one , ~ county_type , nsduh_design , svytotal )
svymean( ~ age_tried_first_cigarette , nsduh_design , na.rm = TRUE )

svyby( ~ age_tried_first_cigarette , ~ county_type , nsduh_design , svymean , na.rm = TRUE )
svymean( ~ health , nsduh_design , na.rm = TRUE )

svyby( ~ health , ~ county_type , nsduh_design , svymean , na.rm = TRUE )
svytotal( ~ age_tried_first_cigarette , nsduh_design , na.rm = TRUE )

svyby( ~ age_tried_first_cigarette , ~ county_type , nsduh_design , svytotal , na.rm = TRUE )
svytotal( ~ health , nsduh_design , na.rm = TRUE )

svyby( ~ health , ~ county_type , nsduh_design , svytotal , na.rm = TRUE )
svyquantile( ~ age_tried_first_cigarette , nsduh_design , 0.5 , na.rm = TRUE )

svyby( 
	~ age_tried_first_cigarette , 
	~ county_type , 
	nsduh_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ age_tried_first_cigarette , 
	denominator = ~ age_tried_cocaine , 
	nsduh_design ,
	na.rm = TRUE
)
sub_nsduh_design <- subset( nsduh_design , preg == 1 )
svymean( ~ age_tried_first_cigarette , sub_nsduh_design , na.rm = TRUE )
this_result <- svymean( ~ age_tried_first_cigarette , nsduh_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ age_tried_first_cigarette , 
		~ county_type , 
		nsduh_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nsduh_design )
svyvar( ~ age_tried_first_cigarette , nsduh_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ age_tried_first_cigarette , nsduh_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ age_tried_first_cigarette , nsduh_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ ever_used_marijuana , nsduh_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( age_tried_first_cigarette ~ ever_used_marijuana , nsduh_design )
svychisq( 
	~ ever_used_marijuana + health , 
	nsduh_design 
)
glm_result <- 
	svyglm( 
		age_tried_first_cigarette ~ ever_used_marijuana + health , 
		nsduh_design 
	)

summary( glm_result )
library(srvyr)
nsduh_srvyr_design <- as_survey( nsduh_design )
nsduh_srvyr_design %>%
	summarize( mean = survey_mean( age_tried_first_cigarette , na.rm = TRUE ) )

nsduh_srvyr_design %>%
	group_by( county_type ) %>%
	summarize( mean = survey_mean( age_tried_first_cigarette , na.rm = TRUE ) )

}
