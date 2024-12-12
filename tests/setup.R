# drinking and thinking
# about your first time, were you
# smoking and joking?
zip_tf <- tempfile()

zip_url <-
	"https://www.samhsa.gov/data/system/files/media-puf-file/NSDUH-2023-DS0001-bndl-data-r_v1.zip"
	
download.file( zip_url , zip_tf , mode = 'wb' )

nsduh_rdata <- unzip( zip_tf , exdir = tempdir() )

nsduh_rdata_contents <- load( nsduh_rdata )

nsduh_df_name <- grep( 'PUF' , nsduh_rdata_contents , value = TRUE , ignore.case = TRUE )

nsduh_df <- get( nsduh_df_name )

names( nsduh_df ) <- tolower( names( nsduh_df ) )

nsduh_df[ , 'one' ] <- 1
# nsduh_fn <- file.path( path.expand( "~" ) , "NSDUH" , "this_file.rds" )
# saveRDS( nsduh_df , file = nsduh_fn , compress = FALSE )
# nsduh_df <- readRDS( nsduh_fn )
library(survey)

nsduh_design <- 
	svydesign( 
		id = ~ verep , 
		strata = ~ vestr_c , 
		data = nsduh_df , 
		weights = ~ analwt2_c , 
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
			
		age_first_cigarette = ifelse( cigtry > 99 , NA , cigtry ) ,
		
		age_tried_cocaine = ifelse( cocage > 99 , NA , cocage ) ,

		ever_used_marijuana = as.numeric( ifelse( mjever < 4 , mjever == 1 , NA ) ) ,
		
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
svymean( ~ age_first_cigarette , nsduh_design , na.rm = TRUE )

svyby( ~ age_first_cigarette , ~ county_type , nsduh_design , svymean , na.rm = TRUE )
svymean( ~ health , nsduh_design , na.rm = TRUE )

svyby( ~ health , ~ county_type , nsduh_design , svymean , na.rm = TRUE )
svytotal( ~ age_first_cigarette , nsduh_design , na.rm = TRUE )

svyby( ~ age_first_cigarette , ~ county_type , nsduh_design , svytotal , na.rm = TRUE )
svytotal( ~ health , nsduh_design , na.rm = TRUE )

svyby( ~ health , ~ county_type , nsduh_design , svytotal , na.rm = TRUE )
svyquantile( ~ age_first_cigarette , nsduh_design , 0.5 , na.rm = TRUE )

svyby( 
	~ age_first_cigarette , 
	~ county_type , 
	nsduh_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ age_first_cigarette , 
	denominator = ~ age_tried_cocaine , 
	nsduh_design ,
	na.rm = TRUE
)
sub_nsduh_design <- subset( nsduh_design , preg == 1 )
svymean( ~ age_first_cigarette , sub_nsduh_design , na.rm = TRUE )
this_result <- svymean( ~ age_first_cigarette , nsduh_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ age_first_cigarette , 
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
svyvar( ~ age_first_cigarette , nsduh_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ age_first_cigarette , nsduh_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ age_first_cigarette , nsduh_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ ever_used_marijuana , nsduh_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( age_first_cigarette ~ ever_used_marijuana , nsduh_design )
svychisq( 
	~ ever_used_marijuana + health , 
	nsduh_design 
)
glm_result <- 
	svyglm( 
		age_first_cigarette ~ ever_used_marijuana + health , 
		nsduh_design 
	)

summary( glm_result )
result <- svymean( ~ alcmon , nsduh_design )

stopifnot( round( coef( result ) , 3 ) == 0.477 )
stopifnot( round( SE( result ) , 4 ) == 0.0049 )
library(srvyr)
nsduh_srvyr_design <- as_survey( nsduh_design )
nsduh_srvyr_design %>%
	summarize( mean = survey_mean( age_first_cigarette , na.rm = TRUE ) )

nsduh_srvyr_design %>%
	group_by( county_type ) %>%
	summarize( mean = survey_mean( age_first_cigarette , na.rm = TRUE ) )
