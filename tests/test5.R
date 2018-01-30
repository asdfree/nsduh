if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

nsduh_cat <-
	get_catalog( "nsduh" ,
		output_dir = file.path( getwd() ) )

nsduh_cat <- nsduh_cat[ split( seq( nrow( nsduh_cat ) ) , 1 + sort( seq( nrow( nsduh_cat ) ) %% 5 ) )[[ this_sample_break ]] , ]


if( any( nsduh_cat$year == 2016 ) ){
library(lodown)
# examine all available NSDUH microdata files
nsduh_cat <-
	get_catalog( "nsduh" ,
		output_dir = file.path( getwd() ) )
print('get_catalog two finished')

# 2016 only
nsduh_cat <- subset( nsduh_cat , year == 2016 )
# download the microdata to your local computer


library(survey)

nsduh_df <- 
	readRDS( file.path( getwd() , "2016 main.rds" ) )
print('readRDS finished')
nsduh_design <- 
	svydesign( 
		id = ~ verep , 
		strata = ~ vestr , 
		data = nsduh_df , 
		weights = ~ analwt_c , 
		nest = TRUE 
	)
print('svydesign finished')
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

}
