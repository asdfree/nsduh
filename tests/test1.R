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
}
