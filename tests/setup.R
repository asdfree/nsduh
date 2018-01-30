if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

nsduh_cat <-
	get_catalog( "nsduh" ,
		output_dir = file.path( getwd() ) )

nsduh_cat <- nsduh_cat[ split( seq( nrow( nsduh_cat ) ) , 1 + sort( seq( nrow( nsduh_cat ) ) %% 5 ) )[[ this_sample_break ]] , ]

lodown( "nsduh" , nsduh_cat )
print('lodown( "nsduh" , nsduh_cat ) finished')
