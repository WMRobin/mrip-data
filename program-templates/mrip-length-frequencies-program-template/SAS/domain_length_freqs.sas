*****************************************************
MRIP length frequency estimation for custom domains

System requirements:
        Recommended Version - SAS 9.3
        Required Version - SAS 8 or higher
        The following components are required:
                Base SAS
                SAS/STAT

This is a template program for estimating length frequencies
using the MRIP public-use datasets.

The program is setup to use information in the trip_yyyyw
dataset to define custom domains.  The length frequencies are
estimated within the domains by merging the trip information onto
the size_yyyyw datasets.

Required input datasets:
 trip_yyyyw
 size_yyyyw

yyyy = year
w    = wave


TO RUN PROGRAM:
        1. Specify location of MRIP trip_yyyyw and size_yyyyw
           SAS datasets in libname statement.
        2. Select a range of years and waves to include in analysis.
		3. Specify a single species of interest (common name).
        4. Define domain.
		   Domain: a defined sub-level, geographic or otherwise,
			of the stratified estimation design.  Typically, these are
			sub-state geographic divisions based on county groups, or
			even sampled site groups if the domain cannot be defined by
			county boarders.
			For length freqs, may also want to define custom size interval
			bins. Included length bin fields are whole inch and centimeter
			(l_in_bin l_cm_bin) where the value represents the floor or
			start of the interval, e.g. 1=[1,2).

JFoster 8/2012
****************************************************;

*location of MRIP trip_yyyyw and catch_yyyyw sas datasets;
*libname mrip "c:\my_mrip_data";
libname mrip "M:\products\mrip_estim\Public_data";

*Select range of years and wave to include in analysis. Program
 will include data from wv_st to wv_nd for each year.
 Note: estimation will take a long time if many years/waves are
 included in a single run;
%let yr_st=2011;  *start year;
%let yr_nd=2011;  *end year;
%let wv_st=3;     *start wave;
%let wv_nd=3;     *end wave;

%let my_common=STRIPED BASS; *common name for species of interest 
                              as listed in the size_yyyyw datasets;

%Macro compile_data;
        data trip;
                set
                        %do y = &yr_st. %to &yr_nd.;
                                %do w = &wv_st. %to &wv_nd.;
                                        mrip.trip_&y.&w.
                                %end;
                        %end;
                ;
        run;
		data size;
                set
                        %do y = &yr_st. %to &yr_nd.;
                                %do w = &wv_st. %to &wv_nd.;
                                        mrip.size_&y.&w.
                                %end;
                        %end;
                ;
        run;
%Mend compile_data;
%compile_data;

proc sort data=trip;
        by strat_id psu_id id_code;
run;

proc sort data=size;
        by strat_id psu_id id_code;
run;

data mysize;
        *keeping st and cnty from trip to define domain, add other fields as needed;
        merge trip(keep=strat_id psu_id id_code st cnty) size(in=s);
        by strat_id psu_id id_code;
        if s;
run;


* DEFINE DOMAIN;

/********************************************************************************
* EXAMPLE 1                                                                     *
* This definition reproduces standard length frequency estimates.               *
********************************************************************************/

data mysize;
        set mysize;
        my_dom_id = compress(year||wave||st||sub_reg||mode_fx||area_x);
		if common="&my_common." then common_dom = common;
		if common^="&my_common." then common_dom = "XXXXXXXXXXXXXXXXXXX";

		*example of length bin recode, converts lngth (mm) to inches and convert to
		 2in bin (floor);
		l_2in_bin=floor((lngth*0.03937)/2)*2;
run;


/********************************************************************************
* EXAMPLE 2                                                                     *
*  Example provided uses county merged in from the trip dataset                 *
*  to define 2 custom domains in NC (North, South).                             *
*                                                                               *
*  my_dom_id can be modified to calculate estimates at higher levels            *
*  by removing variables from the definition.                                   *
*  e.g., my_dom_id = compress(year||st||sub_reg||mode_fx||dom_id);              *
*  will give annuals estimates by state and mode_fx with NC still split         *
*  into North and South.                                                        *
********************************************************************************/
/*
data my_nc_size;
        set mysize;
        if st=37;
        dom_id=0;
        if st=37 and cnty in (15 29 41 53 55 139 143 177 187) then dom_id=1;     *North NC;
        if st=37 and cnty in (13 19 31 49 95 129 133 137 141 147) then dom_id=2; *South NC;
        my_dom_id = compress(year||wave||st||sub_reg||mode_fx||area_x||dom_id);
run;
*/


/**********************************************************************************
*  Surveymeans will estimate length frequencies (catch totals within length bins) *
*																				  *
*  The Sum field in the output dataset contains the landings (A+B1) in numbers of *
*  fish in the length bin listed in the Variable Level (VarLevel) field.          *
*																				  *
*  The Mean field gives the proportion of landings represented by the value in    *
*  the Sum field specific to domain and species.                                  * 
*                                                                                 *
*  While cv's are provided for counts and proportions, values will often be       *
*  0 or 1 due to small sample sizes.  Define domains to be as large as possbile.  *
**********************************************************************************/

proc surveymeans data=mysize missing sum varsum cvsum mean var cv;
		strata strat_id;
		cluster psu_id;
		weight wp_size;
		domain my_dom_id*common_dom;
		class l_in_bin;*class l_cm_bin;
		var l_in_bin;*var l_cm_bin;
		ods output domain=my_domain_l_freqs;
run;

*limiting output to species of interest;
data my_domain_l_freqs;
	set my_domain_l_freqs;
	if common_dom = "&my_common." and sum^=.;
run;
