*****************************************************
MRIP catch estimation for custom domains

System requirements:
        Recommended Version - SAS 9.3
        Required Version - SAS 8 or higher
        The following components are required:
                Base SAS
                SAS/STAT

This is a template program for estimating catch totals
using the MRIP public-use datasets.

The program is setup to use information in the trip_yyyyw
dataset to define custom domains.  The catches are estimated
within the domains by merging the trip information onto
the catch_yyyyw datasets.

Required input datasets:
 trip_yyyyw
 catch_yyyyw

yyyy = year
w    = wave


TO RUN PROGRAM:
        1. Specify location of MRIP trip_yyyyw and catch_yyyyw
           SAS datasets in libname statement.
        2. Select a range of years and waves to include in analysis.
        3. Define domain.
		   Domain: a defined sub-level, geographic or otherwise,
			of the stratified estimation design.  Typically, these are
			sub-state geographic divisions based on county groups, or
			even sampled site groups if the domain cannot be defined by
			county boarders.

JFoster 3/2012

Updated for changes related to APAIS small sample size cases.
JFoster 8/2016

****************************************************;

*location of MRIP trip_yyyyw and catch_yyyyw sas datasets;
libname mrip "c:\my_mrip_data";

*Select range of years and wave to include in analysis. Program
 will include data from wv_st to wv_nd for each year.
 Note: estimation will take a long time if many years/waves are
 included in a single run;

%let yr_st=2011;  *start year;
%let yr_nd=2011;  *end year;
%let wv_st=3;     *start wave;
%let wv_nd=3;     *end wave;

%Macro compile_data;
        data trip;
                set
                        %do y = &yr_st. %to &yr_nd.;
                                %do w = &wv_st. %to &wv_nd.;
                                        mrip.trip_&y.&w.
                                %end;
                        %end;
                ;
			if var_id="" then var_id=strat_id;*added 8/2016;
        run;
        data catch;
                set
                        %do y = &yr_st. %to &yr_nd.;
                                %do w = &wv_st. %to &wv_nd.;
                                        mrip.catch_&y.&w.
                                %end;
                        %end;
                ;
			if var_id="" then var_id=strat_id;*added 8/2016;
			if wp_catch=. then wp_catch=wp_int;*added 8/2016;
        run;
%Mend compile_data;
%compile_data;

proc sort data=trip;
        by strat_id psu_id id_code;
run;

proc sort data=catch;
        by strat_id psu_id id_code;
run;

data mycatch;
        *keeping st and cnty from trip to define domain, add other fields as needed;
        merge trip(keep=strat_id psu_id id_code st cnty) catch(in=c);
        by strat_id psu_id id_code;
        if c;
run;


* DEFINE DOMAIN;

/********************************************************************************
* EXAMPLE 1                                                                     *
* This definition reproduces standard catch estimates.                          *
********************************************************************************/

data mycatch;
        set mycatch;
        if st=37;
        my_dom_id = compress(year||wave||st||sub_reg||mode_fx||area_x);
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
data my_nc_catch;
        set mycatch;
        if st=37;
        dom_id=0;
        if st=37 and cnty in (15 29 41 53 55 139 143 177 187) then dom_id=1;     *North NC;
        if st=37 and cnty in (13 19 31 49 95 129 133 137 141 147) then dom_id=2; *South NC;
        my_dom_id = compress(year||wave||st||sub_reg||mode_fx||area_x||dom_id);
run;
*/


/********************************************************************************
*   Surveymeans will estimate catch totals, by catch type, in each domain for   *
*   every species and put them in the output dataset 'my_domain_totals'.        *
*                                                                               *
*       tot_cat = Total Catch, A + B1 + B2                                      *
*                                                                               *
*    landing = Total Landings, A + B1                                           *
*                                                                               *
*    claim   = Observed Landings, A                                             *
*    harvest = Reported Landings, B1                                            *
*    release = Reported Releases, B2                                            *
*                                                                               *
*       Specifying the survey design information and sample weight              *
*    (var_id, psu_id, wp_catch) and including the 'missing' option              *
*    are necessary to get appropriate variance estimates.                       *
*                                                                               *
*       To reduce time need to run surveymeans, it is allowable to subset       *
*    data by any of the following strata variables if estimates are not         *
*    needed for all combinations:                                               *
*                                                                               *
*    year                                                                       *
*    wave                                                                       *
*    st                                                                         *
*    sub_reg                                                                    *
*                                                                        		*
*                                                                               *
*       For example if estimates are only needed for NC:                        *
*       data my_nc_catch;                                                       *
*               set mycatch;                                                    *
*               if st=37; *fips code for NC;                                    *
*                                                                               *
*       run;                                                                    *
*       Then modify surveymeans to use my_nc_catch in place of mycatch.         *
********************************************************************************/
ods graphics off;
proc surveymeans data=mycatch sum cvsum varsum missing;
        strata      var_id;*changed 8/2016;
        cluster     psu_id;
        weight          wp_catch;*changed 8/2016;
        domain          my_dom_id*common;
        var         tot_cat landing claim harvest release wgt_ab1;
        ods output domain=my_domain_totals;
run;
