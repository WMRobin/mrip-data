********************************************************************
MRIP directed angler-trips (effort) for custom domains

System requirements:
	Recommended Version - SAS 9.3
	Required Version - SAS 8 or higher 
	The following components are required:
		Base SAS
		SAS/STAT

This is a template program for estimating directed trips
using the MRIP public-use datasets.

The program is setup to use trip_yyyyw datasets to define
custom domains and estimate total angler-trips within domains.
Catch information can be used in defining the domains by 
merging the catch_yyyyw datasets onto the trip_yyyyw datasets.

Required input dataset:
 trip_yyyyw
Optional input dataset:
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
	4. Turn %*add_catch_info MACRO on/off based on need. If domain
	   requires merging of catch data (ex: species information), 
	   turn macro ON.  If not, leave macro off.
    5. If you are using catch information that includes A-catch
       (landing, claim, tot_cat, wgt_ab1, wgt_a) to define your domain,
       be sure to set grp_flg=Y which follows the year and wave
	   macro variables.
	6. Edit surveymeans procedure to run on dataset created
	   for specific domain (i.e. edit: data=DATASET_NAME)

JFoster 4/2012

Updated for changes related to APAIS small sample size cases.
JFoster 8/2016

********************************************************************;


*location of MRIP trip_yyyyw and catch_yyyyw sas datasets;
libname mrip "c:\my_mrip_data";

*Select range of years and waves to include in analysis. Program
 will include data from wv_st to wv_nd for each year.
 Note: estimation will take a long time if many years/waves are
 included in a single run;

%let yr_st=2011;  *start year;
%let yr_nd=2011;  *end year;
%let wv_st=3;     *start wave;
%let wv_nd=3;     *end wave;



*If you will be using claim, landing, tot_cat, wgt_ab1, or wgt_a
 to define your domain of interest using that catch information
 for a single species, set grp_flg=Y;
%let grp_flg=N;

%Macro compile_data;
	data mytrip; 
		set 
			%do y = &yr_st. %to &yr_nd.;
				%do w = &wv_st. %to &wv_nd.;
					mrip.trip_&y.&w.
				%end;
			%end;
		;
		if var_id="" then var_id=strat_id;*added 8/2016;
		*dtrip will be used to estimate total directed trips,
		 do not change it;
		dtrip = 1;
	run;
	
	proc sort data=mytrip;
		by strat_id psu_id id_code;
	run;

	%Macro add_catch_info;
	*Catch fields
		common=common name
		sp_code=MRFSS 10 digit species code
		claim=harvested fish observed by interviewer A-catch (no. fish)
		release=reported fish released alive B2-catch (no. fish)
		harvest=harvested fish not observed by interviewer B1-catch (no. fish)
		landing=claim + harvest
		tot_cat=claim + harvest + release
		wgt_ab1=landing in kg instead of no. fish
		wgt_a=claim in kg instead of no. fish
		wgt_b1=harvest in kg instead of no. fish;
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
		proc sort data=catch;
			by strat_id psu_id id_code;
		run;
	%Mend add_catch_info;

	/*comment out add_catch_info macro call below if not needed*/
	*%add_catch_info;

%Mend compile_data;
%compile_data;


* DEFINE DOMAIN;

/********************************************************************************
* EXAMPLE 1                                                                     *
* This definition reproduces standard trip estimates using no catch information.*
********************************************************************************/

data mytrip;
	set mytrip;
	my_dom_id = compress(year||wave||st||sub_reg||mode_fx||area_x);
run;

/*******************************************************************************
* EXAMPLE 2                                                                    *
*  Second example provided uses the standard estimation cell fields and        *
*  prim1_common in the trip dataset and common, tot_cat from the catch dataset *
*  to define two directed trip domains:                                        *
*		1=trips targeting (prim1 only) or catching red drum                    *
*		2=all other trips                                                      *
*  Note that you should set grp_flg=Y near the top of this program since       *
*  since tot_cat includes A-catch.                                             *
*  You must also run the %add_catch_info macro in the %compile_data macro	   *
*  above.																	   *
*                                                                              *
*  my_dom_id can be modified to calculate estimates at higher levels           *
*  by removing variables from the definition.                                  *
*  e.g., my_dom_id = compress(year||st||sub_reg||mode_fx||dom_id);             *
*  will give annuals estimates by state and mode_fx for red drum trips         *
*  and all other trips.                                                        *
*******************************************************************************/

/*
data catch_reddrum;
	set catch;
	if common="RED DRUM";
run;
data mytrip_reddrum;
	merge mytrip(in=t) catch_reddrum(keep=
		strat_id psu_id id_code 
		common sp_code claim release harvest landing tot_cat
		wgt_ab1 wgt_a wgt_b1);
	by strat_id psu_id id_code;
	if t;
run;
data mytrip_reddrum;
	set mytrip_reddrum;
	* dom_id variable allows differentiation between domain of interest and
	  'other' domain:
			dom_id=1  DOMAIN OF INTEREST
			dom_id=2  'OTHER' DOMAIN;

    *coding 'other' domain;
	dom_id=2;

	*examples to code red drum as domain of interest;
	if prim1_common="RED DRUM" or tot_cat>0 then dom_id=1;
	*if release>0 then dom_id=1;
	*if landing>0 then dom_id=1;
run;

%Macro grp_catch;
	%if &grp_flg.=Y %then %do;
		proc sql;
			create table mytrip_reddrum as
			select *,min(dom_id) as gc_flag
			,max(claim) as claim_flag
			from mytrip_reddrum
			group by strat_id,psu_id,leader
			;
		quit;
		data mytrip_reddrum;
			set mytrip_reddrum;
			if gc_flag=1 and claim_flag>0 and dom_id=2 then dom_id=1;
		run;
	%end;
%Mend grp_catch;
%grp_catch;

data mytrip_reddrum;
	set mytrip_reddrum;

	*Including all required by variables in the domain;
	*my_dom_id = compress(year||sub_reg||st||mode_fx||dom_id);
	my_dom_id = compress(year||sub_reg||dom_id);
run;



*** Now mytrip_reddrum is ready for analysis, surveymeans must be modified to
    use mytrip_reddrum;


/********************************************************************************
* Surveymeans will estimate trip totals in each domain and put them				*
*   in the output dataset 'my_domain_trip_totals'.								*
*																				*
*	dtrip = Total count of angler-trips in domains								*
*																				*
*	Specifying the survey design information and sample weight					*
*  (strat_id, psu_id, wp_int) and including the 'missing' option				*
*    are necessary to get appropriate variance estimates.						*
*																				*
*	To reduce time need to run surveymeans, it is allowable to subset			*
*    data by any of the following strata variables if estimates are not			*
*    needed for all combinations:												*
*																				*
*    year																		*
*    wave																		*
*    st																			*
*    sub_reg																	*
*     															             	*
*																				*
*	For example if estimates are only needed for NC:							*
*	data my_nc_trip;															*
*		set mytrip;																*
*		if st=37; *fips code for NC;											*
*	run;																		*
*	Then modify surveymeans to use my_nc_trip in place of mytrip.				*
********************************************************************************/
ods graphics off;
proc surveymeans data=mytrip /*mytrip_reddrum*/ sum cvsum varsum missing;
	strata      var_id;*changed 8/2016;
	cluster     psu_id;
	weight     	wp_int;
	domain   	my_dom_id;
	var         dtrip;
	ods output domain=my_domain_trip_totals;
run;
