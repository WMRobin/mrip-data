*****************************************************
MRIP FES estimation for custom domains

System requirements:
        Recommended Version - SAS 9.3
        Required Version - SAS 8 or higher
        The following components are required:
                Base SAS
                SAS/STAT

This is a template program for estimating effort distributions
using the MRIP public-use datasets.

The program is setup to use information in the fes_person_YYYYW
and the FES_HH_YYYYW dataset to define custom domains.  

Required input datasets:
 FES_person_YYYYW 

yyyy = year
w    = wave


TO RUN PROGRAM:
        1. Specify location of MRIP FES_person_YYYYW
           SAS datasets in libname statement.
        2. Select a range of years and waves to include in analysis.
        3. Define domain.
		   Domain: a defined sub-level, geographic or otherwise,
			of the stratified estimation design.  For the FES, these can be combined
            geographic divisions based on state or demographic characteristics.

JFoster 3/2012

Updated for changes related to APAIS small sample size cases.
JFoster 8/2016

Updated for FES public-use data 6/2022
****************************************************;

*location of MRIP trip_yyyyw and catch_yyyyw sas datasets;
libname mrip "M:\FES\Imputed_Data\public";

*Select range of years and wave to include in analysis. Program
 will include data from wv_st to wv_nd for each year.
 Note: estimation will take a long time if many years/waves are
 included in a single run;

%let yr_st=2020;  *start year;
%let yr_nd=2020;  *end year;
%let wv_st=4;     *start wave;
%let wv_nd=4;     *end wave;

%Macro compile_data;
        data fes;
                set
                        %do y = &yr_st. %to &yr_nd.;
                                %do w = &wv_st. %to &wv_nd.;
                                        mrip.FES_person_&y.&w.
                                %end;
                        %end;
                ;*if var_id="" then var_id=strat_id;*added 8/2016;
        run;
			
        
%Mend compile_data;
%compile_data;

* DEFINE DOMAIN;

/********************************************************************************
* EXAMPLE 1                                                                     *
* This definition produces region-level boat estimates.                         *
* Note that FES estimates are for resdents of coastal states, so will not       *
* Match to other effort queries                                                 *
********************************************************************************/

data fes;
        set fes;
        if st in (23,33,25,44,9) then sub_reg=4;
		  else if st in (36,34,10,24,51) then sub_reg=5;
		  else if st in (37,45,13) then sub_reg=6;
		  else if st in (12,1,28) then sub_reg=7;          *FES does not partition FL into EFL/WFL;
		  else if st in (15) then sub_reg=8;
		  else sub_reg=99;
        my_dom_id = compress(year||sub_reg);
run;

proc surveymeans data=fes sum varsum;
    strata      stratum_ID;
	cluster     HH_ID;
	weight     	final_wt;
	domain   	my_dom_id;
	var         boat_trips;
	ods output domain=boat_trips_sub_reg;
run;


/********************************************************************************
* EXAMPLE 2                                                                     *
*  Example 2 calculates weighted boat trip distributions by state               *
*  and age bin                                                                  *
*                                                                               *
*  The example defines 3 age bins (<18, 18-64 and 65+).                         *  
*                                                                               *
*  my_dom_id can be modified to calculate estimates at different levels of      *
*  resolution by adding/removing variables from the definition.                 *
*  e.g., my_dom_id = compress(year||st||sub_reg||wave);                         *
*                                                                               *
********************************************************************************/

data fes_age;
        length age_bin $ 5;
        set fes;
		if age>=0 and age < 18 then age_bin=1;
          else if age>=18 and age<=64 then age_bin=2;
	      else if age>=65 then age_bin=3;
	      else age_bin="?????";
        my_dom_id=compress(year||st||"_"||age_bin);
run;

proc surveymeans data=fes_age sum varsum;
	    strata      stratum_ID;
	    cluster     HH_ID;
	    weight     	final_wt;
	    domain   	my_dom_id;
	    var         boat_trips;
	    ods output domain=age_dist_boattrips;
run;
