*There's a new file in the Google Drive Time Series HW#3 folder called df_monthly;
*This has monthly well data and dates in format SAS can read automatically;

proc arima data=Time.well plot=all;
	identify var=well_height nlag=12 stationarity=(adf=2);
	*identify var=DailyHigh(1) nlag=10 stationarity=(adf=2);
run;
quit;

proc arima data=Time.well plot=all;
	*identify var=well_height nlag=36 stationarity=(adf=2 dlag=12); * adf=lag, dlag=season length;
	identify var=well_height(12) stationarity=(adf=2); 
run;
quit;

proc arima data=Time.well plot(unpack)=all;
	identify var=well_height nlag=12 outcov=Corr;
	estimate method=ML;
run;
quit;
