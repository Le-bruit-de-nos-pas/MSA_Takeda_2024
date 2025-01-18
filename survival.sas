
proc import datafile="/home/u64131569/Mortality_overall.csv" 
            out=mortality_data
            dbms=csv
            replace;
    getnames=yes;
run;

proc lifetest data=mortality_data;
  /*  strata DC / test=logrank; */
    time elapsed*DC(0);  /* 0 indicates censoring */
run;


proc import datafile="/home/u64131569/Mortality_target_msa.csv" 
            out=mortality_data
            dbms=csv
            replace;
    getnames=yes;
run;

proc lifetest data=mortality_data;
    time elapsed*DC(0);  /* 0 indicates censoring */
run;



proc import datafile="/home/u64131569/Speech_target_msa.csv" 
            out=Speech_data
            dbms=csv
            replace;
    getnames=yes;
run;

proc lifetest data=Speech_data;
    time elapsed*Speech(0);  /* 0 indicates censoring */
run;




proc import datafile="/home/u64131569/Speech_overall.csv" 
            out=Speech_data
            dbms=csv
            replace;
    getnames=yes;
run;

proc lifetest data=Speech_data;
    time elapsed*Speech(0);  /* 0 indicates censoring */
run;


proc import datafile="/home/u64131569/Walk_overall.csv" 
            out=Walk_data
            dbms=csv
            replace;
    getnames=yes;
run;

proc lifetest data=Walk_data;
    time elapsed*Walk(0);  /* 0 indicates censoring */
run;


proc import datafile="/home/u64131569/Walk_target_msa.csv" 
            out=Walk_data
            dbms=csv
            replace;
    getnames=yes;
run;

proc lifetest data=Walk_data;
    strata Walk / test=logrank;
    time elapsed*Walk(0);  /* 0 indicates censoring */
run;



proc import datafile="/home/u64131569/Swallowing_target_msa.csv" 
            out=Swallowing_data
            dbms=csv
            replace;
    getnames=yes;
run;

proc lifetest data=Swallowing_data;
    strata Swallowing / test=logrank;
    time elapsed*Swallowing(0);  /* 0 indicates censoring */
run;



proc import datafile="/home/u64131569/Swallowing_overall.csv" 
            out=Swallowing_data
            dbms=csv
            replace;
    getnames=yes;
run;

proc lifetest data=Swallowing_data;
    strata Swallowing / test=logrank;
    time elapsed*Swallowing(0);  /* 0 indicates censoring */
run;


proc import datafile="/home/u64131569/Falls_overall.csv" 
            out=Falls_data
            dbms=csv
            replace;
    getnames=yes;
run;

proc lifetest data=Falls_data;
    strata Falls / test=logrank;
    time elapsed*Falls(0);  /* 0 indicates censoring */
run;


proc import datafile="/home/u64131569/Falls_target_msa.csv" 
            out=Falls_data
            dbms=csv
            replace;
    getnames=yes;
run;

proc lifetest data=Falls_data;
    strata Falls / test=logrank;
    time elapsed*Falls(0);  /* 0 indicates censoring */
run;


proc import datafile="/home/u64131569/Gastros_target_msa.csv" 
            out=Gastros_data
            dbms=csv
            replace;
    getnames=yes;
run;

proc lifetest data=Gastros_data;
    strata GASTRO / test=logrank;
    time elapsed*GASTRO(0);  /* 0 indicates censoring */
run;


proc import datafile="/home/u64131569/Gastros_overall.csv" 
            out=Gastros_data
            dbms=csv
            replace;
    getnames=yes;
run;

proc lifetest data=Gastros_data;
    strata GASTRO / test=logrank;
    time elapsed*GASTRO(0);  /* 0 indicates censoring */
run;
