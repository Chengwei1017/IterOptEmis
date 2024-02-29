#!/bin/csh
setenv EADIR /home/lcw/LCW/Models/RMODELS/OPENEMISOPT/
setenv RUNYR 2023
setenv RUNMT 03
setenv RUNYE 2023
setenv RUNME 04
setenv RUNEMIS CDF
setenv RUNRAW F

setcmaqtime ${RUNYR}${RUNMT}01-${RUNYE}${RUNME}01
if ( $RUNRAW == T ) then
echo RUN RAW CASE...
sleep 5s
LinkWRF HNF${RUNMT} ${RUNYR}${RUNMT}
cp -rv EMISFILES/STEP00/${RUNEMIS}01 /home/lcw/LCW/Models/RMODELS/MEIC2CMAQ/BASEEMIS/

cd $WDR
source config
./LCWAQS D1M
/home/lcw/LCW/Models/RMODELS/MEIC2CMAQ/MEIC2CMAQ_PROCESSOR.R ${RUNYR}-${RUNMT} ${RUNEMIS}01 T 
./LCWAQS D1IBJCO

backupcmaq ${RUNYR}${RUNMT} 1 STEP00
EXTCMAQ ${RUNYR}-${RUNMT} STEP00 SCEN 1

endif


foreach runstep (1 2 3 4 5 6 7 8)
setenv STEPNAME STEP`printf "%02d\n" $runstep`
echo Running $STEPNAME

cd $EADIR
./OPT_EMIS.R $runstep ${RUNYR}${RUNMT}

cd $WDR
source config
/home/lcw/LCW/Models/RMODELS/MEIC2CMAQ/MEIC2CMAQ_PROCESSOR.R ${RUNYR}-${RUNMT} ${RUNEMIS}01 T 
./LCWAQS D1IBJCO

backupcmaq ${RUNYR}${RUNMT} 1 ${STEPNAME}
EXTCMAQ ${RUNYR}-${RUNMT} ${STEPNAME} SCEN 1

end
cd $EADIR
./OPT_EMIS.R 9 ${RUNYR}${RUNMT}

