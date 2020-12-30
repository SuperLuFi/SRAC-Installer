#!/bin/csh
#
#*******************************************************************
#  This is a sample shell-scripts to make a concatinated library
#                                      by Keisuke OKUMURA (JAERI)
#*******************************************************************
#  << Example >>
#  We genarate the concatinated library based on the JEF-2.2
#  The nuclide data which is not in the JEF-2.2 libary, will be 
#  compensated by the JENDL-3.2 library data.
#  (copy library data in the inversed-order of priority)
#
   set JNDL32 = $HOME/SRACLIB-JDL32
   set JEF22  = $HOME/SRACLIB-JEF22
   set NEWLIB = $HOME/SRACLIB-CONCT
#
#
#--------------------------------------------------------------------
#
 alias  cp    cp
 alias  cd    cd
 alias  echo  echo
 alias  mkdir mkdir
#
   mkdir $NEWLIB
   mkdir $NEWLIB/pds
   mkdir $NEWLIB/pds/pfast
   mkdir $NEWLIB/pds/pmcrs
   mkdir $NEWLIB/pds/pthml
#  
##--------- pfast (fast energy)-------
#
 echo "(pfast-j32 being copyed)"
 cd  $JNDL32/pds/pfast
#cp  *         $NEWLIB/pds/pfast
 cp  [A-D]*    $NEWLIB/pds/pfast
 cp  [E-L]*    $NEWLIB/pds/pfast
 cp  [M-Z]*    $NEWLIB/pds/pfast
#
 echo "(pfast-jef22 being overwitten)"
 cd  $JEF22/pds/pfast
#cp  *         $NEWLIB/pds/pfast
 cp  [A-D]*    $NEWLIB/pds/pfast
 cp  [E-L]*    $NEWLIB/pds/pfast
 cp  [M-Z]*    $NEWLIB/pds/pfast
#
 echo "-- public fast lib concatenated --"
#
##--------- pmcrs (resonance energy)-----
#
 echo "(pmcrs-j32 being copyed)"
 cd  $JNDL32/pds/pmcrs
 cp  *         $NEWLIB/pds/pmcrs
#
 echo "(pmcrs-jef22 being overwitten)"
 cd  $JEF22/pds/pmcrs
 cp  *         $NEWLIB/pds/pmcrs
#
 echo "-- public mcros lib concatenated --"
#
##--------- pthml (thermal energy)-------
#
 echo "(pthml-j32 being copyed)"
 cd  $JNDL32/pds/pthml
#cp  *         $NEWLIB/pds/pthml
 cp  [A-D]*    $NEWLIB/pds/pthml
 cp  [E-J]*    $NEWLIB/pds/pthml
 cp  [K-N]*    $NEWLIB/pds/pthml
 cp  [O-Z]*    $NEWLIB/pds/pthml
#
 echo "(pthml-jef22 being overwitten)"
 cd  $JEF22/pds/pthml
#cp  *         $NEWLIB/pds/pthml
 cp  [A-D]*    $NEWLIB/pds/pthml
 cp  [E-J]*    $NEWLIB/pds/pthml
 cp  [K-N]*    $NEWLIB/pds/pthml
 cp  [O-Z]*    $NEWLIB/pds/pthml
#
 echo "-- public thermal lib concatenated --"
 echo "-- All process finished.           --"
