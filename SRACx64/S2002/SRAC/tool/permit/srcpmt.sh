#!/bin/csh
#
###########################################################################
#  srcpmt : version No.3 (98.1.20)                                         
#  permit source programs of SRAC code system to other users               
#  (by Keisuke OKUMURA)
###########################################################################
#
alias chmod chmod
alias echo  echo
alias cd    cd
#
#####################################################
            set PDIR = `pwd`    
            cd ../..            
            set SRAC_DIR = `pwd`
            cd $PDIR            
            set conf2 = "n"
            while ($conf2 != "y")
              echo " "
              echo "default path of the SRAC code : $SRAC_DIR"
              echo -n "Is that right ? (y/n/q:quit)? ==> "
              set conf2 = $<
              if ($conf2 == "q") then
                exit
              endif
              if ($conf2 != "y") then
                echo -n "Enter full path of the SRAC code ==> "
                set SRAC_DIR = $<
              endif
            end                                                
#####################################################
echo " "
set menu = " "
set MOD  = "Q"
while ($menu != "q")
  echo "=======< Select Access Mode >========"
  echo "*                                   *"
  echo "*  1 : All users (read only)        *" 
  echo "*  2 : Group users (read only)      *"
  echo "*  3 : Owner only (read/write)      *"
  echo "*  q : bye-bye                      *"
  echo "*                                   *"
  echo "====================================="
  echo -n "Set number ==> "
  set number = $<
  switch ($number)
    case 1:
          set MOD = "A"
          set R_X = 755
          set R__ = 744
          set DAT = 644
          breaksw
    case 2:
          set MOD = "G"
          set R_X = 750
          set R__ = 740
          set DAT = 640
          breaksw
    case 3:
          set MOD = "O"
          set R_X = 700
          set R__ = 700
          set DAT = 600
          breaksw
    case q:
          set MOD = "Q"
          set menu = "q"
          breaksw
    default:
          set MOD = "Q"
          echo "$number is invalid number."
  endsw
  if ($MOD != "Q") then
    echo " "
##--------- source programs
chmod $R_X $SRAC_DIR/src
chmod $R_X $SRAC_DIR/src/srac
chmod $DAT $SRAC_DIR/src/srac/*
chmod $R_X $SRAC_DIR/src/cit
chmod $DAT $SRAC_DIR/src/cit/*
chmod $R_X $SRAC_DIR/src/pij
chmod $DAT $SRAC_DIR/src/pij/*
chmod $R_X $SRAC_DIR/src/burn
chmod $DAT $SRAC_DIR/src/burn/*
chmod $R_X $SRAC_DIR/src/read
chmod $DAT $SRAC_DIR/src/read/*
chmod $R_X $SRAC_DIR/src/extnl
chmod $R_X $SRAC_DIR/src/extnl/*
chmod $DAT $SRAC_DIR/src/extnl/*.*
chmod $DAT $SRAC_DIR/src/extnl/@ReadMe
chmod $R_X $SRAC_DIR/src/sracvp
chmod $DAT $SRAC_DIR/src/sracvp/*
chmod $R_X $SRAC_DIR/src/citvp
chmod $DAT $SRAC_DIR/src/citvp/*
chmod $R_X $SRAC_DIR/src/plot
chmod $DAT $SRAC_DIR/src/plot/*.f
chmod $R_X $SRAC_DIR/src/common
chmod $DAT $SRAC_DIR/src/common/*.f
echo "-- SRAC source programs permitted --"
##--------- include statement
chmod $R_X $SRAC_DIR/src/inc
chmod $R_X $SRAC_DIR/src/inc/srac30m
chmod $DAT $SRAC_DIR/src/inc/srac30m/*
chmod $R_X $SRAC_DIR/src/inc/srac50m
chmod $DAT $SRAC_DIR/src/inc/srac50m/*
chmod $R_X $SRAC_DIR/src/inc/srac100m
chmod $DAT $SRAC_DIR/src/inc/srac100m/*
chmod $R_X $SRAC_DIR/src/inc/srac200m
chmod $DAT $SRAC_DIR/src/inc/srac200m/*
chmod $R_X $SRAC_DIR/src/inc/usrinc
chmod $DAT $SRAC_DIR/src/inc/usrinc/*
echo "-- SRAC include statements permitted --"
##------------------------------------
    echo " "
    set MOD  = "Q"
  else
  endif
end
exit
