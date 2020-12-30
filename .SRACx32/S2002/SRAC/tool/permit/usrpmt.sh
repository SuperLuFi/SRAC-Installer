#!/bin/csh
#
###########################################################################
#  usrpmt : version No.3 (98.1.20)
#  permit the SRAC code system to other users  (by Keisuke OKUMURA)
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
##--------- load modules
chmod $R_X $SRAC_DIR
chmod $R_X $SRAC_DIR/bin
chmod $R_X $SRAC_DIR/bin/*
echo "-- SRAC load module permitted --"
##--------- burnup libraries
chmod $R_X $SRAC_DIR/lib
chmod $R_X $SRAC_DIR/lib/burnlibT
chmod $DAT $SRAC_DIR/lib/burnlibT/*
echo "-- SRAC burnup lib permitted --"
##--------- Bickley function table
chmod $DAT $SRAC_DIR/lib/kintab.dat
echo "-- SRAC kin-table permitted --"
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
##--------- utility programs
chmod $R_X $SRAC_DIR/util
chmod $R_X $SRAC_DIR/util/pdscnvt
chmod $R_X $SRAC_DIR/util/pdscnvt/bin
chmod $R_X $SRAC_DIR/util/pdscnvt/bin/*
chmod $R_X $SRAC_DIR/util/pdscnvt/shr
chmod $R__ $SRAC_DIR/util/pdscnvt/shr/*
chmod $R_X $SRAC_DIR/util/pdscnvt/src
chmod $R_X $SRAC_DIR/util/pdsmdl
chmod $R_X $SRAC_DIR/util/pdsmdl/shr
chmod $R__ $SRAC_DIR/util/pdsmdl/shr/*
chmod $R_X $SRAC_DIR/util/pdsmdl/bin
chmod $R_X $SRAC_DIR/util/pdsmdl/bin/*
chmod $R_X $SRAC_DIR/util/pdsmdl/modl
chmod $DAT $SRAC_DIR/util/pdsmdl/modl/*
chmod $R_X $SRAC_DIR/util/pdsmdl/main
chmod $R__ $SRAC_DIR/util/pdsmdl/main/*
chmod $DAT $SRAC_DIR/util/pdsmdl/main/BnupEdit/*
chmod $DAT $SRAC_DIR/util/pdsmdl/main/FluxEdit/*
chmod $DAT $SRAC_DIR/util/pdsmdl/main/FluxPlot/*
chmod $DAT $SRAC_DIR/util/pdsmdl/main/MacroEdit/*
chmod $DAT $SRAC_DIR/util/pdsmdl/main/MicroEdit/*
chmod $R__ $SRAC_DIR/util/pdsmdl/obj
chmod $DAT $SRAC_DIR/util/pdsmdl/obj/*
echo "-- SRAC utility programs permitted --"
##--------- tool programs to install SRAC
chmod $R_X $SRAC_DIR/tool
chmod $R_X $SRAC_DIR/tool/lmmake
chmod $R_X $SRAC_DIR/tool/lmmake/lmmk
chmod $R__ $SRAC_DIR/tool/lmmake/lmmk/*
chmod $R_X $SRAC_DIR/tool/lmmake/objmk
chmod $R__ $SRAC_DIR/tool/lmmake/objmk/*
chmod $R_X $SRAC_DIR/tool/lmmake/lmupdt
chmod $R__ $SRAC_DIR/tool/lmmake/lmupdt/*
echo "-- SRAC tool programs permitted --"
##--------- sample problems
chmod $R_X $SRAC_DIR/smpl
chmod $R_X $SRAC_DIR/smpl/shr
chmod $R__ $SRAC_DIR/smpl/shr/*
chmod $R_X $SRAC_DIR/smpl/outp
chmod $DAT $SRAC_DIR/smpl/outp/*
echo "-- SRAC sample problems permitted --"
##--------- private command procedures 
chmod $R_X $SRAC_DIR/cmnd
chmod $R_X $SRAC_DIR/cmnd/*
echo "-- private command permitted --"
##-------------------------------------
##------------------------------------
    echo " "
    set MOD  = "Q"
  else
  endif
end
exit
