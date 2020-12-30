#!/bin/csh
#
#################################################################       
#                                                                       
#  Function : allow library access to other users
#                                                                       
#                        by keisuke OKUMURA (JAERI)                     
#                                                                       
   set  LIB_DIR = $SRAC_LIB_JDL32
#                                                                           
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the library file in the following statement, and use it instead
#         of the above statement.
#                                                                           
#  set  LIB_DIR = $HOME/SRACLIB-JDL32
#                                                                       
#################################################################       
#
alias chmod chmod
alias echo  echo
alias cd    cd
#
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
    chmod $R_X $LIB_DIR
    chmod $R_X $LIB_DIR/tool
    chmod $R_X $LIB_DIR/tool/help
    chmod $DAT $LIB_DIR/tool/help/help*.txt
    chmod $R_X $LIB_DIR/pds
    chmod $R_X $LIB_DIR/pds/pfast
    chmod $R_X $LIB_DIR/pds/pmcrs
    chmod $R_X $LIB_DIR/pds/pthml
    cd $LIB_DIR/pds/pfast
#   chmod $DAT *
    chmod $DAT [A-D]*
    chmod $DAT [E-L]*
    chmod $DAT [M-Z]*
    echo " Fast    library was permitted"
    cd $LIB_DIR/pds/pmcrs
#   chmod $DAT *
    chmod $DAT [A-E]*
    chmod $DAT [F-Z]*
    echo " Mcross  library was permitted"
    cd $LIB_DIR/pds/pthml
#   chmod $DAT *
    chmod $DAT [A-D]*
    chmod $DAT [E-J]*
    chmod $DAT [K-N]*
    chmod $DAT [O-Z]*
    echo " Thermal library was permitted"
    echo " "
    set MOD  = "Q"
  else
  endif
end
exit
