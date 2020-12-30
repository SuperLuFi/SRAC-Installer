#!/bin/csh
#
#################################################################
#   Library Installation need a utility program in the SRAC code.
#   Directory name of the SRAC code will be set, here.
#   (by Keisuke OKUMURA)
#################################################################
    set  LIB_DIR = $SRAC_LIB_JDL32
#
alias  echo  echo 
alias  cd    cd 
alias  cat   cat
  echo " "                                             
  echo "** I need a utility program in the SRAC code to install library. **"
  echo "** Tell me where SRAC is.                                        **"
#
if ( -e $LIB_DIR/tool/sracdir.txt) then
  rm $LIB_DIR/tool/sracdir.txt
endif
set Now_Dir = `pwd`
cd $LIB_DIR/..
set SRAC_DIR = `pwd`/SRAC
cd $Now_Dir
#
set conf = "n"
while ($conf != "y")
  echo " "
  echo "default path of the SRAC code : $SRAC_DIR"
  echo -n "Is that right ? (y/n/q:quit)? ==> "
  set conf = $<
  if ($conf == "q") then
    exit
  endif
  if ($conf != "y") then
    echo -n "Enter full path of the SRAC code ==> "
    set SRAC_DIR = $<
  else
    if ( -e $SRAC_DIR ) then
      echo $SRAC_DIR >! $LIB_DIR/tool/sracdir.txt
    else
      echo ""
      echo '** File was not found !  Confirm and try again ! '
      set conf = "n"
    endif
  endif
end
