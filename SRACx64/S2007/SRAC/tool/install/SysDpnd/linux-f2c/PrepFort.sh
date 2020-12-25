#!/bin/csh
#
##################################################################
#
#  I am installer of the Fortran compiler ksk77 (by Keisuke OKUMURA)
#  For Linux/f2c+gcc
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#                                                                          
#  setenv SRAC_CODE $HOME/SRAC                                             
#
##################################################################
   set SRAC_DIR = $SRAC_CODE
#
alias echo echo
alias rm   rm
alias cp   cp
#
set nowd   = $SRAC_DIR/tool/install/SysDpnd/linux-f2c
set yorn   = ""
set gonext = ""
echo ""
while ($yorn != "q")
  echo -n 'Do you have "f2c" and "gcc" ?  (y/n) ==> '
  set yorn = $<
  if ($yorn == "n") then
    echo ""
    echo "******************************************************"
    echo ' Get "f2c" and "gcc" from the Free Software Foundation'
    echo ' See the home page ( http://www.gnu.org/ ),           '
    echo " and download them.  See you again.                   "
    echo "******************************************************"
    echo ""
    exit
  else if ($yorn == "y") then
    set yorn = "q"
  endif
end
#
##### TEST to check the fort77 script is available or not ####
#
if (-e $SRAC_DIR/tmp/check.out) then
  rm $SRAC_DIR/tmp/check.out
endif
if (-e $SRAC_DIR/tmp/check.tmp) then
  rm $SRAC_DIR/tmp/check.tmp
endif
fort77 -o $SRAC_DIR/tmp/check.out $nowd/check.f >& $SRAC_DIR/tmp/check.tmp
if (-e $SRAC_DIR/tmp/check.out) then
# $SRAC_DIR/tmp/check.out
  rm $SRAC_DIR/tmp/check.out
  rm $SRAC_DIR/tmp/check.tmp
  set gonext = "y"
else
  set gonext = "n"
endif
#=============== You have the fort77 script
if ($gonext == "y") then
  echo ""
  $nowd/PrepInst.sh
  exit
endif
#=============== You do not have the fort77 script
#
set menu = " "
while ($menu != "q")
  echo " "
  echo "======================================================="
  echo "You don't have the fort77 compiler. What shall we do ? "
  echo "======================================================="
  echo "*                                                     *"
  echo "*  1 : Help (Read me anyway)                          *"
  echo "*  2 : Install my hand-made Fortran compiler (fort77).*"
  echo "*                                                     *"
  echo "*  q : You don't like my hand-made Fortran compiler.  *"
  echo "*      Terminate this pre-processor, and come again   *"
  echo "*      after you get the fort77 compiler.             *"
  echo "*                                                     *"
  echo "======================================================="
  echo -n "Set number ==> "
  set number = $<
  switch ($number)
    case 1:
          echo ""
          cat $nowd/help.txt | more
          echo -n "Return>"
          set dummy  = $< 
          breaksw
    case 2:
          echo ""
          echo "The fort77 command should be installed in the directory"
          echo "where your private commands are stored.(e.g. $HOME/bin)."
          if ( -e $HOME/bin) then
            set outd = $HOME/bin
          else
            set outd = $SRAC_DIR/cmnd
          endif
          set conf  = "n"
          set conf2 = " "
          while ($conf != "y")
            echo " "
            echo    "Default directory : $outd"
            echo    "Enter full path name if you want to change the default,"
            echo -n "or just Return ==> "
            set dummy = $<
            set nwd = `echo $dummy | wc -w`
            if ($nwd != "0") then
              set outd = $dummy
            endif
            echo ""
            echo "$outd/fort77 will be generated."
            echo -n "Are you ready ? (y/n/q:quit) ==> "
            set conf = $<
            if ($conf == "y") then
              if( ! -e $outd) then
                echo ""
                echo "*** No such directory : $outd"
                set conf = "n"
              else
                if (-e $outd/fort77) then
                  echo ""
                  echo "*** fort77 already exists in $outd"
                  echo -n "*** Do you overwrite it ? (y/n) ==> "
                  set conf = $<
                endif
              endif
            else if ($conf == "q") then
              set conf2 = "q"
              set conf = "y"
            else
              set conf = "n"
            endif
          end
#
          if ($conf2 == "q") then
            echo ""
            echo "*************************************"
            echo "The fort77 command was not installed."
            echo "*************************************"
          else
            cp $SRAC_DIR/cmnd/ksk77 $outd/fort77
            chmod 700 $outd/fort77
            echo ""
            echo "The fort77 command was installed in $outd"
            echo ""
## Check working of the fort77 command
            if (-e $SRAC_DIR/tmp/check.out) then
              rm $SRAC_DIR/tmp/check.out
            endif
            if (-e $SRAC_DIR/tmp/check.tmp) then
              rm $SRAC_DIR/tmp/check.tmp
            endif
            $outd/fort77 -o $SRAC_DIR/tmp/check.out $nowd/check.f >& $SRAC_DIR/tmp/check.tmp
            if (-e $SRAC_DIR/tmp/check.out) then
              $SRAC_DIR/tmp/check.out
              rm $SRAC_DIR/tmp/check.out
              rm $SRAC_DIR/tmp/check.tmp
              echo ""
              echo "*********************************( Your Next Actions) **"
              echo "(1) Quit @PunchMe here, and                             "
              echo "(2) Reset command-search-path so that the fort77 command"
              echo "    can be executed in any directory (e.g. source .cshrc)"
              echo "(3) Execute @PunchMe and pre-processor again.           "
              echo "               <<<  See you again.  >>>"
              echo "********************************************************"
              echo "" 
            else
              echo ""
              echo "*********************************************"
              echo "The fort77 script do not work well.          "
              echo "  I do not know why ?                        "
              echo "  Do you have really the f2c and gcc ?       "
              echo "  Check the shell script : $outd/fort77      "
              echo "*********************************************"
            endif
            echo -n "Return>"
            set dummy = $<
            echo ""
          endif
          set menu = "q"
          breaksw
    case q:
          set menu = "q"
          breaksw
    default:
          echo "$number is invalid selection."
  endsw
end
exit
