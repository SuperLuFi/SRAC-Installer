#!/bin/csh
#
##################################################################
#
#  <<  Make a plot data of Public Fast and Thermal Libraries >>
#      for developers of SRAC
#
#  Consultant : keisuke OKUMURA,
#               E-mail okumura@mike.tokai.jaeri.go.jp
#
##################################################################
   alias cd    cd
   alias mkdir mkdir
   alias echo  echo
   alias rm    rm
   alias ls    ls
#
#  SRAC_DIR : top directory of the SRAC code
#  CASE   : case name which is refered as names of output files
#  LIB    : library name(A20) which will be used as a comment in output
#           Characters blank or '(', ')' are not allowed in LIB.
#  PFAST  : directory name of public fast liblary
#  PMCRS  : directory name of public mcross(PEACO) library
#  PTHML  : directory name of public thermal library
#  OUTD   : directory name in which output data will be stored
#           Major information will be written in $OUTD/$CASE.lst
#           Other files store debug information.
#
   set SRAC_DIR = $HOME/SRAC2K/SRAC
   set CASE     = PLIB
#--- Check SRAC95 library
   set PFAST    = $HOME/SRAC2K/SRACLIB-JDL32/pds/pfast
   set PTHML    = $HOME/SRAC2K/SRACLIB-JDL32/pds/pthml
   set OUTD     = $SRAC_DIR/tmp
#
#
#=============  Change if necessary ===========================
#
   setenv  PFAST    $PFAST
   setenv  PTHERMAL $PTHML
   set DATE    = `date +%b%d.%H.%M.%S`
   set LM      = $SRAC_DIR/util/admin/bin/pubxsec.exe
   set OUTLST  = $OUTD/PubXsec-$CASE.$DATE.ft06
   setenv fu10   $OUTD/PubXsec-$CASE.$DATE.table
#
#--Input Manual ------------------------------------------
# Block-1 Control options  /2/
# IOPT1 : Output option
#       = 0  Print only contents of control member(Czzmc000)
#            on standard output(6)
#       = 1  Print contents of control member(Czzmc000)
#            and make a plot table (bar graph type) on device 10
#       =-1  Print contents of control member(Czzmc000)
#            and make a plot table (histgram type) on device 10
# IOPT2 : Energy range
#       = 1  PFAST
#       = 2  PTHERMAL
#       = 3  PFAST and PTHERMAL
#       =-N  PFAST and PTHERMAL concatinated after N-th fast group
#            (Not available yet)
# Block-2 Member name to specify a nuclide (Xzzmc00t)  /A8/
#         (repeat until blank member name is entered)
#---------------------------------------------------------
#
#=============  Execution ========================================
#
cat - << END_DATA | $LM >& $OUTLST
-1  3              /  IPOT1, IOPT2
XO060001           /  O-16  (free gas)
XU080001           /  U-238 (free gas)
                   /  end of member input
END_DATA
#
#=============  Remove temporary and debug files =================
#
#  rm $OUTLST
