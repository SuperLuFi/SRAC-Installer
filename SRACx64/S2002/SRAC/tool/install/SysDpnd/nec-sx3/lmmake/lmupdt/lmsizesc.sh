#!/bin/csh -f
#
######################################################################
#
#    lmsizesc.sh : Objects ===> SRAC Load Module (All Scalar Version)
#    produce load module from objects by changing only memory size  
#    and include parameters for read(PDS) routines.
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#                                                                          
#  setenv SRAC_CODE $HOME/SRAC                                             
#
######################################################################
   set SRAC_DIR = $SRAC_CODE
#
   set    F77      = f77sx
   set    SRC_DIR  = $SRAC_DIR/src
   set    OBJSC    = $SRAC_DIR/obj/SCall
   set    OPTSC    = "-Nv"
#  set    OPTSC    = "-Nv -O nomove"
   set    LIB      =
#
#---------- Working Directory -----------
#
   set    WKPAT    = $HOME
   set    DATE     = `date +%b%d.%H.%M.%S`
   set    TMPWK    = tmpOBJ.$DATE
   set    TMPUP    = tmpUPD.$DATE
   set    WKDWK    = $WKPAT/$TMPWK
   set    WKDUP    = $WKPAT/$TMPUP
#
#---------- Set Load Module Name & Directory Name of Include Statement
#
#  LMN : new load module name 
#  INC : directory name where include-files are stored
#
#  set    LMN      = $SRAC_DIR/bin/SRACsc.50m
#  set    INC      = $SRC_DIR/inc/srac50m
#
   set    LMN      = $SRAC_DIR/bin/SRACsc.100m
   set    INC      = $SRC_DIR/inc/srac100m
#
#  set    LMN      = $SRAC_DIR/bin/SRACsc.200m
#  set    INC      = $SRC_DIR/inc/srac200m
#
#---------- Compile New main, read-PDS Routines -----------
#
   alias  rm    rm
   alias  cp    cp
   alias  mkdir mkdir
   mkdir  $WKDWK
   mkdir  $WKDUP
#
   cd $SRC_DIR
   cp read/*.f       $WKDUP
   cp srac/input1.f  $WKDUP
   cp srac/main000.f $WKDUP
   cp $INC/MAINSINC  $WKDUP
   cp $INC/READPINC  $WKDUP
   cd $WKDUP
   $F77 -c $OPTSC *.f
#
#---------- Over Write Updated Object on Old Objects --------
#
   cd $OBJSC
   cp *.o  $WKDWK
   cd $WKDUP
   cp *.o  $WKDWK
#
#---------- Link and Remove Working Directories -------------
#
   cd $WKDWK
   $F77 -o $LMN $LIB *.o
   cd $WKPAT
   rm -r $TMPWK
   rm -r $TMPUP
#
#---------- End Process ------------------------------------- 
