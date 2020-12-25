#! /bin/csh
#  Shell Script to Make a Makefile for SRAC2003
#  (for the system C-compiler is necessary)
#  This is a special version for F2C/G77, Grep "F2C" to find different parts
#
alias cd    cd
#
######## Set input variables ########################################
#
#   hmdir   : top directory name of this system   (full path)
#   fort    : fortran compile driver name (e.g. f77)
#   fopt    : fortran compile options (e.g. -O)
#   cc      : C compile driver name (e.g. cc)
#   copt    : C compile options (e.g. -DPOSIX_C)
#   arcv    : archive command (e.g. ar)
#   aopt    : archive command options (e.g. r)
#   lmn     : load module name                    (full path)
#   lib     : library names to be linked.
#   incdir  : directory name of include files     (full path)
#   srcsdir : directory names of source programs  (full path)
#   ccdir   : directory name(one) including c-source programs  (full path)
#   objdir  : directory in which objects will be stored. 
#   mkfile  : file name of the generated Makefile (full path)
#   wkdir   : working directory for this script
#             temporary files (temp,temp2,incs) will be generated. 
#--------------------------------------------------------------------
#
set pdir = `pwd`
cd ../../..
set hmdir = `pwd`
cd $pdir
set fort    = "Fortran-Driver"
set fopt    = "Scalar_Option"
set cc      = "C-Driver"
set copt    = "C_Option"
#set cc      = gcc
#set copt    = -DPOSIX_C
set lib     =
set incdir  = $hmdir/src/inc/srac100m
set objdir  = $hmdir/obj/SCall
set ccdir   = $hmdir/src/extnl
set wkdir   = $hmdir/tmp
#
set lmn     = $hmdir/bin/SRAC.100m
set srcsdir = ($hmdir/src/common $hmdir/src/read $hmdir/src/extnl \
               $hmdir/src/pij  $hmdir/src/cit  $hmdir/src/burn    \
               $hmdir/src/plot $hmdir/src/srac)
set mkfile  = $objdir/Makefile.org
#
############################################################################
#
# reset command
alias ls    ls
alias rm    rm
alias cd    cd
alias cp    cp
alias echo  echo
alias cat   cat
alias grep  grep
alias sort  sort
alias uniq  uniq
#
# remove working files if existing 
#
if (-e $mkfile) then
  rm $mkfile
endif
if (-e $wkdir/temp) then
  rm $wkdir/temp
endif
if (-e $wkdir/temp2) then
  rm $wkdir/temp2
endif
if (-e $wkdir/incs) then
  rm $wkdir/incs
endif
#
########## start process ############################################
#
#   Set variables use in Makefile
#
#echo "Write variables use in Makefile."
echo "#! /bin/csh"                                   >  $mkfile
echo "#"                                             >> $mkfile
echo "#   Variables use in this Makefile."           >> $mkfile
echo "#"                                             >> $mkfile
echo "F77    = $fort"                                >> $mkfile
echo "FFLAGS = $fopt"                                >> $mkfile
echo "CC     = $cc"                                  >> $mkfile
echo "CFLAGS = $copt"                                >> $mkfile
echo "HMDIR  = $hmdir"                               >> $mkfile
echo "LIB    = $lib"                                 >> $mkfile
echo "INCDIR = $incdir"                              >> $mkfile
echo "OBJDIR = $objdir"                              >> $mkfile
echo "LMN    = $lmn"                                 >> $mkfile
echo ""                                              >> $mkfile

#
#   FORTRAN object files
#
#echo "Write object files."
echo "#"                                             >> $mkfile
echo "#   Object files"                              >> $mkfile
echo "#"                                             >> $mkfile
echo "OBJS = \"                                      >> $mkfile
#
## For C-sources %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cd $ccdir
ls -1 *.c  > $wkdir/temp
cd $wkdir
set numln = `wc -l temp`
@ numln[1]--
head -$numln[1] temp | sed 's/\.c/\.o \\/g'          >> $mkfile
tail -1 temp | sed 's/\.c/\.o \\/g'                  >> $mkfile
rm $wkdir/temp
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
foreach dirnm ($srcsdir)
   cd $dirnm
   if (-e $wkdir/temp) then
     ls -1 *.f >> $wkdir/temp
   else
     ls -1 *.f  > $wkdir/temp
   endif
end
cd $wkdir
set numln = `wc -l temp`
@ numln[1]--
head -$numln[1] temp | sed 's/\.f/\.o \\/g'          >> $mkfile
tail -1 temp | sed 's/\.f/\.o/g'                     >> $mkfile
echo ""                                              >> $mkfile
rm $wkdir/temp
#
#
#   Write target and command lines
#
#echo "Write link line."
echo "#"                                             >> $mkfile
echo "#   Link object files"                         >> $mkfile
echo "#"                                             >> $mkfile
echo '$(LMN) : $(OBJS) $(LIB)'                       >> $mkfile
#echo '	$(F77) -o $@ $(FFLAGS) $(OBJS)'              >> $mkfile
echo '	$(F77) -o $@ $(OBJS)'                        >> $mkfile
echo ""                                              >> $mkfile
#echo "Write compile line."
#
## For C-sources %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
echo "#"                                             >> $mkfile
echo "#   Compile C source files"                    >> $mkfile
echo "#"                                             >> $mkfile
echo ""                                              >> $mkfile
echo "   Now directory is $ccdir."
cd $ccdir
   ls -1 *.c > $wkdir/temp
   cd $wkdir
   set fln = `cat temp`
   rm $wkdir/temp
   echo "#   $ccdir"                                 >> $mkfile
   foreach flnm ($fln)
      set obn = `basename $flnm .c`.o                #  Set object
      echo "$obn : $ccdir/$flnm"                     >> $mkfile
      echo '	$(CC) -c $(CFLAGS)'" $ccdir/$flnm"   >> $mkfile
      echo ""                                        >> $mkfile
   end
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
echo "#"                                             >> $mkfile
echo "#   Compile FORTRAN source files"              >> $mkfile
echo "#"                                             >> $mkfile
echo ""                                              >> $mkfile
foreach dirnm ($srcsdir)
   echo "   Now directory is $dirnm."
   cd $dirnm
   ls -1 *.f > $wkdir/temp
   cd $wkdir
   set fln = `cat temp`
   rm $wkdir/temp
   echo "#   $dirnm"                                 >> $mkfile
   foreach flnm ($fln)
      set obn = `basename $flnm .f`.o                #  Set object
      grep INCLUDE $dirnm/$flnm > temp               #
      sed -f $pdir/script.sed temp > temp2           #  Set include
#     set incnms = `cat temp2`                       #
      set incnms = `cat temp2 | tr -d \' `           #
      if (-e $wkdir/incs) then
        echo "$incnms" >> incs
      else
        echo "$incnms"  > incs
      endif
      echo "$obn : $dirnm/$flnm $incnms"             >> $mkfile
#----- F2C or G77 can not open include file if its path is not specifed ------
#      No space between -I and the following directory of include file
#     echo '	$(F77) -c $(FFLAGS)'" $dirnm/"'$*.f' >> $mkfile
#     echo '	$(F77) -c $(FFLAGS)'" $dirnm/$flnm"  >> $mkfile
      echo '	$(F77) -I$(OBJDIR) -c $(FFLAGS)'" $dirnm/$flnm"  >> $mkfile
      echo ""                                        >> $mkfile
      rm $wkdir/temp
      rm $wkdir/temp2
   end
end
echo ""                                              >> $mkfile

#echo "Write include copy line."
echo "#"                                             >> $mkfile
echo "#   Copy include files if it's changed."       >> $mkfile
echo "#"                                             >> $mkfile
set incnm = `cat incs`
foreach flnm ($incnm)
   if (-e $wkdir/temp) then
     echo "$flnm" >> temp
   else
     echo "$flnm"  > temp
   endif
end
set incnm = `sort $wkdir/temp | uniq`
foreach flnm ($incnm)
   echo "$flnm"' : $(INCDIR)/'"$flnm"' ; cp $? $@'   >> $mkfile
end
rm $wkdir/incs
rm $wkdir/temp
echo ""                                              >> $mkfile

#
#   End of making Makefile
#
echo "#"                                             >> $mkfile
echo "#   End of Makefile"                           >> $mkfile
echo "#"                                             >> $mkfile
#cp $mkfile $objdir/.
#echo ""
#echo "Makefile was generated in the following file :"
#echo "  $mkfile"
#echo "Copy it to the object directory : "
#echo "  $objdir"
#echo "Enter : 'make -f [makefile-name]' in the object directory"
