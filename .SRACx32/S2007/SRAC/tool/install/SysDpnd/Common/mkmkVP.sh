#! /bin/csh
#  Shell Script to Make a Makefile for vector SRAC95
#  (for the system C-compiler is not necessary)
#  Shell Script for Making Makefile for VPP-5000
#
#   User input variables
#
alias cd    cd
#
######## Set input variables ######################################## 
#
#   hmdir   : top directory name of this system   (full path)
#   fort    : fortran compile driver name (e.g. frtpx)
#   fopts   : fortran scalor compile options (e.g. -Oe -sc)
#   foptv   : fortran vector compile options (e.g. -Oe)
#   lmn     : load module name                    (full path)
#   lib     : library names to be linked.
#   incdir  : directory name of include files     (full path)
#   srcsdir : directory names of source programs  (full path)
#   obsdir  : directory in which scalar objects will be stored.
#   mkfile  : file name of the generated Makefile (full path)         
#   wkdir   : working directory for this script
#             temporary files (temp,temp2,temp3,incs) will be generated.
#-------------------------------------------------------------------- 
#
  set pdir = `pwd`
  cd ../../..
  set hmdir = `pwd`
  cd $pdir
set fort    = "Fortran-Driver"
set fopts   = "Scalar_Option"
set foptv   = "Vector_Option"
set lmn     = $hmdir/bin/SRACvp.200m
set lib     =
set incdir  = $hmdir/src/inc/srac200m
set obsdir  = $hmdir/obj/SCall
set obvdir  = $hmdir/obj/VPpart
set srcsdir = ($hmdir/src/burn  $hmdir/src/cit  $hmdir/src/common \
               $hmdir/src/extnl $hmdir/src/pij  $hmdir/src/plot   \
               $hmdir/src/read  $hmdir/src/srac)
if (-e $hmdir/src/sracvp.sx) then
  set srcvdir = ($hmdir/src/citvp.sx $hmdir/src/sracvp.sx)
else
  set srcvdir = ($hmdir/src/citvp $hmdir/src/sracvp)
endif
#set mkfile  = $pdir/Makefile.org
set mkfile  = $obsdir/Makefile.org
set wkdir   = $hmdir/tmp
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
if (-e $wkdir/temp3) then
  rm $wkdir/temp3
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
echo "#   If name of this file is not Makefile,"     >> $mkfile
echo "#   Set the name to the following MKFILE."     >> $mkfile
echo "#"                                             >> $mkfile
echo "MKFILE = Makefile"                             >> $mkfile
echo "F77    = $fort"                                >> $mkfile
echo "FFLAGS = $fopts"                               >> $mkfile
echo "FFLAGV = $foptv"                               >> $mkfile
echo "HMDIR  = $hmdir"                               >> $mkfile
echo "LIB    = $lib"                                 >> $mkfile
echo "INCDIR = $incdir"                              >> $mkfile
echo "OBSDIR = $obsdir"                              >> $mkfile
echo "OBVDIR = $obvdir"                              >> $mkfile
echo "LMN    = $lmn"                                 >> $mkfile
echo ""                                              >> $mkfile

#
#   FORTRAN object files
#
#echo "Write object files."
echo "#"                                             >> $mkfile
echo "#   Object files (vector)"                     >> $mkfile
echo "#"                                             >> $mkfile
foreach dirnm ($srcvdir)
   if(-e temp == 0)then
      cd $dirnm
      ls -1 *.f > $wkdir/temp
      cd $wkdir
   else
      cd $dirnm
      ls -1 *.f >> $wkdir/temp
      cd $wkdir
   endif
end
set fln = `cat temp`
\rm temp
foreach flnm ($fln)
   if(-e temp == 0)then
      echo '$(OBVDIR)/'"$flnm" > temp
   else
      echo '$(OBVDIR)/'"$flnm" >> temp
   endif
end
sed 's/\.f/\.o/g' temp > temp2
set onm = `cat temp2`
set numln = `wc -l temp2`
\rm temp temp2
set nwd = 1
set rest =
set add1 =
set add2 =
while($nwd <= $numln[1])
   @ rest = $nwd % 3
   echo "$onm[$nwd]" > temp
   set nc = `wc -c temp`
   @ add1 = $nc[1] % 10
   @ add2 = 10 - $add1
   \rm temp
   if($rest == 0 || $nwd == $numln[1])then
      echo -n "$onm[$nwd]" >> temp2
      set n = 1
      while($n < $add2)
         echo -n " " >> temp2
         @ n++
      end
      echo " \" >> temp2
   else
      if(-e temp2 == 0)then
         echo -n "$onm[$nwd]" > temp2
         set n = 1
         while($n <= $add2)
            echo -n " " >> temp2
            @ n++
         end
      else
         echo -n "$onm[$nwd]" >> temp2
         set n = 1
         while($n <= $add2)
            echo -n " " >> temp2
            @ n++
         end
      endif
   endif
   @ nwd++
end
set numln = `wc -l temp2`
set devln =
set rstln =
@ devln = $numln[1] / 10
@ rstln = $numln[1] % 10
@ rstln--
set num = 1
while($num <= $devln)
   echo "OBJV$num = \"                               >> $mkfile
   head -9 temp2                                     >> $mkfile
   sed '1,9d' temp2 > temp
   \mv temp temp2
   head -1 temp2 | sed 's/\\//g'                     >> $mkfile
   sed '1d' temp2 > temp
   \mv temp temp2
   echo " "                                          >> $mkfile
   @ num++
end
if(-z temp2 == 0)then
   echo "OBJV$num = \"                               >> $mkfile
   head -$rstln temp2                                >> $mkfile
   tail -1 temp2 | sed 's/\\//g'                     >> $mkfile
   echo " "                                          >> $mkfile
else
   @ devln--
endif
\rm temp2
set num = 1
echo -n "OBJV = "                                    >> $mkfile
while($num <= $devln)
   @ rest = $num % 6
   if($rest == 0)then
      echo '$'"(OBJV$num) \"                         >> $mkfile
   else
      echo -n '$'"(OBJV$num) "                       >> $mkfile
   endif
   @ num++
end
echo '$'"(OBJV$num)"                                 >> $mkfile

echo "#"                                             >> $mkfile
echo "#   Object files"                              >> $mkfile
echo "#"                                             >> $mkfile
foreach dirnm ($srcsdir)
   if(-e temp == 0)then
      cd $dirnm
      ls -1 *.f > $wkdir/temp
      cd $wkdir
   else
      cd $dirnm
      ls -1 *.f >> $wkdir/temp
      cd $wkdir
   endif
end
set fln = `cat temp`
\rm temp
foreach flnm ($fln)
   set flg = 0
   foreach dirnm ($srcvdir)
      if(-e $dirnm/$flnm == 1) then
         set flg = 1
         break
      endif
   end
   if($flg == 0) then
      if(-e temp == 0)then
         echo "$flnm" > temp
      else
         echo "$flnm" >> temp
      endif
   endif
end
sort temp | uniq > temp2
sed 's/\.f/\.o/g' temp2 > temp3
set onm = `cat temp3`
set numln = `wc -l temp3`
\rm temp temp2 temp3
set nwd = 1
while($nwd <= $numln[1])
   @ rest = $nwd % 6
   echo "$onm[$nwd]" > temp
   set nc = `wc -c temp`
   @ add1 = $nc[1] % 11
   @ add2 = 11 - $add1
   \rm temp
   if($rest == 0 || $nwd == $numln[1])then
      echo -n "$onm[$nwd]" >> temp2
      set n = 1
      while($n < $add2)
         echo -n " " >> temp2
         @ n++
      end
      echo " \" >> temp2
   else
      if(-e temp2 == 0)then
         echo -n "$onm[$nwd]" > temp2
         set n = 1
         while($n <= $add2)
            echo -n " " >> temp2
            @ n++
         end
      else
         echo -n "$onm[$nwd]" >> temp2
         set n = 1
         while($n <= $add2)
            echo -n " " >> temp2
            @ n++
         end
      endif
   endif
   @ nwd++
end
set numln = `wc -l temp2`
@ devln = $numln[1] / 10
@ rstln = $numln[1] % 10
@ rstln--
set num = 1
while($num <= $devln)
   echo "OBJS$num = \"                               >> $mkfile
   head -9 temp2                                     >> $mkfile
   sed '1,9d' temp2 > temp
   \mv temp temp2
   head -1 temp2 | sed 's/\\//g'                     >> $mkfile
   sed '1d' temp2 > temp
   \mv temp temp2
   echo " "                                          >> $mkfile
   @ num++
end
if(-z temp2 == 0)then
   echo "OBJS$num = \"                               >> $mkfile
   head -$rstln temp2                                >> $mkfile
   tail -1 temp2 | sed 's/\\//g'                     >> $mkfile
   echo " "                                          >> $mkfile
else
   @ devln--
endif
\rm temp2
set num = 1
echo -n "OBJS = "                                    >> $mkfile
while($num <= $devln)
   @ rest = $num % 6
   if($rest == 0)then
      echo '$'"(OBJS$num) \"                         >> $mkfile
   else
      echo -n '$'"(OBJS$num) "                       >> $mkfile
   endif
   @ num++
end
echo '$'"(OBJS$num)"                                 >> $mkfile

#
#   Write target and command lines
#
#echo "Write link line."
echo "#"                                             >> $mkfile
echo "#   Link object files"                         >> $mkfile
echo "#"                                             >> $mkfile
echo '$(LMN) : $(OBJS) $(OBJV) $(LIB)'               >> $mkfile
echo '	$(F77) -o $@ $(FFLAGS) $(OBJS) $(OBJV)'      >> $mkfile
echo ""                                              >> $mkfile

#echo "Write compile line."
echo "#"                                             >> $mkfile
echo "#   Compile FORTRAN source files"              >> $mkfile
echo "#"                                             >> $mkfile
#echo ""                                              >> $mkfile
foreach dirnm ($srcvdir)
   echo "   Now directory is $dirnm."
   cd $dirnm
   ls -1 *.f > $wkdir/temp
   cd $wkdir
   set fln = `cat temp`
   echo "#   $dirnm"                                 >> $mkfile
   \rm temp
   foreach flnm ($fln)
      set obn = `basename $flnm .f`.o                #  Set object
      grep INCLUDE $dirnm/$flnm > temp               #
      sed -f $pdir/script.sed temp > temp2           #  Set include
#     set incnms = `cat temp2`                       #
      set incnms = `cat temp2 | tr -d \' `           #
      if(-e incs == 0)then
         \cp temp2 incs
      else
         cat incs temp2 > temp3
         \mv temp3 incs
      endif
############## Compile for vector sources
      echo '$(OBVDIR)/'"$obn : $dirnm/$flnm $incnms" >> $mkfile
      echo '	cd $(OBVDIR) ; \'                    >> $mkfile
      if($incnms == "") then
        echo '	make -f $(OBSDIR)/$(MKFILE)'" $obn"  >> $mkfile
      else
        echo '	cp $(OBSDIR)/'"$incnms"' $(OBVDIR) ; \'  >> $mkfile
        echo '	make -f $(OBSDIR)/$(MKFILE)'" $obn"  >> $mkfile
      endif
##    echo '	make -f $(OBSDIR)/'"$mkfile $obn"    >> $mkfile
      echo ""                                        >> $mkfile
      echo "$obn : $dirnm/$flnm $incnms"             >> $mkfile
##    echo '	$(F77) -c $(FFLAGS)'" $dirnm/$flnm"  >> $mkfile
      echo '	$(F77) -c $(FFLAGV)'" $dirnm/$flnm"  >> $mkfile
      echo ""                                        >> $mkfile
      \rm temp temp2
   end
end
echo ""                                              >> $mkfile

foreach dirnm ($srcsdir)
   echo "   Now directory is $dirnm."
   cd $dirnm
   ls -1 *.f > $wkdir/temp
   cd $wkdir
   set fln = `cat temp`
   foreach flnm ($fln)
      set flg = 0
      foreach dirn ($srcvdir)
         if(-e $dirn/$flnm == 1) then
            set flg = 1
            break
         endif
      end
      if($flg == 0)then
         if(-e temp2 == 0)then
            echo "$flnm" > temp2
         else
            echo "$flnm" >> temp2
         endif
      endif
   end
   \rm temp
   if (-e temp2 == 1)then
      sort temp2 | uniq > temp3
      echo "#   $dirnm"                              >> $mkfile
      set fln = `cat temp3`
      foreach flnm ($fln)
         set obn = `basename $flnm .f`.o             #  Set object
         grep INCLUDE $dirnm/$flnm > temp4           #
         sed -f $pdir/script.sed temp4 > temp5       #  Set include
#        set incnms = `cat temp5`                    #
         set incnms = `cat temp5 | tr -d \' `        #
         if(-e incs == 0)then
            \cp temp5 incs
         else
            cat incs temp5 > temp6
            \mv temp6 incs
         endif
############## Compile for scaler sources
         echo "$obn : $dirnm/$flnm $incnms"          >> $mkfile
         echo '	$(F77) -c $(FFLAGS)'" $dirnm/$flnm"  >> $mkfile
         echo ""                                     >> $mkfile
         \rm temp4 temp5
      end
      \rm temp3
   endif
   \rm temp2
end
echo ""                                              >> $mkfile

#echo "Write include copy line."
echo "#"                                             >> $mkfile
echo "#   Copy include files if it's changed."       >> $mkfile
echo "#"                                             >> $mkfile
#set incnm = `sort incs | uniq`
set incnm = `sort incs | uniq | tr -d \' `
foreach flnm ($incnm)
   echo "$flnm"' : $(INCDIR)/'"$flnm"' ; cp $? $@'   >> $mkfile
end
\rm incs
echo ""                                              >> $mkfile

#
#   End of making Makefile
#
echo "#"                                             >> $mkfile
echo "#   End of Makefile"                           >> $mkfile
echo "#"                                             >> $mkfile
#
#cp $mkfile $obsdir/.
#echo ""
#echo "Makefile was generated in the following file :"
#echo "  $mkfile"
#echo "Copy it to the scalor object directory : "
#echo "  $obsdir"
#echo "Enter : 'make -f [makefile-name]' in the object directory" 
