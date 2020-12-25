#
#  Makefile to produce load module for pubxsec(make plot data of Public Libs)
#  by K.Okumura (JAERI)
#
SRAC_DIR = $(HOME)/SRAC2K6/SRAC
#
#------------------------------------------------------------------------------
F77    = f77
FFLAGS = 
DIR   = $(SRAC_DIR)/util/admin
SRC   = $(DIR)/pubxsec/src
OBD   = $(DIR)/pubxsec/obj
LMD   = $(DIR)/bin

PON   = $(SRC)
EXT   = $(SRAC_DIR)/src/extnl

#------------------------------------------------------------------------------
 
LM    = $(LMD)/pubxsec.exe
OBJ   = $(OBD)/main000.o  $(OBD)/logopr.o                            \
$(OBD)/zerocl.o $(OBD)/getdir.o  $(OBD)/rfast.o   $(OBD)/rthml.o     \
$(OBD)/rthml2.o $(OBD)/concat.o   $(OBD)/uiount.o                    \
$(OBD)/pdser.o  $(OBD)/pdsot.o   $(OBD)/pdssr.o   $(OBD)/pondel.o    \
$(OBD)/ponred.o $(OBD)/ponwtv.o  $(OBD)/pdsin.o   $(OBD)/pdsrd.o     \
$(OBD)/pdswt.o  $(OBD)/pondmp.o  $(OBD)/ponsrc.o  $(OBD)/packrd.o    \
$(OBD)/pdsln.o  $(OBD)/pdsrm.o   $(OBD)/poncla.o  $(OBD)/pongst.o    \
$(OBD)/pontxt.o $(OBD)/packwt.o  $(OBD)/pdsnm.o   $(OBD)/pdssc.o     \
$(OBD)/poncmp.o $(OBD)/ponpon.o  $(OBD)/ponwtp.o                     \
$(OBD)/uioint.o $(OBD)/ugtenv.o  $(OBD)/uioset.o

$(LM):			$(OBJ)
			$(F77) -o $(LM) $(FFLAGS) $(OBJ)

#---------------------- source ------------------------------------------------

$(OBD)/main000.o:	$(SRC)/main000.f  $(OBD)/INCPDS  $(OBD)/INCMAX
			cd $(OBD); $(F77) -c $(FFLAGS) $(SRC)/main000.f

$(OBD)/logopr.o:	$(SRC)/logopr.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(SRC)/logopr.f

$(OBD)/zerocl.o:	$(SRC)/zerocl.f   $(OBD)/INCMAX
			cd $(OBD); $(F77) -c $(FFLAGS) $(SRC)/zerocl.f

$(OBD)/getdir.o:	$(SRC)/getdir.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(SRC)/getdir.f

$(OBD)/rfast.o:		$(SRC)/rfast.f   $(OBD)/INCMAX
			cd $(OBD); $(F77) -c $(FFLAGS) $(SRC)/rfast.f

$(OBD)/rthml.o:		$(SRC)/rthml.f   $(OBD)/INCMAX
			cd $(OBD); $(F77) -c $(FFLAGS) $(SRC)/rthml.f

$(OBD)/rthml2.o:	$(SRC)/rthml2.f   $(OBD)/INCMAX
			cd $(OBD); $(F77) -c $(FFLAGS) $(SRC)/rthml2.f

$(OBD)/concat.o:	$(SRC)/concat.f  $(OBD)/INCMAX
			cd $(OBD); $(F77) -c $(FFLAGS) $(SRC)/concat.f

$(OBD)/uiount.o:	$(SRC)/uiount.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(SRC)/uiount.f

#---- PONPON ------------------------------------------------------------

$(OBD)/packrd.o:	$(PON)/packrd.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/packrd.f

$(OBD)/packwt.o:	$(PON)/packwt.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/packwt.f

$(OBD)/pdser.o:		$(PON)/pdser.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pdser.f

$(OBD)/pdsin.o:		$(PON)/pdsin.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pdsin.f

$(OBD)/pdsln.o:		$(PON)/pdsln.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pdsln.f

$(OBD)/pdsnm.o:		$(PON)/pdsnm.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pdsnm.f

$(OBD)/pdsot.o:		$(PON)/pdsot.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pdsot.f

$(OBD)/pdsrd.o:		$(PON)/pdsrd.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pdsrd.f

$(OBD)/pdsrm.o:		$(PON)/pdsrm.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pdsrm.f

$(OBD)/pdssc.o:		$(PON)/pdssc.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pdssc.f

$(OBD)/pdssr.o:		$(PON)/pdssr.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pdssr.f

$(OBD)/pdswt.o:		$(PON)/pdswt.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pdswt.f

$(OBD)/poncla.o:	$(PON)/poncla.f   $(OBD)/INCPDS
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/poncla.f

$(OBD)/poncmp.o:	$(PON)/poncmp.f   $(OBD)/INCPDS
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/poncmp.f

$(OBD)/pondel.o:	$(PON)/pondel.f   $(OBD)/INCPDS
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pondel.f

$(OBD)/pondmp.o:	$(PON)/pondmp.f   $(OBD)/INCPDS
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pondmp.f

$(OBD)/pongst.o:	$(PON)/pongst.f   $(OBD)/INCPDS
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pongst.f

$(OBD)/ponpon.o:	$(PON)/ponpon.f   $(OBD)/INCPDS
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/ponpon.f

$(OBD)/ponred.o:	$(PON)/ponred.f   $(OBD)/INCPDS
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/ponred.f

$(OBD)/ponsrc.o:	$(PON)/ponsrc.f   $(OBD)/INCPDS
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/ponsrc.f

$(OBD)/pontxt.o:	$(PON)/pontxt.f   $(OBD)/INCPDS
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/pontxt.f

$(OBD)/ponwtp.o:	$(PON)/ponwtp.f   $(OBD)/INCPDS
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/ponwtp.f

$(OBD)/ponwtv.o:	$(PON)/ponwtv.f   $(OBD)/INCPDS
			cd $(OBD); $(F77) -c $(FFLAGS) $(PON)/ponwtv.f

#---- SYSTEM DEPENDENT SOURCES-------------------------------------------

$(OBD)/ugtenv.o:	$(EXT)/ugtenv.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(EXT)/ugtenv.f

$(OBD)/uioint.o:	$(EXT)/uioint.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(EXT)/uioint.f

$(OBD)/uioset.o:	$(EXT)/uioset.f
			cd $(OBD); $(F77) -c $(FFLAGS) $(EXT)/uioset.f

#
#   Copy latest include files to object directory
$(OBD)/INCPDS : $(SRC)/INCPDS ; cp $(SRC)/INCPDS $(OBD)/INCPDS
$(OBD)/INCMAX : $(SRC)/INCMAX ; cp $(SRC)/INCMAX $(OBD)/INCMAX
#
#   End of Makefile
#
