######################################################################  
#    Makefile to produce load module for pdsmdl(pds access program)
#    main : FluxPlot : prepare plot tables for neutron spectrum by R-Region 
#                      or X-Region from FLUX file
#
SRAC_DIR = $(SRAC_CODE)
#
#  Note : If this is not executed by @PunchMe, set the full path name         
#         of the SRAC code file in the following statement, and use it instead
#         of the above statement.                                             
#
#SRAC_DIR = $(HOME)/SRAC
######################################################################  
#
#--------------- Set by User --------------------------------------------------
#  MAIN : main program name, which must be same to the directory name
MAIN  = FluxPlot
#-------------------------------------------------------------------------------

F77   = g77
OPT   =    
DIR   = $(SRAC_DIR)/util/pdsmdl
MDL   = $(DIR)/modl
SRC   = $(DIR)/main/$(MAIN)
WKO   = $(DIR)/obj
LMD   = $(DIR)/bin
 
EXT   = $(SRAC_DIR)/src/extnl

#--------------- Set Necessary Module -----------------------------------------
 
LMN   = $(LMD)/$(MAIN).out
OBJ   = $(WKO)/$(MAIN).o  \
$(WKO)/engedt.o  $(WKO)/flxedt.o  \
$(WKO)/pdsin.o  $(WKO)/pdsnm.o  $(WKO)/pdsrd.o  $(WKO)/pdssr.o  \
$(WKO)/pdswt.o  $(WKO)/uioset.o  $(WKO)/uioint.o $(WKO)/ugtenv.o

#---------------------- Make Private Load Module ------------------------------

$(LMN):			$(OBJ)
			cd $(WKO); $(F77) -o $(LMN) $(OPT) $(OBJ)

$(WKO)/$(MAIN).o:	$(SRC)/$(MAIN).f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/$(MAIN).f

$(MDL)/PARAMINC:	$(SRC)/PARAMINC
			cp $(SRC)/PARAMINC $(MDL)/PARAMINC

#---------------------- pdsmdl ------------------------------------------------

$(WKO)/bnpedt.o:	$(MDL)/bnpedt.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/bnpedt.f

$(WKO)/bnpwrt.o:	$(MDL)/bnpwrt.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/bnpwrt.f

$(WKO)/dlyedt.o:	$(MDL)/dlyedt.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/dlyedt.f

$(WKO)/dntedt.o:	$(MDL)/dntedt.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/dntedt.f

$(WKO)/dntwrt.o:	$(MDL)/dntwrt.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/dntwrt.f

$(WKO)/engedt.o:	$(MDL)/engedt.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/engedt.f

$(WKO)/engwrt.o:	$(MDL)/engwrt.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/engwrt.f

$(WKO)/fastlb.o:	$(MDL)/fastlb.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/fastlb.f

$(WKO)/flxedt.o:	$(MDL)/flxedt.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/flxedt.f

$(WKO)/flxwrt.o:	$(MDL)/flxwrt.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/flxwrt.f

$(WKO)/macedt.o:	$(MDL)/macedt.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/macedt.f

$(WKO)/macwrt.o:	$(MDL)/macwrt.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/macwrt.f

$(WKO)/micedt.o:	$(MDL)/micedt.f  $(MDL)/PARAMINC
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/micedt.f

$(WKO)/pdsin.o:		$(MDL)/pdsin.f
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/pdsin.f

$(WKO)/pdsln.o:		$(MDL)/pdsln.f
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/pdsln.f

$(WKO)/pdsnm.o:		$(MDL)/pdsnm.f
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/pdsnm.f

$(WKO)/pdsrd.o:		$(MDL)/pdsrd.f
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/pdsrd.f

$(WKO)/pdssr.o:		$(MDL)/pdssr.f
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/pdssr.f

$(WKO)/pdswt.o:		$(MDL)/pdswt.f
			cd $(WKO); $(F77) -c $(OPT) $(MDL)/pdswt.f

$(WKO)/uioset.o:	$(EXT)/uioset.f
			cd $(WKO); $(F77) -c $(OPT) $(EXT)/uioset.f

$(WKO)/uioint.o:	$(EXT)/uioint.f
			cd $(WKO); $(F77) -c $(OPT) $(EXT)/uioint.f

$(WKO)/ugtenv.o:	$(EXT)/ugtenv.f
			cd $(WKO); $(F77) -c $(OPT) $(EXT)/ugtenv.f
