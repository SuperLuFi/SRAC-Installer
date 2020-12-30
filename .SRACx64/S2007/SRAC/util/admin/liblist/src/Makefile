######################################################################  
#    Makefile to produce load module for pdsmdl(pds access program)
#    main : liblist   : print library information
#
SRAC_DIR = $(HOME)/SRAC2K6/SRAC
######################################################################  
#
#--------------- Set by User --------------------------------------------------

F77   = f77
OPT   =
DIR   = $(SRAC_DIR)/util/admin
SRC   = $(DIR)/liblist/src
WKO   = $(DIR)/liblist/obj
LMD   = $(DIR)/bin

EXT   = $(SRAC_DIR)/src/extnl

#--------------- Set Necessary Module -----------------------------------------
LMN   = $(LMD)/liblist.exe
OBJ   = $(WKO)/main000.o  \
$(WKO)/clea.o       \
$(WKO)/dpread.o     \
$(WKO)/getdir.o     \
$(WKO)/tlib.o       \
$(WKO)/pdser.o      \
$(WKO)/pdsin.o      \
$(WKO)/pdsln.o      \
$(WKO)/pdsnm.o      \
$(WKO)/pdsrd.o      \
$(WKO)/pdssr.o      \
$(WKO)/uiount.o     \
$(WKO)/uioset.o     \
$(WKO)/uioint.o     \
$(WKO)/ugtenv.o


#---------------------- Make Private Load Module ------------------------------

$(LMN):			$(OBJ)
			cd $(WKO); $(F77) -o $(LMN) $(OPT) $(OBJ)

#---------------------- liblist -----------------------------------------------

$(WKO)/main000.o:	$(SRC)/main000.f
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/main000.f

$(WKO)/tlib.o:		$(SRC)/tlib.f
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/tlib.f

$(WKO)/dpread.o:	$(SRC)/dpread.f
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/dpread.f

$(WKO)/getdir.o:	$(SRC)/getdir.f
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/getdir.f

$(WKO)/uiount.o:	$(SRC)/uiount.f
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/uiount.f

$(WKO)/clea.o:		$(SRC)/clea.f
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/clea.f

#---------------------- pdsmdl ------------------------------------------------

$(WKO)/pdsin.o:		$(SRC)/pdsin.f
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/pdsin.f

$(WKO)/pdsln.o:		$(SRC)/pdsln.f
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/pdsln.f

$(WKO)/pdsnm.o:		$(SRC)/pdsnm.f
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/pdsnm.f

$(WKO)/pdsrd.o:		$(SRC)/pdsrd.f
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/pdsrd.f

$(WKO)/pdssr.o:		$(SRC)/pdssr.f
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/pdssr.f

$(WKO)/pdser.o:		$(SRC)/pdser.f
			cd $(WKO); $(F77) -c $(OPT) $(SRC)/pdser.f

#---------------------- system dependent --------------------------------------

$(WKO)/uioset.o:	$(EXT)/uioset.f
			cd $(WKO); $(F77) -c $(OPT) $(EXT)/uioset.f

$(WKO)/uioint.o:	$(EXT)/uioint.f
			cd $(WKO); $(F77) -c $(OPT) $(EXT)/uioint.f

$(WKO)/ugtenv.o:	$(EXT)/ugtenv.f
			cd $(WKO); $(F77) -c $(OPT) $(EXT)/ugtenv.f
