#This is the template file of the command (sed) to change 
#Fortran complie driver and its options. (By Keisuke OKUMURA)
# for Makefile    
 {
  s/Fortran-Driver/g77/
  s/Scalar_Option/-fno-automatic -finit-local-zero -O2 -funroll-loops/
  s/Vector_Option/tmp3/
 }
