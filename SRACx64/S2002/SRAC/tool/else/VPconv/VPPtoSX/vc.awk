#::::::::::::::
#vc.awk
#        Change Vector Control Statements 
#        FACOM FORTRAN77/EX ==> Monte-4(bec-sx) FORTRAN77/SX
#        *VOCL *,*  ==> *vdir *
#
#::::::::::::::
{
  if($1 =="*VOCL"){
    if(substr($2,6,6)=="SCALAR"){
      print "*vdir novector"
     }
    if(substr($2,6,6)=="NOVREC"){
       if(length($2)==11){
          print "*vdir nodep"
       }
       else{
          print "*vdir nodep("substr($2,13,5)","substr($2,19,5)")"
       }
     }
    if(substr($2,6,6)=="REPEAT"){
      print "*vdir loopcnt="substr($2,13,2)
     }
   }
  else{
      print
    }
}
