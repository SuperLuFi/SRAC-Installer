#::::::::::::::
#incvs.awk
#
#   MSP  ==> UNIX
#    *INCLUDE filename      ==>      include 'filename'
#
#::::::::::::::
{
  if($1 =="*include"){
    print "      include  '"$2"'"
    }
  else{
    if($1 =="*INCLUDE"){
      print "      INCLUDE  '"$2"'"
      }
    else{
      print
    }
  }
}
