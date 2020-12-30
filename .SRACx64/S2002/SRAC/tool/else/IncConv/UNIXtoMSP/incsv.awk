#::::::::::::::
#incsv.awk
#
#  UNIX ==> MSP
#         include 'filename'==> *INCLUDE filename
#
#::::::::::::::
{
  if($1 =="include"){
    if(substr($2,1,1)=="'"&&substr($2,length($2),length($2))=="'")
      {
      print "*"$1"  "substr($2,2,length($2)-2)
      }
    else
      {
      print "*"$1"  "$2
      }
  }
  else{
  if($1 =="INCLUDE"){
    if(substr($2,1,1)=="'"&&substr($2,length($2),length($2))=="'")
      {
      print "*"$1"  "substr($2,2,length($2)-2)
      }
    else
      {
      print "*"$1"  "$2
      }
  }
  else{
      print
    }
  }
}
