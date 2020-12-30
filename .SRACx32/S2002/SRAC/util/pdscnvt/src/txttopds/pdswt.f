C***********************************************************************
C UTILITY PROGRAM TO WRITE CONTENTS OF PDS MEMBER                      *
C***********************************************************************

      SUBROUTINE PDSWT(FILNAM,WORK,LENG,IERR)
C
      DIMENSION WORK(1)
      CHARACTER FILNAM*81
C
C-------------------------------INPUT-----------------------------------
C     FILNAM     : FULL PDS MEMBER NAME : /XXX/XXX/MACRO/CASEA010
C     WORK       : CONTENTS OF MEMBER
C     LENG       : LENGTH OF DATA IN MEMBER (EXCEPT LENG ITSELF)
C-------------------------------OUTPUT----------------------------------
C     IERR       : ERROR CODE =0 : NORMAL END
C                             =1 : OPEN ERROR
C                             =2 : CLOSE ERROR
C                             =3 : WRITE ERROR
C-----------------------------------------------------------------------
C
      IERR = 0
      IOPDS = 49
C
      OPEN(UNIT=IOPDS, FILE=FILNAM, ERR=100, STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='UNFORMATTED',IOSTAT=IOS)
      WRITE(UNIT=IOPDS,ERR=300,IOSTAT=IOS)  LENG,(WORK(I),I=1,LENG)
      CLOSE(UNIT=IOPDS, ERR=200, STATUS='KEEP',IOSTAT=IOS)
      RETURN
  100 IERR = 1
C     WRITE(6,*) ' OPEN ERROR , IOSTAT=',IOS
      RETURN
  200 IERR = 2
C     WRITE(6,*) ' CLOSE ERROR , IOSTAT=',IOS
      RETURN
  300 IERR = 3
C     WRITE(6,*) ' WRITE ERROR , IOSTAT=',IOS
      RETURN
      END
