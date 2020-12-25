C***********************************************************************
C UTILITY PROGRAM TO READ CONTENTS OF PDS MEMBER                       *
C***********************************************************************

      SUBROUTINE PDSRD(FILNAM,WORK,LENG,IERR)
C
      DIMENSION WORK(1)
      CHARACTER FILNAM*81
C
C-------------------------------INPUT-----------------------------------
C     FILNAM     : FULL PDS MEMBER NAME : /XXX/XXX/MACRO/CASEA010
C-------------------------------OUTPUT----------------------------------
C     WORK       : CONTENTS OF MEMBER
C     LENG       : LENGTH OF DATA IN MEMBER (EXCEPT LENG ITSELF)
C     IERR       : ERROR CODE =0 : NORMAL END
C                             =1 : OPEN ERROR
C                             =2 : CLOSE ERROR
C                             =3 : READ ERROR
C-----------------------------------------------------------------------
C
      IERR = 0
      IOPDS = 49
C
      OPEN(UNIT=IOPDS, FILE=FILNAM, ERR=100, STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='UNFORMATTED',IOSTAT=IOS)
      READ(UNIT=IOPDS,ERR=300,IOSTAT=IOS)  LENG,(WORK(I),I=1,LENG)
      CLOSE(UNIT=IOPDS, ERR=200, STATUS='KEEP',IOSTAT=IOS)
      RETURN
  100 IERR = 1
      WRITE(6,*) ' OPEN ERROR , IOSTAT=',IOS
      RETURN
  200 IERR = 2
      WRITE(6,*) ' CLOSE ERROR , IOSTAT=',IOS
      RETURN
  300 IERR = 3
      WRITE(6,*) ' READ ERROR , IOSTAT=',IOS
      RETURN
      END
