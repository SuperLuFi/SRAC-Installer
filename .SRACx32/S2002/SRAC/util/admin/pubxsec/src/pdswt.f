C***********************************************************************
C UTILITY PROGRAM TO WRITE CONTENTS OF PDS MEMBER                      *
C***********************************************************************
C
      SUBROUTINE PDSWT(FILNAM,WORK,LENG,IERR)
C
      COMMON /PDSDEV/ IOPDS
      DIMENSION WORK(*)
      CHARACTER FILNAM*129
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
      IF(IOPDS.LE.0) IOPDS = 41
C
      IF(LENG.LE.0) THEN
        IERR = 3
        RETURN
      ENDIF
      LN = INDEX(FILNAM,' ') - 1 
      OPEN(UNIT=IOPDS, FILE=FILNAM(1:LN), ERR=100, STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='UNFORMATTED',IOSTAT=IOS)
      WRITE(UNIT=IOPDS,ERR=300,IOSTAT=IOS)  LENG,(WORK(I),I=1,LENG)
      CLOSE(UNIT=IOPDS, ERR=200, STATUS='KEEP',IOSTAT=IOS)
      RETURN
  100 IERR = 1
      RETURN
  200 IERR = 2
      RETURN
  300 IERR = 3
      RETURN
      END
