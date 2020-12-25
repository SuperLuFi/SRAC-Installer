C***********************************************************************
C UTILITY PROGRAM TO READ CONTENTS OF PDS MEMBER                       *
C***********************************************************************
C
      SUBROUTINE PDSRD(FILNAM,WORK,LENG,IERR)
C
      DIMENSION WORK(*)
      CHARACTER FILNAM*129
C
C-------------------------------INPUT-----------------------------------
C     FILNAM     : FULL PDS MEMBER NAME : /XXX/XXX/MACRO/CASEA010
C     LENG       : >,= 0 : READ ALL DATA
C                  <   0 : READ ONLY SPECIFIED LENGTH OF DATA
C-------------------------------OUTPUT----------------------------------
C     WORK       : CONTENTS OF MEMBER
C     LENG       : LENGTH OF ALL DATA IN MEMBER (EXCEPT LENG ITSELF)
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
      IF (LENG.GE.0) THEN
        READ(UNIT=IOPDS,ERR=300,IOSTAT=IOS)  LENG,(WORK(I),I=1,LENG)
      ELSE
        LENG = IABS(LENG)
        READ(UNIT=IOPDS,ERR=300,IOSTAT=IOS)  LDUM,(WORK(I),I=1,LENG)
        LENG = LDUM
      ENDIF
      CLOSE(UNIT=IOPDS, ERR=200, STATUS='KEEP',IOSTAT=IOS)
      RETURN
  100 IERR = 1
      RETURN
  200 IERR = 2
      RETURN
  300 IERR = 3
      RETURN
      END
