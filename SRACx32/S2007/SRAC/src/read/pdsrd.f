C***********************************************************************
C UTILITY PROGRAM TO READ CONTENTS OF PDS MEMBER                       *
C***********************************************************************

      SUBROUTINE PDSRD(PATH,LNPATH,MEMBER,WORK,LENG,IERR)
C
      DIMENSION WORK(1)
      CHARACTER FILNAM*80
      CHARACTER PATH  *68
      CHARACTER MEMBER*8
C
      LOGICAL   EX
C
C-------------------------------INPUT-----------------------------------
C     PATH       : PDS PATH NAME        : /XXX/XXX/MACRO
C     LNPATH     : LENGTH OF PATH NAME  : 14
C     MEMBER     : PDS MEMBER NAME      : CASEA010
C     LENG       : LENGTH OF DATA IN MEMBER (BYTES)
C-------------------------------OUTPUT----------------------------------
C     WORK       : CONTENTS OF MEMBER
C     IERR       : ERROR CODE =0 : NORMAL END
C                             =4 : MEMBER NOT FOUND
C                             =8 : DEFINE LENGTH OVER
C                             =12: OPEN ERROR
C                             =16: CLOSE ERROR
C                             =20: READ I/O ERROR
C-----------------------------------------------------------------------
C
      IERR = 0
      IOPDS = 49
C
      LENMEM = 0
      CALL LNMEMB( MEMBER , LENMEM )
      LNFILE  = LNPATH + 1 + LENMEM
      FILNAM(1:LNFILE) = PATH(1:LNPATH)//'/'//MEMBER(1:LENMEM)
C
      INQUIRE(FILE=FILNAM(1:LNFILE),EXIST=EX)
      IF(.NOT.EX) THEN
                  IERR = 4
                  RETURN
C
                  ELSE
      OPEN(UNIT=IOPDS,FILE=FILNAM(1:LNFILE),ERR=100, STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='UNFORMATTED',IOSTAT=IOS)
                  ENDIF
C
      READ(UNIT=IOPDS,ERR=300,IOSTAT=IOS)  LNOW,(WORK(I),I=1,LNOW)
C
      CLOSE(UNIT=IOPDS, ERR=200, STATUS='KEEP')
C
      IF(LENG.GT.LNOW*4) IERR =  8
      RETURN
C
  100 IERR = 12
C     WRITE(6,*) ' OPEN ERROR , IOSTAT=',IOS
      RETURN
  200 IERR = 16
C     WRITE(6,*) ' CLOSE ERROR , IOSTAT=',IOS
      RETURN
  300 IERR = 20
C     WRITE(6,*) ' READ ERROR , IOSTAT=',IOS
      RETURN
      END
