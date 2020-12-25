C***********************************************************************
C UTILITY PROGRAM TO WRITE CONTENTS OF PDS MEMBER                      *
C***********************************************************************

      SUBROUTINE PDSWRT(PATH,LNPATH,MEMBER,WORK,LENG,IERR)
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
C     LENG       : LENGTH OF DATA TO BE WRITTEN IN MEMBER (BYTES)
C                  = LENG/4 (WORDS)
C     WORK       : CONTENTS OF MEMBER
C-------------------------------OUTPUT----------------------------------
C     IERR       : ERROR CODE =0 : NORMAL END
C                             =4 : ALREADY MEMBER EXIST
C                             =12: OPEN ERROR
C                             =16: CLOSE ERROR
C                             =20: WRITE I/O ERROR
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
      OPEN(UNIT=IOPDS,FILE=FILNAM(1:LNFILE),ERR=100, STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='UNFORMATTED',IOSTAT=IOS)
C
                  ELSE
                  IERR = 4
                  RETURN
                  ENDIF
C
      LNOW = LENG / 4
      WRITE(UNIT=IOPDS,ERR=300,IOSTAT=IOS)  LNOW,(WORK(I),I=1,LNOW)
C
      CLOSE(UNIT=IOPDS, ERR=200, STATUS='KEEP')
C
      RETURN
C
  100 IERR = 12
C     WRITE(6,*) ' OPEN ERROR , IOSTAT=',IOS
      RETURN
  200 IERR = 16
C     WRITE(6,*) ' CLOSE ERROR , IOSTAT=',IOS
      RETURN
  300 IERR = 20
C     WRITE(6,*) ' WRITE ERROR , IOSTAT=',IOS
      RETURN
      END
