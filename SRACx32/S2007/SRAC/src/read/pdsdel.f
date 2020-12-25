C***********************************************************************
C UTILITY PROGRAM TO DELETE CONTENTS OF PDS MEMBER                     *
C***********************************************************************

      SUBROUTINE PDSDEL(PATH,LNPATH,MEMBER,IERR)
C
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
C-------------------------------OUTPUT----------------------------------
C     IERR       : ERROR CODE =0 : NORMAL END
C                             =8 : MEMBER NOT FOUND
C                             =12: OPEN ERROR
C                             =16: CLOSE ERROR
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
                  IERR = 8
                  RETURN
C
                  ELSE
      OPEN(UNIT=IOPDS,FILE=FILNAM(1:LNFILE),ERR=100, STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='UNFORMATTED',IOSTAT=IOS)
                  ENDIF
C
      CLOSE(UNIT=IOPDS, ERR=200, STATUS='DELETE')
C
      RETURN
C
  100 IERR = 12
C     WRITE(6,*) ' OPEN ERROR , IOSTAT=',IOS
      RETURN
  200 IERR = 16
C     WRITE(6,*) ' CLOSE ERROR , IOSTAT=',IOS
      RETURN
      END
