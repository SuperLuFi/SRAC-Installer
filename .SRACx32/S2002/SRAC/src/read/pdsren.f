C***********************************************************************
C UTILITY PROGRAM TO RENAME PDS MEMBER                                 *
C***********************************************************************

      SUBROUTINE PDSREN(PATH,LNPATH,MEMBER,MEMB2,IERR,LENWRK,WORK)
C
      DIMENSION WORK(LENWRK)
      CHARACTER FILNAM*80
      CHARACTER PATH  *68
      CHARACTER MEMBER*8
      CHARACTER MEMB2 *8
C
      LOGICAL   EX
C
C-------------------------------INPUT-----------------------------------
C     PATH       : PDS PATH NAME        : /XXX/XXX/MACRO
C     LNPATH     : LENGTH OF PATH NAME  : 14
C     MEMBER     : PDS MEMBER NAME      : COLDA010
C     MEMB2      : NEW PDS MEMBER NAME  : CNEWA010
C     LENWRK     : LENGTH OF WORK DIMENSION
C     WORK       : CONTENTS OF MEMBER
C-------------------------------OUTPUT----------------------------------
C     IERR       : ERROR CODE =0 : NORMAL END
C                             =4 : MEMBER NOT FOUND
C                             =8 : WORK DIMENSION OVER
C                             =12: OPEN ERROR
C                             =16: CLOSE ERROR
C                             =20: READ I/O ERROR
C                             =24: MEMB2 ALREADY EXIST
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
      READ(UNIT=IOPDS,ERR=300,IOSTAT=IOS)  LNOW
      IF(LNOW.GT.LENWRK) THEN
                         IERR =  8
                         RETURN
                         ENDIF
C
      READ(UNIT=IOPDS,ERR=300,IOSTAT=IOS)  LNOW,(WORK(I),I=1,LNOW)
C
      CLOSE(UNIT=IOPDS, ERR=200, STATUS='DELETE')
C
      JERR = 0
      CALL PDSWRT(PATH,LNPATH,MEMB2,WORK,LNOW*4,JERR)
      IF(JERR.EQ.4) THEN
                    IERR = 24
                    ELSE
                    IERR = JERR
                    ENDIF
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
C     WRITE(6,*) ' READ ERROR , IOSTAT=',IOS
      RETURN
      END
