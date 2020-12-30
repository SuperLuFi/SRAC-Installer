C***********************************************************************
      SUBROUTINE PONTXT( FUNC,SUBNAM,IPDS,MEMBER,RENAME,IOTXT,IPRN,IERR)
C***********************************************************************
C   TXTPON : ADMINISTRATOR OF MANY TEXT FILES
C   SEVERAL MODULAR CODES CAN BE INTEGRATED BY PONPON AND PONTXT
C   VERSION:1.00 PROGRAMED BY KEISUKE OKUMURA(JAERI), IN MAY/2000
C   LAST MODIFIED:(18 MAY 2000)
C***********************************************************************
C        DEFINITION  PDS    : DIRECTORY TO STORE MEMBERS
C                    MEMBER : FILE NAMED WITH 8 CHARCTERS IN PDS
C        PONTXT USES PDSNM & PDSSR
C***********************************************************************
C
C(I/O : INPUT/OUTPUT)
C
C  I  FUNC  : SELLECTION OF FUNCTION (A4)
C    
C   / i:essential input / (i):not essential/ -: no meaning / o:output /
C
C     = SRCH  SERACH
C                    FUNC,SUBNAM,IPDS,MEMBER,RENAME,IOTXT,IPRN,IERR
C                     i    (i)     i    i       -     -    (i)   o
C     = OPEN  OPEN
C                    FUNC,SUBNAM,IPDS,MEMBER,RENAME,IOTXT,IPRN,IERR
C                     i    (i)     i    i       -     i    (i)   -
C                    if opened file is opened, it means rewinding.
C     = CLSE  CLOSE
C                    FUNC,SUBNAM,IPDS,MEMBER,RENAME,IOTXT,IPRN,IERR
C                     i    (i)    (i)  (i)      -     i    (i)   -
C                    if closed file is closed, it means nothing.
C     = REWD  REWIND
C                    FUNC,SUBNAM,IPDS,MEMBER,RENAME,IOTXT,IPRN,IERR
C                     i    (i)    (i)  (i)      -     i    (i)   -
C     = DELT  DELETE (open and close)
C                    FUNC,SUBNAM,IPDS,MEMBER,RENAME,IOTXT,IPRN,IERR
C                     i    (i)     i    i       -     -    (i)   -
C     = COPY  COPY          (open2 & read & write & close2)
C                    FUNC,SUBNAM,IPDS,MEMBER,RENAME,IOTXT,IPRN,IERR
C                     i    (i)     i    i       i     -    (i)   -
C     = RENM  RENAME=MOVE   (open2 & read & write & close2)
C                    FUNC,SUBNAM,IPDS,MEMBER,RENAME,IOTXT,IPRN,IERR
C                     i    (i)     i    i       i     -    (i)   -
C
C     [RESTRICTIONS]
C     -COPY/RENM CAN BE DONE FOR THE MEMBERS IN THE SAME PDS DIRECTORY
C     -MAXIMUM TEXT LENGTH FOR COPY/RENM IS 150
C-----------------------------------------------------------------------
C
C  I  SUBNAM  SUBROUTINE NAME(A8) WHICH CALLS THIS SYSTEM
C             (USED ONLY FOR ERROR AND CAUTION MESSAGES)
C  I  IPDS    PDS NUMBER CORRESPONDING (INCLUDING) A MEMBER
C             (NOT EFFECTIVE IN CLSE/REWD BUT USED FOR PRINT OPTION)
C  I  MEMBER  MEMBER NAME(A8)
C             (NOT EFFECTIVE IN CLSE/REWD BUT USED FOR PRINT OPTION)
C  I  RENAME  NEW MEMBER NAME(A8)
C             (EFFECTIVE ONLY WHEN FUNC=COPY/RENM, OTHERWISE SET DUMMY)
C  I  IOTXT   DEVICE NUMBER FOR OPEN/CLOSE/REWIND MODE
C  I  IPRN    =0 : NO PRINT EXCEPT ERROR MESSAGES (DEVICE:IOMSG)
C             =1 : PRINT MEMBER ACCESS INFORMATION FOR
C                  DELETE/COPY/RENAME
C                  NO PRINT FOR SEARCH/OPEN/CLOSE/REWIND/
C             >1 : PRINT MEMBER ACCESS INFORMATION FOR ALL MODE
C  O  IERR    ERROR CODE DEPENDING ON FUNC
C             <SEARCH MODE>
C             =0 : MEMBER IS EXISTING / =1 : NOT EXISTING
C             <OTHER MODES>
C             =0  (OR, STOPPED HERE)
C
C***********************************************************************
C     BEFORE USING PONTXT,
C         /USPDSI/ IOMSG, (IOVPDS), IOTMP1, IOTMP2
C         default    6       41       42      43
C         /USPDSC/ PDSDIR,(MOACS), (MOSTY)
C     MUST DE DEFINED IN USERS PROGRAMS
C
C  PDSDIR(J)  DIRECTORY(=PATH) NAME(<A120) OF J-TH PDS
C             EX:/HOME/OKUMURA/TEXT
C  IOMSG      PRINT DEVICE FOR ERROR OR ACCESS INFORMATION MESSAGES
C  IOVPDS     NOT USED (USED IN PONPON)
C  IOTMP1     WORK DEVICE FOR DELETE/COPY/MOVE MODE
C  IOTMP2     WORK DEVICE FOR COPY/MOVE MODE
C  (OTHERS ARE USED IN PONPON)
C
C--------1---------2---------3---------4---------5---------6---------7--
C *START*
C
C---- THE FOLLOWING COMMON IS NECESSARY IN USERS PROGRAMS
C
      INCLUDE 'INCPDS'
      CHARACTER        PDSDIR*120
      COMMON  /USPDSC/ PDSDIR(MXPDS)
      COMMON  /USPDSI/ IOMSG, IOVPDS, IOTMP1, IOTMP2
C
      CHARACTER*8      MEMBER, RENAME
      CHARACTER*8      SUBNAM
      CHARACTER*4      FUNC
      CHARACTER*129    FILNAM, NEWNAM
      CHARACTER*150    LINE
C
C----- SAMPLE FOR RENAMING ---------------------------------------------
C     FUNC      = 'RENM'
C     SUBNAM    = 'TEST    '
C     IPDS      = 1
C     MEMBER    = 'TEXT1000'
C     RENAME    = 'TEXT2000'
C     IOTXT     = 0
C     IPRN      = 1
C     PDSDIR(1) = 'PDS'
C***********************************************************************
C GENERAL CHECKING OF INPUT DATA
C***********************************************************************
C
      IF(IOMSG .LE.0 .OR. IOMSG .GE.100) IOMSG =6
      IF(IOTMP1.LE.0 .OR. IOTMP1.GE.100) IOTMP1=42
      IF(IOTMP2.LE.0 .OR. IOTMP2.GE.100) IOTMP2=43
C
      IF ( IPDS.LE.0 .OR. IPDS.GT.MXPDS) THEN
        WRITE(IOMSG,600) SUBNAM, IPDS, FUNC
        STOP 999
      ENDIF
C
C---- SET FULL FILE NAME : /HOME/OKUMURA/PDS/CASE0000
C
      CALL PDSNM(PDSDIR(IPDS),MEMBER,FILNAM,NLENG,IERR)
      IF(IERR.EQ.1) THEN
        WRITE(IOMSG,601) SUBNAM, FUNC
        STOP 999
      ELSEIF(IERR.EQ.2) THEN
        WRITE(IOMSG,602) SUBNAM, FUNC
        STOP 999
      ELSEIF(IERR.EQ.3) THEN
        WRITE(IOMSG,603) SUBNAM, FILNAM
        STOP 999
      ENDIF
C
C***********************************************************************
C
      MOVE  = 0
      IF(FUNC.EQ.'SRCH')  GOTO 1000
      IF(FUNC.EQ.'OPEN')  GOTO 2000
      IF(FUNC.EQ.'CLSE')  GOTO 3000
      IF(FUNC.EQ.'REWD')  GOTO 4000
      IF(FUNC.EQ.'DELT')  GOTO 5000
      IF(FUNC.EQ.'COPY')  GOTO 6000
      IF(FUNC.EQ.'RENM')  THEN
        MOVE  = 1
        GOTO 6000
      ENDIF
      WRITE(IOMSG,699) SUBNAM, FUNC
      STOP 999
C
C***********************************************************************
C (SRCH) SEARCH A MEMBER FROM IPDS-TH PDS
C***********************************************************************
C
 1000 CONTINUE
      CALL PDSSR(FILNAM,IERR)
C     IERR = 0  EXIST
C          = 1  NOT EXIST
      IF(IPRN.GE.2) THEN
        IF (IERR.EQ.0) THEN
          WRITE(IOMSG,9011) FILNAM(1:NLENG)
        ELSE
          WRITE(IOMSG,9012) FILNAM(1:NLENG)
        ENDIF
      ENDIF
      GOTO 9999
C
C***********************************************************************
C (OPEN) OPEN A TEXT MEMBER IN IPDS-TH PDS
C***********************************************************************
C
 2000 CONTINUE
      OPEN(UNIT=IOTXT, FILE=FILNAM(1:NLENG), STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='FORMATTED',IOSTAT=IOS,ERR=2010)
      IF(IPRN.GE.2) THEN
        WRITE(IOMSG,9021) FILNAM(1:NLENG)
      ENDIF
      IERR = 0
      GOTO 9999
 2010 WRITE(IOMSG,621) SUBNAM, FILNAM(1:NLENG)
      STOP 999
C
C***********************************************************************
C (CLSE) CLOSE A TEXT MEMBER IN IPDS-TH PDS
C***********************************************************************
C
 3000 CONTINUE
      CLOSE(UNIT=IOTXT, STATUS='KEEP',IOSTAT=IOS, ERR=3010)
      IF(IPRN.GE.2) THEN
        WRITE(IOMSG,9031) FILNAM(1:NLENG)
      ENDIF
      IERR = 0
      GOTO 9999
 3010 WRITE(IOMSG,631) SUBNAM, FILNAM(1:NLENG)
      STOP 999
C
C***********************************************************************
C (REWD) REWIND A TEXT MEMBER IN IPDS-TH PDS
C***********************************************************************
C
 4000 CONTINUE
      REWIND IOTXT
      IF(IPRN.GE.2) THEN
        WRITE(IOMSG,9041) FILNAM(1:NLENG)
      ENDIF
      IERR = 0
      GOTO 9999
C
C***********************************************************************
C (DELT) DELETE A TEXT MEMBER IN IPDS-TH PDS
C***********************************************************************
C
 5000 CONTINUE
      CALL PDSSR(FILNAM,IERR)
C     IERR = 0  EXIST
C          = 1  NOT EXIST (SAME AS DELETED)
      IF(IERR.EQ.1) THEN
        IERR = 0
        GOTO 9999
      ENDIF
C
      OPEN(UNIT=IOTMP1, FILE=FILNAM(1:NLENG), ERR=5010, IOSTAT=IOS)
      CLOSE(UNIT=IOTMP1, ERR=5020, STATUS='DELETE',IOSTAT=IOS)
      IF(IPRN.GE.1) THEN
        WRITE(IOMSG,9051) FILNAM(1:NLENG)
      ENDIF
      IERR = 0
      GOTO 9999
 5010 WRITE(IOMSG,651) SUBNAM, FILNAM(1:NLENG)
      STOP 999
 5020 WRITE(IOMSG,652) SUBNAM, FILNAM(1:NLENG)
      STOP 999
C
C***********************************************************************
C (COPY) MOVE=0
C        COPY A TEXT MEMBER(MEMBER) TO NEW MEMBER NAME(RENAME)
C        STOP IF NEWLY NAMED MEMBER IS ALREADY EXISTING
C        NOTE: TIME CONSUMING FOR LARGE TEXT FILE
C (RENM) MOVE=1
C        RENAME A TEXT MEMBER(MEMBER) TO NEW MEMBER NAME(RENAME)
C        OVERWITE IF NEWLY NAMED MEMBER IS ALREADY EXISTING
C        NOTE: TIME CONSUMING FOR LARGE TEXT FILE
C***********************************************************************
C
 6000 CONTINUE
      IF(IOTMP1.EQ.IOTMP2) THEN
        WRITE(IOMSG,661) IOTMP1
        STOP
      ENDIF
C
      IF(MEMBER.EQ.RENAME) THEN
        WRITE(IOMSG,662) MEMBER
        STOP
      ENDIF
C
C---- SEARCH ORIGINAL MEMBER
      CALL PDSSR(FILNAM,IERR)
      IF(IERR.EQ.1) THEN
        WRITE(IOMSG,663) SUBNAM, FILNAM(1:NLENG)
        STOP 999
      ENDIF
C
C---- SEARCH AND DELETE NEWLY NAMED MEMBER IF EXISTING(IN MOVE MODE)
      CALL PDSNM(PDSDIR(IPDS),RENAME,NEWNAM,NLEN2,IERR)
      IF(IERR.EQ.1) THEN
        WRITE(IOMSG,601) SUBNAM, FUNC
        STOP 999
      ELSEIF(IERR.EQ.2) THEN
        WRITE(IOMSG,602) SUBNAM, FUNC
        STOP 999
      ELSEIF(IERR.EQ.3) THEN
        WRITE(IOMSG,603) SUBNAM, NEWNAM
        STOP 999
      ENDIF
C
      CALL PDSSR(NEWNAM,IERR)
      IF(IERR.EQ.0) THEN
        IF( MOVE.EQ.0 ) THEN
          WRITE(IOMSG,664) SUBNAM, NEWNAM(1:NLEN2)
          STOP 999
        ELSE
          OPEN(UNIT=IOTMP1, FILE=NEWNAM(1:NLEN2), ERR=6010, IOSTAT=IOS)
          CLOSE(UNIT=IOTMP1, ERR=6020, STATUS='DELETE',IOSTAT=IOS)
          GOTO 6100
        ENDIF
      ELSE
        GOTO 6100
      ENDIF
 6010 WRITE(IOMSG,665) SUBNAM, NEWNAM(1:NLEN2)
      STOP 999
 6020 WRITE(IOMSG,666) SUBNAM, NEWNAM(1:NLEN2)
      STOP 999
C
C---- READ & WRITE (COPY MEMBER)
 6100 CONTINUE
      OPEN(UNIT=IOTMP1, FILE=FILNAM(1:NLENG), STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='FORMATTED',IOSTAT=IOS,ERR=6010)
      OPEN(UNIT=IOTMP2, FILE=NEWNAM(1:NLEN2), STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='FORMATTED',IOSTAT=IOS,ERR=6010)
      REWIND IOTMP1
      REWIND IOTMP2
C
 6200 CONTINUE
        READ(IOTMP1,'(A)',END=6300) LINE
C       SEARCH EFFECTIVE TEXT LINE LENGTH
        LAST = 0
        DO 100 I=1,150
          IF (LINE(I:I).NE.' ') THEN
            LAST = I
          ENDIF
  100   CONTINUE
        IF (LAST.EQ.0) THEN
          WRITE(IOTMP2,*)
        ELSE
          WRITE(IOTMP2,'(A)') LINE(1:LAST)
        ENDIF
      GOTO 6200
 6300 CONTINUE
C
C---- READ & WRITE (COPY / MOVE MEMBER)
C**
      IF( MOVE.EQ.0 ) THEN
        CLOSE(UNIT=IOTMP1, ERR=6020, STATUS='KEEP',IOSTAT=IOS)
        CLOSE(UNIT=IOTMP2, ERR=6020, STATUS='KEEP',IOSTAT=IOS)
        IF(IPRN.GE.1) THEN
          WRITE(IOMSG,9061) FILNAM(1:NLENG), NEWNAM(1:NLEN2)
        ENDIF
      ELSE
        CLOSE(UNIT=IOTMP1, ERR=6020, STATUS='DELETE',IOSTAT=IOS)
        CLOSE(UNIT=IOTMP2, ERR=6020, STATUS='KEEP',IOSTAT=IOS)
        IF(IPRN.GE.1) THEN
          WRITE(IOMSG,9071) FILNAM(1:NLENG), NEWNAM(1:NLEN2)
        ENDIF
      ENDIF
      IERR = 0
      GOTO 9999
C
C***********************************************************************
C
  600 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'TEXT PDS FILE NUMBER(=',I4,') IS OUT OF RANGE IN ACCESS MODE(',
     & A4,')',/,1X,'CHECK AND CHANGE THE ABOVE PROGRAM')
  601 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'TEXT MEMBER NAME IS EMPTY OR INVALID IN ACCESS MODE(',A4,')',
     &/,1X,'CHECK AND CHANGE THE ABOVE PROGRAM')
  602 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'TEXT PDS DIRECTORY NAME IS EMPTY OR INVALID IN ACCESS MODE(',
     & A4,')',/,1X,'CHECK AND CHANGE THE ABOVE PROGRAM')
  603 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'THE FOLLOWING TEXT FILE NAME IS TOO LONG. (LONGER THAN 129 )',/,
     & A )
C
  621 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'INVALID ACCESS TO TEXT PDS MEMBER IN OPEN MODE, FILE NAME:',/1X,
     & A )
  631 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'INVALID ACCESS TO TEXT PDS MEMBER IN CLOSE MODE, FILE NAME:',/1X,
     & A )
  651 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'INVALID ACCESS TO TEXT PDS MEMBER IN OPEN FOR DELETE MODE,',
     &' FILE NAME:',/1X,A )
  652 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'INVALID ACCESS TO TEXT PDS MEMBER IN CLOSE FOR DELETE MODE,',
     &' FILE NAME:',/1X,A )
C
  661 FORMAT(//1H ,'<<<  ERROR STOP (PONTXT)  >>>',/,1X,
     &'DEVICE NUMBER FOR TEXT PDS MEMBER ACCESS : IOTMP1(=',
     & I3,') MUST BE DIFFERENT FROM IOTMP2')
  662 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'MEMBER NAMES ARE SAME IN RENAME MODE, MEMBER NAME:',A )
  663 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'TEXT MEMBER NOT FOUND IN RENAME MODE, FILE NAME:',/1X,A )
  664 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'TEXT MEMBER ALREADY EXISTING IN COPY MODE, FILE NAME:',/1X,A )
  665 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'INVALID ACCESS TO TEXT PDS MEMBER IN OPEN FOR RENAME MODE,',
     &' FILE NAME:',/1X,A )
  666 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'INVALID ACCESS TO TEXT PDS MEMBER IN CLOSE FOR RENAME MODE,',
     &' FILE NAME:',/1X,A )
C
  699 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'INVALID FUNCTION (',A4,') WAS SELECTED IN PDS FILE ACCESS ',/,1X,
     &'CHECK AND CHANGE THE ABOVE PROGRAM')
C
 9011 FORMAT(5X,'TEXT MEMBER WAS SEARCHED AND FOUND (PONTXT):',/,5X,A )
 9012 FORMAT(5X,'TEXT MEMBER WAS SEARCHED BUT NOT FOUND (PONTXT):',/,
     &       5X,A )
 9021 FORMAT(5X,'TEXT MEMBER WAS OPENED   (PONTXT):',/,5X,A )
 9031 FORMAT(5X,'TEXT MEMBER WAS CLOSED   (PONTXT):',/,5X,A )
 9041 FORMAT(5X,'TEXT MEMBER WAS REWINDED (PONTXT):',/,5X,A )
 9051 FORMAT(5X,'TEXT MEMBER WAS DELETED  (PONTXT):',/,5X,A )
 9061 FORMAT(5X,'TEXT MEMBER WAS COPYED   (PONTXT):',/,5X,A,/,5X,A )
 9071 FORMAT(5X,'TEXT MEMBER WAS RENAMED  (PONTXT):',/,5X,A,/,5X,A )
C***********************************************************************
C
 9999 RETURN
      END
