C***********************************************************************
      SUBROUTINE DTLIST(NIN,NOUT,IPR,IOPT)
C
C***********************************************************************
C  PRINT INPUT LIST AND SAVE IN NOUT-TH DEVICE. AFTER DTLIST,
C  ALL INPUT DATA SHOULD BE READ FROM NOUT-TH DEVICE. 
C  IT IS ALLOWED NOUT = NIN.(JUST ECHO BACK OF INPUT DATA), BUT
C  NOUT=5 (CAN NOT REWIND) IS NOT ALLOWED.
C  THEREFORE, DTLIST SHOULED BE CALLED BEFORE READING INPUT.
C  IF IOPT=1, THEN CHANGE ALL SMALL CHRACTERS TO LARGE
C  CHARACTERS.
C  MAXIMUM INPUT COLUMNS OF 80 IS SUPPOSED HERE.
C  MAXIMUM INPUT LINES OF 50*MAXPAG IS SUPPOSED HERE.
C  THE INPUT LINE WHOSE FIRST COLUMN IS '*' IS TAKEN AS A COMMENT 
C  LINE.
C                                                   BY K.OKUMURA(JAERI)
C***********************************************************************
C
      PARAMETER ( MAXCOL = 80 )
      PARAMETER ( MAXPAG = 1000)
      CHARACTER*80  AA
      CHARACTER*26 SMALL, LARGE
C
CKSK  NIN    =  5
CKSK  IPR    =  6
CKSK  IOPT   =  0
      IF(NOUT.EQ.5 .OR. NOUT.EQ.6 .OR. NOUT.EQ.IPR) THEN
        WRITE(IPR,5000)
        STOP 999
      ENDIF
C
C-----  ACTUAL LINE NUMBER = LCOUNT*100000 + ICOUNT
      ICOUNT =  0
      LCOUNT =  0
      SMALL  = 'abcdefghijklmnopqrstuvwxyz'
      LARGE  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      REWIND NOUT
C
      DO 100 IPAGE=1,MAXPAG
        IF(LCOUNT.LE.0) THEN
          WRITE(IPR,6000) IPAGE
        ELSE
          WRITE(IPR,6010) IPAGE, LCOUNT*100000
        ENDIF
        WRITE(IPR,6020)
        DO 200 I=1,50
          ICOUNT=ICOUNT+1
          IF(ICOUNT.EQ.100000) THEN
            LCOUNT = LCOUNT + 1
            ICOUNT = 0
          ENDIF
          READ (NIN ,'(A80)',END=900) AA
C
C*********** CHANGE small TO LARGE characters ********
C
          IF(IOPT.EQ.1) THEN
            DO 300 J=1,MAXCOL
              DO 300 K=1,26
                IF(AA(J:J).EQ.SMALL(K:K)) AA(J:J)=LARGE(K:K)
  300       CONTINUE
          ENDIF
C
C*********** CUTSPACE *********************************
C
          IENDCL = 1
          DO 400 ICOL = 1,MAXCOL
            IF(AA(ICOL:ICOL).NE.' ') IENDCL = ICOL
  400     CONTINUE
C
C*** WRITE INPUT DATA ON SCRATCH AND STANDARD OUTPUT DEVICE
C
          IF(NOUT.NE.NIN) THEN
            IF(AA(1:1).NE.'*') THEN
              WRITE(NOUT,'(A80)')    AA
            ENDIF
          ENDIF
          WRITE(IPR,6030) ICOUNT,AA(1:IENDCL)
  200   CONTINUE
        WRITE(IPR,6020)
        WRITE(IPR,6040)
  100 CONTINUE
C
  900 CONTINUE
      REWIND NOUT
      WRITE(IPR,*)
      WRITE(IPR,6050)
C***********************************************************************
C
 5000 FORMAT(//1H ,'<<<  ERROR STOP (DTLIST)  >>>',/,1X,
     & 'INVALID I/O DEVICE NUMBER (=',I3,') FOR A SCRATCH FILE.',/,1X,
     & 'CHANGE DEVICE NUMBER IN PROGRAM')
 6000 FORMAT(/,28X,'*********************',
     &       /,28X,'*  INPUT DATA LIST  *','     PAGE-',I4.4,
     &       /,28X,'*********************',/)
 6010 FORMAT(/,28X,'*********************',
     &       /,28X,'*  INPUT DATA LIST  *','     PAGE-',I4.4,
     &       /,28X,'*********************','     LINE +',I7,/)
 6020 FORMAT(8X,'....*....1....*....2....*....3....*....4',
     &          '....*....5....*....6....*....7....*....8')
 6030 FORMAT(1X,I5,': ',A)
 6040 FORMAT(30X,'*** CONTINUE ***')
 6050 FORMAT(/,28X,'*********************',
     &       /,28X,'*  INPUT DATA END   *',
     &       /,28X,'*********************',/)
C
      RETURN
      END
