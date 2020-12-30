      SUBROUTINE DTLIST(NIN,NOUT,IPR,IOPT)
C
C*************************************************************
C  PRINT INPUT LIST AND SAVE IN NOUT-TH DEVICE. AFTER DTLIST,
C  ALL INPUT DATA SHOULD BE READ FROM NOUT-TH DEVICE. 
C  THEREFORE, DTLIST SHOULED BE CALLED FIRST OF ALL ROUTINES.
C  IF IOPT=1, THEN CHANGE ALL SMALL CHRACTERS TO LARGE
C  CHARACTERS.
C*************************************************************
C
      CHARACTER*80 AA
      CHARACTER*26 SMALL, LARGE
C
CKSK  NIN    =  5
CKSK  IPR    =  6
CKSK  IOPT   =  0
C
      ICOUNT =  0
      SMALL  = 'abcdefghijklmnopqrstuvwxyz'
      LARGE  = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      REWIND NOUT
C
      DO 20 IPAGE=1,1000
      WRITE(IPR,1000) IPAGE
      WRITE(IPR,1010)
      WRITE(IPR,1020)
      DO 10 I=1,50
      ICOUNT=ICOUNT+1
      READ (NIN ,'(A80)',END=30) AA
C
C*********** CHANGE small TO LARGE characters ********
C
      IF(IOPT.EQ.1) THEN
        DO 100 J=1,80
          DO 100 K=1,26
            IF(AA(J:J).EQ.SMALL(K:K)) AA(J:J)=LARGE(K:K)
  100   CONTINUE
      ENDIF
C
C******************************************************
C
      WRITE(NOUT,'(A80)')        AA
      WRITE(IPR,1030) ICOUNT,AA,ICOUNT
   10 CONTINUE
      WRITE(IPR,1020)
      WRITE(IPR,1040)
   20 CONTINUE
C
   30 CONTINUE
      REWIND NOUT
      WRITE(IPR,1050)
      RETURN
C
 1000 FORMAT(1H1,92X,'PAGE-',I4.4)
 1010 FORMAT(18X,'*********************'
     &      /18X,'*                   *'
     &      /18X,'*  INPUT DATA LIST  *'
     &      /18X,'*                   *'
     &      /18X,'*********************')
 1020 FORMAT(12X,'....*....1....*....2....*....3....*....4',
     &          '....*....5....*....6....*....7....*....8')
 1030 FORMAT(I8,4X,A80,I7)
 1040 FORMAT(12X,'*** CONTINUE ***')
 1050 FORMAT(12X,'*** INPUT DATA END ***')
C
      END
