      SUBROUTINE CNVPRE(IFNAME,FINAME,NCOMPF,IOPT4 ,MVX  ,NOUT1 ,
     1                  LREAC1,LREAC2,CFNAME)
C
CCNVPRE --- CONVERSION RATIO CALCULATION PREPARATION ARRY
C
      CHARACTER*4 FINAME(2,MVX)
      CHARACTER*4 CFNAME(1)
C
      DIMENSION IFNAME(1),NCOMPF(MVX)
      DIMENSION LREAC1(IOPT4),LREAC2(IOPT4)
C
C
C
      CALL  ICLEA( NCOMPF , MVX ,0 )
      LOC         = 1
      NREG        = IFNAME(LOC)
C
      DO 100    I = 1 , MVX
      FINAME(1,I) = '    '
      FINAME(2,I) = '    '
 100  CONTINUE
      DO 110 I = 1,NREG
      IF(I.LE.MVX) THEN
                   FINAME(1,I) = CFNAME(LOC+1)
                   FINAME(2,I) = CFNAME(LOC+2)
                   IF(CFNAME(LOC+1).NE.'    ') NCOMPF(I) = 1
                   ELSE
                   WRITE(NOUT1,1000) (CFNAME(LOC+K),K=1,2),I
                   ENDIF
      LOC      = LOC + 2
  110 CONTINUE
C
      DO 120  I = 1,IOPT4
      LREAC1(I) = IFNAME(LOC+1)
      LREAC2(I) = IFNAME(LOC+2)
      LOC       = LOC + 2
  120 CONTINUE
C
CM    WRITE(6,*) ' ** NCOMPF : ',(NCOMPF(M),M=1,MVX)
C
      RETURN
C
 1000 FORMAT('0=== WARNNING === ',2A4,' IS OUT OF RANGE REGION NO.',I5)
      END
