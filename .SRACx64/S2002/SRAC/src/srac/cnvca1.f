      SUBROUTINE  CNVCA1(FINAME,SIGMA ,IOPT4 ,P1E   ,NRGNE ,NCOMPF,
     1                   PVOL  ,CAP   ,ABS   ,NCOMP ,JVX   ,IVX   ,
     2                   KBVX  ,IGMAX ,LVX   ,MVX   ,IDIM  ,NOUT2 ,
     3                   IOFLUX,LREAC1,LREAC2                      )
C
CCNVCA1 --- CONVERSION RATIO CALCULATION AND PRINTOUT
C
      CHARACTER*8  FINAME(MVX)
      CHARACTER*12 REAC(5)
C
      DIMENSION SIGMA(5,IGMAX,MVX),P1E(JVX,IVX,KBVX),
     1          NRGNE(JVX,IVX,KBVX),NCOMPF(MVX),PVOL(LVX),
     2          CAP(5,MVX),ABS(5,MVX),NCOMP(LVX),
     3          LREAC1(IOPT4),LREAC2(IOPT4)
C
C
C
CKSK  REAC(1) = 12H 1.00
CKSK  REAC(2) = 12H FISSION
CKSK  REAC(3) = 12H CAPTURE
CKSK  REAC(4) = 12H ABSORPTION
CKSK  REAC(5) = 12H PRODUCTION
      REAC(1) = ' 1.00       '
      REAC(2) = ' FISSION    '
      REAC(3) = ' CAPTURE    '
      REAC(4) = ' ABSORPTION '
      REAC(5) = ' PRODUCTION '
C
      CALL IVALUE(CAP,IOPT4*5,0.0)
      CALL IVALUE(ABS,IOPT4*5,0.0)
CADD
CM    WRITE(6,*) ' ** NCOMP  : ',NCOMP
CM    WRITE(6,*) ' ** NCOMPF : ',NCOMPF
CM    WRITE(6,*) ' ** NRGNE  : ',NRGNE
CEND
      REWIND  IOFLUX
      DO 130 K = 1,IGMAX
      IF (IDIM.NE.3) THEN
         READ(IOFLUX) (((P1E(J,I,KB),DUM,J=1,JVX),I=1,IVX),KB=1,KBVX)
         ELSE
           READ(IOFLUX) (((P1E(J,I,KB),J=1,JVX),I=1,IVX),KB=1,KBVX)
      ENDIF
      DO 120 KB = 1,KBVX
      DO 110  I = 1,IVX
      DO 100  J = 1,JVX
      L = NRGNE(J,I,KB)
      IF(L.LE.0.OR.L.GT.LVX) GO TO 100
C
      M = NCOMP (L)
      IF(M.LE.0.OR.M.GT.MVX) GO TO 100
      IF(NCOMPF(M).EQ.0)     GO TO 100
C
      DO 90  MT = 1 , 5
      ABS(MT,M) = ABS(MT,M) + SIGMA(MT,K,M)*P1E(J,I,KB)*PVOL(L)
      CAP(MT,M) = CAP(MT,M) + SIGMA(MT,K,M)*P1E(J,I,KB)*PVOL(L)
   90 CONTINUE
  100 CONTINUE
  110 CONTINUE
  120 CONTINUE
  130 CONTINUE
      REWIND  IOFLUX
C
      DO 150 I = 1,IOPT4
      CAPT = 0.0
      ABST = 0.0
      IPOSC = LREAC1(I) + 2
      IPOSA = LREAC2(I) + 2
      WRITE(NOUT2,1000)  REAC(IPOSC),REAC(IPOSA)
C
      DO 140 M = 1 , MVX
      IF(NCOMPF(M).EQ.0) GO TO 140
      CAPT = CAPT + CAP(IPOSC,M)
      ABST = ABST + ABS(IPOSA,M)
      CNV  = 0.0
      IF (ABS(IPOSA,M).GT.0.0)   CNV =  CAP(IPOSC,M)/ABS(IPOSA,M)
      WRITE(NOUT2,1010) FINAME(M),CAP(IPOSC,M),ABS(IPOSA,M),CNV
  140 CONTINUE
      CNV = 0.0
      IF (ABST.GT.0.0) CNV = CAPT / ABST
      WRITE(NOUT2,1020) ABST,CAPT,CNV
  150 CONTINUE
      RETURN
C
 1000 FORMAT(1H0,
     1  1H0,'  -- CONVERSION RATIO (CORE CALCULATION BY CITATION) --'
     2 /1H0,' MATERIAL NUMERATOR / DENOMINATOR    RACTION RATE ',
     3 /1H ,' NAME     ',2A12,                '   RATIO        ',
     4 /1H ,' -------------------------------------------------')
 1010 FORMAT(2X,A8,1X,1P2E12.5,3X,E12.5)
 1020 FORMAT(1H ,' -------------------------------------------------'
     1      /1H ,' TOTAL  ',2X,1P2E12.5,3X,E12.5//)
C
      END
