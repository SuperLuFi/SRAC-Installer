CFWRD-VP-105 ***CITATION*** LINE RELAXATION ON ROWS FOR 1,2-D/ CF-DNSD
C
      SUBROUTINE FWRD( SCAT, P2,DCONB,DCONR,PTSA, IVX,JVX,KVX, IVXP1,
     & JVXP1,IVZ,KVZ,BET,DEL,NRGN,E1,LVX, IOVX,IOVZ,PY,PPY,DCONBY,
     & DCONRY,SCATY,ZZY,ISWOE)
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
C
      REAL*8 PY((JVX+2+ISWOE)*(IVX+2)),PPY((JVX+2+ISWOE)*(IVX+2))
      REAL*8 DCONBY((JVX+2+ISWOE)*(IVX+2))
      REAL*8 DCONRY((JVX+2+ISWOE)*(IVX+2))
      REAL*8 SCATY((JVX+2+ISWOE)*(IVX+2))
      REAL*8 ZZY((JVX+2+ISWOE)*(IVX+2))
      REAL*8 P2(JVX,IVX,KVX),SCAT(JVX,IVX)
      REAL*8 TMF
C
      COMMON/ALSUB/BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,
     & LMAX,MMAX, NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,
     & IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(100), IX(200),INNO(100),
     &  NGC(24),IEDG(24),ITMX(24),TIMX(6), GLIM(6),NDPL(24),IEDP1(24),
     & IEDP2(24),IEDP3(24), DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),
     & XSRH1(6), XTR1(WZX),XTR2(WZX),NXTR1(WZX),NXTR2(WZX),SPARE(200),
     & IXPUT(200),XPUT(200)
      COMMON/AFLUX/BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,
     & ISTART,IEP, VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,
     & XLEK,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3, NI3,IEXTR,
     & IRECV,VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2, BETTA,SUMXI,IX25,IX28,I,J,
     &  KB,K,ITMAX,ITIME, BAT(MSX),DAL(MSX)
C
      DIMENSION DCONB(JVX,IVXP1,IOVX),DCONR(JVXP1,IVZ,IOVZ),
     &  PTSA(JVX,IVX,IOVX)
      DIMENSION NRGN(JVX,IVX),E1(LVX,KVX)
CMOD  DATA KXY/0/
      COMMON   /CITKXY/ KXY
C
C     INRB = 1   ORDINARY
C     INRB = 2   PERIODIC (REPEATING)
C     INRB = 3   90  DRGREE ROTATIONAL
C     INRB = 4   180 DEGREE ROTATIONAL
C
      INRB=IX(72)+1
      N = IX(20)
C
      IF(KXY.EQ.0) THEN
      JP2=JVX+2+ISWOE
      JIS=JP2+2
      JIE=JP2*IVX+JVXP1
      LYY=JP2*IVXP1+1
      LLY=JIE+1+JP2+JVXP1
      DO 100 JI=1,JP2*(IVX+2)
  100   PPY(JI)=0.0D0
CBUG FIX FOR INTERNAL BLACKABSORBER WITH GROUPE DEPENDENT BOUNDARY
C     DO 102 I=1,IVX
C     DO 102 J=1,JVX
C 102   PPY(JP2*I+J+1)=P2(J,I,K)
      KXY=1
      ENDIF
C
      DO 110 JI=JP2+1,JP2*IVX+1,JP2
  110   PY(JI)=0.0D0
      IF(INRB.EQ.1) THEN
        DO 120 JI=JP2+JVXP1+1,JIE+1,JP2
  120     PY(JI)=0.0D0
      ELSE IF(INRB.EQ.2) THEN
*VOCL LOOP,NOVREC
        DO 130 JI=JP2+1,JP2*IVX+1,JP2
          PY(JI)=PY(JI+JVX)
  130     PY(JI+JVXP1)=PY(JI+1)
      ELSE IF(INRB.EQ.3) THEN
*VOCL LOOP,NOVREC
        DO 140 JI=1,JVX
          MAY=JI*JP2+JVXP1
          PY(JI+LYY)=PY(MAY)
  140     PY(MAY+1)=PY(JI+LYY-JP2)
      ELSE IF(INRB.EQ.4) THEN
*VOCL LOOP,NOVREC
        DO 150 JI=JP2+JVXP1+1,JIE+1,JP2
  150     PY(JI)=PY(LLY-JI)
      ENDIF
C
      DO 200 JI=JIS,JIE,2
CKSK    IF(PPY(JI).NE.0.0) THEN
CBUG FIX FOR INTERNAL BLACKABSORBER WITH GROUPE DEPENDENT BOUNDARY
C       IF(PPY(JI).NE.0.0.AND.ZZY(JI).NE.0.0) THEN
        IF(PY (JI).NE.0.0.AND.ZZY(JI).NE.0.0) THEN
        PPY(JI)=(DCONRY(JI)*PY(JI-1)+DCONRY(JI+1)*PY(JI+1)
     &    +DCONBY(JI)*PY(JI-JP2)+DCONBY(JI+JP2)*PY(JI+JP2)
     &    +SCATY(JI))/ZZY(JI)
CBUG FIX FOR INTERNAL BLACKABSORBER WITH GROUPE DEPENDENT BOUNDARY
         ELSE
         PPY(JI)=0.0D0
        ENDIF
  200 CONTINUE
C
C
      IF(IEP) 300,320,340
C
  300 DO 310 JI=JIS,JIE,2
  310   PY(JI)=PPY(JI)
      GO TO 390
C
  320 DO 330 JI=JIS,JIE,2
  330   PY(JI)=(PPY(JI)-PY(JI))*BETTA+PY(JI)
      GO TO 390
C
  340 DO 380 JI=JIS,JIE,2
        TMF=(PPY(JI)-PY(JI))*BETTA+PY(JI)
        IF(TMF-PPY(JI)) 350,370,360
  350     TMF=DMAX1(TMF,0.5D0*PPY(JI))
          GO TO 370
  360     TMF=DMIN1(TMF,PPY(JI)+PY(JI))
  370     PY(JI)=TMF
  380 CONTINUE
  390 CONTINUE
C
C
C
      DO 410 JI=JP2+1,JP2*IVX+1,JP2
  410   PY(JI)=0.0D0
      IF(INRB.EQ.1) THEN
        DO 420 JI=JP2+JVXP1+1,JIE+1,JP2
  420     PY(JI)=0.0D0
      ELSE IF(INRB.EQ.2) THEN
*VOCL LOOP,NOVREC
        DO 430 JI=JP2+1,JP2*IVX+1,JP2
          PY(JI)=PY(JI+JVX)
  430     PY(JI+JVXP1)=PY(JI+1)
      ELSE IF(INRB.EQ.3) THEN
*VOCL LOOP,NOVREC
        DO 440 JI=1,JVX
          MAY=JI*JP2+JVXP1
          PY(JI+LYY)=PY(MAY)
  440     PY(MAY+1)=PY(JI+LYY-JP2)
      ELSE IF(INRB.EQ.4) THEN
*VOCL LOOP,NOVREC
        DO 450 JI=JP2+JVXP1+1,JIE+1,JP2
  450     PY(JI)=PY(LLY-JI)
      ENDIF
C
      DO 500 JI=JIS+1,JIE,2
CKSK    IF(PPY(JI).NE.0.0) THEN
CBUG FIX FOR INTERNAL BLACKABSORBER WITH GROUPE DEPENDENT BOUNDARY
C       IF(PPY(JI).NE.0.0.AND.ZZY(JI).NE.0.0) THEN
        IF(PY (JI).NE.0.0.AND.ZZY(JI).NE.0.0) THEN
        PPY(JI)=(DCONRY(JI)*PY(JI-1)+DCONRY(JI+1)*PY(JI+1)
     &    +DCONBY(JI)*PY(JI-JP2)+DCONBY(JI+JP2)*PY(JI+JP2)
     &    +SCATY(JI))/ZZY(JI)
CBUG FIX FOR INTERNAL BLACKABSORBER WITH GROUPE DEPENDENT BOUNDARY
         ELSE
         PPY(JI)=0.0D0
        ENDIF
  500 CONTINUE
C
C
      IF(IEP) 600,620,640
C
  600 DO 610 JI=JIS+1,JIE,2
  610   PY(JI)=PPY(JI)
      GO TO 690
C
  620 DO 630 JI=JIS+1,JIE,2
  630   PY(JI)=(PPY(JI)-PY(JI))*BETTA+PY(JI)
      GO TO 690
C
  640 DO 680 JI=JIS+1,JIE,2
        TMF=(PPY(JI)-PY(JI))*BETTA+PY(JI)
        IF(TMF-PPY(JI)) 650,670,660
  650     TMF=DMAX1(TMF,0.5D0*PPY(JI))
          GO TO 670
  660     TMF=DMIN1(TMF,PPY(JI)+PY(JI))
  670     PY(JI)=TMF
  680 CONTINUE
  690 CONTINUE
C
C
      RETURN
      END