      SUBROUTINE CLIN(NPIN,RX,RDP,RPP,NTR,V,NAMEP,NAMER,F)
      DIMENSION NPIN(1),RX(1),RDP(1),RPP(1),
     1    NTR(1),V(1),NAMEP(2,NDPIN,1)
     2   ,F(NDPIN1,1),NAMER(1)
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGA,
     1                 NDPIN,IDIVP,BETM,NX1,IDUM(7),NDPIN1
     2                ,NDR,NDA,LL,L0,RO1,DRO,FVOL,RAN,ISERI
     3                ,ROG,ANG,I32(5)
      COMMON / PIJ2C / IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,LCOUNT,IEDPIJ,
     1                 IFORM,NTTAB,NUTAB,SZ,IDUM14(26),LCMMR,
     4                 LCNREG,LCIRR,LCIXR,LCMAR,LCMAT,LCVOL,
     5                 LCVOLR,LCVOLX,LCVOLM,NO2,AA(950)
      COMMON / MAINC / DDM(63),NOUT1,NOUT2,DDM1(37),TITLE(18),DDM2(380)
      DATA PI/3.141593/
C
      NT=0
      DO 25 I=1,NAPIN
      NT=NT+NPIN(I)
   25 CONTINUE
      IF(NT-NTPIN) 28,27,28
   28 WRITE(NOUT2,3012) NT,NTPIN
      GO TO 1000
   27 CONTINUE
C     VOLUME OF ZONE
      IF(IDIVP.NE.0) GO TO 34
      IX=1
      DO 32 IP=1,NAPIN
   31 IF(RPP(IP).LT.RX(IX)) GO TO 32
      IX=IX+1
      GO TO 31
   32 NTR(IP)=IX-1
      NZA=NX+NDPIN*NAPIN
      GO TO 38
   34 IX=NX1
      IP=NAPIN
      NX=NX+NAPIN
      NX1=NX+1
      DO 36 IK=2,NX1
      K=NX1+2-IK
      IF(RX(IX).GE.RPP(IP)) GO TO 35
      RX(K)=RPP(IP)
      NTR(IP)=K
      IP=IP-1
      IF(IP.EQ.0) GO TO 37
      GO TO 36
   35 RX(K)=RX(IX)
      IX=IX-1
   36 CONTINUE
   37 CONTINUE
      NZA=NX+NDPIN*NAPIN*IDIVP
   38 CONTINUE
      WRITE(NOUT2,3010) (I,RX(I),I=1,NX1)
      NZP=NZA-NX
      DO 40 I=1,NX
      J=I+NZP
      V(J)=PI*(RX(I+1)**2-RX(I)**2)
      NAMER(I)=J
   40 CONTINUE
      IF(IDIVP.NE.0) GO TO 50
      IJ=0
      DO 45 IP=1,NAPIN
      AN=NPIN(IP)
      NNR=NTR(IP)+NZP
      NTR(IP)=0
      V(NNR)=V(NNR)-       AN*PI*RDP(NDPIN1)**2
      DO 44 ND=1,NDPIN
      IJ=IJ+1
      V(IJ)=AN*PI*(RDP(ND+1)**2-RDP(ND)**2)
      NAMEP(1,ND,IP)=IJ
      NAMEP(2,ND,IP)=IJ
   44 CONTINUE
   45 CONTINUE
      GO TO 80
   50 IJ=0
      DO 55 IP=1,NAPIN
      F(1,IP)=0.
      NNR=NTR(IP)
      RR =RX(NNR)
      DO 54 ND=2,NDPIN1
      XX = (RR**2+RPP(IP)**2-RDP(ND)**2)/2./RPP(IP)
      YY = SQRT(RR**2-XX**2)
      F(ND,IP) = RR**2*ACOS(XX/RR)+RDP(ND)**2*ACOS((RPP(IP)-XX)/
     1           RDP(ND))          -RPP(IP)*YY
   54 CONTINUE
      AN=NPIN(IP)
      NNR=NTR(IP)+NZP-1
      V(NNR)=V(NNR)-AN*F(NDPIN1,IP)
      V(NNR+1)=V(NNR+1)-AN*(PI*RDP(NDPIN1)**2-F(NDPIN1,IP))
   55 CONTINUE
      IF(IDIVP.GT.1) GO TO 70
      DO 65 IP=1,NAPIN
      AN=NPIN(IP)
      DO 64 ND=1,NDPIN
      IJ=IJ+1
      V(IJ)=AN*PI*(RDP(ND+1)**2-RDP(ND)**2)
      NAMEP(1,ND,IP)=IJ
      NAMEP(2,ND,IP)=IJ
   64 CONTINUE
      NTR(IP)=0
   65 CONTINUE
      GO TO 80
   70 DO 75 IP=1,NAPIN
      AN=NPIN(IP)
      DO 73 ND=1,NDPIN
      IJ=IJ+1
      NDI=NDPIN1-ND
      V(IJ)=AN*(F(NDI+1,IP)-F(NDI,IP))
      NAMEP(1,NDI,IP)=IJ
   73 CONTINUE
      DO 74 ND=1,NDPIN
      IJ=IJ+1
      V(IJ)=AN*(PI*(RDP(ND+1)**2-RDP(ND)**2)-(F(ND+1,IP)-F(ND,IP)))
      NAMEP(2,ND ,IP)=IJ
   74 CONTINUE
   75 CONTINUE
   80 CONTINUE
      IF(NZA.EQ.NZ) GO TO 81
      WRITE(NOUT2,3012) NZA,NZ
      GO TO 1000
   81 CONTINUE
C
      RAN =RPP(NAPIN)+RDP(NDPIN1)
      RO1=0.
      SZ=2.*PI*RX(NX1)
      FVOL=2.0
      NDR=NX
C *** DRO : EQUAL INTERVAL FOR RADIAL INTEGRATION NOT USED IN CLUP
      DRO=RX(NX1)/FLOAT(NDR*NGR)
      RETURN
CKSK
*3010 FORMAT(10X17HRADII  OF  ZONES  /(10X,I3,1H),1PE11.4,I3,1H),   E11.
*    14,I3,1H),E11.4,I3,1H),E11.4,I3,1H),E11.4,I3,1H),E11.4,I3,1H),E11.4
*    2,I3,1H),E11.4))
*3012 FORMAT(10X 30H** INCONSISTENT INTEGER DATA   2I5 )
*3008 FORMAT(10X 30H** ILLEAGAL REAL NUMBER        2E12.5)
 3010 FORMAT(10X,'RADII  OF  ZONES ',/,(10X,I3,1H),1PE11.4,I3,1H),
     &E11.4,I3,1H),E11.4,I3,1H),E11.4,I3,1H),E11.4,I3,1H),E11.4,I3,1H),
     &E11.4,I3,1H),E11.4))  
 3012 FORMAT(10X,30H** INCONSISTENT INTEGER DATA  ,2I5 )
 3008 FORMAT(10X,30H** ILLEAGAL REAL NUMBER       ,2E12.5)
CKSK
 1000 STOP
      END
