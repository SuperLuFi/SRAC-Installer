      SUBROUTINE PREPA(RX,V,NREG)
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGA,
     1                 NDPIN,IDIVP,BETM,NX1,IDUM(8)
     2                ,NDR,NDA,LL,L0,RO1,DRO,FVOL,IC2D,FL,ROG,
     3                 ANG,I32,I33,SINB,COSB,I36
      COMMON / MAINC / DDM(63),NOUT1,NOUT2,IT0,DDM1(36),TITLE(18),
     1                 DDM2(380)
      COMMON / PIJ2C / IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,LCOUNT,IEDPIJ,
     1                 IFORM,NTTAB,NUTAB,SZ,IDUM1(18),ICOOD
      DIMENSION RX(1),V(1),NREG(1)
      DATA PI/3.14159/
      IF(IGT.EQ.1) ICOOD=201
      IF(IGT.EQ.2) ICOOD=001
      IF(IGT.EQ.3) ICOOD=101
      IF(IGT.EQ.4) ICOOD=111
      IF(IGT.EQ.5) ICOOD=112
      IF(IGT.EQ.6) ICOOD=121
      IF(IGT.EQ.7) ICOOD=122
      IDIM=ICOOD-10*(ICOOD/10)
      ICOOD=ICOOD/10
      ISH =ICOOD-10*(ICOOD/10)
      ICOOD=ICOOD/10
      ICOOD1=ICOOD+1
      NZ=IDIM*NX
      ISHH=ISH+1
      NX1=NX+1
      NDR=NX
      RO1=0.
      FVOL=2.
      IF(NGR*NDA.NE.0) GO TO 2
      NGR = 8
      NDA = 15
    2 IF(ICOOD.NE.0) GO TO 3
C *** SLAB IGT=2
      NDR=1
      NGR=1
      NDA=2
      BETM=PI
      FVOL=2.*PI
      SZ = 2.0
      FACT=1.0
      GO TO 27
    3 IF(ISH.NE.0) GO TO 4
C *** CYL (IGT=3) OR SHERE (IGT=1)
      NDA = 1
      BETM=1.0
      GO TO 21
C  ** SQUARE OR HEXAGON   FL=HALF OF LATTICE PITCH (INPUT DATA)
    4 FL=RX(NX1)
      BETM=PI/FLOAT(ISHH)
      RX(NX1)=2.0/SQRT(FLOAT(ISHH))*RX(NX1)
C     SURFACE AREA SZ
   21 GO TO (22,26),ICOOD
   22 FACT=PI
      GO TO (23,24,25),ISHH
C  ** CIRCULAR CYLINDER
   23 SZ = 2.*PI*RX(NX1)
      GO TO 27
C ** SQUARE
   24 SZ = 8.*FL
      GO TO 27
C ** HEXAGON
   25 SZ = 6.*RX(NX1)
      GO TO 27
C ** SPHERE
   26 SZ = 4.*PI*RX(NX1)**2
      FACT=1.333333*PI
      FVOL=2.*PI
C     VOLUME
   27 DO 30 I=1,NX
      V(I)   =  (RX(I+1)**ICOOD1-RX(I)**ICOOD1)*FACT
   30 CONTINUE
      IF(IGT.LE.3) GO TO 550
      F46=2.*FLOAT(ISHH)
      DELA=PI/F46
      IZ=0
      DO 46 I=1,NX
      RM=RX(I)
      RP=RX(I+1)
      AM=0.
      AP=0.
      SI=F46*(RP**2-RM**2)
      V(IZ+1)=0.
      V(IZ+2)=0.
      IF(FL.LE.RP) BETA=ACOS(FL/RP)
      IF(FL.LE.RM) GAMMA=ACOS(FL/RM)
      DO 45 J=1,IDIM
      IZ=IZ+1
      AP=DELA/FLOAT(IDIM)+AP
      IF( RP*COS(AM) .LE. FL )                           GO TO 40
      IF( RP*COS(AP) .LE. FL .AND. RM*COS(AM) .LE. FL )  GO TO 41
      IF( RP*COS(AP) .LE. FL .AND. RM*COS(AP) .LE. FL )  GO TO 43
      IF( RM*COS(AM) .LE. FL )                           GO TO 42
      IF( RM*COS(AP) .LE. FL )                           GO TO 44
      GO TO 45
   40 V(IZ) = SI * (AP-AM)
      GO TO 45
   41 V(IZ) = F46*FL**2*(TAN(BETA)-TAN(AM))+SI*(AP-BETA)
     1       -F46*RM**2*(BETA-AM)
      GO TO 45
   42 V(IZ) = F46*FL**2*(TAN(AP)-TAN(AM))  -F46*RM**2*(AP-AM)
      GO TO 45
   43 V(IZ) = F46*FL**2*(TAN(BETA)-TAN(GAMMA)) + SI*(AP-BETA)
     1       -F46*RM**2*(BETA-GAMMA)
      GO TO 45
   44 V(IZ) = F46*FL**2*(TAN(AP)-TAN(GAMMA)) - F46*RM**2*(AP-GAMMA)
   45 AM=AP
   46 CONTINUE
C
  550 IC2D=0
      IF(ICOOD.NE.1 .OR. ISH.EQ.0 .OR. IDIM.NE.2 ) GO TO 555
      DO 551 I=1,NX
      J=2*I-1
      IF(NREG(J).NE.NREG(J+1))GO TO 555
  551 IC2D=I
  555 CONTINUE
      RETURN
      END
