C             READQF              LEVEL=1        DATE=81.11.14
      SUBROUTINE READQF ( C,Q,GR,X,Y,FLAG,NL,IOP,ITJT,IGM,IT,JT,NC,S,
     &INEG )
C
C     FLAG = 0, PROCESS SOURCES
C     FLAG = 1, PROCESS FLUXES
C
      COMMON /TW1C/ DD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2,AA(130)
      DIMENSION D(212),A(132)
      EQUIVALENCE (D(1),DD(1)),(AAA(1),A(1))
      EQUIVALENCE (D(112),IPFX),(D(113),LTFX),(D(123),I1),(D(127),I5),
     &(D(157),NOUT),(D(120),IPQS),(D(121),LTQS),(D(164),ITFLUX),
     &(D(166),IFIXSR)
      COMMON /MAINC/ KKKK(100)
C
      DIMENSION C(1),Q(NL,ITJT),GR(IGM,NC),X(IT,NC),Y(JT,NC),S(ITJT,NL)
      DIMENSION B(2), NXYZ01(3), NXYZ02(3)
C
      INTEGER FLAG,G
C
      EQUIVALENCE (IA(21),IQB),(IA(32),IGEOM),(IA(33),IQR),(IA(34),IQT),
     &(IA(57),MM),(IA(58),NM),(IA(67),ITMM),(IA(68),JTMM),
     &(IA(71),ISPANC),(IA(72),IHMT),(IA(73),ISPANF),(IA(79),ICLIM),
     &(A(61),LC),(A(68),LQR1),(A(69),LQR2),(A(70),LQT1),(A(71),LQT2),
     &(A(76),LFL),(A(117),LQB1),(A(118),LQB2)
C
CSASA      REAL*8 B,NXYZ01,NXYZ02
      CHARACTER*8 B,NXYZ01,NXYZ02
C
      DATA      B/'SOURCE  ','FLUX    '/
      DATA NXYZ02/'   Y S  ','   Z S  ','   T S  '/
C
C
C     WHEN FLUXES ARE READ--- NC = 1 FOR IOP .LT. 0
C     WHEN FLUXES ARE READ--- NC = NL FOR IOP .GT. 0
C     NO FLUX INPUT NEEDED---ZEROS FOR ENTIRE FLUX BLOCK FOR IOP .EQ. 0
C     NO Q SOURCE INPUT NEEDED FOR IOP .EQ. 0
C     ADDITIONAL STANDARD OPERATIONS FOR BOTH FLUX AND SOURCE---
C     IOP=1  Q=GR(G,N) FOR ALL I AND J
C     IOP=2  Q=Q(G,N,I,J) AS INPUT
C     IOP=3  Q=GR(G,N)*INPUT Q(N,I,J)
C     IOP=4  Q=GR(G,N)*X(I,N)*Y(J,N)
C     ADDITIONAL INPUT FROM STANDARD INTERFACE FILES
C     IOP=5  Q=INTERFACE FILE INPUT
C
      WRITE (NOUT,490)B(FLAG+1)
      INTAPE=ITFLUX
      IF (FLAG.NE.0) GO TO 140
      INTAPE=IFIXSR
C     SET SOURCE INPUT AND PRINT TRIGGERS
      IOPLDQ=1
      IOPPTQ=0
      IF (I5.EQ.0) GO TO 150
      IF (I5-2) 100,110,120
  100 I5=1
      GO TO 150
  110 I5=0
      GO TO 130
  120 I5=1
  130 IOPLDQ=2
      IOPPTQ=1
      GO TO 150
  140 IF (IOP.EQ.0) WRITE (NOUT,430)
C
C     BEGIN PROCESSING
C
  150 CONTINUE
      REWIND INTAPE
      DO 420 G=1,IGM
      IF (IOP.NE.0) GO TO 160
      IF (FLAG.NE.0) GO TO 410
      IF ((IQR+IQB+IQT).EQ.0) GO TO 380
      IF (G.EQ.1) WRITE (NOUT,440)
      GO TO 350
  160 CONTINUE
      DO 340 N=1,NC
      ITEMP=N-1
      GO TO (200,180,200,200,170), IOP
C
C     READ FLUX OR SOURCE FROM STANDARD INTERFACE FILE
C
  170 CONTINUE
C     CALL IFINQF (Q,S,NL,ITJT,NC,FLAG,N,G)
      READ (INTAPE) (Q(N,I),I=1,ITJT)
CKH   WRITE(6,600) G,INTAPE
CKH   WRITE(6,610) (Q(N,I),I=1,ITJT)
CK600 FORMAT(' FLUX GUESS(GROUP',I2,') READ FROM FILE',I2,' READQF')
CK610 FORMAT(10E12.4)
      GO TO 340
C     LOAD EVERY GROUP
  180 IF (ITEMP.NE.0) GO TO 190
      WRITE (NOUT,450)G
      GO TO 220
  190 WRITE (NOUT,460)ITEMP,G
      GO TO 220
C     LOAD ONLY SHAPES
  200 IF (G.NE.1) GO TO 220
      IF (ITEMP.NE.0) GO TO 210
      WRITE (NOUT,470)
      GO TO 220
  210 WRITE (NOUT,480)ITEMP
  220 CONTINUE
      GO TO (230,250,280,310), IOP
C
C     GROUP AND N DEPENDENT CONSTANT FOR ALL I AND J
C
  230 CONTINUE
      IF (G.EQ.1) CALL LOAD (B(FLAG+1),'ESHAPE',GR(1,N),GR(1,N),IGM,1)
      DO 240 I=1,ITJT
  240 Q(N,I)=GR(G,N)
      GO TO 340
C
C     COMPLETE ARRAY
C
  250 IOPNOW=IOPLDQ
      IF (FLAG.EQ.0) GO TO 260
      IOPNOW=1
      IF (I1.NE.0) IOPNOW=2
  260 CALL LOAD (B(FLAG+1),'      ',S(1,N),S(1,N),ITJT,IOPNOW)
      DO 270 I=1,ITJT
  270 Q(N,I)=S(I,N)
      GO TO 340
C
C     E SHAPE TIMES I,J SHAPE
C
  280 IF (G.GT.1) GO TO 290
      CALL LOAD ('   E S','HAPE  ',GR(1,N),GR(1,N),IGM,1)
      CALL LOAD (B(FLAG+1),'      ',S(1,N),S(1,N),ITJT,1)
  290 DO 300 I=1,ITJT
  300 Q(N,I)=GR(G,N)*S(I,N)
      GO TO 340
  310 IF (G.GT.1) GO TO 320
C
C     E SHAPE TIMES I SHAPE TIMES J SHAPE
C
      CALL LOAD ('   E S','HAPE  ',GR(1,N),GR(1,N),IGM,1)
      CALL LOAD (NXYZ01(IGEOM),'HAPE  ',X(1,N),X(1,N),IT,1)
      CALL LOAD (NXYZ02(IGEOM),'HAPE  ',Y(1,N),Y(1,N),JT,1)
  320 DO 330 J=1,JT
      DO 330 I=1,IT
      JA=(J-1)*IT+I
  330 Q(N,JA)=GR(G,N)*X(I,N)*Y(J,N)
  340 CONTINUE
      IF (FLAG.GT.0) GO TO 410
C
C     READ BOUNDARY SOURCES (IF ANY)
C
  350 IF (IQR.EQ.0) GO TO 360
CKSK  CALL LOAD ('RT. BD','. FL.1',A(LQR1),A(LQR1),JTMM,IOPLDQ)
CKSK  CALL LOAD ('RT. BD','. FL.2',A(LQR2),A(LQR2),JTMM,IOPLDQ)
      CALL LOAD ('RT. BD','. FL.1',AAA(LQR1),AAA(LQR1),JTMM,IOPLDQ)
      CALL LOAD ('RT. BD','. FL.2',AAA(LQR2),AAA(LQR2),JTMM,IOPLDQ)
  360 IF (IQB.EQ.0) GO TO 370
CKSK  CALL LOAD ('BT. BD','. FL.1',A(LQB1),A(LQB1),ITMM,IOPLDQ)
CKSK  CALL LOAD ('BT. BD','. FL.2',A(LQB2),A(LQB2),ITMM,IOPLDQ)
      CALL LOAD ('BT. BD','. FL.1',AAA(LQB1),AAA(LQB1),ITMM,IOPLDQ)
      CALL LOAD ('BT. BD','. FL.2',AAA(LQB2),AAA(LQB2),ITMM,IOPLDQ)
  370 IF (IQT.EQ.0) GO TO 380
CKSK  CALL LOAD ('TOP BD','. FL.1',A(LQT1),A(LQT1),ITMM,IOPLDQ)
CKSK  CALL LOAD ('TOP BD','. FL.2',A(LQT2),A(LQT2),ITMM,IOPLDQ)
      CALL LOAD ('TOP BD','. FL.1',AAA(LQT1),AAA(LQT1),ITMM,IOPLDQ)
      CALL LOAD ('TOP BD','. FL.2',AAA(LQT2),AAA(LQT2),ITMM,IOPLDQ)
  380 CONTINUE
C
C     STORE SOURCES IN LCM
C
      CALL RITE (0,IPQS+(G-1)*LTQS,Q,LTQS,1)
      IF (IOPPTQ.NE.0) GO TO 420
C
C     PRINT BOUNDARY SOURCES
C
      IF (IQR.EQ.0) GO TO 390
      WRITE (NOUT,500)G
CKSK  CALL WWRITE ( 2,2,A(LQR1),JT,MM,1,'INPUT ','RTBY1 ','SOURCE',
      CALL WWRITE ( 2,2,AAA(LQR1),JT,MM,1,'INPUT ','RTBY1 ','SOURCE',
     &'DIREC ')
CKSK  CALL WWRITE ( 2,2,A(LQR2),JT,MM,1,'INPUT ','RTBY2 ','SOURCE',
      CALL WWRITE ( 2,2,AAA(LQR2),JT,MM,1,'INPUT ','RTBY2 ','SOURCE',
     &'DIREC ')
  390 IF (IQB.EQ.0) GO TO 400
      WRITE (NOUT,500)G
CKSK  CALL WWRITE ( 2,2,A(LQB1),IT,MM,1,'INPUT ','BTBY1 ','SOURCE',
      CALL WWRITE ( 2,2,AAA(LQB1),IT,MM,1,'INPUT ','BTBY1 ','SOURCE',
     &'DIREC ')
CKSK  CALL WWRITE ( 2,2,A(LQB2),IT,MM,1,'INPUT ','BTBY2 ','SOURCE',
      CALL WWRITE ( 2,2,AAA(LQB2),IT,MM,1,'INPUT ','BTBY2 ','SOURCE',
     &'DIREC ')
  400 IF (IQT.EQ.0) GO TO 420
      WRITE (NOUT,500)G
CKSK  CALL WWRITE ( 2,2,A(LQT1),IT,MM,1,'INPUT ','TPBY1 ','SOURCE',
      CALL WWRITE ( 2,2,AAA(LQT1),IT,MM,1,'INPUT ','TPBY1 ','SOURCE',
     &'DIREC ')
CKSK  CALL WWRITE ( 2,2,A(LQT2),IT,MM,1,'INPUT ','TPBY2 ','SOURCE',
      CALL WWRITE ( 2,2,AAA(LQT2),IT,MM,1,'INPUT ','TPBY2 ','SOURCE',
     &'DIREC ')
      GO TO 420
  410 CONTINUE
      CALL RITE (0,IPFX+(G-1)*LTFX,Q,LTFX,1)
  420 CONTINUE
      REWIND INTAPE
      RETURN
C
C
  430 FORMAT (/'0 ZERO FLUX')
  440 FORMAT (/'0 ZERO DISTRIBUTED SOURCE')
  450 FORMAT ('0 ISOTROPIC COMPONENT FOR GROUP ',I3)
  460 FORMAT ('0 ANISOTROPIC COMPONENT NUMBER ',I3,' FOR GROUP ',I3)
  470 FORMAT ('0 ISOTROPIC COMPONENT ')
  480 FORMAT ('0 ANISOTROPIC COMPONENT NUMBER ',I3)
  490 FORMAT (1H0,'* * * * *  INPUT FOR ',A6,' * * * * * ')
  500 FORMAT (//' GROUP ',I3)
      END