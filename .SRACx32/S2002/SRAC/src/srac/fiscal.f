C             FISCAL              LEVEL=1        DATE=81.11.14
      SUBROUTINE FISCAL ( C,FG,FISSA,IDX,IDYA,A5,YH,IDCS,IDY,QG,CHIA,
     &IHM,MT,IT,JT,FT,IM,JM,XDF,YDF )
C
      COMMON /TW1C/ DD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2,AA(132)
      DIMENSION D(212),A(132)
      EQUIVALENCE (D(1),DD(1)),(AAA(1),A(1))
      EQUIVALENCE (D(115),IPXS),(D(112),IPFX),(D(113),LTFX),(D(118),LTOX
     &S),(D(117),LTXS)
C
      DIMENSION C(IHM,MT),FG(1),FISSA(IT,JT),IDX(1),IDYA(1),A5(1),YH(1),
     &IDCS(1),IDY(1),QG(1),CHIA(1),FT(IM,JM),XDF(1),YDF(1)
      DIMENSION NA(1)
      DIMENSION ER(9)
C
C     MATERIAL MESH INDEXING
C
CKSK  EQUIVALENCE (NA(1),D(1))
      EQUIVALENCE (NA(1),DD(1))
      EQUIVALENCE (D(177),LDXC),(D(178),LDYC)
C
      INTEGER G
      REAL NORM
C
      EQUIVALENCE (IA(1),ITH),(IA(4),IGM),(IA(11),IEVT),(IA(48),NORM),
     &(IA(52),IHF),(IA(56),IMJM),(IA(62),IGP),(IA(73),ISPANF),
     &(A(76),LFL),(A(86),LQQ),(A(88),LFUT),(A(28),ALA),(A(30),FTP),
     &(A(31),IFN)
C
CSASA REAL*8 ER
      CHARACTER*8 ER
C
      DATA ER/ 'TOTAL ','FISSIO','N SOUR','CE .LE','. ZERO',', CANN',
     &'OT BE ','NORMAL','IZED  '/
C
C     CALCULATE VOLUME INTEGRAL OF FISSION SOURCE
C
      FG(IGP)=0.
      CALL CLEARW(0.0,FT,IMJM)
      IF (ITH.EQ.0) GO TO 120
C
C     ADJOINT
C
CKH
CKH   WRITE(6,600)
CK600 FORMAT(' FISSA(I,J),A5(I),YH(J),C(IHF,IC),XDF,YDF,T --- FISCAL')
      DO 110 G=1,IGM
      CALL REED (0,IPXS+(G-1)*LTXS,C,LTOXS,1)
      FG(G)=0.0
      DO 100 J=1,JT
      DO 100 I=1,IT
      IZ=NA(LDXC+I-1)+NA(LDYC+J-1)
      IC=IABS(IDCS(IZ))
      IB=IDX(I)
      JA=IDYA(J)
      T=C(IHF,IC)*XDF(I)*YDF(J)*FISSA(I,J)*A5(I)*YH(J)
      FT(IB,JA)=FT(IB,JA)+T
CKH
CKH   WRITE(6,610) FISSA(I,J),A5(I),YH(J),C(IHF,IC),XDF(I),YDF(J),T
CH610 FORMAT(7E12.4)
CKH
  100 FG(G)=FG(G)+T
  110 FG(IGP)=FG(IGP)+FG(G)
      GO TO 150
C
C     DIRECT
C
  120 T=0.
CKH
CKH   WRITE(6,600)
CK600 FORMAT(' FISSA(I,J),A5(I),YH(J),FT(IB,JA),CHIA(IGP) --- FISCAL')
CKH
      DO 130 J=1,JT
      DO 130 I=1,IT
      IB=IDX(I)
      JA=IDYA(J)
      TA=FISSA(I,J)*A5(I)*YH(J)
      FT(IB,JA)=FT(IB,JA)+TA*CHIA(IGP)
CKH
CKH   WRITE(6,610) FISSA(I,J),A5(I),YH(J),FT(IB,JA),CHIA(IGP)
CK610 FORMAT(5E12.4)
CKH
  130 T=T+TA
      DO 140 G=1,IGM
      FG(G)=CHIA(G)*T
CKH
CKH   WRITE(6,620) G,CHIA(G),T
CK620 FORMAT(' G=',E12.4,2X,'CHIA(G)=',E12.4,2X,'T=',E12.4)
CKH
  140 FG(IGP)=FG(IGP)+FG(G)
C
C     LAMBDA CALCULATION    IFN SET TO 1 IN INITAL
C     FTP=FISSION TOTAL PREVIOUS, STORED IN INITAL AND GRIND2
C
  150 IF (IFN.GT.0) GO TO 190
      ALA=(FG(IGP)+QG(IGP))/(FTP+QG(IGP))
      IF (IEVT.NE.1) GO TO 190
C
C     KEFF REDUCTIONS
C
      T=1.0/ALA
      DO 160 G=1,IGP
      FG(G)=T*FG(G)
  160 CHIA(G)=T*CHIA(G)
      DO 170 J=1,JM
      DO 170 I=1,IM
  170 FT(I,J)=T*FT(I,J)
      IF (ITH.EQ.0) GO TO 190
      DO 180 J=1,JT
      DO 180 I=1,IT
  180 FISSA(I,J)=T*FISSA(I,J)
C
C     PERFORM NORMALIZATIONS
C
  190 IFN=0
      IF (IEVT.EQ.0) GO TO 260
      IF (NORM.EQ.0.) GO TO 260
      IF (FG(IGP).GT.0.0) GO TO 200
      CALL ERROR (4,ER,9)
      GO TO 260
  200 T=NORM/FG(IGP)
      DO 210 G=1,IGP
  210 FG(G)=T*FG(G)
      DO 220 J=1,JT
      DO 220 I=1,IT
  220 FISSA(I,J)=T*FISSA(I,J)
      J=LFL+LTFX-1
      DO 240 G=1,IGM
CKSK  CALL REED (0,IPFX+(G-1)*LTFX,A(LFL),LTFX,1)
      CALL REED (0,IPFX+(G-1)*LTFX,AAA(LFL),LTFX,1)
      DO 230 I=LFL,J
CK230 A(I)=T*A(I)
  230 AAA(I)=T*AAA(I)
CKSK  CALL RITE (0,IPFX+(G-1)*LTFX,A(LFL),LTFX,1)
      CALL RITE (0,IPFX+(G-1)*LTFX,AAA(LFL),LTFX,1)
  240 CONTINUE
C
C     NORMALIZE COARSE MESH TOTALS
C
      J=LQQ-1
      DO 250 I=LFUT,J
CK250 A(I)=T*A(I)
  250 AAA(I)=T*AAA(I)
  260 RETURN
      END
