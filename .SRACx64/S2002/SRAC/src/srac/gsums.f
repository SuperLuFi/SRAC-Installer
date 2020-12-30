C             GSUMS               LEVEL=1        DATE=81.11.14
      SUBROUTINE GSUMS ( QG,FG,SIN,SS,SOUT,HL,RL,VL,TL,NL,ABG,BAL,FU,FD,
     &FR,FL,FUT,FDT,FRT,FLT,IM,JM,IP,JP )
C
      COMMON /TW1C/ DD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2,AA(130)
      DIMENSION D(212),A(132)
      EQUIVALENCE (D(1),DD(1)),(AAA(1),A(1))
C
      DIMENSION QG(1),FG(1),SIN(1),SS(1),SOUT(1),HL(1),RL(1),VL(1),
     &TL(1),NL(1),ABG(1),BAL(1),FU(IM,JP),FD(IM,JP),FUT(IM,JP),
     &FDT(IM,JP),FR(IP,JM),FL(IP,JM),FRT(IP,JM),FLT(IP,JM)
C
      INTEGER G
      REAL NL
C
      EQUIVALENCE (IA(4),IGM),(IA(62),IGP),( A( 9),TN2N),( A(21),TS),
     &( A(22),G),( A(23),ICONV)
C
      HL(G)=0.
      RL(G)=0.
      VL(G)=0.
      TL(G)=0.
      NL(G)=0.
      BAL(G)=0.
      IF (TS.LE.0.) GO TO 160
C     OUTSCATTER
      SOUT(G)=SOUT(G)-ABG(G)-SS(G)
C     LEAKAGES
      DO 100 J=1,JM
      RL(G)=RL(G)+FR(IP,J)-FL(IP,J)
  100 HL(G)=HL(G)+FR(IP,J)-FL(IP,J)-FR(1,J)+FL(1,J)
      DO 110 I=1,IM
      TL(G)=TL(G)+FU(I,JP)-FD(I,JP)
  110 VL(G)=VL(G)+FU(I,JP)-FD(I,JP)-FU(I,1)+FD(I,1)
      NL(G)=VL(G)+HL(G)
C     ACCUMULATE LEAKAGES OVER GROUPS
      DO 120 J=1,JM
      DO 120 I=1,IP
      FRT(I,J)=FRT(I,J)+FR(I,J)
  120 FLT(I,J)=FLT(I,J)+FL(I,J)
      DO 130 J=1,JP
      DO 130 I=1,IM
      FUT(I,J)=FUT(I,J)+FU(I,J)
  130 FDT(I,J)=FDT(I,J)+FD(I,J)
C     BALANCE
      IF (FG(G)+QG(G)+SIN(G).EQ.0.0) GO TO 140
      BAL(G)=1.-(NL(G)+ABG(G)+SOUT(G))/(FG(G)+QG(G)+SIN(G))
      GO TO 150
  140 BAL(G)=0.0
  150 CONTINUE
C
C     CORRECT FOR N,2N REACTIONS
C
      IF (G.EQ.1) T=0.0
      T=T+TN2N
C
C     ACCUMULATE OVER GROUPS
C
  160 IF (G.LT.IGM) RETURN
      SIN(IGP)=0.
      SS(IGP)=0.
      SOUT(IGP)=0.
      HL(IGP)=0.
      RL(IGP)=0.
      VL(IGP)=0.
      TL(IGP)=0.
      NL(IGP)=0.
      ABG(IGP)=0.
      DO 170 I=1,IGM
      SIN(IGP)=SIN(IGP)+SIN(I)
      SS(IGP)=SS(IGP)+SS(I)
      SOUT(IGP)=SOUT(IGP)+SOUT(I)
      HL(IGP)=HL(IGP)+HL(I)
      RL(IGP)=RL(IGP)+RL(I)
      VL(IGP)=VL(IGP)+VL(I)
      TL(IGP)=TL(IGP)+TL(I)
      NL(IGP)=NL(IGP)+NL(I)
  170 ABG(IGP)=ABG(IGP)+ABG(I)
      BAL(IGP)=1.-(NL(IGP)+ABG(IGP)+SOUT(IGP))/(FG(IGP)+QG(IGP)+SIN(IGP)
     &)
      SOUT(IGP)=SOUT(IGP)+T
      RETURN
      END
