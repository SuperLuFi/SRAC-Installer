C     MEMBER DELTVP2 FOR IDRECT=2 WITHOUT TAYLOR EXPANSION
C     VECTORIZE BY LINE BAND
C
CVP
C     SUBROUTINE DELT2(NR,III,S,U,X,P,PR,MAT,SIG,LENG)
      SUBROUTINE DELT2B(NR,III,S,U,X,P,PR,MAT,SIG,LENG,PP,PPR,
     >                 IBANK)
C
C       CALLED BY ENERGY GROUP FOR CYLINDRICAL COORDINATE
C       IN CASE OF IDRECT=2
C     KIN FUNCTION IS INTERPOLATED FROM TABULATED VALUE OF DOUBLE
C     PRECISION STORED IN ABC11
C     L0: NUMBER OF CUTS IN THE SOURCE CELL ON A LINE
C     LLL: TOTAL NUMBER OF CUTS ON A LINE
C     WEIGHT: WEIGHT OF A LINE
C     III(L): REGION IDENTIFICATION NUMBER OF CUT L
C     MAT   : MATERIAL NUMBER
C     SIG :   MACRO CROSS SECTION OF MATERIAL M
C     S(L):   MACRO CROSS SECTION OF CUT L
C     X(L):   LENGTH OF CUT L
C     U(L):   OPTICAL LENGTH OF CUT L
C     N :    CURRENT ENERGY GROUP NUMBER
C     NG:    MAXIMUM NUMBER OF ENERGY GROUPS
C     NR   : TOTAL NUMBER OF REGION
C     NR1  : NR+1
C     IL   : FILE ALLOCATION NUMBER BETWEEN 1,2,3
C
      COMMON / PIJ2C / IDUM(5), IBOUND,IDRECT,LCOUNT,
     1         IDUM2(25),IL
      COMMON /BANK/ MAXL0(4),MAXLL(4),LBANK
      COMMON /ABC11/ A(5500),B(5500),C(5500)
      REAL   *8        A,B,C ,T1,T2,T3,T4
      DIMENSION III(LENG,*),SIG(*),U(LENG,*),X(LENG,*),S(LENG,*)
      DIMENSION P(NR,NR+1),PR(NR,NR+1),MAT(*)
CVP
      DIMENSION PP(NR,NR+1,IBANK),PPR(NR,NR+1,IBANK)
CVP
      DIMENSION LLL(512),L0(512),WEIGHT(514),WI(512)
     &         ,U0(512),T1(2,512),T2(2,512),T3(2,512)
     &         ,T4(2,512),II(512),JJ(512),UI(512)
     &         ,IEND(512),JEND(512),ELM1(512),ELM2(512)
C     INTEGRATION
      DO 100 J=1,NR+1
      DO 100 I=1,NR
      P (I,J)=0.
      PR(I,J)=0.
  100 CONTINUE
CVP
      DO 101 K=1,IBANK
      DO 101 J=1,NR+1
      DO 101 I=1,NR
      PP(I,J,K)=0.
      PPR(I,J,K)=0.
  101 CONTINUE
      CALL OPNBUF(IL)
*     WRITE(6,*) 'IL=',IL,' E.G.=',N, ' AT DELTVP2'
      LTIME=LCOUNT/LBANK+1
      LEFT=LCOUNT
      DO 1000 LBAND=1,LTIME
      LAST=MIN(LBANK,LEFT)
      DO 150 LINE=1,LAST
      CALL RDBUF(LLL(LINE),L0(LINE),WEIGHT(LINE),X(1,LINE)
     &           ,III(1,LINE),IL)
      IEND(LINE)=0
  150 CONTINUE
      DO 200 L=1,MAXLL(IL)
      DO 180 LINE=1,LAST
      IF(L.LE.LLL(LINE))THEN
      I       =III(L,LINE)
      S(L,LINE) = SIG(MAT(I       ))
      U(L,LINE) = S(L,LINE)*X(L,LINE)
                        ENDIF
  180 CONTINUE
  200 CONTINUE
C
      DO 400 L=1,MAXL0(IL)
      DO 310 LINE=1,LAST
      IF(L.LE.L0(LINE))                                   THEN
      II(LINE)=III(L,LINE)
      UI(LINE)=U(L,LINE)
*     IF(UI(LINE).GT. 0.003)                    THEN
C
C     SIG(L) .NE. 0
C
      WI(LINE)=WEIGHT(LINE)/S(L,LINE)
      U0(LINE)=-UI(LINE)
      T3(1,LINE)=UI(LINE)
      T3(2,LINE)=0.6666666666666D0*UI(LINE)
      T4(1,LINE)=0.
      T4(2,LINE)=0.
      JEND(LINE)=0
*                                               ELSE
*                    IEND(LINE)=1
*                                               ENDIF
                                                          ELSE
                     IEND(LINE)=1
                                                          ENDIF
  310 CONTINUE
      DO 340 LD=L,MAXLL(IL)
      DO 320 LINE=1,LAST
      IF(IEND(LINE).EQ.0 .AND. JEND(LINE).EQ.0) THEN
      ELM1(LINE)=0.
      ELM2(LINE)=0.
      JJ(LINE)=III(LD,LINE)
      UJ      =U(LD,LINE)
      U0(LINE)=U0(LINE)+UJ
      Y      =U0(LINE)
      T1(1,LINE)=T3(1,LINE)
      T1(2,LINE)=T3(2,LINE)
      T2(1,LINE)=T4(1,LINE)
      T2(2,LINE)=T4(2,LINE)
C     FKIN(3,Y) FKIN(5,Y)
      J=INT(100.*Y)+1
      IF(Y.GT.11.0) J=1100
      NN=J+2200
      DY=Y-0.01D0*DFLOAT(J-1)
      T3(1,LINE)= (B(NN)*DY+A(NN))*DY+C(NN)
      NN=J+4400
      T3(2,LINE)= (B(NN)*DY+A(NN))*DY+C(NN)
C     FKIN(3,Y) FKIN(5,Y)
      Y      =U0(LINE)+UI(LINE)
      J=INT(100.*Y)+1
      IF(Y.GT.11.0) J=1100
      NN=J+2200
      DY=Y-0.01D0*DFLOAT(J-1)
      T4(1,LINE)= (B(NN)*DY+A(NN))*DY+C(NN)
      NN=J+4400
      T4(2,LINE)= (B(NN)*DY+A(NN))*DY+C(NN)
      ELM1(LINE)=
     & WI(LINE)*(T1(1,LINE)-T2(1,LINE)-T3(1,LINE)+T4(1,LINE))/S(LD,LINE)
      ELM2(LINE)=
     & WI(LINE)*(T1(2,LINE)-T2(2,LINE)-T3(2,LINE)+T4(2,LINE))/S(LD,LINE)
                                                ENDIF
  320 CONTINUE
CVP
      DO 330 LINE=1,LAST,IBANK
      LE = MIN(LAST,LINE+IBANK-1)
      L2 = 0
*VOCL LOOP,NOVREC
      DO 330 L1 = LINE,LE
      L2 = L2 + 1
      IF(IEND(L1).EQ.0 .AND. JEND(L1).EQ.0) THEN
      PP (II(L1),JJ(L1),L2)=PP (II(L1),JJ(L1),L2) + ELM1(L1)
      PPR(II(L1),JJ(L1),L2)=PPR(II(L1),JJ(L1),L2) + ELM2(L1)
      IF(U0(L1).GT.6.0) JEND(L1)=1
      IF(LD.EQ.LLL(L1)) JEND(L1)=1
                                                ENDIF
  330 CONTINUE
  340 CONTINUE
C
CVP
      IJEND = 0
      DO 345 LINE=1,LAST
       IF (IEND(LINE).EQ.0 .AND. JEND(LINE).EQ.1) IJEND=1
  345 CONTINUE
      IF (IJEND .EQ. 0) GO TO 400
      DO 350 LINE=1,LAST,IBANK
      LE = MIN(LAST,LINE+IBANK-1)
      L2 = 0
*VOCL LOOP,NOVREC
      DO 350 L1 = LINE,LE
      L2 = L2 + 1
      IF(IEND(L1).EQ.0 .AND. JEND(L1).EQ.1) THEN
      PP (II(L1),NR+1,L2)= PP (II(L1),NR+1,L2)
     &   +WI(L1)*(T3(1,L1)-T4(1,L1))
      PPR(II(L1),NR+1,L2)= PPR(II(L1),NR+1,L2)
     &   +WI(L1)*(T3(2,L1)-T4(2,L1))
      JEND(L1)=2
                                                ENDIF
  350 CONTINUE
  400 CONTINUE
      LEFT=LEFT-LBANK
 1000 CONTINUE
CVP
      DO 500 L1 = 1,IBANK
      DO 500 I = 1,NR+1
      DO 500 J = 1,NR
      P (J,I) = P (J,I) + PP (J,I,L1)
      PR(J,I) = PR(J,I) + PPR(J,I,L1)
  500 CONTINUE
      RETURN
      END
