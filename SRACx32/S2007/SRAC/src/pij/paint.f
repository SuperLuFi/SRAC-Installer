C **********************************************************************
C                             PAINT VP VERSION
C **********************************************************************
CITJ  SUBROUTINE PAINT( SIG,P,PR,G,XX,III,U,S,IDIM1,IDIM2,MAT,VOL)
      SUBROUTINE PAINT( SIG,P,PR,G,XX,III,U,S,IDIM1,IDIM2,MAT,VOL,SIGN)
      COMMON / PIJ2C / IGT,NZ,NR,NRR,NXR, IBOUND,IDRECT,LCOUNT,
     1          IEDPIJ,IFORM,NTTAB,NUTAB,SZ,IDUM2(18),ICOOD,NM,IL,
     2      IVP,IOPT,IDUM37(964)
      COMMON /PIJ1C/ DDDD(10),BETM
      COMMON /BANK/ MAXL0(4),MAXLL(4),LBANK
*     DIMENSION PP(100,100)
      DIMENSION  SIG(IDIM2,*),P(IDIM1,*),PR(IDIM1,*),G(*),XX(*),III(*)
     1          ,U(*),S(*),MAT(*),VOL(*)
CITJ  DIMENSION  SIGN(20)
      DIMENSION  SIGN(NM)
      DIMENSION         CCI(2,3),T1(2),T2(2),T3(2),T4(2)
      DIMENSION CG(2,3),WEIGHT(3)
      DATA CCI/1.0, 0.3333333,1.0,0.6666666,1.0,1.0/
      DATA CG/ 1.0,1.5,1.0,1.125, 1.0,1.0/
      ICOOD1=ICOOD+1
  100 NR1=IDIM1+1
      NPRL=58
      NPRBL=(IDIM1+9)/10+1
C     WRITE(6,*) 'IL=',IL,' AT PAINT'
      DO 999 N=1,IDIM2
      IF(ICOOD1.EQ.2)  THEN
      DO 110 M=1,NM
      SIGN(M)=SIG(N,M)
  110 CONTINUE
C
CM    WRITE(6,*) ' ***PAINT OPTION IOPT=',IOPT,' IVP=',IVP
CM   &  ,' IDRECT=',IDRECT
C
      IF(IOPT.LE.2.AND.IDRECT.EQ.2)    THEN
            IF(IVP.EQ.0) THEN
C     MODULE DELTSC2,DELTVP2
      CALL DELT2(IDIM1,III,S,U,XX,P,PR,MAT,SIGN,MAXLL(IL))
                         ELSE
C     MODULE DELTSC4,DELTVP4
      CALL DELT4(IDIM1,III,S,U,XX,P,PR,MAT,SIGN,MAXLL(IL))
                         ENDIF
                                       ELSE
            IF(IVP.EQ.0) THEN
C     MODULE DELTSC ,DELTVP
      CALL DELT (IDIM1,III,S,U,XX,P   ,MAT,SIGN,MAXLL(IL))
                         ELSE
C     MODULE DELTSC3,DELTVP3
      CALL DELT3(IDIM1,III,S,U,XX,P   ,MAT,SIGN,MAXLL(IL))
                         ENDIF
                                       ENDIF
                      GO TO 900
                       ENDIF
      CALL OPNBUF(IL)
      DO 120 I=1,IDIM1
      DO 120 J=1,NR1
      P(I,J)=0.
      IF(IDRECT.EQ.2)
     *PR(I,J)=0.
  120 CONTINUE
      DO 400 LINE=1,LCOUNT
      CALL RDBUF(LLL,L0,WEIGHT,XX,III,IL)
*     WRITE(6,*)' COUNT=',LINE,' LLL=',LLL,' L0=',L0,' WEIGHT=', WEIGHT
*     WRITE(6,*) '  III=',(III(L),L=1,LLL)
*     WRITE(6,*) '  XX =',(XX (L),L=1,LLL)
      DO 200 L=1,LLL
      I=III(L)
      M=MAT(I)
      S(L) = SIG(N,M)
      U(L) = S(L)*XX(L)
  200 CONTINUE
C
      DO 399 L=1,L0
      I=III(L)
      UI=U(L)
      IF(UI.LT.0.01) GO TO 350
C
C     SIG(L) .NE. 0  SIG(LD) .NE. 0
C
      WI=WEIGHT(1)/S(L)
      U0=-UI
      T3(1)=UI
      T4(1)=0.
      IF(IDRECT.EQ.1) GO TO 305
      T3(2)=CCI(2,ICOOD1)*UI
      T4(2)=0.
  305 DO 340 LD=L,LLL
      J=III(LD)
      UJ=U(LD)
      IF(UJ.LE.0.01) GO TO 325
C     SIG(L).NE.0  SIG(LD).NE.0
      U0=U0+UJ
      GO TO (315,310),IDRECT
  310 T1(2)=T3(2)
      T2(2)=T4(2)
      CALL TWO (U0,UI,T3(2),T4(2),ICOOD1,5)
      PR(I,J)=PR(I,J)+WI*(T1(2)-T2(2)-T3(2)+T4(2))/S(LD)
  315 T1(1)=T3(1)
      T2(1)=T4(1)
      CALL TWO (U0,UI,T3(1),T4(1),ICOOD1,3)
      P (I,J)=P (I,J)+WI*(T1(1)-T2(1)-T3(1)+T4(1))/S(LD)
      IF(U0.GT.9.0) GO TO 399
      GO TO 340
C     SIG(L).NE.0 SIG(LD).EQ.0
  325 U0=U0+UJ
      GO TO (335,330),IDRECT
  330 CALL ONE (U0,T1(2),ICOOD1,4)
      CALL ONE (U0+UI,T2(2),ICOOD1,4)
      CALL TWO (U0,UI,T3(2),T4(2),ICOOD1,5)
      PR(I,J)=PR(I,J)+WI*(T1(2)-T2(2))*XX(LD)
  335 CALL ONE (U0,T1(1),ICOOD1,2)
      CALL ONE (U0+UI,T2(1),ICOOD1,2)
      CALL TWO (U0,UI,T3(1),T4(1),ICOOD1,3)
      P (I,J)=P (I,J)+WI*(T1(1)-T2(1))*XX(LD)
  340 CONTINUE
C     IF(IBOUND.EQ.1) GO TO 399
      P (I,NR1)=P (I,NR1)+WI*(T3(1)-T4(1))
      IF(IDRECT.EQ.1) GO TO 399
      PR(I,NR1)=PR(I,NR1)+WI*(T3(2)-T4(2))
      GO TO 399
C     SIG(L).EQ.0
  350 FACT=0.5
      U0=-UI/2.
      T2(1)=CCI(1,ICOOD1)
      T2(2)=CCI(2,ICOOD1)
      WI=WEIGHT(1)*XX(L)
      DO 390 LD=L,LLL
      J=III(LD)
      UJ=U(LD)
      IF(UJ.LE.0.01) GO TO 375
C     SIG(L).EQ.0  SIG(LD).NE.0
      U0=U0+UJ
      T1(1)=T2(1)
      T1(2)=T2(2)
      GO TO (365,360),IDRECT
  360 CALL ONE (U0,T2(2),ICOOD1,4)
      PR(I,J)=PR(I,J)+WI*(T1(2)-T2(2))/S(LD)
  365 CALL ONE (U0,T2(1),ICOOD1,2)
      P (I,J)=P (I,J)+WI*(T1(1)-T2(1))/S(LD)
      IF(U0.GT.9.0) GO TO 399
      GO TO 390
C
C     SIG(L).EQ.0  SIG(LD).EQ.0
C
  375 CONTINUE
      U0=U0+UJ
      GO TO (385,380),IDRECT
  380 CALL ONE (U0,T1(2),ICOOD1,3)
      CALL ONE (U0,T2(2),ICOOD1,4)
      PR(I,J)=PR(I,J)+WI*T1(2)*XX(LD)*FACT
  385 CALL ONE (U0,T1(1),ICOOD1,1)
      CALL ONE (U0,T2(1),ICOOD1,2)
      P (I,J)=P (I,J)+WI*T1(1)*XX(LD)*FACT
      FACT=1.0
  390 CONTINUE
C     IF(IBOUND.EQ.1) GO TO 399
      P(I,NR1)=P(I,NR1)+WI*T2(1)
      IF(IDRECT.EQ.1) GO TO 399
      PR(I,NR1)=PR(I,NR1)+WI*T2(2)
  399 CONTINUE
  400 CONTINUE
C
  900 CONTINUE
*     IF(N.EQ.1) THEN
*     WRITE(6,'(A,5E12.5)') ' SIGMA 1G PAINT=',(SIG(N,M),M=1,5)
*     DO 1200 I=1,IDIM1
*     SUM=0.
*     DO 1100 J=1,IDIM1
*     PP(I,J)=P(I,J)*SIG(N,MAT(J))/VOL(I)/BETM
*     PP(I,J)=P(I,J)              /VOL(I)/BETM
*     SUM=SUM+PP(I,J)
*1100 CONTINUE
*     PP(I,NR1)=P(I,NR1)/VOL(I)/BETM
*     SUM=SUM+PP(I,NR1)
*     WRITE(6,*) 'PIJ FALSE I=',I
*     WRITE(6,'(10F12.4)') (PP(I ,J),J=1,IDIM1)
*     WRITE(6,'(10F12.4)')  PP(I ,NR1),SUM
*1200 CONTINUE
*                 ENDIF
      CALL FORM(CG(1,ICOOD1),P ,N,SIG,NPRL,NPRBL,1,G,IDIM1,NR1,
     1     IDIM2,NM,MAT,VOL)
      IF(IDRECT.EQ.2)
     1CALL FORM(CG(2,ICOOD1),PR,N,SIG,NPRL,NPRBL,2,G,IDIM1,NR1,
     1     IDIM2,NM,MAT,VOL)
  999 CONTINUE
      RETURN
      END
