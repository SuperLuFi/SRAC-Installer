      SUBROUTINE P1B1(MTNM,F0,F1,LENG,E,W,ICELL)
C
C     SUBPROGRAM TO CALCULATE THE NEUTRON SPECTRUM BY B1 OR P1 APPR.
C     SOURCE FED BY THE MEMBER FISSYILD IN FASTU FILE
C     NEGATIVE BSQ ACCEPTABLE
C
C     MTNM  MATERIAL NAME BY A8(IN)
C     F0    P0 COMPONENT OF NEUTRON SPECTRUM (OUT)
C     F1    P1 COMPONENT OF NEUTRON SPECTRUM (OUT)
C     LENG  LENGTH OF MATRICES               (IN/OUT)
C     E     ENERGY BOUNDARIES                (IN)
C     W     WEIGHT USED FOR THERMAL GUESS    (IN)
C     ICELL =0 FOR MIXTURE,
C           =1  FOR CELL AVERAGE             (IN)
C     IF ICELL = 1 , IOPT(16) > 0
C
C     LENG    1  2  3  4  5  6  7  8
C     ===============================
C     E RANG  F  F  F  T  T  T  A  A
C     IPL     TR 0  1  TR 0  1  TR TR
C     FINE    F  F  F  F  F  F  F  C
C     ===============================
C
C
      CHARACTER*4   FILENM
C
      COMMON /MAINC / IOPT(100),AA(900)
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
C
      COMMON /WORK  / ALPHA(80),X0(80),X1(48),SIGT(80),SIGA(107),
     *                SIGSS0(80),SIGSS1(80),F00(48),F10(48),
     *                A0(7500),A1(7500),AN2N(7500),SIGN2N(107),
     *                S0(107),S1(107),X00(80)
C
      DIMENSION       I0(7500),I1(7500),I2(7500)
      DIMENSION       F0(107),F1(107),E(108),W(107),LENG(8)
C
      CHARACTER*4     MTNM(2),NODE(2),NODE2(2),IPB1(2)
C
      EQUIVALENCE    (NGRF ,IOPT(55)),(NGRT ,IOPT(56))
      EQUIVALENCE    (BSQ  ,IOPT(61)),(NOUT1,IOPT(64)),(NOUT2,IOPT(65))
      EQUIVALENCE    (IOPT(42),ICOND)
C
      EQUIVALENCE   (A0(1),I0(1)),(A1(1),I1(1))
      EQUIVALENCE   (AN2N(1),I2(1))
CTFREE
      REAL*4          REBA(48),REBQ(48),REBF(48),REBR(48),REBS(48,48)
CEND
CDEL  DATA           OMEGA /1.2/  , EPS/0.0001/ , ITMAX/100/
      DATA           OMEGA /1.200/, EPS/0.00001/, ITMAX/200/
      DATA           EPSOV /0.001/
CKSK  DATA           IPB1  /3HP1 ,3HB1 /
      DATA           IPB1  /'P1  ','B1  '/
C
C === START OF PROCESS
C
      ICOUNT = 0
      IC16SV = IOPT(16)
C
   10 CONTINUE
      IXB1   = 0
      IFNEGA = 0
C
      IF(IOPT(19).NE.0.AND.IOPT(16).NE.0)
     +                 WRITE(NOUT2,410) MTNM,IPB1(IOPT(16))
      IF(IOPT(19).GT.1.AND.IOPT(16).NE.0)
     +                 WRITE(NOUT2,420) MTNM,LENG
C
      IFLSW     = 1
      FILENM(1) = 'FAST'
      FILENM(2) = 'U   '
      NODE(1)   = 'FISS'
      NODE(2)   = 'YILD'
CM    CALL READ(NODE,X0,NGRF)
      CALL READ(NODE,X00,NGRF)
C ==  MEMBER CHECK
      FILENM(1) = 'MACR'
      FILENM(2) = 'OWRK'
      NODE(1)   = MTNM(1)
      NODE(2)   = MTNM(2)
      NODE(2) (1:1) = 'F'
      IF(LENG(1).GT.0) GO TO 2000
C
C     IF IOPT(16)=0  CONSISTENT TRANSPORT CORRECTION
C     IF IOPT(16)=1  P1 APPROX.
C     IF IOPT(16)=2  B1 APPROX.
C     IF IOPT(16)=3  READ F0,F1 FROM FLUX FILE
C
      LGTF0         = LENG(2)
C === CHECK THE PRESENCE OF P0 MACRO. COMPONENT
      IF(LGTF0.EQ.0) GO TO 900
      L0            = 0
      L1            = 0
      CALL  CLEA ( A0 , 7500 , 0.0 )
      CALL  CLEA ( A1 , 7500 , 0.0 )
      CALL  CLEA (AN2N, 7500 , 0.0 )
      CALL  CLEA ( S0 ,  107 , 0.0 )
      CALL  CLEA ( S1 ,  107 , 0.0 )
C
      NODE(2) (4:4) = '4'
      CALL READ ( NODE , A0 , LGTF0 )
      LGTF1         = LENG(3)
C === CHECK THE PRESENCE OF P1 MACRO. COMPONENT
      IF(LGTF1.EQ.0) GO TO 880
      NODE(2) (4:4) = '3'
      CALL READ ( NODE , A1 , LGTF1 )
C
      ISWKAI        =  0
      IF(A0(7).GT.0.0) ISWKAI = 1
C*****READ (N,2N) DATA
      NODE(2) (4:4) = 'M'
      LTHN2N        =  0
      JSW           =  0
      CALL SEARCH(NODE,LTHN2N,JSW)
      IF(JSW.EQ.0)     THEN
                       CALL READ  (NODE,AN2N,LTHN2N)
                       ELSE
                       LTHN2N  = -1
                       ENDIF
C**** SET SIGN2N DATA
      CALL CLEA ( SIGN2N,  107 , 0.0 )
      IF(LTHN2N.GT.0) THEN
                      L2         = 0
                      DO 11   NG = 1,NGRF
                      SIGN2N(NG) = AN2N(L2+6)
                      L2         = L2+10+I2(L2+2)
   11                 CONTINUE
*                     WRITE(6,*) ' ** SIGN2N CHECK WRITE AT P1B1 ** '
*                     WRITE(6,'(1H ,1P10E11.4)') (SIGN2N(I),I=1,NGRF)
                      ENDIF
C
      DO 20  NG = 1,NGRF
      ALPHA(NG) = 1.0
      SIGT(NG)  = A0(L0+6)
      SIGA(NG)  = A0(L0+10)
CDEL  SIGA(NG)  = A0(L0+10) - SIGN2N(NG)
      LOCSS     = L0+10+I0(L0+1)
      X0  (NG)  = A0(L0+7)
      IF(ISWKAI.EQ.0)  X0(NG) = X00(NG)
      SIGSS0(NG)= A0(LOCSS)
      IF(IOPT(16).EQ.0) THEN
                        A0(LOCSS) = A0(LOCSS) - A1(L1+6)*0.33333333
                        A0(L0+6)  = A0(L0+6)  - A1(L1+6)*0.33333333
                        A0(L0+8)  = 0.333333333/A0(L0+6)
                        ENDIF
      SIGSS1(NG)= A1(L1+10+I1(L1+1))
      L0        = L0+10+I0(L0+2)
      L1        = L1+10+I1(L1+2)
      IF (IOPT(16).LE.1) GO TO 20
CDEL
C     X    = SQRT(ABS(BSQ))/SIGT(NG)
C     IF(X .GT.1.) IXB1 = IXB1 + 1
C     X2   = BSQ/SIGT(NG)/SIGT(NG)
C     IF(X.GE.0.2)  THEN
C                   IF(BSQ.GT.0.) A = ATAN (X)/X
C                   IF(BSQ.LT.0.) A = ATANH(X)/X
C                   ALPHA(NG) = X2*A/3.0/(1.0-A)
C                   ELSE
C                   ALPHA(NG) = (1.0-SIGN(0.333333,BSQ)*X2+  0.2*X2*X2)
C    *                         /(1.0-SIGN(0.6,BSQ)*X2 + 0.428571*X2*X2)
CEND                ENDIF
CADD
      Y  =  BSQ/SIGT(NG)/SIGT(NG)
      IF(ABS(Y).GT.0.1) THEN
                        IF(BSQ.GT.0.0) THEN
                                X   = SQRT(Y)
                                XX  = ATAN(X)
                                       ELSE
                                YY  = SQRT(-Y)
                                X   = AMIN1(YY,0.99900)
                                XX  = 0.5000*ALOG((1.0000+X)/(1.0000-X))
                                       ENDIF
                         ALPHA(NG)  = 0.33333333*Y*XX/(X-XX)
                         ELSE
                         X  = 0.33333333-Y*(0.2000000-0.1428572*Y)
                         ALPHA(NG)  = 0.33333333*( 1.00000/X - Y )
                         ENDIF
C
   20 CONTINUE
      IF(IOPT(16).EQ.0) GO TO 53
C
C === CALCULATE FAST NEUTRON SPECTRUM WHEN IOPT(16) > 0
C
      L0      = 0
      L1      = 0
      IF(IOPT(19).GT.1) WRITE(NOUT2,25)
      ISW16   = 0
      IF(IOPT(16).EQ.1.OR.IOPT(16).EQ.2) ISW16 = 1
C
      DO 50 NG=1,NGRF
      IF( ISW16 .EQ. 1 ) THEN
      F0(NG)   = ( (S0(NG)+X0(NG))*(3.*ALPHA(NG)*SIGT(NG)-SIGSS1(NG))
     *         - S1(NG))/((SIGT(NG)-SIGSS0(NG))*(3.*ALPHA(NG)*SIGT(NG)
     *         - SIGSS1(NG))  + BSQ )
      F1(NG)= (S1(NG)+ BSQ*F0(NG))/(3.*ALPHA(NG)*SIGT(NG)-SIGSS1(NG))
                         ENDIF
C
      IF(IOPT(19).GT.1)  THEN
        WRITE(NOUT2,26)  F0(NG),F1(NG),X0(NG),S0(NG),S1(NG),
     *                   ALPHA(NG),SIGT(NG),SIGSS0(NG),SIGSS1(NG),BSQ
                         ENDIF
C
      IF(F0(NG)    .LT.0.0) IFNEGA = 1
CMOD  IF(F1(NG)*BSQ.LT.0.0) IFNEGA = 1
      IF(F1(NG)*BSQ.LE.0.0) IFNEGA = 1
C
C     TRANSPORT CROSS SECTION
C     SIGTR=BSQ/3.*F0(NG)/F1(NG)
C     DIFFER=A0(L0+6)-SIGTR
C
      DIFFER    = (1.0-ALPHA(NG))*SIGT(NG)
CMOD *          +  0.33333333*(S1(NG)+F1(NG)*SIGSS1(NG))/F1(NG)
      IF(F1(NG).NE.0.0) THEN
         DIFFER = DIFFER + 0.33333333*(S1(NG)+F1(NG)*SIGSS1(NG))/F1(NG)
                ENDIF
CEND
      A0(L0+6)  = A0(L0+6)-DIFFER
C     DIFFUSION COEFFICIENT
      A0(L0+8)  = 0.333333333/A0(L0+6)
      A0(L0+10+I0(L0+1)) = A0(L0+10+I0(L0+1))-DIFFER
      LGT       = I0(L0+2)
      L0        = L0 + 10
      DO 30   J = 1,LGT
      L0        = L0 +  1
      S0(NG+J-1)= S0(NG+J-1) +  A0(L0)*F0(NG)
   30 CONTINUE
      LGT       = I1(L1+2)
      L1        = L1 + 10
      DO 40   J = 1,LGT
      L1        = L1 + 1
      S1(NG+J-1)= S1(NG+J-1) +  A1(L1)*F1(NG)
   40 CONTINUE
   50 CONTINUE
C
      IF(IOPT(16).EQ.3.OR.IFNEGA.EQ.0) GO TO 53
C
      IF(ICELL.EQ.0) THEN
                     WRITE(NOUT1,502) MTNM,IOPT(16)
                     IOPT(16) = 0
                     LENG( 1) = 0
                     LENG( 4) = 0
                     GO TO 10
                     ENDIF
C
      WRITE(NOUT1,385) MTNM
      STOP
C
C     END OF FAST NEUTRON SPECTRUM CALCULATION
C
   53 NODE(2) (4:4) = '2'
C === OUTPUT NAMEFBN2 TO MACROWRK FILE ONLY MIXTURE CASE
      IF(ICELL.EQ.0) THEN
                     IF(ICOUNT.GT.0) CALL DELETE(NODE)
                     LENG(1)  = LGTF0
                     CALL WRITE(NODE,A0,LGTF0)
                     ENDIF
C
C     WRITE(NOUT2,51) NODE
C  51 FORMAT(1H0,10X,'MACRO-CROSS-SECTION ***',2A4,'*** WRITTEN  '
C    * ,'BY ',A3,' APPROXIMATION')
C
C
C ==  THERMAL RANGE
C
      IF(IOPT(4).EQ.0) GO TO 216
      NGR           = NGRF
      CALL  CLEA ( A0 , 7500 , 0.0 )
      CALL  CLEA ( A1 , 7500 , 0.0 )
C
      NODE(2) (1:1) = 'T'
      NODE(2) (4:4) = '4'
      LGT0          = LENG(5)
      LENGTH        = LENG(5)
      IPL           = 1
      IF(LENGTH.EQ.0) THEN
                      LGT0          = LENG(4)
                      NODE(2) (4:4) = '2'
                      IPL           = 0
                      ENDIF
C
      IF(LGT0.EQ.0)  GO TO 900
      CALL READ(NODE,A0,LGT0)
C
      IF(IPL.EQ.1) THEN
                   NODE(2) (4:4) = '3'
                   LGT1          = LENG(6)
                   IF(LGT1.EQ.0) GO TO 890
                   CALL READ (NODE,A1,LGT1)
                   ENDIF
C
      NGR      = NGRF+NGRT
      L0       = 0
      L1       = 0
      XNORM    = 0.0
      ISW16    = 0
      ISWPL1   = 0
      IF(IOPT(16).EQ.1.OR.IOPT(16).EQ.2) ISW16  = 1
      IF(IOPT(16).EQ.0.AND.IPL.EQ.1)     ISWPL1 = 1
C
      RATIO      = F0(NGRF)/W(NGRF)
      IF(RATIO.LE.0.0)  RATIO = 1.00000
*     WRITE(6,*) ' *** NGRF RATIO *** ',NGRF,RATIO,F0(NGRF),W(NGRF)
C
      CALL CLEA(  REBQ , 48 , 0.0 )
C
      DO 60  NG = 1,NGRT
      X0(NG)    = S0(NG + NGRF)
      REBQ(NG)  = S0(NG + NGRF)
      XNORM     = XNORM + X0(NG)
      X1(NG)    = S1(NG + NGRF)
CDEL  IF(ISW16.EQ.1) F0(NG+NGRF) = W(NGRF+NG)
      IF(ISW16.EQ.1) F0(NG+NGRF) = W(NGRF+NG)*RATIO
      SIGSS0(NG)= 0.0
      SIGSS1(NG)= 0.0
      ALPHA(NG) = 1.0
      IF(ISWPL1.EQ.1)   A0(L0+6)  = A0(L0+6)  - A1(L1+6)*0.33333333
C     DIFFUSION COEFFICIENT
      IF(IOPT(16).EQ.0) A0(L0+8)  = 0.33333333 / A0(L0+6)
      SIGT(NG)     = A0(L0+6)
      SIGA(NG+NGRF)= A0(L0+10)
      LOCSS        = L0+10+I0(L0+1)
      IF(ISWPL1.EQ.1)   A0(LOCSS) = A0(LOCSS) - A1(L1+6)*0.33333333
CDEL  SIGSS0(NG)   = A0(LOCSS)
CDEL  IF (IPL.EQ.1)     SIGSS1(NG)= A1(L1+10+I1(L1+1))
      IF (IOPT(16).LE.1) GO TO 58
CDEL  X            = SQRT(ABS(BSQ))/SIGT(NG)
C     IF(X .GT.1.0) IXB1 = IXB1 + 1
C     X2           = BSQ/SIGT(NG)/SIGT(NG)
C     IF(X.GE.0.20) THEN
C                   IF(BSQ.GT.0.0) A = ATAN (X)/X
C                   IF(BSQ.LT.0.0) A = ATANH(X)/X
C                   ALPHA(NG) = X2*A/3.0/(1.0-A)
C                   ELSE
C                   ALPHA(NG) = (1.0-SIGN(0.333333,BSQ)*X2 + 0.2*X2*X2)
C    *                        / (1.0-SIGN(0.6,BSQ)*X2 + 0.428571*X2*X2)
CEND                ENDIF
CADD
      Y  =  BSQ/SIGT(NG)/SIGT(NG)
      IF(ABS(Y).GT.0.1) THEN
                        IF(BSQ.GT.0.0) THEN
                                X   = SQRT(Y)
                                XX  = ATAN(X)
                                       ELSE
                                YY  = SQRT(-Y)
                                X   = AMIN1(YY,0.99900)
                                XX  = 0.5000*ALOG((1.0000+X)/(1.0000-X))
                                       ENDIF
                         ALPHA(NG)  = 0.33333333*Y*XX/(X-XX)
                         ELSE
                         X  = 0.33333333-Y*(0.2000000-0.1428572*Y)
                         ALPHA(NG)  = 0.33333333*( 1.00000/X - Y )
                         ENDIF
   58 CONTINUE
      IF(ISW16.EQ.1) F1(NG+NGRF) = BSQ*F0(NG+NGRF)/3./ALPHA(NG)/SIGT(NG)
                     L0          = L0 + I0(L0+2) + 10
      IF (IPL.EQ.1)  L1          = L1 + I1(L1+2) + 10
   60 CONTINUE
C
      IF(IOPT(16).EQ.0) GO TO 371
      IF(IOPT(16).EQ.3) GO TO 217
C
C     ITERATIVE SOLUTION OF THERMAL SPECTRUM
C
      DO 200 IT = 1,ITMAX
      ANORM     = 0.0
      CALL CLEA (S0(NGRF+1),NGRT,0.0)
      CALL CLEA (S1(NGRF+1),NGRT,0.0)
CTFREE
      CALL CLEA(  REBA  , 48  , 0.0 )
      CALL CLEA(  REBF  , 48  , 0.0 )
      CALL CLEA(  REBR  , 48  , 0.0 )
      CALL CLEA(  REBS  , 48*48 ,  0.0 )
CEND
      L0        = 0
      L1        = 0
      DO 150 NG = 1,NGRT
      LGT       = I0(L0+2)
      N         = NG - I0(L0+1)
      L0        = L0 + 10
                  DO 130   J = 1,LGT
                  N          = N  + 1
                  L0         = L0 + 1
                  IF(N.LE.0) GO TO 130
CDEL              IF(N.EQ.NG) GO TO 130
                  S0(N+NGRF) = S0(N+NGRF) + A0(L0)*F0(NG+NGRF)
  130             CONTINUE
      IF(IPL.EQ.0) GO TO 150
C
      LGT       = I1(L1+2)
      N         = NG - I1(L1+1)
      L1        = L1 + 10
      DO  140 J = 1,LGT
      N         = N  +  1
      L1        = L1 +  1
      IF(N.LE. 0) GO TO 140
CDEL  IF(N.EQ.NG) GO TO 140
      S1(N+NGRF)= S1(N+NGRF) + A1(L1)*F1(NG+NGRF)
  140 CONTINUE
  150 CONTINUE
CTFREE
      L0        = 0
CEND
      DO 180 NG = 1,NGRT
      F00(NG)   = ( ( S0(NG+NGRF) + X0(NG) ) *
     *              ( 3.0*ALPHA(NG)*SIGT(NG) - SIGSS1(NG) )
     *             -( S1(NG+NGRF) + X1(NG) )   )
     *          /  ( ( SIGT(NG) - SIGSS0(NG) ) *
     *             ( 3.0*ALPHA(NG)*SIGT(NG) - SIGSS1(NG) )  +  BSQ   )
      F10(NG)   = ( S1(NG+NGRF) + X1(NG) + BSQ*F0(NG+NGRF) )
     *          / ( 3.0*ALPHA(NG)*SIGT(NG) - SIGSS1(NG)    )
CM    ANORM     = ANORM + SIGA(NG+NGRF)*F00(NG) + F10(NG)
      REBA(NG)  =         SIGA(NG+NGRF)*F00(NG) + F10(NG)
      LGT       = I0(L0+2)
      N         = NG - I0(L0+1)
      L0        = L0 + 10
      DO 175 J  = 1,LGT
      N         = N  + 1
      L0        = L0 + 1
      IF(N.LE. 0)  GO TO 175
      REBS(NG,N)= A0(L0)*F00(NG)
  175 CONTINUE
  180 CONTINUE
C
      CALL  FUNMOD(48,1,F00,REBS,REBA,REBQ,REBR,REBF,NOUT1,1,NGRT,IT)
      DO 181 NG = 1 , NGRT
      ANORM     = ANORM + REBA(NG)
      F10(NG)   = F10(NG)*REBF(NG)
  181 CONTINUE
C
*     WRITE(6,*)  ' *** IT,XNORM,ANORM,XNORM/ANORM ** ',
*    +                  IT,XNORM,ANORM,XNORM/ANORM
C
      ANORM      = XNORM/ANORM
      OMEGA2     = 1.000
      IF(IT.GT.  1)                   OMEGA2 = 0.5000
      IF(IT.GT. 20.AND.RES.LT.EPSOV)  OMEGA2 = OMEGA
      IF(IT.GT.100)                   OMEGA2 = 0.500000
      RES        = 0.0
      DO 190 NG  = 1,NGRT
      RES        = RES + (SIGA(NG+NGRF)*(F00(NG)*ANORM-F0(NG+NGRF)))**2
CM    F0(NG+NGRF)= F0(NG+NGRF) + OMEGA *(F00(NG)*ANORM-F0(NG+NGRF))
      F0(NG+NGRF)= F0(NG+NGRF) + OMEGA2*(F00(NG)*ANORM-F0(NG+NGRF))
      F1(NG+NGRF)= F1(NG+NGRF) + OMEGA2*(F10(NG)*ANORM-F1(NG+NGRF))
  190 CONTINUE
      RES        = SQRT(RES)/XNORM
      IF(IT    .LT. 10)    GO TO 200
      IF(RES   .LT.EPS)    GO TO 202
  200 CONTINUE
C
      DO 201  NG = 1 , NGRT
      IF(F0(NG+NGRF)    .LT.0.0) IFNEGA = 1
CM    IF(F1(NG+NGRF)*BSQ.LT.0.0) IFNEGA = 1
      IF(F1(NG+NGRF)*BSQ.LE.0.0) IFNEGA = 1
  201 CONTINUE
      IF(IFNEGA.EQ.1)    GO TO 204
      IF(ICELL .EQ.0)    GO TO 204
      WRITE(NOUT1,240)   ITMAX,RES,EPS
      STOP
C
C === CHECK NEGATIVE FLUX
C
  202 CONTINUE
      DO 203  NG = 1 , NGRT
      IF(F0(NG+NGRF)    .LT.0.0) IFNEGA = 1
      IF(F1(NG+NGRF)*BSQ.LT.0.0) IFNEGA = 1
  203 CONTINUE
C
      IF(IFNEGA.EQ.0) THEN
                      WRITE(NOUT1,215) IT,RES
                      WRITE(NOUT1,235) ANORM
                      GO TO 217
                      ENDIF
C
  204 CONTINUE
      IF(ICELL.EQ.0) THEN
                     WRITE(NOUT1,502) MTNM,IOPT(16)
                     IOPT(16) = 0
                     LENG( 1) = 0
                     LENG( 4) = 0
                     ICOUNT   = ICOUNT + 1
*                    WRITE(6,*) ' ** NEGATIVE FLUX ** '
*                    WRITE(6,*) (F0(NG+NGRF),NG=1,NGRT)
*                    WRITE(6,*) ' ** NEGATIVE CURRENT ** '
*                    WRITE(6,*) (F1(NG+NGRF),NG=1,NGRT)
                     GO TO 10
                     ENDIF
C
      WRITE(NOUT1,385) MTNM
      STOP
C
C === TRANSPORT CROSS SECTION
C
  217 CONTINUE
      IF(IPL.EQ.0) GO TO 351
C
      CALL CLEA (S1(NGRF+1),NGRT,0.0)
      L1        = 0
      DO 350 NG = 1,NGRT
      LGT       = I1(L1+2)
      N         = NG - I1(L1+1)
      L1        = L1 + 10
      DO  340 J = 1,LGT
      N         = N  +  1
      L1        = L1 +  1
      IF(N.LE. 0) GO TO 340
      S1(N+NGRF)= S1(N+NGRF) + A1(L1)*F1(NG+NGRF)
  340 CONTINUE
  350 CONTINUE
C
  351 CONTINUE
C
      L0           =  0
      DO 370    NG = 1,NGRT
      S0(NG+NGRF)  = S0(NG+NGRF) + X0(NG)
      S1(NG+NGRF)  = S1(NG+NGRF) + X1(NG)
CDEL  DIFFER       = 0.33333333*S1(NG+NGRF)/F1(NG+NGRF)
      DIFFER       = (1.0-ALPHA(NG))*SIGT(NG)
     *             + 0.33333333*S1(NG+NGRF)/F1(NG+NGRF)
      A0(L0+6)     = A0(L0+6) - DIFFER
CTTT  A0(L0+6)     = A0(L0+6)-A1(L1+6)/3.
C     DIFFUSION COEFFICIENT
      A0(L0+8)     = 0.333333333/A0(L0+6)
      LOCSS        = L0+10+I0(L0+1)
CTTT  A0(LOCSS)    = A0(LOCSS)-A1(L1+6)/3.
      A0(LOCSS)    = A0(LOCSS)-DIFFER
      L0           = L0 + 10 + I0(L0+2)
  370 CONTINUE
C
  371 CONTINUE
      IF(ICELL.EQ.0) THEN
                     NODE(2) (4:4) = '2'
                     IF(LENG(4).LE.0)  THEN
                                       CALL WRITE(NODE,A0,LGT0)
                                       LENG(4) = LGT0
                                       ENDIF
                     ENDIF
C
  216 CONTINUE
      IF(IOPT(16).EQ.0)    GO TO 2000
      IF(IOPT(16).EQ.3)    GO TO 2000
C === PRINT RESLUTS OF CALCULATION
      IF(IOPT(19).GT.0) THEN
                        DO 383 NG = 1,NGR
                        S0(NG)    = F0(NG)/ALOG(E(NG)/E(NG+1))
                        S1(NG)    = F1(NG)/ALOG(E(NG)/E(NG+1))
  383                   CONTINUE
                        WRITE(NOUT2,220)(S0(NG),NG=1,NGR)
                        WRITE(NOUT2,230)(S1(NG),NG=1,NGR)
                        ENDIF
C
      IF(ICELL.GT.0.OR.ICOND.EQ.0)    GO TO  2000
C ==  WRITE P0 COMPONENT INTO FLUX FILE
      FILENM(1)     = 'FLUX'
      FILENM(2)     = '    '
      NODE(2) (1:1) = 'A'
      IF(IOPT(4).EQ.0) NODE(2) (1:1) = 'F'
      NODE(2) (4:4) ='2'
      CALL OVRWRT( NODE , F0 , NGR )
      NODE(2) (4:4) ='3'
      CALL OVRWRT( NODE , F1 , NGR )
      GO TO 2000
C
C ==== RENAME 'NAMEFBN2' TO  'NAMEFBN4'
C
  880 NODE2(1) = NODE(1)
      NODE2(2) = NODE(2)
      NODE (2) (4:4) = '4'
      NODE2(2) (4:4) = '2'
      LENG(1)  = LENG(2)
      LENG(2)  = 0
CKSK  CALL RENAME(NODE,NODE2)
CKSK  RENAME IS BUILTIN FUNCTION IN G77
      CALL RINAME(NODE,NODE2)
      NODE (2) (1:1) = 'T'
C ==== RENAME 'NAMETBN2' TO  'NAMETBN4'
  890 NODE2(1) = NODE(1)
      NODE2(2) = NODE(2)
      NODE (2) (4:4) = '4'
      NODE2(2) (4:4) = '2'
      LENG(4)  = LENG(5)
      LENG(5)  = 0
CKSK  CALL RENAME(NODE,NODE2)
      CALL RINAME(NODE,NODE2)
C
C     END OF PROCESS
C
 2000 CONTINUE
      IF(IOPT(19).GT.1.AND.IOPT(16).NE.0)  WRITE(NOUT2,420) MTNM,LENG
      IOPT(16) = IC16SV
*     WRITE(6,*)  ' ** MATERIAL(',MTNM,') IS NORMALLY ENDED IN P1B1 **'
      RETURN
C
C === ERROR CASE : MEMBER OF NODE IS NOT FOUND ]]]
C
  900 WRITE(NOUT1,390) NODE
      STOP
C
   25 FORMAT(11X,'FLUX 0       FLUX 1       XKAI 0      S 0         ',
     * 'S 1         ALPHA       SIG T       SIG GG 0    SIG GG 1    ',
     * 'B SQUARE')
   26 FORMAT(10X,10E12.5)
C
  215 FORMAT(1H ,10X,'*** THERMAL SPECTRUM CONVERGED AT',I3
     * ,' ITERATIONS WITH RESIDUE ',E12.5)
  220 FORMAT(10X,'NEUTRON SPECTRUM MULTIPLIED BY LETHARGY WIDTH' /
     *    ,(10X,10E12.5))
  221 FORMAT(10X,'P0 SCATTERING SOURCE '/(10X,10E12.5))
  230 FORMAT(10X,'NEUTRON CURRENT MULTIPLIED BY LETHARGY WIDTH'  /
     *    ,(10X,10E12.5))
  231 FORMAT(10X,'P1 SCATTERING SOURCE '/(10X,10E12.5))
  240 FORMAT(10X,'*** CONVERGENCE FAILED AT ',I4,' ITERATIONS '/ 10X,
     *  '  *** RESIDUAL ABSORPTION RATE ',E12.5,' OVER ' ,E12.5)
  235 FORMAT(10X,'RATIO OF THERMAL SOURCE TO REMOVAL ',E12.5)
  385 FORMAT('  *** NEGATIVE P0 OR P1 COMPONENT DETECTED FOR ',2A4,
     *' WHICH MAY BE CAUSED BY IMPROPER USE OF GAM-P1B1 ROUTINE'
     *  /'  ***  FOR STRONG ABSORBER OR ALMOST EMPTY MATERIAL OR ',
     *           'TOO SMALL BUCKLING'
     *  /'  ***  CHANGE IC16 TO ZERO')
  390 FORMAT(1H0,10X,'** MEMBER **',2A4,'** NOT FOUND IN MACROWRK FILE'
     *  ,'*** STOP AT P1B1 ROUTINE')
C
  410 FORMAT(1H0///,10X,'  ***  BARE REACTOR SPECTRUM CALCULATION ***'
     *   ,' MATERIAL ***',2A4,'*** BY ',A3,' APPROXIMATION')
  420 FORMAT(10X,'MATERIAL NAME ',2A4,' LENGTHS OF MATRICES'/10X,8I12)
C
C 501 FORMAT('  *** SMALL SIG T LESS THAN 1/B  DETECTED ON ',I3,' GROUPS
C    * FOR MATERIAL ',2A4,' WHICH VIOLATES B1P1 CALCULATION ***' /
C    *  '  *** RETRY BY CHANGING IC16 TO ZERO')
C
  502 FORMAT('  *** NEGATIVE P0 OR P1 COMPONENT DETECTED FOR ',2A4,
     *' WHICH MAY BE CAUSED BY IMPROPER USE OF GAM-P1B1 ROUTINE'
     *  /'  ***  FOR STRONG ABSORBER OR ALMOST EMPTY MATERIAL'
     *  /'  ***  RESET IC16(',I2,') TO ZERO AND CONTINUE ]]]')
C
      END
