      SUBROUTINE HOMOSM
C
C     SUBPROGRAM TO CALCULATE THE NEUTRON SPECTRUM BY B1 OR P1 APPR.
C     IN FIXED SOURCE MODE AND MULTIPLE X-REGIONS.
C     NEGATIVE BSQ ACCEPTABLE
C
CMOD  PARAMETER  ( MXSTEP = 31 )
      INCLUDE  'BURNPINC'
      PARAMETER  ( MXWRK  = 100000 )
CKSK  SET MXWRK GREATER THAN 7500 AND ALSO GREATER THAN NGR*NRR
CKSK  NGR:NUMBER OF FINE GROUPS, NRR:NUMBER OF R-REGIONS
C
      COMMON /MAINC / IOPT(54),NEF,NET,DUM1(4),BSQ,I62,I63,NOUT1,NOUT2,
     *                DUM65(35),CASENM(2),DUM103(898)
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      COMMON /TMPSET/ STND(35),NUMB(61),NTDUMY
C
      COMMON /WORK  / ALPHA(80),X0(80),X1(48),SIGT(80),SIGA(107),
     *            SIGSS0(80),SIGSS1(80),F0(107),F1(107),F00(48),
CKSK *            F10(48),A0(7500),A1(7500),AN2N(7500),SIGN2N(107),
     *            F10(48),A0(MXWRK),A1(MXWRK),AN2N(MXWRK),SIGN2N(107),
     *            S0(107),S1(107),E(108),W(107),XSIGF(107),X00(107)
C
C
      COMMON /DEPLET/ AKEFF (MXSTEP),AKINF (MXSTEP)
C
CKSK  DIMENSION       I0(7500),I1(7500),I2(7500)
      DIMENSION       I0(MXWRK),I1(MXWRK),I2(MXWRK)
      CHARACTER*4     NODE(2),IPB1(2)
      CHARACTER*4     FILENM,CASENM,NUMB
C
      EQUIVALENCE    (A0(1),I0(1)),(A1(1),I1(1)),(AN2N(1),I2(1))
CTFREE
      REAL*4          REBA(48),REBQ(48),REBF(48),REBR(48),REBS(48,48)
*     REAL*4          FLXCHN(48)
CEND
C
CDEL  DATA OMEGA/1.2/,EPS/0.0001/,ITMAX/150/,IPB1/3HP1 ,3HB1 /
CKSK  DATA OMEGA/1.200/,EPS/0.00001/,ITMAX/200/,IPB1/3HP1 ,3HB1 /
      DATA OMEGA/1.200/,EPS/0.00001/,ITMAX/200/,IPB1/'P1  ','B1  '/
      DATA EPSOV/0.001/
CM    DATA EPSF /0.0001/
C
C     IBSPCT=IOPT(38) ; FLAG FOR CONDENSE BY B1 SPECTRUM
C     BUCKLING BSQ
C
      WRITE(NOUT2,410) (IOPT(I+100),I=1,20),IPB1(IOPT(9))
      IBKSCH    = IOPT(46)
CADD
      IFLSW     = 1
      FILENM(1) = 'FAST'
      FILENM(2) = 'U   '
      NODE(1)   = 'FISS'
      NODE(2)   = 'YILD'
      CALL CLEA(X00 ,107,0.0)
      CALL READ(NODE,X00,NEF)
C
      IFLSW     = 1
      FILENM(1) = 'MACR'
      FILENM(2) = 'OWRK'
      FILENM(3) = '    '
      NODE(1)   = 'CONT'
      NODE(2)   = 'A002'
      IF(IOPT(4).EQ.0) NODE(2)='F002'
      LTH       = 2*(NEF+NET)+2
      CALL READ(NODE,I0(2001),LTH)
      NGR       = LTH/2-1
      ISHIFT    = NGR + 2001
      DO 760 NG = 1,NGR
      W(NG)     = A0(NG+2001)
      E(NG)     = A0(ISHIFT+NG)
  760 CONTINUE
      E(NGR+1)  = A0(ISHIFT+NGR+1)
C
C     IF IOPT(9)=1  P1 APPROX.
C     IF IOPT(9)=2  B1 APPROX.
C
CKSK  CALL CLEA ( A0    , 7500 , 0.0 )
CKSK  CALL CLEA ( A1    , 7500 , 0.0 )
CKSK  CALL CLEA ( AN2N  , 7500 , 0.0 )
      CALL CLEA ( A0    , MXWRK , 0.0 )
      CALL CLEA ( A1    , MXWRK , 0.0 )
      CALL CLEA ( AN2N  , MXWRK , 0.0 )
C
      IF1           = 1
      NODE(1)       = CASENM(1)
      NODE(2)       = 'F0T4'
C
      IF(IOPT(79).GT.0) NODE(2) (2:2)  =  NUMB(IOPT(79)) (4:4)
      CALL GETLEN(NODE,LTH)
CKSK
      IF(LTH.GT.MXWRK) THEN
        WRITE(NOUT1,6000) MXWRK, LTH
        STOP
      ENDIF
CKSK
      CALL READ  (NODE,A0  ,LTH)
      CALL DELETE(NODE)
      LGTF0         = LTH
C*****READ (N,2N) DATA
      NODE(2) (4:4) = 'M'
      JSW           =  0
      LTHN2N        =  0
      CALL SEARCH(NODE,LTHN2N,JSW)
      IF(JSW.EQ.0)     THEN
CKSK
      IF(LTHN2N.GT.MXWRK) THEN
        WRITE(NOUT1,6000) MXWRK, LTHN2N
        STOP
      ENDIF
CKSK
        CALL READ  (NODE,AN2N,LTHN2N)
        CALL DELETE(NODE)
      ELSE
        LTHN2N = -1
      ENDIF
C
      LGTF1         = 0
      IF(IF1 .EQ. 1) THEN
        NODE(2) (4:4) = '3'
        CALL GETLEN(NODE,LTH)
CKSK
      IF(LTH.GT.MXWRK) THEN
        WRITE(NOUT1,6000) MXWRK, LTH
        STOP
      ENDIF
CKSK
        CALL READ  (NODE,A1,LTH)
        CALL DELETE(NODE)
        LGTF1         = LTH
      ENDIF
C
      L0       = 0
      L1       = 0
CM    IF(A0(L0+7).EQ.0.0) GO TO 210
C**** SET SIGN2N DATA
      CALL CLEA ( SIGN2N,  107 , 0.0 )
      IF(LTHN2N.GT.0) THEN
                      L2       = 0
                      DO 10   NG = 1,NEF
                      SIGN2N(NG) = AN2N(L2+6)
                      L2         = L2+10+I2(L2+2)
   10                 CONTINUE
CM                    WRITE(6,*) ' ** SIGN2N CHECK WRITE AT HOMOSM ** '
CM                    WRITE(6,'(1H ,1P10E11.4)') (SIGN2N(I),I=1,NEF)
                      ENDIF
C
      ISWKAI        =  0
      IF(A0(7).GT.0.0) ISWKAI = 1
C
C**** MODIFYED BY JAIS K.KANEKO  7/27/1994
C
      IBCNT      = 1
 7777 CALL CLEA(  S0 ,  107 , 0.0 )
      CALL CLEA(  S1 ,  107 , 0.0 )
      IF(BSQ.EQ.0.0)  BSQ  = 1.0000E-30
      L0         = 0
      L1         = 0
C
      DO 20   NG = 1,NEF
      SIGSS1(NG) = 0.0
      ALPHA(NG)  = 1.0
      X0(NG)     = A0(L0+7)
      IF(ISWKAI.EQ.0)  X0(NG) = X00(NG)
      SIGT(NG)   = A0(L0+6)
      SIGA(NG)   = A0(L0+10)
CDEL  SIGA(NG)   = A0(L0+10) - SIGN2N(NG)
      XSIGF(NG)  = A0(L0+5)
      SIGSS0(NG) = A0(L0+10+I0(L0+1))
      IF (IF1 .EQ. 1)   SIGSS1(NG) = A1(L1+10+I1(L1+1))
      L0         = L0+10+I0(L0+2)
      IF (IF1 .EQ. 1)   L1         = L1+10+I1(L1+2)
      IF (IOPT(9).EQ.1) GO TO 20
CDELETE
C     X          = SQRT(ABS(BSQ))/SIGT(NG)
C     X2         = BSQ/SIGT(NG)  /SIGT(NG)
C     IF(X.GE.0.2)  THEN
C                   IF(BSQ.GT.0.) A=ATAN (X)/X
C                   IF(BSQ.LT.0.) A=ATANH(X)/X
C                   ALPHA(NG) = X2*A*0.33333333/(1.0-A)
C                   ELSE
C                   ALPHA(NG) =(1.0-SIGN(0.333333,BSQ)*X2 + 0.2*X2*X2)
C    *                      /  (1.0-SIGN(0.6,BSQ)*X2 + 0.428571*X2*X2)
C                   ENDIF
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
      IF(IOPT(19).GT.1) THEN
           WRITE(NOUT1,*) ' NG=',NG,' Y=',Y,' X=',X,' ALPHA=',ALPHA(NG)
                        ENDIF
   20 CONTINUE
C
C     CALCULATE FAST NEUTRON SPECTRUM
C
      L0       = 0
      L1       = 0
      DO 50 NG = 1,NEF
      F0(NG)   = ( (S0(NG)+X0(NG)) * (3.0*ALPHA(NG)*SIGT(NG)-SIGSS1(NG))
     *         - S1(NG)  )
     *   / ( (SIGT(NG)-SIGSS0(NG)) * (3.0*ALPHA(NG)*SIGT(NG)-SIGSS1(NG))
     *         + BSQ )
      F1(NG)   = ( S1(NG) + BSQ*F0(NG) )
     *         / ( 3.0*ALPHA(NG)*SIGT(NG) - SIGSS1(NG) )
      LGT      = I0(L0+2)
      L0       = L0 + 11
      DO 30 J   = 2 , LGT
      L0        = L0 +  1
      S0(NG+J-1)= S0(NG+J-1)+A0(L0)*F0(NG)
   30 CONTINUE
C
      IF(IF1.EQ.1) THEN
                   LGT         = I1(L1+2)
                   L1          = L1 + 11
                   DO 40     J = 2,LGT
                   L1          = L1 + 1
                   S1(NG+J-1)  = S1(NG+J-1)+ A1(L1)*F1(NG)
   40              CONTINUE
                   ENDIF
C
   50 CONTINUE
C
C     END OF FAST NEUTRON SPECTRUM CALCULATION
C
      REACAF   = 0.0
      REACFF   = 0.0
      REACXF   = 0.0
      DO 52 NG = 1,NEF
CMOD  REACAF   = REACAF + SIGA (NG)*F0(NG)
      REACAF   = REACAF + ( SIGA(NG)-SIGN2N(NG) )*F0(NG)
      REACFF   = REACFF + XSIGF(NG)*F0(NG)
      REACXF   = REACXF + F1(NG)
   52 CONTINUE
C
      IF(IOPT(4).EQ.0) GO TO 216
      IF(IBCNT  .GT.1) GO TO  55
C
      NODE(1)  = CASENM(1)
      NODE(2)  = 'T0T4'
      IF(IOPT(79).GT.0) NODE(2) (2:2)  =  NUMB(IOPT(79)) (4:4)
      CALL GETLEN(NODE,LTH)
CKSK
      LGT0     = LGTF0 + LTH
      IF(LGT0.GT.MXWRK) THEN
        WRITE(NOUT1,6000) MXWRK, LGT0
        STOP
      ENDIF
CKSK
      CALL READ(NODE,A0(LGTF0+1),LTH)
      CALL DELETE(NODE)
CKSK  LGT0     = LGTF0 + LTH
C
      IF (IF1 .EQ. 1) THEN
        NODE(2) (4:4) =  '3'
        CALL GETLEN(NODE,LTH)
CKSK
        LGT1=LGTF1+LTH
        IF(LGT1.GT.MXWRK) THEN
          WRITE(NOUT1,6000) MXWRK, LGT1
          STOP
        ENDIF
        CALL READ (NODE,A1(LGTF1+1),LTH)
        CALL DELETE(NODE)
CKSK    LGT1=LGTF1+LTH
      ENDIF
C
   55 L0         = LGTF0
      L1         = LGTF1
      XNORM      = 0.0
      RATIO      = F0(NEF)/W(NEF)
*     WRITE(6,*) ' *** RATIO *** ',RATIO,F0(NEF),W(NEF)
C
      CALL CLEA(  REBQ , 48 , 0.0 )
C
      DO 60  NG  = 1,NET
      X0(NG)     = S0(NG+NEF)
      REBQ(NG)   = S0(NG+NEF)
      XNORM      = XNORM + X0(NG)
      X1(NG)     = S1(NG+NEF)
CDEL  F0(NG+NEF) = W(NEF+NG)
      F0(NG+NEF) = W(NEF+NG)*RATIO
      SIGSS0(NG) = 0.0
      SIGSS1(NG) = 0.0
      ALPHA(NG)  = 1.0
      SIGT (NG)  = A0(L0+6)
      SIGA (NG+NEF) = A0(L0+10)
      XSIGF(NG+NEF) = A0(L0+5)
CDEL  SIGSS0(NG)    = A0(L0+10+I0(L0+1))
CDEL  IF ( IF1 .EQ. 1)  SIGSS1(NG) = A1(L1+10+I1(L1+1))
      IF (IOPT(9).EQ.1) GO TO 58
CDELETE
C     X          = SQRT(ABS(BSQ))/SIGT(NG)
C     X2         = BSQ/SIGT(NG)/SIGT(NG)
C     IF(X.GE.0.2)  THEN
C                   IF(BSQ.GT.0.) A = ATAN (X)/X
C                   IF(BSQ.LT.0.) A = ATANH(X)/X
C                   ALPHA(NG) = X2*A*0.33333333/(1.0-A)
C                   ELSE
C     ALPHA(NG)=(1.-0.333333*X2+0.2*X2*X2)/(1.-0.6*X2+0.428571*X2*X2)
C                   ALPHA(NG) = (1.0-SIGN(0.333333,BSQ)*X2 + 0.2*X2*X2)
C    *                        / (1.0-SIGN(0.6,BSQ)*X2 + 0.428571*X2*X2)
C                   ENDIF
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
      IF(IOPT(19).GT.1) THEN
       WRITE(NOUT1,*)' NG=',NG+NEF,' Y=',Y,' X=',X ,' ALPHA=',ALPHA(NG)
                        ENDIF
C
   58 CONTINUE
      F1(NG+NEF) = BSQ*F0(NG+NEF)*0.33333333/ALPHA(NG)/SIGT(NG)
      L0         = L0 + I0(L0+2) + 10
      IF ( IF1 . EQ.  1)  L1  = L1 + I1(L1+2) + 10
   60 CONTINUE
C
*     WRITE(6,*) ' ** F0 ** ',(F0(NG+NEF),NG=1,NET)
*     WRITE(6,*) ' ** F1 ** ',(F1(NG+NEF),NG=1,NET)
C
C     ITERATIVE SOLUTION OF THERMAL SPECTRUM
C
      DO 200 IT = 1,ITMAX
      ANORM     = 0.
CTFREE
      CALL CLEA(  REBA  , 48  , 0.0 )
      CALL CLEA(  REBF  , 48  , 0.0 )
      CALL CLEA(  REBR  , 48  , 0.0 )
      CALL CLEA(  REBS  , 48*48 ,  0.0 )
*     CALL CLEA(  FLXCHN, 48  , 0.0 )
CEND
      DO 110 NG = 1,NET
      S0(NG)    = 0.0
      S1(NG)    = 0.0
  110 CONTINUE
      L0        = LGTF0
      L1        = LGTF1
C
      DO 150 NG = 1,NET
      LGT       = I0(L0+2)
      N         = NG - I0(L0+1)
      L0        = L0 + 10
      DO 130 J  = 1,LGT
      N         = N  + 1
      L0        = L0 + 1
      IF(N.LE. 0)  GO TO 130
CDEL  IF(N.EQ.NG)  GO TO 130
      S0(N)     = S0(N) + A0(L0)*F0(NG+NEF)
  130 CONTINUE
C
      IF( IF1 .EQ. 1 ) THEN
                       LGT      = I1(L1+2)
                       N        = NG - I1(L1+1)
                       L1       = L1 + 10
                       DO 140 J = 1,LGT
                       N        = N+1
                       L1       = L1+1
                       IF(N.LE.0)  GO TO 140
CDEL                   IF(N.EQ.NG) GO TO 140
                       S1(N)    = S1(N) + A1(L1)*F1(NG+NEF)
  140                  CONTINUE
                       ENDIF
  150 CONTINUE
C
      L0        = LGTF0
      DO 180 NG = 1,NET
      F00(NG)=( (S0(NG)+X0(NG)) * (3.0*ALPHA(NG)*SIGT(NG)-SIGSS1(NG))
     *         - ( S1(NG) + X1(NG) )  )
     *   / ( (SIGT(NG)-SIGSS0(NG)) * (3.0*ALPHA(NG)*SIGT(NG)-SIGSS1(NG))
     *         + BSQ   )
      F10(NG)=( S1(NG) + X1(NG) + BSQ*F0(NG+NEF) )
     *       /( 3.0*ALPHA(NG)*SIGT(NG) - SIGSS1(NG) )
CDEL  ANORM  = ANORM + SIGA(NG+NEF)*F00(NG) + F10(NG)
      REBA(NG) =       SIGA(NG+NEF)*F00(NG) + F10(NG)
C
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
*     IF(IT.EQ.1) THEN
*                 WRITE(6,*) ' ** F00 ** ',(F00(NG),NG=1,NET)
*                 WRITE(6,*) ' ** F10 ** ',(F10(NG),NG=1,NET)
*                 ENDIF
C
      CALL  FUNMOD(48 , 1 ,F00 ,REBS,REBA,REBQ,REBR,REBF,NOUT1,1,NET,IT)
      DO 181 NG = 1 , NET
      ANORM     = ANORM + REBA(NG)
      F10(NG)   = F10(NG)*REBF(NG)
  181 CONTINUE
C
*     WRITE(6,*)  ' *** IT,XNORM,ANORM,XNORM/ANORM ** ',
*    +                  IT,XNORM,ANORM,XNORM/ANORM
C
      ANORM     = XNORM/ANORM
      OMEGA2    = 1.000
      IF(IT.GT.  1)                   OMEGA2 = 0.5000
      IF(IT.GT. 20.AND.RES.LT.EPSOV)  OMEGA2 = OMEGA
      IF(IT.GT.100)                   OMEGA2 = 0.500000
      RES       = 0.0
C
      EPSFLX    =-1.0000E+10
      DO 190 NG = 1,NET
      RES       = RES + ( SIGA(NG+NEF)*(F00(NG)*ANORM - F0(NG+NEF)) )**2
      SAVE      = ABS ( F00(NG)*ANORM - F0(NG+NEF) ) / F0(NG+NEF)
*     FLXCHN(NG)= SAVE
      IF(SAVE.GT.EPSFLX) EPSFLX = SAVE
      F0(NG+NEF)= F0(NG+NEF) + OMEGA2*( F00(NG)*ANORM - F0(NG+NEF) )
      F1(NG+NEF)= F1(NG+NEF) + OMEGA2*( F10(NG)*ANORM - F1(NG+NEF) )
  190 CONTINUE
      RES       = SQRT(RES)/XNORM
C
*     WRITE(6,*)  ' *** IT,XNORM,ANORM,RES,EPSFLX ** ',
*    *                  IT,XNORM,ANORM,RES,EPSFLX
C
      IF(IT .LT.10)   GO TO 200
CM    IF(RES.LT.EPS)  GO TO 217
      IF(RES.LT.EPS)  THEN
                      IF(EPSFLX.LT.EPS)  GO TO 217
CM                    IF(EPSFLX.LT.EPSF)  GO TO 217
*                     ELSE
*                     WRITE(6,*) ' ** IT EPSFLX EPS ',IT,EPSFLX,EPS
*                     WRITE(6,201) (FLXCHN(NG),NG=1,NET)
                      ENDIF
  200 CONTINUE
C
* 201 FORMAT(1H ,' ## FLXCHN ## ',1P10E11.4)
C
C     END OF ITERATION
C
      WRITE(NOUT1,240)  ITMAX,RES,EPS
CADD
      IF(RES.LT.50.0*EPS) GO TO 217
CEND
      STOP
  210 WRITE(NOUT1,215)
      STOP
C
  217 CONTINUE
  216 CONTINUE
C
      IF(IBKSCH.EQ.0) GO TO 219
C
      REACA     = 0.0
      REACX     = 0.0
      REACF     = 0.0
      DO 218 NG = 1,NGR
      REACA     = REACA + ( SIGA(NG) - SIGN2N(NG) )*F0(NG)
      REACF     = REACF + XSIGF(NG)*F0(NG)
      REACX     = REACX + F1(NG)
 218  CONTINUE
C
      REACA  = REACA
      REACF  = REACF
      REACX  = REACX
      XKEFF  = REACF/(REACA+REACX)
      XKINF  = REACF/REACA
C
      IF(REACF.LE.0.0)             GO TO 219
      IF(ABS(XKEFF-1.0).LT.1.0E-4) GO TO 219
      IF(IBCNT.GE.100)             GO TO 219
C
      IF(ABS(XKEFF/XKINF-1.00).LT.1.00E-10) THEN
                               BSQ =BSQ*1000.0
                               IBCNT  = IBCNT + 1
                               GO TO 7777
                               ENDIF
C
      AMIGRA = (XKINF/XKEFF-1.000)/BSQ
      BSQ    = (XKINF-1.000)/AMIGRA
      WRITE(6,*) ' NEW BUCKLING & KEFF & KINF : ',IBCNT,BSQ,XKEFF,XKINF
      IBCNT  = IBCNT + 1
      GO TO 7777
C
 219  IF(IOPT(4).GT.0) WRITE(NOUT2,235) ANORM
      IF(IOPT(38).EQ.0) GO TO 300
      FILENM(1) = 'FLUX'
      FILENM(2) = '    '
      NODE(1)   = CASENM(1)
      NODE(2)   = 'A0T2'
      IF(IOPT(79).GT.0) NODE(2) (2:2)  =  NUMB(IOPT(79)) (4:4)
      IF(IOPT(4).EQ.0) NODE(2) (1:1) = 'F'
      CALL OVRWRT(NODE,F0,NGR)
C  MODIFICATION OF SPACE DEPENDENT SPECTRUM BY F0
      IF(IOPT(4).EQ.0) GO TO 300
      NODE(2)  = 'F002'
      IF(IOPT(79).GT.0) NODE(2) (2:2)  =  NUMB(IOPT(79)) (4:4)
      CALL SEARCH(NODE,LTH,ISW)
      IF(ISW.EQ.1)     GO TO 500
      LTH1    = 0
CKSK
      IF(LTH.GT.MXWRK) THEN
        WRITE(NOUT1,6000) MXWRK, LTH
        STOP
      ENDIF
CKSK
      CALL READ (NODE,A0,LTH)
      IF(IOPT(4).EQ.0) GO TO 310
C
      NODE(2) (1:1) = 'T'
      CALL SEARCH(NODE,LTH1,ISW)
      IF(ISW.EQ.1)     GO TO 500
CKSK
      IF(LTH+LTH1.GT.MXWRK) THEN
        WRITE(NOUT1,6000) MXWRK, LTH+LTH1
        STOP
      ENDIF
CKSK
      CALL READ (NODE,A0(LTH+1),LTH1)
C
  310 NRR       = (LTH+LTH1)/NGR
      IF(IOPT(38).EQ.0) GO TO 350
      IJ        = 0
      DO 320 NG = 1,NGR
      A1(NG)    = 0.0
      DO 320 IR = 1,NRR
      IJ        = IJ + 1
  320 A1(NG)    = A1(NG)+A0(IJ)
      IJ        = 0
      DO 340 NG = 1,NGR
      DO 340 IR = 1,NRR
      IJ        = IJ+1
  340 A0(IJ)    = A0(IJ)*F0(NG)/A1(NG)
C
C     SPECTRUM ADJUSTED BY THE HOMO SPECTRUM
C     MEMBER CASEAB02 INTO FLUX FILE
C
  350 NODE(2) = 'A002'
      IF(IOPT(4).EQ.0) NODE(2)='F002'
      IF(IOPT(79).GT.0) NODE(2) (2:2)  =  NUMB(IOPT(79)) (4:4)
      CALL OVRWRT(NODE,A0,LTH+LTH1)
C
  300 REACA     = 0.0
      REACX     = 0.0
      REACF     = 0.0
      DO 250 NG = 1,NGR
CMOD  REACA     = REACA + SIGA (NG)*F0(NG)
      REACA     = REACA + ( SIGA(NG) - SIGN2N(NG) )*F0(NG)
      REACF     = REACF + XSIGF(NG)*F0(NG)
      REACX     = REACX + F1(NG)
      F0(NG)    = F0(NG)/ALOG(E(NG)/E(NG+1))
      F1(NG)    = F1(NG)/ALOG(E(NG)/E(NG+1))
 250  CONTINUE
C
      WRITE(NOUT2,
     1        '(10X,''NEUTRON SPECTRUM PER LETHARGY''/(10X,10E12.5))')
     2        (F0(NG),NG=1,NGR)
      WRITE(NOUT2,
     1        '(10X,''NEUTRON CURRENT PER LETHARGY''/(10X,10E12.5))')
     2        (F1(NG),NG=1,NGR)
C
      RATE   = REACA+REACX
      REACFF = REACFF/RATE
      REACAF = REACAF/RATE
      REACXF = REACXF/RATE
      REACA  = REACA /RATE
      REACF  = REACF /RATE
      REACX  = REACX /RATE
      XKEFF  = REACF/(REACA+REACX)
      XKINF  = REACF/REACA
C
      WRITE(NOUT2,270) REACFF,REACAF,REACXF,REACF,REACA,REACX
      WRITE(NOUT1,260) XKEFF,XKINF,BSQ
      WRITE(NOUT2,260) XKEFF,XKINF,BSQ
C
      IF(IOPT(20).EQ.1) THEN
                        ISTEP = IOPT(79) + 1
                        AKEFF(ISTEP) = XKEFF
                        AKINF(ISTEP) = XKINF
                        ENDIF
C
C     END OF PROCESS
C
      RETURN
C
  410 FORMAT(1H1,10X,'  ***  BARE REACTOR SPECTRUM CALCULATION ***'
     *,' OF STEP 15 ***'/10X,'***',2A4,'***',18A4,'***'
     *   /10X,'*** BY ',A3,'APPROXIMATION ***')
  240 FORMAT(10X,'*** CONVERGENCE FAILED AT ',I4,' ITERATIONS '/ 10X,
     *  '  *** RESIDUAL ABSORPTION RATE ',E12.5,' OVER ' ,E12.5)
  215 FORMAT(10X,'*** NON FISSIONABLE MATERIAL HAS NO ITS OWN SPECTRUM',
     * '***')
  235 FORMAT(10X,'RATIO OF THERMAL SOURCE TO REMOVAL ',E12.5)
  260 FORMAT(10X,'K-EFF=',F8.5,' K-INF=',F8.5,' UNDER GEOMTRICAL '
     * ,'BUCKLING=',E12.5)
  270 FORMAT(10X,'FAST FISSION            ',E12.5/
     *       10X,'FAST ABSORPTION         ',E12.5/
     *       10X,'FAST LEAKAGE            ',E12.5/
     *       10X,'TOTAL FISSION           ',E12.5/
     *       10X,'TOTAL ABSORPTION        ',E12.5/
     *       10X,'TOTAL LEAKAGE           ',E12.5)
  510 FORMAT(' **** MEMBER', 2A4,' NOT FOUND IN FLUX FILE')
 6000 FORMAT(' ERROR STOP: FIXED DIMENSION SIZE(MXWRK=',I10,
     &       ') IN HOMOSM ROUTINE IS TOO SMALL',/,
     &       ' REQUIRED SIZE :',I10 )
C
  500 WRITE(NOUT1,510) NODE
      STOP
      END
