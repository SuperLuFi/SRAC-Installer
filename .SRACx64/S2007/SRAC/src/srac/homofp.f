      SUBROUTINE HOMOFP
C                                                            1993/08/16
C     SUBPROGRAM TO CALCULATE THE NEUTRON SPECTRUM BY B1 OR P1 APPR.
C     IN EIGENVALUE MODE. NEGATIVE BSQ ACCEPTABLE
C
C     SUBSTITUTE OF HOMOSP FOR EIGENVALUE CALCULATION ASSUMING
C      DOWN SCATTERING (IOPT(4)=0) AND ISOTROPIC SCATTERING
CMOD  PARAMETER  ( MXSTEP = 31 )
      INCLUDE  'BURNPINC'
      PARAMETER  ( MXWRK  = 100000 )
CKSK  SET MXWRK GREATER THAN 7500 AND ALSO GREATER THAN NGR*NRR
CKSK  NGR:NUMBER OF FINE GROUPS, NRR:NUMBER OF R-REGIONS
C
C
      COMMON /MAINC / IOPT(54),NEF,NET,DUM1(4),BSQ,I62,I63,NOUT1,NOUT2,
     *                DUM65(35),CASENM(2),DUM103(898)
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      COMMON /TMPSET/ STND(35),NUMB(61),NTDUMY
C
      COMMON /WORK  / ALPHA(80),X0(80),SIGT(80),SIGA(107),
     *                SIGSS0(80),F0(107),F1(107),
CKSK *                A0(7500),AN2N(7500),SIGN2N(107),
     *                A0(MXWRK),AN2N(MXWRK),SIGN2N(107),
     *                S0(107),E(108),W(107),XSIGF(107)
C
      COMMON /DEPLET/ AKEFF (MXSTEP),AKINF (MXSTEP)
C
CKSK  DIMENSION       I0(7500),I2(7500)
      DIMENSION       I0(MXWRK),I2(MXWRK)
      CHARACTER*4     NODE(2),IPB1(2)
      CHARACTER*4     FILENM,CASENM,NUMB
C
      EQUIVALENCE    (A0(1),I0(1)),(AN2N(1),I2(1))
CTFREE
C
      DATA IPB1/'P1  ' ,'B1  ' /
C
C     IBSPCT=IOPT(38) ; FLAG FOR CONDENSE BY B1 SPECTRUM
C     BUCKLING BSQ
C
      WRITE(NOUT2,410) (IOPT(I+100),I=1,20),IPB1(IOPT(9))
      IFLSW     = 1

C SUN 1991-09-11(WED) DELETEDE BY SUGITA, H. JRI. -------
      FILENM(1) = 'MACR'
      FILENM(2) = 'OWRK'
C -----------------------
*     FILENM(1) = 'PD66'
*     FILENM(2) = 'MCWK'
C -----------------------

      FILENM(3) = '    '
      NODE(1)   = 'CONT'
      NODE(2)   = 'A002'
      IF(IOPT(4).EQ.0) NODE(2)='F002'
C     LTH       = 2*(NEF+NET)+2
      CALL GETLEN(NODE,LTH)
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
CKSK  CALL CLEA ( AN2N  , 7500 , 0.0 )
      CALL CLEA ( A0    , MXWRK , 0.0 )
      CALL CLEA ( AN2N  , MXWRK , 0.0 )
C
      NODE(1)       = CASENM(1)
      NODE(2)       = 'A012'
      IF(IOPT(4).EQ.0) NODE(2)='F012'
C
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
      ELSE
        LTHN2N = -1
      ENDIF
C
      L0       = 0
      L1       = 0
      IF(A0(L0+7).EQ.0.0) GO TO 210
      CALL CLEA(  S0 ,  107 , 0.0 )
C**** SET SIGN2N DATA
      CALL CLEA ( SIGN2N,  107 , 0.0 )
      IF(LTHN2N.GT.0) THEN
                      L2       = 0
                      DO 10   NG = 1,NGR
                      SIGN2N(NG) = AN2N(L2+6)
                      L2         = L2+10+I2(L2+2)
                      IF(L2.GE.LTHN2N) GO TO 15
   10                 CONTINUE
   15                 CONTINUE
*                     WRITE(6,*) ' ** SIGN2N CHECK WRITE AT HOMOSP ** '
*                     WRITE(6,'(1H ,1P10E11.4)') (SIGN2N(I),I=1,NGR)
                      ENDIF
C**** MODIFYED BY JAIS K.KANEKO 11/7/1989
      IF(BSQ.EQ.0.0)  BSQ  = 1.0000E-30
C
      DO 20   NG = 1,NGR
      ALPHA(NG)  = 1.0
      X0(NG)     = A0(L0+7)
      SIGT(NG)   = A0(L0+6)
      SIGA(NG)   = A0(L0+10)
CDEL  SIGA(NG)   = A0(L0+10) - SIGN2N(NG)
      XSIGF(NG)  = A0(L0+5)
      SIGSS0(NG) = A0(L0+10+I0(L0+1))
      L0         = L0+10+I0(L0+2)
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
      DO 50 NG = 1,NGR
      F0(NG)   = ( (S0(NG)+X0(NG)) * (3.0*ALPHA(NG)*SIGT(NG)))
     *   / ( (SIGT(NG)-SIGSS0(NG)) * (3.0*ALPHA(NG)*SIGT(NG))+BSQ)
      F1(NG)   = (  BSQ*F0(NG) )   / ( 3.0*ALPHA(NG)*SIGT(NG) )
      LGT      = I0(L0+2)
      L0       = L0 + 11
      DO 30 J   = 2 , LGT
      L0        = L0 +  1
      S0(NG+J-1)= S0(NG+J-1)+A0(L0)*F0(NG)
   30 CONTINUE
C
C
   50 CONTINUE
C
C     END OF FAST NEUTRON SPECTRUM CALCULATION
C
      IF(IOPT(38).EQ.0) GO TO 300

C SUN 1991-09-11(WED) DELETEDE BY SUGITA, H. JRI. -------
      FILENM(1) = 'FLUX'
      FILENM(2) = '    '
C -----------------------
*     FILENM(1) = 'PD67'
*     FILENM(2) = 'FLUX'
C -----------------------

      NODE(1)   = CASENM(1)
      NODE(2)   = 'A012'
      IF(IOPT(79).GT.0) NODE(2) (2:2)  =  NUMB(IOPT(79)) (4:4)
      IF(IOPT(4).EQ.0) NODE(2) (1:1) = 'F'
      CALL OVRWRT(NODE,F0,NGR)
C  MODIFICATION OF SPACE DEPENDENT SPECTRUM BY F0
      NODE(2)  = 'A002'
      IF(IOPT(4).EQ.0)   NODE(2)  = 'F002'
      IF(IOPT(79).GT.0) NODE(2) (2:2)  =  NUMB(IOPT(79)) (4:4)
      CALL SEARCH(NODE,LTH,ISW)
CKSK
      IF(LTH.GT.MXWRK) THEN
        WRITE(NOUT1,6000) MXWRK, LTH
        STOP
      ENDIF
CKSK
      IF(ISW.EQ.1)     GO TO 500
      LTH1    = 0
      CALL READ (NODE,A0,LTH)
C
C
  310 NRR       = (LTH)/NGR
      IF(IOPT(38).EQ.0) GO TO 350
      IJ        = 0
      DO 320 NG = 1,NGR
      S0(NG)    = 0.0
      DO 320 IR = 1,NRR
      IJ        = IJ + 1
  320 S0(NG)    = S0(NG)+A0(IJ)
      IJ        = 0
      DO 340 NG = 1,NGR
      DO 340 IR = 1,NRR
      IJ        = IJ+1
  340 A0(IJ)    = A0(IJ)*F0(NG)/S0(NG)
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
C     REACFF = REACFF/RATE
C     REACAF = REACAF/RATE
C     REACXF = REACXF/RATE
      REACA  = REACA /RATE
      REACF  = REACF /RATE
      REACX  = REACX /RATE
      XKEFF  = REACF/(REACA+REACX)
      XKINF  = REACF/REACA
C
      WRITE(NOUT2,270) REACF,REACA,REACX
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
  210 WRITE(NOUT1,215)
      STOP
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
  270 FORMAT(10X,'TOTAL FISSION           ',E12.5/
     *       10X,'TOTAL ABSORPTION        ',E12.5/
     *       10X,'TOTAL LEAKAGE           ',E12.5)
  510 FORMAT(' **** MEMBER', 2A4,' NOT FOUND IN FLUX FILE')
 6000 FORMAT(' ERROR STOP: FIXED DIMENSION SIZE(MXWRK=',I10,
     &       ') IN HOMOFP ROUTINE IS TOO SMALL',/,
     &       ' REQUIRED SIZE :',I10 )
C
  500 WRITE(NOUT1,510) NODE
      STOP
      END
