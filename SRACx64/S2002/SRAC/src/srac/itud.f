C
C     ELEMENT   ITUD
C
      SUBROUTINE ITUD  (
     &    NRMAX,    NMMAX,    NGMAX,    NNMAX,
     &    NNMAX1,   NNMAXR,   NK,       IK,       RK,
     &    RN,       TAU,      VK,       LOC,      LSS,
     &    LGV, XEC,RRK,       RE,        RRN,      H,        FLUX,
     &    EX,        S,      DUM,       DUMD,     DUMR,     DUMQ,
CFREE
     &    REBS,   REBA,     REBQ,       REBR,     REBF,     REBTMP )
CEND
C * * * MAIN CALCULATION * *
      DIMENSION            NK(NRMAX),IK(NRMAX),RK(NRMAX),RN(NNMAX1),
     &    TAU(NNMAX1,8)   ,LOC(NGMAX,NMMAX),   LSS(NGMAX,NMMAX),
     &    LGV(NGMAX,NMMAX),XEC(5000),
     &    RRK(NRMAX),RE(NGMAX),RRN(NNMAXR),H(NNMAXR,NGMAX),
     &    FLUX(NNMAX1,NGMAX), EX(NGMAX),    S(NNMAXR,NGMAX),VK(NRMAX)
CFREE
      REAL*4      REBS(NGMAX,NGMAX)  ,REBTMP(NNMAXR,NGMAX)
      REAL*4      REBQ(NGMAX)        ,REBA(NGMAX)
      REAL*4      REBR(NGMAX)        ,REBF(NGMAX)
CEND
      COMMON /TUD1C/ II1(3),NGUP,NGKMAX,II6,IG,IBOUND,
     &               IGUESS,ID,ITMAX,ITMOUT,ITBG,LCMX,ITDM,IPT,
     &               EPSI,EPSO,EPSG,RELCA,OVERX,FACTOR,XLAMD,BSQ1,
     &               IPTXEC,ITFLUX,IPTS,IDOPT,NXR,LCIK,LCNK,LCXR,
     &               LCRK,LCNN1,LCVOLR,LCMTM,LCMTR,DUMT(13),II(500)
C
      COMMON /MAINC/ IOPT(20),JNFSTL(2),FNFSTL(2),JNTHEL(2),FNTHEL(2)
     &    ,JNEFST(2),FNEFST(2),JNETHE(2),FNETHE(2),JNMACR(2),FNMACR(2)
     &    ,JNMCRS(2),FNMCRS(2),JNEMIC(2),FNEMIC(2),JNFLUX(2),FNFLUX(2)
     &   ,NEFL     ,NETL     ,NEF      ,NET      ,NERF     ,NERT
     &   ,NMAT     ,NETL1    ,BSQQ     ,NIN1     ,NIN2     ,NOUT1
     &   ,NOUT2,IT0,NEFL1    ,NEFL2    ,NEFL3    ,NEF1     ,NEF2
     &   ,NEF3     ,ISTEP    ,DUM1(3)  ,ITYPE    ,DUM2(3)
     &   ,LCNEGF   ,LCNEGT   ,LCNECF   ,LCNECT   ,LCMTNM   ,LCNISO
     &   ,LCTEMP   ,LCXL     ,LCXCDC   ,LCLISO   ,LCIDNT   ,LCDN
     &   ,LCIRES   ,LCIXMC   ,DUMMY2(3),IRANG,ICF,DUM100
     &   ,CASEID(2),TITLE(18)
     &   ,III(580)
C
      COMMON /DEPLET/ AKEFF (50)
C
      DIMENSION     DUM(9),   DUMD(9),  DUMR(9),  DUMQ(NNMAX1,6)
     &  ,RANGE(2,3),TYPE(3,2)
CADD SASAQ
      CHARACTER*4 RANGE,TYPE
      DATA RANGE/'FAST','    ','THER','MAL ','ALL ','    '/
     &    ,TYPE /'EIGE','N-VA','LUE ','FIXE','D SO','URCE'/
C    ITYPE  E-RANGE      NG1    NGF
C        0   0-FAST      NGMAX  NONE
C        0   2-ALL       NGUP-1 NGUP
C        1   0^FAST      NGMAX  NONE
C        1   1-THERMAL   NONE   1
C
      NGF    = 1
      IF(IRANG.EQ.2) NGF = NGUP
      NG1    = NGMAX
      IF(ITYPE.EQ.0 .AND. IRANG.EQ.2) NG1 = NGUP - 1
      ITTOT  = 0
      RNEW   = 0
      FNEW   = 0.0
      SOLD   = 0.0
      FNORM  = 0.0
      FNORMO = 1.0
      RENORM = 0.0
      SNORM  = 1.0
C****  FIXED SOURCE NORMALIZATION ****
      IF( ITYPE .NE. 0 ) THEN
                         A        = 0.0
                         DO 20 NG = 1,NGMAX
                         DO 10 NN = 1,NNMAXR
   10                    DUM(NN)  = S(NN,NG)
                         CALL VINT(NRMAX,NK,RRK,TAU,DUM,ANS,NNMAX1)
                         A        = A + ANS
                         REBQ(NG) = ANS
   20                    CONTINUE
                         SOLD     = A
                         DO 30 NG = 1,NGMAX
                         REBQ(NG) = REBQ(NG)/A
                         DO 30 NN = 1,NNMAXR
                         S(NN,NG) = S(NN,NG)/A
 30                      CONTINUE
                         ENDIF
C
C * *  FISSION SOURCE NORMALIZATION
C
      IF(NGKMAX.GT. 0) THEN
                       CALL VINT(NRMAX,NK,RRK,TAU,RRN,ANS,NNMAX1)
                       IF(ITYPE.NE.0) ANS=A
                       DO 800 NN = 1,NNMAXR
  800                  RRN(NN)   = RRN(NN)/ANS
                       ENDIF
C
C *** OUTER ITERATION ***
C
      DO 4000 ITO=1,ITMOUT
      ITOUT  = ITO
C-----CHECK THERMAL RANGE ONLY
      IF(IRANG.EQ.1) GO TO 2000
C------------------- EIGENVALUE PROBLEM
      IF(ITYPE.EQ.0) THEN
                     DO 1000 NG = 1,NGMAX
                     DO 1000 NN = 1,NNMAXR
 1000                H(NN,NG)   = 0.0
C------------------- FIXED SOURCE PROBLEM
                     ELSE
                     DO 1060 NG = 1,NGMAX
                     DO 1060 NN = 1,NNMAXR
 1060                H(NN,NG)   = S(NN,NG)
                     ENDIF
C
C *** FAST ENERGY RANGE ***
C
      DO 1300 NG = 1,NG1
      NNR        = 0
C    REMOVAL RATE ADD FISSION SOURCE TO H
      DO 1100 NR = 1,NRMAX
      MAT        = IK(NR)
      L          = LOC(NG,MAT) + 6
      NC         = NK(NR)      + 1
      DUMR(NR)   = XEC(L+IDOPT)*BSQ1 + XEC(L-1)
      DUMD(NR)   = XEC(L+IDOPT)
      DO 1100 NN = 1,NC
      NNR        = NNR + 1
      DUM(NNR)   = H(NNR,NG) + XEC(L)*RRN(NNR)
 1100 CONTINUE
      EXX        = EX(NG)
      CALL PROD(NRMAX,NNMAX,NNMAX1,NNMAXR,NK,RN,TAU,DUM,FLUX(1,NG),
     &        EXX,DUMD,DUMR,DUMQ(1,1),DUMQ(1,2),DUMQ(1,3),DUMQ(1,4)
     &         ,DUMQ(1,5),DUMQ(1,6) )
C****  SKIP FOR THE CASE OF FAST ENERGY RANGE ONLY
      IF(NG.EQ.NGMAX) GO TO 1300
C****  SLOWING DOWN SOURCE       ****
      NNR  = 0
      NN1  = 1
      NGD  = NG + 1
      DO 1250 NR = 1,NRMAX
      NN1        = NN1-1
      MAT        = IK(NR)
      L          = LOC(NG,MAT)
      NC         = NK(NR) +  1
      NGE        = NG     -  1   +  LGV(NG,MAT)
      IF(NGD .GT. NGE) GO TO 1250
      LGG        = L      + 10
      DO 1200 NN = 1,NC
      NNR        = NNR    +  1
      NN1        = NN1    +  1
      LGD        = LGG
      DO 1190 NGP= NGD,NGE
      LGD        = LGD    +  1
 1190 H(NNR,NGP) = H(NNR,NGP) + XEC(LGD)*FLUX(NN1,NG)
 1200 CONTINUE
 1250 CONTINUE
 1300 CONTINUE
C-----SKIP FOR THE CASE OF FAST ENERGY ONLY
      IF(NG1.EQ.NGMAX) GO TO 3960
C
C *** PREPARATION FOR THERMAL MULTIGROUP CALCULATION ***
C
      ANS        = 0
      DO 1400 NG = NGUP,NGMAX
      DO 1350 NN = 1,NNMAXR
 1350 S(NN,NG)   = H(NN,NG)
      CALL VINT(NRMAX,NK,RRK,TAU,S(1,NG),A,NNMAX1)
      ANS        = ANS + A
      REBQ(NG)   = A
 1400 CONTINUE
C  ** SNORM : PSEUDO THERMAL FIXED SOURCE IN EIGENVALUE PROBLEM
      SNORM      = ANS
*     WRITE(6,*) 'SNORM=',SNORM
      DO 1700 NG = NGUP,NGMAX
      REBQ(NG)   = REBQ(NG)/SNORM
      DO 1500 NN = 1,NNMAXR
      S(NN,NG)   = S(NN,NG)/SNORM
 1500 CONTINUE
      DO 1600 NN = 1,NNMAX1
      FLUX(NN,NG)= FLUX(NN,NG)/SNORM
 1600 CONTINUE
 1700 CONTINUE
C
 2000 CONTINUE
      IDOP   = 2
      IF(IDOPT.EQ.2) IDOP = 1
C
C ***  THERMAL MULTIGROUP CALCULATION ***
C
      DO 3000 ITI = 1,ITMAX
      ITCNT       = ITI - 1
      ITTOT       = ITTOT + 1
C****  SOURCE DISTRIBUTION ******
      DO 2150  NG = NGF, NGMAX
      DO 2150  NN =   1, NNMAXR
 2150 H(NN,NG)    = S(NN,NG)
      DO 2200  NG = NGF, NGMAX
      NNR         = 0
      NN1         = 1
      DO 2200  NR = 1, NRMAX
      NN1         = NN1    - 1
      MAT         = IK(NR)
      L           = LOC(NG,MAT)
      NGD         = NG     - LSS(NG,MAT)
      LENGTH      = LGV(NG,MAT)
      NC          = NK(NR) + 1
      DO 2200 NN  = 1,NC
      NNR         = NNR    + 1
      NN1         = NN1    + 1
      LGD         = L      + 9
      NGP         = NGD
      DO 2190  LL = 1, LENGTH
      LGD         = LGD    + 1
      NGP         = NGP    + 1
      IF(NGP.LE.0) GO TO 2190
      H(NNR,NGP)  = H(NNR,NGP) + XEC(LGD)*FLUX(NN1,NG)
 2190 CONTINUE
 2200 CONTINUE
C
C * *  FLUX CALCULATION
C
      DO 2400 NG = NGF,NGMAX
      NNR        = 0
      DO 2300 NR = 1,NRMAX
      MAT        = IK(NR)
      NC         = NK(NR) + 1
      L          = LOC(NG,MAT) + 6 + IDOPT
      DUMR(NR)   = XEC(L)*BSQ1+XEC(L-1-IDOPT)
      DUMD(NR)   = XEC(L)
      DO 2300 NN = 1,NC
      NNR        = NNR    + 1
      DUM(NNR)   = H(NNR,NG)
 2300 CONTINUE
      EXX        = EX(NG)
C
      CALL PROD(NRMAX,NNMAX,NNMAX1,NNMAXR,NK,RN,TAU,DUM,H(1,NG),
     &          EXX,DUMD,DUMR,DUMQ(1,1),DUMQ(1,2),DUMQ(1,3),DUMQ(1,4),
     &          DUMQ(1,5),DUMQ(1,6) )
C**** LEAKAGE
      RE(NG)=DUMQ(NNMAX1,3)*(H(NNMAX1-1,NG)-H(NNMAX1,NG))*RN(NNMAX1)**IG
      IF(IG.EQ.2)
     &RE(NG)= (DUMQ(NNMAX1,3)*H(NNMAX1-1,NG)
     &       -(DUMQ(NNMAX1,2)-EX(NG))*H(NNMAX1,NG)) * RN(NNMAX1)
C
 2400 CONTINUE
C
C     PREPARE THERMAL REBALANCING
C
      CALL CLEA(  REBS  , NGMAX*NGMAX , 0.0 )
      CALL CLEA(  REBA  , NGMAX       , 0.0 )
      CALL CLEA(  REBR  , NGMAX       , 0.0 )
      CALL CLEA(  REBF  , NGMAX       , 0.0 )
C
      DO 2500  NG = NGF,NGMAX
      NNR         = 0
      NN1         = 1
      REBA(NG)    = RE(NG)
      CALL CLEA(  REBTMP, NNMAXR*NGMAX , 0.0)
      DO 2480  NR = 1,NRMAX
      NN1         = NN1 - 1
      MAT         = IK(NR)
C **  ABSORPTION RATE
      L           = LOC(NG,MAT) +  9
      LENGTH      = LGV(NG,MAT)
      NGD         = NG          -  LSS(NG,MAT)
      A           = XEC(L)      +  XEC(L-IDOP)*BSQ1
      NC          = NK(NR)      +  1
      DO 2480  NN = 1,NC
      NNR         = NNR + 1
      NN1         = NN1 + 1
      DUM(NNR)    = A*H(NN1,NG)
C **  SCATTERING RATE IN THERMAL ENERGY RAGE
      LGD         = L
      NGP         = NGD
      DO 2475  LL = 1 , LENGTH
      LGD         = LGD    + 1
      NGP         = NGP    + 1
      IF(NGP.LE.0)     GO TO 2475
      IF(NGP.GT.NGMAX) GO TO 2475
      REBTMP(NNR,NGP) = XEC(LGD)*H(NN1,NG)
 2475 CONTINUE
 2480 CONTINUE
      CALL VINT(NRMAX,NK,RRK,TAU,DUM,A  ,NNMAX1)
      REBA(NG)    = REBA(NG)    + A
      DO 2495  KK =  NGF ,NGMAX
      SUM         = 0.0
      DO 2490   J =    1 ,NNMAXR
 2490 SUM         = SUM  +  REBTMP(J,KK)
      IF(SUM.LE.0.0)  GO TO 2495
      CALL VINT(NRMAX,NK,RRK,TAU,REBTMP(1,KK),A  ,NNMAX1)
      REBS(NG,KK) = A
 2495 CONTINUE
 2500 CONTINUE
C
      ICHECK      = ITCNT
      CALL  FUNMOD(NGMAX,NNMAXR,H    ,REBS  ,REBA  ,REBQ  ,REBR  ,
     @             REBF ,NOUT1 ,NGF  ,NGMAX ,ICHECK)
C
      ANS         = 0.0
      DO 2710 NG  = NGF,NGMAX
      ANS         = ANS  + REBA(NG)
 2710 CONTINUE
      RENORM      = ANS
C
*     WRITE(6,*) ' *** ITO,ITI,ASN(2710) ** ',ITO,ITI,ANS
C
      RSAVE       = 1.000
      IF(ANS.GT.0.0)  RSAVE = 1.0000 / ANS
      FCHAN       =  0.0
      DO 2730 NG  = NGF,NGMAX
      FMAX        =  0.0
      EMAX        =  0.0
      NN1         =  1
      DO 2720 NR  = 1  ,NRMAX
      NN1         = NN1 - 1
      MAT         = IK(NR)
      L           = LOC(NG,MAT) + 9
      A           = XEC(L)      + XEC(L-IDOP)*BSQ1
      NC          = NK(NR)      + 1
      DO 2720  NN = 1,NC
      NN1         = NN1         + 1
      FTEMP       = H(NN1,NG)*RSAVE
      DELFLX      = A*ABS( FTEMP - FLUX(NN1,NG) )
      ABTEMP      = A*ABS( FTEMP )
      IF(ABTEMP.GT.FMAX)  FMAX   = ABTEMP
      IF(DELFLX.GT.EMAX)  EMAX   = DELFLX
 2720 CONTINUE
      IF(FMAX.GT.0.0) THEN
                      SAVE        = EMAX/FMAX
                      IF(SAVE.GT.FCHAN)  FCHAN = SAVE
                      ENDIF
      DO 2730  NN1 = 1 ,NNMAX1
      H(NN1,NG)    = H(NN1,NG)*RSAVE - FLUX(NN1,NG)
 2730 CONTINUE
C
      CALL  CLEA(  DUM  , NNMAXR  , 0.0 )
C
      DO 2750  NG = NGF,NGMAX
      NNR         = 0
      NN1         = 1
      DO 2750  NR = 1,NRMAX
      NN1         = NN1 - 1
      MAT         = IK(NR)
      L           = LOC(NG,MAT) + 9
      A           = XEC(L)      + XEC(L-IDOP)*BSQ1
      NC          = NK(NR)      + 1
      DO 2750  NN = 1,NC
      NNR         = NNR         + 1
      NN1         = NN1         + 1
      DUM(NNR)    = DUM(NNR)    + (A*H(NN1,NG))**2
 2750 CONTINUE
C
      CALL VINT(NRMAX,NK,RRK,TAU,DUM,RES,NNMAX1)
      RNEW        = SQRT(RES)
      EPS         = EPSI
C     IF(ITO.LE.3 .AND. ITMOUT.NE.1) EPS=EPSI*10.
      IF(ITO.LE.3 .AND. ITMOUT.NE.1) EPS=0.05
CEND
*     WRITE(6,*) ' * ITO ITCNT RNEW FCHAN * ',ITO,ITCNT,RNEW,FCHAN
      IF(RNEW .LT. EPS . AND . FCHAN .LT. EPS ) GO TO 3001
C
C****  RENORMALIZATION OF FLUX ****
C
      CALL RELAXT(ITCNT,RNEW,OVER,RENORM)
*     WRITE(6,*) ' * OVER RNEW RENORM EPS EPSI EPSO * ',
*    @               OVER,RNEW,RENORM,EPS,EPSI,EPSO
      IF(ITCNT.EQ.0)  OVER = 1.000
C
      DO 2900    NG  = NGF,NGMAX
      DO 2900    NN1 =   1,NNMAX1
      FLUX(NN1,NG)   =  FLUX(NN1,NG)    +  OVER * H(NN1,NG)
 2900 CONTINUE
 3000 CONTINUE
C
C***** END OF THERMAL ITERATIONS
C
C3001 IF(NGF.EQ.1 .OR. NG1.EQ.NGMAX) GO TO 3960
C
 3001 CONTINUE
      NEGAF       = 0
      DO 3950  NG = NGF,NGMAX
      DO 3950  NN =   1,NNMAX1
      IF(FLUX(NN,NG).LT.0.) NEGAF=1
      FLUX(NN,NG) = FLUX(NN,NG)*SNORM
 3950 CONTINUE
      IF(NEGAF.EQ.1) THEN
                     WRITE(NOUT1,3955)
                     WRITE(NOUT2,3955)
                     ENDIF
C
 3955 FORMAT(' *** NEGATIVE FLUX ENCOUNTERED IN THERMAL RANGE '
     & ,' ** RERUN WITH LOWER VALUE OF OVER-RELAXATION FACTOR **')
 3960 IF(NGKMAX.EQ.0) GO TO 4000
C
C      POWER INTEGRATION
C
      DO 3100 NNR=1,NNMAXR
 3100 DUM(NNR)   =0.0
      NNR        = 0
      NN1        = 1
      DO 3200 NR = 1,NRMAX
      NN1        = NN1    - 1
      MAT        = IK(NR)
      NC         = NK(NR) + 1
      DO 3200 NN = 1,NC
      NNR        = NNR    + 1
      NN1        = NN1    + 1
      DO 3200 NG = 1,NGMAX
      L          = LOC(NG,MAT) + 4
      DUM(NNR)   = DUM(NNR)    + XEC(L)*FLUX(NN1,NG)
 3200 CONTINUE
C
C****  RENORMALIZATION OF FISSION SOURCE ****
C
      CALL VINT(NRMAX,NK,RRK,TAU,DUM,A  ,NNMAX1)
      FNORM      = A
      IF(ITYPE.EQ.1) A = 1.0
      CALL  CLEA(REBTMP(1,1) , NNMAXR , 0.0 )
C
      DO 3300 NNR = 1,NNMAXR
      REBTMP(NNR,1)= DUM(NNR)/A
      DUM(NNR)    = DUM(NNR)/A - RRN(NNR)
      DUMD(NNR)   = DUM(NNR)**2
 3300 CONTINUE
      CALL VINT(NRMAX,NK,RRK,TAU,DUMD,RES,NNMAX1)
      FNEW        = SQRT(RES)
C****  CHECK POWER DISTRIBUTION  *****
      SEMAX       = -1.000E+30
      SEMIN       = +1.000E+30
      DO 3400 NNR = 1,NNMAXR
      IF(RRN(NNR).LE.0.0) GO TO 3400
      SAVE        =  REBTMP(NNR,1) / RRN(NNR)
      REBTMP(NNR,1)= SAVE
      IF(SAVE.GT.SEMAX)  SEMAX = SAVE
      IF(SAVE.LT.SEMIN)  SEMIN = SAVE
 3400 CONTINUE
C****  SUCCESIVE OVER RELAXATION *****
      SOVER       = 1.0
      IF(ITO.GT.2)  SOVER = 1.200
      DO 3500 NNR = 1,NNMAXR
C3500 RRN(NNR)    = RRN(NNR) + 1.20 * DUM(NNR)
 3500 RRN(NNR)    = RRN(NNR) + SOVER* DUM(NNR)
C
C     IF( IPT .GT.0)      THEN
      WRITE(NOUT2,3990)   ITOUT,FNORM,FNEW
C                         ENDIF
C
      IF(ITO .LE.3)  THEN
                     FNORMO  = FNORM
                     GO TO 4000
                     ENDIF
C
      IF(ABS(FNORMO-FNORM)/FNORMO.LT.EPSO) THEN
                         IF(FNEW.LT.EPSO)  THEN
*                        WRITE(6,*) ' * SEMIN SEMAX * ',SEMIN,SEMAX
*                        WRITE(6,3991) (REBTMP(I,1),I=1,NNMAXR)
                         IF(ABS(SEMAX-SEMIN)/SEMIN.LT.EPSO) GO TO 4100
                                           ENDIF
                                           ENDIF
C
      FNORMO  = FNORM
C
C     END OF POWER LOOP
C
 4000 CONTINUE
C
 3990 FORMAT(6X,
     & 'OUTER  ITERATION COUNT ',I6,
     & 5X,'K EFFECTIVE ',F12.6,
     & 5X,'RESIDUE OF FISSION RATE ',E12.5)
 3991 FORMAT(1H ,' ## SNEW(R)/SOLD(R) ## ',1P8E12.5)
 4110 FORMAT(1H0,9X,'*** ',2A4,'***',18A4,' *** ON TUD STEP ***'/,
     & 10X,'*** ',3A4,' PROBLEM IN ',2A4,' ENERGY RANGE ***'/
     & 10X,'OUTER  ITERATION COUNT                     ',I6/
     & 10X,'RESIDUE OF FISSION RATE DISTRIBUTION       ',E12.5/
     & 10X,' K EFFECTIVE                               ',F12.6/
     & 10X,'TOTAL THERMAL ITERATION COUNT              ',I6/
     & 10X,'RESIDUE OF REMOVAL IN THERMAL ITERATION    ',E12.5/
     & 10X,'SOURCE/REMOVAL OF LAST THERMAL ITERATION   ',E12.5/
     & 10X,'TRANSVERSE BUCKLING                        ',E12.5)
C
C    END OF PROCESS
C
 4100 WRITE(NOUT2,4110) CASEID,TITLE,TYPE(1,ITYPE+1),TYPE(2,ITYPE+1)
     & ,TYPE(3,ITYPE+1),RANGE(1,IRANG+1),RANGE(2,IRANG+1),
     & ITOUT,FNEW,FNORM,ITTOT,RNEW,RENORM,BSQ1
CADD
C
      IF(IOPT(20).EQ.1) THEN
                        ISTEP = IOPT(79) + 1
                        AKEFF(ISTEP) = FNORM
CDEL                    AKINF(ISTEP) = FNORM
                        ENDIF
C
      RETURN
C     DEBUG SUBCHK
      END
