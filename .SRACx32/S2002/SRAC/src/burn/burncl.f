      SUBROUTINE BURNCL(PHIP ,IBZ  ,VOL  ,DELT ,NZONE,IOPT ,
     1                  NDN  ,LNMAX,NMAX ,NFIS ,NPAR ,EVTOJ,
     2                  EFC  ,DN   ,DNOLD,SSF  ,SSC  ,SSN2N,
     3                  CUMWD,IDBG ,NSYSO,LCHA ,NCHA ,IPBURN,
     4                  NUCLP,GAM  ,GAMI ,NBIC ,PBIC ,KSTP ,
     5                  LONG ,LL   ,IP   ,KP   ,NPN  ,PHAI ,
     6                  IBC2 ,ST235,TIMEU5,IDU235 , FACPOW ,
     7                  POWZN1,POWZN2 ,NUCL  ,NCH   ,RAMDA ,FACT2N )
C
C       BURNUP DENSITY CALCULATION USING D-CHAIN METHOD
C       THIS ROUTINE IS POWER CONSTANTS MODE
C
      DIMENSION   PHIP (NZONE)
      DIMENSION   VOL  (NZONE)
      DIMENSION   IBZ  (NZONE)
      DIMENSION   POWZN1(NZONE)
      DIMENSION   POWZN2(NZONE)
C
      DIMENSION   EFC  (2,LNMAX)
      DIMENSION   DN   (LNMAX,NZONE)
      DIMENSION   DNOLD(LNMAX,NZONE)
      DIMENSION   SSF  ( NMAX,NZONE)
      DIMENSION   SSC  ( NMAX,NZONE)
      DIMENSION   SSN2N( NMAX,NZONE)
      DIMENSION   CUMWD(NZONE)
      DIMENSION   IPBURN(LNMAX,NZONE)
C
      INCLUDE  'BURNPINC'
C
      DIMENSION         NUCLP (NPAR,NMAX)
      DIMENSION         GAM   (NFIS,NMAX)
      DIMENSION         GAMI  (NFIS,NMAX)
C
      DIMENSION         NBIC  (NPAR  ,NMAX)
      DIMENSION         PBIC  (NPAR  ,NMAX)
      DIMENSION         KSTP  (NCHA  ,NMAX)
      DIMENSION         LONG  (NCHA  ,NMAX)
      DIMENSION         LL    (NCHA  ,NMAX)
      DIMENSION         IP    (LCHA  ,NCHA  ,NMAX)
      DIMENSION         KP    (LCHA  ,NCHA  ,NMAX)
      DIMENSION         NPN   (NPAR  ,NMAX)
      DIMENSION         PHAI  (NPAR  ,NMAX)
C
      DIMENSION         RAMDA (MXNUC)
      DIMENSION         NCH   (MXNUC)
      DIMENSION         NOL   (MXNUC)
      DIMENSION         NUCL  (MXNUC)
      DIMENSION         FACT2N(MXNUC)
C
      DIMENSION         GAMMA (MXNUC)
      DIMENSION         GAMX  (MXNUC)
      DIMENSION         PHIC  (MXNUC)
      DIMENSION         PHIL  (MXNUC)
CMOD  DIMENSION         R     (LCHA )
CMOD  DIMENSION         G     (LCHA )
CMOD  DIMENSION         P     (LCHA )
      DIMENSION         R     (MXNUC)
      DIMENSION         G     (MXNUC)
      DIMENSION         P     (MXNUC)
      DIMENSION         RAM   (MXNUC)
      DIMENSION         MAP   (MXNUC)
      REAL*8            DEX   (MXNUC)
      REAL*8            DEXX  (MXNUC)
CMOD  REAL*8            DCOEF (LCHA )
      REAL*8            DCOEF (MXNUC)
      REAL*8            TSTEP
      REAL*8            TTSTEP
C
      DIMENSION         SGF   (MXNUC)
      DIMENSION         SGA   (MXNUC)
      DIMENSION         SGC   (MXNUC)
      DIMENSION         SGN2N (MXNUC)
      DIMENSION         FNN   (MXNUC)
      DIMENSION         FNF   (MXNUC)
C
      DIMENSION         FU235 (MXNUC)
C
C *** START PROCESS
C
      CALL CLEA( CUMWD , NZONE , 0.0 )
C
      IF(DELT.LE.0.0) RETURN
C
      IF(NMAX .GT.MXNUC) STOP 901
C
C     REWIND  97
C     READ(97) (NUCL(I),I=1,NMAX),(NCH(I),I=1,NMAX),
C    1         (RAMDA(I),I=1,NMAX),
C    2         ((GAM  (J,I),J=1,NFIS),I=1,NMAX),
C    3         ((NUCLP(J,I),J=1,NPAR),I=1,NMAX),
C    4         ((NBIC (J,I),J=1,NPAR),I=1,NMAX),
C    5         ((PBIC (J,I),J=1,NPAR),I=1,NMAX)
C    6        ,(FACT2N(I),I=1,NMAX)
C     REWIND  97
C
      ITYP  = 1
      IF(IOPT.EQ.1) ITYP = 2
C
      CALL       LNCHAI(NUCL  ,RAMDA ,NCH   ,NUCLP ,NBIC  ,PBIC  ,
     *                  NOL   ,KSTP  ,LONG  ,LL    ,IP    ,KP    ,
     *                  NPN   ,NMAX  ,NPAR  ,NCHA  ,LCHA  ,ITYP   )
C
C
C----------------------------------------------------------------------+
C
      CALL CLEA  ( GAMI , NFIS*NMAX   ,    0.0 )
      CALL MEMOVE( GAM  , GAMI    , NFIS*NMAX   )
C
       TSTEP  = DELT*24.0*3600.0
       IF(IOPT.EQ.0) GO TO 2001
C
       DO 1700 KZ  = 1, NZONE
       IF(IBZ(KZ).EQ.0) GO TO 1700
C
      CALL ICLEA( MAP   , MXNUC , 0   )
      MAPLEN   = 0
      DO 800 I = 1 , NMAX
      IPOS     = IPBURN(I,KZ)
      IF(IPOS.GT.0) THEN
                    MAPLEN =  MAPLEN + 1
                    MAP(MAPLEN) = I
                    ENDIF
  800 CONTINUE
C
            DO 1340  J  = 1, NMAX
               FNN(J)   =  DN   (J,KZ)
               FNF(J)   =  0.0
               SGF(J)   =  SSF  (J,KZ)
               SGC(J)   =  SSC  (J,KZ)
CMOD           SGN2N(J) =  SSN2N(J,KZ)
               SGN2N(J) =  SSN2N(J,KZ)*FACT2N(J)
               SGA(J)   =  SGF(J) + SGC(J)
 1340       CONTINUE
C
C  **** COOLING *****************
C
            FLUX   = 0.0
            XM     = 0.0
C
            DO 1505 II = 1 , NMAX
            GAMX(II)  = 0.0
            GAMMA(II) = 0.0
 1505       CONTINUE
C
            CALL FNCALC(
     *          RAMDA ,NCH   ,NBIC  ,PBIC  ,NOL   ,KSTP  ,
     *          LONG  ,LL    ,IP    ,KP    ,NPN   ,
     *          GAMMA ,GAMX  ,SGA   ,SGC   ,SGN2N ,PHIC  ,
     *          PHIL  ,PHAI  ,P     ,R     ,G     ,RAM   ,
     *          FNN   ,FNF   ,DEX   ,DEXX  ,DCOEF ,MAP   ,
     *          FLUX  ,TSTEP ,XM    ,
     *          NMAX  ,NPAR  ,NCHA  ,LCHA  ,MAPLEN )
C
            DO 1610   J  = 1, NMAX
            DN(J,KZ)     = FNF(J)
 1610       CONTINUE
 1700 CONTINUE
      RETURN
C
C  *** BURN-UP
C
 2001 CONTINUE
      CALL  CLEA ( FU235 , MXNUC , 0.0 )
      FACPOW = 0.0
C
                   IF(IDBG.GT.1)    THEN
                                    DO 105 L = 1 , NMAX
                   WRITE(NSYSO,111) L,NUCL(L),NCH(L),RAMDA(L)
                                    DO 105 LN= 1 , NCH(L)
                   WRITE(NSYSO,112) LN,NUCLP(LN,L),NBIC(LN,L),PBIC(LN,L)
  105                               CONTINUE
                                    ENDIF
C
                   IF(IDBG.GT.2)    THEN
                                    DO 110 L = 1 , NFIS
                   WRITE(NSYSO,113) L,NUCL(L)
                   WRITE(NSYSO,114) (GAM(L,LN),LN=1,NMAX)
  110                               CONTINUE
                                    ENDIF
C
  111 FORMAT(1H0,' ## L  NUCL  NCH   RAMDA ## ',3I8,1PE12.5)
  112 FORMAT(1H ,' << LN NUCLP NBIC  PBIC  >> ',3I8,F10.5)
  113 FORMAT(1H ,' ## L  NUCL  (GAM)       ## ',2I8)
  114 FORMAT(1H ,' << GAM >> ',1P10E11.4)
C
      TEMP0  = 0.0
      ACOEF  = EVTOJ*1.0000E+6
C
      SUMU5     = 0.0
      DO 140 KZ = 1,NZONE
C
      IF(IDBG.GT.0)   THEN
                      WRITE(NSYSO,*) ' ** KZ VOL PHIP ACOEF IBZ(KZ) **'
     1                               ,KZ,VOL(KZ),PHIP(KZ),ACOEF,IBZ(KZ)
                      ENDIF
      IF(IBZ(KZ).LE.0) GO TO 140
C
      SUMU5     = SUMU5 + VOL(KZ)*DN(IDU235,KZ)
      DO 135 LN = 1, NMAX
      TEMP0  =TEMP0 + ( SSF(LN,KZ)*EFC(1,LN)
     1              +   SSC(LN,KZ)*EFC(2,LN) )
     2              * VOL(KZ)*PHIP(KZ)*DN(LN,KZ)*ACOEF
C
      IF(IDBG.GT.1) THEN
                      WRITE(NSYSO,*) ' ** KZ LN SSF EF SSC EC : ',
     1                KZ,LN,SSF(LN,KZ),EFC(1,LN),SSC(LN,KZ),EFC(2,LN)
                      ENDIF
C
  135 CONTINUE
  140 CONTINUE
      FU235(1) =  100.0 * ( ST235 - SUMU5*1.000E+24) / ST235
C
      NLOOP  = NDN
      SAVED  = DELT /DFLOAT(NLOOP)
      IF(SAVED.LT.1.00) THEN
                        NLOOP = DELT + 0.01
                        ENDIF
      IF(SAVED.GT.10.0) THEN
                        NLOOP = DELT/10.000 + 0.01
                        ENDIF
C
      IF(NLOOP.LT. 5) NLOOP =  5
      IF(NLOOP.GT.50) NLOOP = 50
      NLOOP1 = NLOOP - 1
      TTSTEP = TSTEP/DFLOAT(NLOOP)
      DELDAY = DELT /DFLOAT(NLOOP)
C
      IF(IDBG.GT.0) THEN
      WRITE(NSYSO,*) ' ** TEMP0 NLOOP DELDAY (BURNCL) : ',
     1                    TEMP0,NLOOP,DELDAY
                    ENDIF
C
C-----LOOP OF SUB-TIME STEP
C
      DAYSUM        = 0.0
      FACFLX        = 1.00000
      DO 4000 LOOPT = 1 , NLOOP
      IF(TTSTEP.LE.0.0) GO TO 4100
      DAYSUM        = DAYSUM + DELDAY
C-----FIRST  LOOP OF MATERIAL ZONE
C-----LOOP OF MATERIAL ZONE
      TEMP1       = 0.0
      TEMP2       = 0.0
      DO 2700 KZ  = 1, NZONE
      IF(IBZ(KZ).EQ.0) GO TO 2700
      VOLDEP     = VOL(KZ)
C
      CALL ICLEA( MAP   , MXNUC , 0   )
      MAPLEN   = 0
      DO 810 I = 1 , NMAX
      IPOS     = IPBURN(I,KZ)
      IF(IPOS.GT.0) THEN
                    MAPLEN =  MAPLEN + 1
                    MAP(MAPLEN) = I
                    ENDIF
  810 CONTINUE
C
            DO 2340 J = 1, NMAX
               FNN(J) = DN  (J,KZ)
               FNF(J) = 0.0
               SGF(J) = SSF(J,KZ)
               SGC(J) = SSC(J,KZ)
               SGA(J) = SGF(J) + SGC(J)
               SGN2N(J) = SSN2N(J,KZ)*FACT2N(J)
 2340       CONTINUE
C
            FLUX   = PHIP(KZ)*1.0E-24*FACFLX
            XM     = 0.0
C
            DO 2502 J = 1, NFIS
            XM        = XM + SGF(J)*FNN(J)
 2502       CONTINUE
            RXM       = 0.0
            IF(XM.GT.0.0)  RXM = 1.00000 / XM
C
            DO 2505 II = 1 , NMAX
               SUMGX     = 0.0
               SUMGI     = 0.0
               DO 2504 J = 1, NFIS
                  SUMGX=SUMGX+GAM (J,II)*FNN(J)*SGF(J)
                  SUMGI=SUMGI+GAMI(J,II)*FNN(J)*SGF(J)
 2504          CONTINUE
CM             GAMX(II)  = SUMGX/XM
CM             GAMMA(II) = SUMGI/XM
               GAMX(II)  = SUMGX*RXM
               GAMMA(II) = SUMGI*RXM
 2505       CONTINUE
C
      XM = XM * FLUX
C
            CALL FNCALC(
     *          RAMDA ,NCH   ,NBIC  ,PBIC  ,NOL   ,KSTP  ,
     *          LONG  ,LL    ,IP    ,KP    ,NPN   ,
     *          GAMMA ,GAMX  ,SGA   ,SGC   ,SGN2N ,PHIC  ,
     *          PHIL  ,PHAI  ,P     ,R     ,G     ,RAM   ,
     *          FNN   ,FNF   ,DEX   ,DEXX  ,DCOEF ,MAP   ,
     *          FLUX  ,TTSTEP,XM    ,
     *          NMAX  ,NPAR  ,NCHA  ,LCHA  ,MAPLEN  )
C
      DO 2600 LN =1, NMAX
      SAVE       =  ( SGF(LN)*EFC(1,LN) + SGC(LN)*EFC(2,LN) )*FACFLX
      TEMP1      = TEMP1  + SAVE*VOLDEP*PHIP(KZ)*FNN(LN)*ACOEF
      TEMP2      = TEMP2  + SAVE*VOLDEP*PHIP(KZ)*FNF(LN)*ACOEF
 2600 CONTINUE
 2700 CONTINUE
C
C-----SECOND LOOP OF MATERIAL ZONE
      FACFLX      =  FACFLX * 2.000*TEMP0/(TEMP1+TEMP2)
C
      IF(IDBG.GT.0) THEN
       WRITE(NSYSO,*) ' **LOOPT TEMP1 TEMP2 FACFLX DAYSUM (BURNCL) ** ',
     1                    LOOPT,TEMP1,TEMP2,FACFLX,DAYSUM
                    ENDIF
      CALL  CLEA ( POWZN1 , NZONE , 0.0 )
      CALL  CLEA ( POWZN2 , NZONE , 0.0 )
C-----LOOP OF MATERIAL ZONE
      TEMP3       = 0.0
      TEMP4       = 0.0
      DO 3700 KZ  = 1, NZONE
      IF(IBZ(KZ).EQ.0) GO TO 3700
      VOLDEP     = VOL(KZ)
C
      CALL ICLEA( MAP   , MXNUC , 0   )
      MAPLEN   = 0
      DO 820 I = 1 , NMAX
      IPOS     = IPBURN(I,KZ)
      IF(IPOS.GT.0) THEN
                    MAPLEN =  MAPLEN + 1
                    MAP(MAPLEN) = I
                    ENDIF
  820 CONTINUE
C
            DO 3340 J = 1, NMAX
               FNN(J) = DN  (J,KZ)
               FNF(J) = 0.0
               SGF(J) = SSF(J,KZ)
               SGC(J) = SSC(J,KZ)
               SGA(J) = SGF(J) + SGC(J)
               SGN2N(J) = SSN2N(J,KZ)*FACT2N(J)
 3340       CONTINUE
C
            FLUX   = PHIP(KZ)*1.0E-24*FACFLX
            XM     = 0.0
C
            DO 3502 J = 1, NFIS
            XM        = XM + SGF(J)*FNN(J)
 3502       CONTINUE
C
            RXM       = 0.0
            IF(XM.GT.0.0)  RXM = 1.00000 / XM
C
            DO 3505 II = 1 , NMAX
               SUMGX     = 0.0
               SUMGI     = 0.0
               DO 3504 J = 1, NFIS
                  SUMGX=SUMGX+GAM (J,II)*FNN(J)*SGF(J)
                  SUMGI=SUMGI+GAMI(J,II)*FNN(J)*SGF(J)
 3504          CONTINUE
CMOD           GAMX(II)  = SUMGX/XM
CMOD           GAMMA(II) = SUMGI/XM
               GAMX(II)  = SUMGX*RXM
               GAMMA(II) = SUMGI*RXM
 3505       CONTINUE
C
      XM = XM * FLUX
C
            CALL FNCALC(
     *          RAMDA ,NCH   ,NBIC  ,PBIC  ,NOL   ,KSTP  ,
     *          LONG  ,LL    ,IP    ,KP    ,NPN   ,
     *          GAMMA ,GAMX  ,SGA   ,SGC   ,SGN2N ,PHIC  ,
     *          PHIL  ,PHAI  ,P     ,R     ,G     ,RAM   ,
     *          FNN   ,FNF   ,DEX   ,DEXX  ,DCOEF ,MAP   ,
     *          FLUX  ,TTSTEP,XM    ,
     *          NMAX  ,NPAR  ,NCHA  ,LCHA  ,MAPLEN       )
CCHECK
      IF(IDBG.GT.1) THEN
                    IF(KZ.EQ.1) THEN
                             WRITE(NSYSO,7774) NMAX,TEMP0,FACFLX
                             WRITE(NSYSO,7775) (EFC(1,LN),LN=1,NMAX)
                             WRITE(NSYSO,7776) (EFC(2,LN),LN=1,NMAX)
                             WRITE(NSYSO,7777) (IBZ(LN) ,LN=1,NZONE)
                             ENDIF
                    WRITE(NSYSO,7771) LOOPT,KZ,PHIP(KZ),FLUX
                    WRITE(NSYSO,7782) SGF
                    WRITE(NSYSO,7783) SGC
                    WRITE(NSYSO,7772) SGA
                    WRITE(NSYSO,7773) SGN2N
                    WRITE(NSYSO,7778) (FNN(J),J=1,NMAX)
                    WRITE(NSYSO,7779) (FNF(J),J=1,NMAX)
                    WRITE(NSYSO,7780) (DNOLD(J,KZ),J=1,NMAX)
                    ENDIF
C
 7771 FORMAT(1H0,' ## LOOPT KZ PHIP FLUX (BURNCL)## ',2I6,1P5E12.5)
 7772 FORMAT(1H ,' ## SGA ## ',1P10E11.4)
 7782 FORMAT(1H ,' ## SGF ## ',1P10E11.4)
 7783 FORMAT(1H ,' ## SGC ## ',1P10E11.4)
 7773 FORMAT(1H ,' ## SGN2N# ',1P10E11.4)
 7774 FORMAT(1H0,' ## NMAX TEMP1 TEMP2  (BURNCL)## ',I6,1P5E12.5)
 7775 FORMAT(1H ,' ## EFIS## ',1P10E11.4)
 7776 FORMAT(1H ,' ## CFIS## ',1P10E11.4)
 7777 FORMAT(1H ,' ## NZONE ## ', 10I11 )
 7778 FORMAT(1H ,' ## FNN ## ',1P10E11.4)
 7779 FORMAT(1H ,' ## FNF ## ',1P10E11.4)
 7780 FORMAT(1H ,' ## DNOLD# ',1P10E11.4)
C
      DO 3600 LN = 1, NMAX
      SAVE       =  ( SGF(LN)*EFC(1,LN) + SGC(LN)*EFC(2,LN) )*FACFLX
      TEMP3      = TEMP3      + SAVE*VOLDEP*PHIP(KZ)*FNN(LN)*ACOEF
      TEMP4      = TEMP4      + SAVE*VOLDEP*PHIP(KZ)*FNF(LN)*ACOEF
      POWZN1(KZ) = POWZN1(KZ) + SAVE*VOLDEP*PHIP(KZ)*FNN(LN)*ACOEF
      POWZN2(KZ) = POWZN2(KZ) + SAVE*VOLDEP*PHIP(KZ)*FNF(LN)*ACOEF
      DN (LN,KZ) = FNF(LN)
 3600 CONTINUE
 3700 CONTINUE
C
      SUMU5       = 0.0
      DO 3900 KZ  = 1, NZONE
      IF(IBZ(KZ).GT.0) SUMU5 = SUMU5 + VOL(KZ)*DN(IDU235,KZ)
 3900 CONTINUE
      SAVEU5 =  100.0 * ( ST235 - SUMU5*1.000E+24) / ST235
C
      FU235(LOOPT+1) =  SAVEU5
      FACPOW = FACPOW + FACFLX*DELDAY
C
      IF(IDBG.GT.0) THEN
       WRITE(NSYSO,*) ' ** LOOPT TEMP3 TEMP4 SAVEU5(BURNCL) ** ',
     1                     LOOPT,TEMP3,TEMP4,SAVEU5
       WRITE(NSYSO,*) ' ** LOOPT TEMP0 (TEMP3+TEMP4)/2 FACFLX** '
       WRITE(NSYSO,*)      LOOPT,TEMP0,(TEMP3+TEMP4)/2.000,FACFLX
       WRITE(NSYSO,*) ' ** LOOPT FACPOW DAYSUM FACPOW/DAYSUM ** '
       WRITE(NSYSO,*)      LOOPT,FACPOW,DAYSUM,FACPOW/DAYSUM
                    ENDIF
C
      DO 3920 KZ  = 1, NZONE
      CUMWD(KZ) = CUMWD(KZ) + DELDAY*(POWZN1(KZ)+POWZN2(KZ))*0.5000000
 3920 CONTINUE
C
      IF(IBC2.EQ.5.AND.LOOPT.GE.2.AND.LOOPT.LE.NLOOP1) THEN
                IF(SAVEU5.GE.TIMEU5) GO TO 4100
                    DELU5  = FU235(LOOPT+1) - FU235(LOOPT)
                    U5NEXT = SAVEU5 + DELU5
      IF(IDBG.GT.0) WRITE(NSYSO,*) ' ** LOOPT DELU5 U5NEXT TIMEU5 : ',
     1          LOOPT,DELU5,U5NEXT,TIMEU5,FU235(LOOPT+1),FU235(LOOPT)
C
                IF(LOOPT.LT.NLOOP1) THEN
                    IF(U5NEXT.GT.TIMEU5) THEN
                          TTSTEP = TTSTEP * ( TIMEU5 - SAVEU5 )/ DELU5
                          DELDAY = DELDAY * ( TIMEU5 - SAVEU5 )/ DELU5
                          ENDIF
                    ELSE
                    TTSTEP = TTSTEP * ( TIMEU5 - SAVEU5 )/ DELU5
                    DELDAY = DELDAY * ( TIMEU5 - SAVEU5 )/ DELU5
                    ENDIF
               ENDIF
 4000 CONTINUE
C
C *** END OF PROCESS
C
 4100 DELT  = DAYSUM
      FACPOW = FACPOW/DAYSUM
      RETURN
      END
