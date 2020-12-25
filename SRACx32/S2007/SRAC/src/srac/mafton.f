      SUBROUTINE   MAFTON(NISO  ,IRES  ,DN    ,SIG0  ,ISW   ,
     2                    VOLM  ,IDENTH,DENHM ,SIG0HM,SSTHM ,SIGTHM,
     3                    NCODE ,DENWRK,SSTWRK,SIGTIJ,PIJ   ,MATD  ,
     4                    ISTONE,MTNAME,IDENT ,DANCOF,XL    ,BELL)
C
      CHARACTER*8     JNEFST,FNEFST,JNMACR,FNMACR
C
      COMMON  /MAINC / NNOPT(500)
      COMMON  /MAFCNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     +                 NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     +                 MXREAC,NOUT1,NOUT2,NORES,NMP,NISOHM,ISNCAL,IPLMAX
     +                ,IGMAX,NET,NET5
C
      EQUIVALENCE     (NSIGTF,NNOPT(95))
C
      DIMENSION        NISO(NMAT),IRES(KNMAX,NMAT),DN(KNMAX,NMAT)
      DIMENSION        SIG0(NEF,KNMAX),ISW(NMAT),MATD(NMP)
      DIMENSION        VOLM(NMAT),DENHM(NISOHM),SIG0HM(NEF,NISOHM)
      DIMENSION        SSTHM(NEF,2,NISOHM),SIGTHM(NEF,NMAT)
      DIMENSION        NCODE(NISOHM,NMAT),DENWRK(NISOHM),SSTWRK(NISOHM)
      DIMENSION        SIGTIJ(NEF,NMP),PIJ(NMP,NMP,NEF)
      DIMENSION        DANCOF(KNMAX,NMAT),XL(NMAT)
C
      CHARACTER*8      IDENTH(NISOHM)
      CHARACTER*8      MTNAME(NMAT),IDENT(KNMAX,NMAT)
C
      DIMENSION        WORK(100)
C    *************************************************
C    * INITIAL SET                                   *
C    *************************************************
      IF(NMP.LE.0) RETURN
C
      LENG1  = NEF*KNMAX
      LENG2  = NEF*NMP
      LENG3  = NMP*NMP*NEF
C
      CALL  CLEA(SIGTIJ, LENG2 , 0.0 )
      CALL  CLEA(PIJ   , LENG3 , 1.0 )
C
      TVOL     = 0.0
      DO 110 N = 1 , NMAT
      TVOL     = TVOL + VOLM(N)
  110 CONTINUE
C
*     WRITE(6,*) ' **** MATTON PASSED *** TVOL = ',TVOL
C    *************************************************
C    *  SET MACROSCOPIC TOTAL X-SECTION FOR PIJ      *
C    *  AND CALCULATES PIJ                           *
C    *************************************************
      DO 200    K = 1 , NMP
      NN          = MATD(K)
      DO 150    I = 1 , NEF
      SIGTIJ(I,K) = SIGTHM(I,NN)
  150 CONTINUE
  200 CONTINUE
C
      REWIND   NSIGTF
      DO 210 K = 1 , NMP
      WRITE(NSIGTF) (SIGTIJ(I,K),I=ISTONE,NEF)
*     WRITE(6,*) ' ** K NMP MATD(K) ISTONE NEF ** ',
*    +                K,NMP,MATD(K),ISTONE,NEF
*     WRITE(6,231)  (SIGTIJ(I,K),I=ISTONE,NEF)
  210 CONTINUE
      REWIND   NSIGTF
      NG       = NEF + 1 - ISTONE
      CALL PIJ2(NG,3)
C
      REWIND 21
      DO 230 N = ISTONE , NEF
      READ(21) ((PIJ(I,J,N),I=1,NMP),J=1,NMP)
*     DO 230 I = 1,NMP
*     WRITE(6,232) N,I,(PIJ(I,J,N),J=1,NMP)
  230 CONTINUE
      REWIND 21
C
  231 FORMAT(1H ,' ## SIGT ## ',1P10E11.4)
  232 FORMAT(1H ,' ## NG I PIJ(I,J) ## ',2I4,10F10.6)
C    *************************************************
C    *  CALCULATES SIGMA-0 VALUE                     *
C    *************************************************
      REWIND 3
      SETVAL     = 1.0000E+10
C
      DO 1000 NN = 1,NMAT
      CALL CLEA(SIG0,LENG1,SETVAL)
      IF(ISW(NN).NE.3) GO TO 900
      MMK        = NISO(NN)
      IF(MMK.LE.0)     GO TO 900
C
      IF(VOLM(NN).LE.0.0) THEN
                          DO  300 J = 1 , NISOHM
                          M         = NCODE(J,NN)
                          IF(M.LE.0)           GO TO 300
                          DNTMP     = DN(M,NN)
                          IF(DNTMP.LE.0.0)     GO TO 300
                          IF(IRES(M,NN).EQ.-1) GO TO 300
                          DNTMP     = 1.0 / DNTMP
                          DO 250 NG = 1,NEF
                          SIG0(NG,M)= SIGTHM(NG,NN)*DNTMP-SSTHM(NG,2,J)
  250                     CONTINUE
  300                     CONTINUE
                          GO TO 900
                          ENDIF
C
      DO  400 J = 1 , NISOHM
      M         = NCODE(J,NN)
      IF(M.LE.0)           GO TO 400
      IF(IRES(M,NN).EQ.-1) GO TO 400
      DO 350 NG = 1,NEF
      SIG0(NG,M)= SIG0HM(NG,J)
  350 CONTINUE
  400 CONTINUE
C
      JPOS     = 0
      DO 410 K = 1 , NMP
      IF(MATD(K).EQ.NN) JPOS = K
  410 CONTINUE
      IF(JPOS.LE.0) GO TO 900
C-----LOOP OF ENERGY GROUP
      DO 800 NG= ISTONE , NEF
      CALL CLEA ( DENWRK , NISOHM , 0.0 )
      CALL CLEA ( SSTWRK , NISOHM , 0.0 )
C-----SET  DENWRK & SSTWRK
      DO 500 J = 1 , NISOHM
      SUM1     = 0.0
      SUM2     = 0.0
      DO 450 K = 1 , NMP
      KK       = MATD(K)
      IPOS     = NCODE(J,KK)
      IF(IPOS.LE.0)           GO TO 450
      SAVE     = DN(IPOS,KK)*VOLM(KK)*PIJ(K,JPOS,NG)
      SUM1     = SUM1 + SAVE
      SUM2     = SUM2 + SAVE*SSTHM(NG,1,J)
  450 CONTINUE
      DENWRK(J)= SUM1 / TVOL
      IF(SUM1.GT.0.0) SSTWRK(J) = SUM2/SUM1
  500 CONTINUE
C
*     IF(NG.EQ.NEF) THEN
*                   WRITE(6,501) NN,NG,MTNAME(NN)
*                   WRITE(6,502) IDENTH
*                   WRITE(6,503) DENWRK
*                   WRITE(6,504) DENHM
*                   WRITE(6,505) SSTWRK
*                   WRITE(6,506) (SSTHM(NG,1,J),J=1,NISOHM)
*                   ENDIF
C
  501 FORMAT(1H ,' ## NN NG MTNAME ## ',2I6,2X,A8)
  502 FORMAT(1H ,' ## IDENTH    ## ',10A10)
  503 FORMAT(1H ,' ## DENWRK    ## ',1P10E11.4)
  504 FORMAT(1H ,' ## DENHM     ## ',1P10E11.4)
  505 FORMAT(1H ,' ## SSTWRK    ## ',1P10E11.4)
  506 FORMAT(1H ,' ## SSTHM     ## ',1P10E11.4)
C-----SET SIGMA0
      DO 700 J = 1 , NISOHM
      IPOS     = NCODE(J,NN)
      IF(IPOS.LE.0)    GO TO 700
      DNTMP    = DENWRK(J)
      IF(DNTMP.LE.0.0) GO TO 700
      DNTMP    = 1.0 / DNTMP
      SUM      = 0.0
      DO 600 M = 1 , NISOHM
      IF(M.EQ.J) GO TO 600
      SUM      = SUM + DNTMP*DENWRK(M)*SSTWRK(M)
  600 CONTINUE
      SIG0(NG,IPOS) = SUM
  700 CONTINUE
  800 CONTINUE
C-----CHECK WRITE AND CONVENTIONAL SIG0 CAL. AND COMPARES THEM
      XLL      = XL(NN)
      IF(XLL.LE.0.0)  XLL=1.0E+20
      DO 880 J  = 1,NISOHM
      CALL CLEA( WORK , 100 , SETVAL)
      M         = NCODE(J,NN)
      IF(M.LE.0)           GO TO 880
      IF(IRES(M,NN).EQ.-1) GO TO 880
      DNTEMP    = DN(M,NN)
      IF(DNTEMP.LE.0.0)    GO TO 880
      DNTMP     = 1.0 / DNTEMP
      DANCF     = DANCOF(M,NN)
      SS        = ( BELL / (1.0+(BELL-1.0)*DANCF) ) * ( 1.0-DANCF )/XLL
      S         = SS  * DNTMP
      DO 860  I = 1,NEF
      WORK(I)   = SIGTHM(I,NN)*DNTMP - SSTHM(I,1,J) + S
  860 CONTINUE
C
*     WRITE(6,891) MTNAME(NN),IDENT(M,NN),DANCF,DNTEMP,SS
*     WRITE(6,892) (SIG0(I,M),I=1,NEF)
*     WRITE(6,893) (WORK(I),I=1,NEF)
*     WRITE(6,894) (SIG0HM(I,J),I=1,NEF)
  880 CONTINUE
C
  900 WRITE(3) NN,SIG0
 1000 CONTINUE
      REWIND 3
C
* 891 FORMAT(1H ,' ## MATNAME IDENT DANCOF DENSITY SS ## ',
*    +       A8,2X,A8,2X,3F12.6)
* 892 FORMAT(1H ,' ## SIG0(TONE) ## ',1P10E11.4)
* 893 FORMAT(1H ,' ## SIG0(DANC) ## ',1P10E11.4)
* 894 FORMAT(1H ,' ## SIG0(HOMO) ## ',1P10E11.4)
C
      RETURN
      END
