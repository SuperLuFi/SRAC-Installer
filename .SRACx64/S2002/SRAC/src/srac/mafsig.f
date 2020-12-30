      SUBROUTINE   MAFSIG(MTNAME,NISO  ,XL    ,DC    ,
     1                    IDENT ,IRES  ,DN    ,LXMICR,
     2                    SIG0  ,SIGT  ,PIJ   ,ENBND ,ISW   ,
     3                    SIGWF ,SIGWM ,SIGWT ,DANCOF,NAMRES,
     4                    IND   ,MCODE ,DENRES,SIGT0J,GAMMA ,
     5                    MATD  ,VOLM  ,SIG0HM,SSTHM ,SIGTHM,
     6                    NCODE ,KCODE ,ISWF  ,SIGOLD,ISTONE )
C
CDEL  PARAMETER   (MXLISO= 2000)
      INCLUDE  'MATDTINC'
C
      CHARACTER*8     JNEFST,FNEFST,JNMACR,FNMACR
      CHARACTER*4     NFILE,NAMEP,MTNAME,IDENT
      CHARACTER*4     ISAVE1
C
      COMMON /MAINC / NNOPT(500)
      COMMON /PIJ2C / IPAA(1000)
      COMMON  /MAFCNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     +                 NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     +                 MXREAC,NOUT1,NOUT2,NORES,NMP,NISOHM,ISNCAL,IPLMAX
     +                ,IGMAX,NET,NET5
      COMMON /MAFCNT/ AMASS,SIGP,ICAPT,IFISS,IIRES,LTOT,IFS,IFTR,IFC,
     +                IFF,IFE,IFER,NGMIN,NGMAX,NSIG,NTEMP,SIGC0
      COMMON /MAFDBL/ MTREPL,MICFL,MICMOD,IPATH,SXLL,SBELL,SVF,SVM,
     +                SDAN  ,SGAMMA,IGEOMS
      COMMON /MAFWRK/ A(17000),NAMEP(2),LOCAM(11),LOCAF(6)
      COMMON /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,ITEMP
      COMMON /DOUBLE/ LDOUBL(50)
      COMMON /NEWDAN/ DANNEW(MXLISO)
C
      DIMENSION      IA(17000),DDOUBL(50)
      EQUIVALENCE    (A(1),IA(1)),(NSIGTF,NNOPT(95))
      EQUIVALENCE    (LDOUBL(1),DDOUBL(1))
C
      DIMENSION  MTNAME(2,NMAT),NISO(NMAT),XL(NMAT),
     1           DC(NMAT),IDENT(2,KNMAX,NMAT),IRES(KNMAX,NMAT),
     2           DN(KNMAX,NMAT),LXMICR(KNMAX,NMAT),
     4           SIG0(NEF,KNMAX),SIGT(NMAT),
     5           PIJ(NMP,NMP),ENBND(NEF1),ISW(NMAT)
C
      DIMENSION  SIGWF(NEF),SIGWM(NEF),SIGWT(NEF,1)
      DIMENSION   DANCOF(KNMAX,NMAT),IND(NORES,NMAT),MCODE(NORES,NMAT)
      DIMENSION   DENRES(NORES,NMAT),SIGT0J(NMP),GAMMA(NMP,NMP)
      CHARACTER*4 NAMRES(NORES)
CJAIS ADDED FOR TONE'S METHOD  5/10/1989
      DIMENSION   VOLM(NMAT),SSTHM(NEF,2,NISOHM),SIGTHM(NEF,NMAT)
      DIMENSION   NCODE(NISOHM,NMAT),SIG0HM(NEF,NISOHM)
      DIMENSION   MATD(NMP),KCODE(NORES,NMAT)
      DIMENSION   ISWF(NMAT),SIGOLD(NMP)
CJAIS END
C    *************************************************
C    * INITIAL SET                                   *
C    *************************************************
      LENG1 = NEF*KNMAX
      LENG2 = NMP*NMP
      LENG3 = NORES*NMAT
C
      CALL  CLEA(SIGT  , NMAT  , 0.0 )
      CALL  CLEA(PIJ   , LENG2 , 0.0 )
      CALL  CLEA(SIGT0J, NMAT  , 0.0 )
C
      MTREPL = 0
      MICFL  = 0
      MICMOD = 0
CDELETE FOR NEW EFFECITVE MICRO CROSS SECTION TREATMENT JULY/1994
C    *************************************************
C    * RESET LXMICR WHEN USER SPECIFY IC5 = 2 OR -2  *
C    * ADDED BY JAIS K.KANEKO 2/19/1986 IN JEARI.    *
C    *************************************************
CDEL  IF(IABS(IOPT(5)).EQ.2) THEN
CDEL                         IFLSW   = 1
CDEL                         NFILE(1)= 'MICR'
CDEL                         NFILE(2)= 'EF  '
C
CDEL                         DO 9300 I = 1,NMP
CDEL                         NN        = MATD(I)
CDEL                         MMK       = NISO(NN)
CDEL                         IF(MMK.LE.0)         GO TO 9300
CDEL                         DO 9200 M = 1 , MMK
CDEL                         IF(IRES(M,NN).EQ. 1) GO TO 9200
CDEL                         IF(IRES(M,NN).EQ.-1) GO TO 9200
CDEL                         NAMEP(1)  = IDENT(1,M,NN)
CDEL                         NAMEP(2)  = IDENT(2,M,NN)
CDEL                         NAMEP(1) (1:1) = 'C'
CDEL                         NAMEP(2) (1:1) = 'F'
CDEL                         NAMEP(2) (2:3) = MTNAME(2,NN) (2:3)
CDEL                         LENG       = 0
CDEL                         JSW        = 0
CDEL                         CALL SEARCH(NAMEP(1),LENG,JSW)
CM    WRITE(NOUT2,FMT='(15H  NAMEP JSW => ,2A4,I6)')  NAMEP,JSW
CDEL                         IF(JSW.NE.0) LXMICR(M,NN) = 1
C9200                        CONTINUE
C9300                        CONTINUE
CDEL                         ENDIF
C    *************************************************
C    * PARAMETER SET FOR DOUBLE HETRO. TREATMENT     *
C    *************************************************
      IF(IPATH.EQ.1) THEN
                     DO 4400 I=1,NMP
                     NN = MATD(I)
                     MMK= NISO(NN)
                     IF(MMK.LE.0)  GO TO 4400
                     DO 4200 M = 1 , MMK
                     IF(DN(M,NN).LT.1.0E-6) GO TO 4200
                     IF(IRES(M,NN).EQ.2)    GO TO 4300
 4200                CONTINUE
                     GO TO 4400
 4300                MTREPL = NN
 4400                CONTINUE
C
                     IF(MTREPL.EQ.0) THEN
                                     WRITE(NOUT1,4410)
                                     WRITE(NOUT2,4410)
                                     STOP
                                     ENDIF
C
                     MICFL = MTREPL - 2
                     MICMOD= MTREPL - 1
                     MMK   = NISO(MICFL)
                     DO 4600 M = 1 , MMK
                     IF(IRES(M,MICFL).EQ.2) GO TO 4700
 4600                CONTINUE
                     MICFL = MTREPL - 1
                     MICMOD= MTREPL - 2
 4700                CONTINUE
                     IGEOMS= LDOUBL(02)
                     SXLL  = DDOUBL(10)
                     VF    = DDOUBL(11)
                     VM    = DDOUBL(12)
                     VCELL = DDOUBL(13)
                     SDAN  = DC(MICFL)
                     SGAMMA= 1.0 - SDAN
                     SBELL = 1.200
                     IF(IGEOMS.EQ.1) SBELL = 1.40
                     SVF   = VF / VCELL
                     SVM   = VM / VCELL
                     CALL  CLEA( SIGWF , NEF       , 0.0 )
                     CALL  CLEA( SIGWM , NEF       , 0.0 )
                     CALL  CLEA( SIGWT , NEF*KNMAX , 0.0 )
                     ENDIF
C
 4410 FORMAT(//1H ,5X,' ERROR STOP AT MAFSIG ]]]]',
     *        /1H ,5X,' NO RESONAT MATERIAL IN DOUBLE HETERO. PROBREM.')
C    *************************************************
C    *  READ 'FASTLIB' MEMBER FROM FAST-LIBRARY      *
C    *************************************************
      IFLSW    = 1
      NFILE(1) = 'FAST'
      NFILE(2) = 'U   '
      NAMEP(1) = 'FAST'
      NAMEP(2) = 'LIB '
      CALL READ(NAMEP(1),IA,4)
      NGF      = IA(1)
      NGF1     = IA(2)
      NGF2     = IA(3)
      NGF3     = IA(4)
      IF(NGF.NE.NEF)  GO TO 9999
      LENG     = 5 + 2*NGF
C
      CALL  READ(NAMEP(1),A,LENG)
      IST      = 4 + NEF
      ISTONE   = 0
      ERESON   = 5.000E+4
C
      DO 3010 I= 1,NEF+1
      IST      = IST+1
      ENBND(I) = A(IST)
      IF(I.EQ.1) GO TO 3010
      IF(ENBND(I-1).GT.ERESON.AND.ENBND(I).LE.ERESON) ISTONE = I-1
 3010 CONTINUE
*     WRITE(6,3004) ISTONE,ERESON,ENBND(ISTONE),ENBND(ISTONE+1)
C
 3001 FORMAT(1H ,' ##NGF  ## ',I6)
 3002 FORMAT(1H ,' ##WEIGH## ',1P10E12.5)
 3003 FORMAT(1H ,' ##ENBND## ',1P10E12.5)
 3004 FORMAT(1H ,' ##ISTONE ERESON EU EL ## ',I6,1P5E12.5)
C    *************************************************
C    *  WRITE 'CONT0002' MEMBER TO MACROWRK-FILE     *
C    *************************************************
      IFLSW    = 1
      NFILE(1) = 'MACR'
      NFILE(2) = 'OWRK'
      NAMEP(1) = 'CONT'
      NAMEP(2) = 'F002'
      ISWCNT   = 0
      CALL SEARCH(NAMEP(1),LENG,IISW)
      IF(IISW.NE.0) THEN
                    IST      = 1
                    ISWCNT   = 1
                    DO 3030 I= 1,2*NEF+1
                    IST      = IST+1
                    A(IST)   = A(IST+3)
 3030               CONTINUE
                    CALL  WRITE(NAMEP(1),A,IST)
                    ENDIF
CADD---- ALSO TO MICREF
      IMCEF    = NNOPT(78)
      IF(MOD(IMCEF,2).EQ.1)  THEN
           JJSW     = 0
           NFILE(1) = 'MICR'
           NFILE(2) = 'EF  '
           NAMEP(1) = 'CONT'
           NAMEP(2) = 'F002'
           CALL SEARCH(NAMEP(1),LENG,JJSW)
           IF(JJSW.NE.0) THEN
                    IF(ISWCNT.EQ.0) THEN
                          IST      = 1
                          DO 3035 I= 1,2*NEF+1
                          IST      = IST+1
                          A(IST)   = A(IST+3)
 3035                     CONTINUE
                          ENDIF
                    CALL  WRITE(NAMEP(1),A,IST)
                    ENDIF
            ENDIF
CEND
C
C    *************************************************
C    *  READ MACROSCOPIC TOTAL X-SCTION FROM MACROWRK*
C    *  FILE ACCORDING TO ISW INDEX ARRARY.          *
C    *************************************************
*     WRITE(6,*) ' *** ISW *** ',(ISW(I),I=1,NMAT)
C
      NFILE(1) = 'MACR'
      NFILE(2) = 'OWRK'
      DO 3300 NN = 1,NMAT
      IF(ISW(NN).EQ.3) GO TO 3300
      ISWW       = 0
      NAMEP(1)   = MTNAME(1,NN)
      NAMEP(2)   = MTNAME(2,NN)
      NAMEP(2) (1:1) = 'F'
      NAMEP(2) (4:4) = '2'
      IF(MOD(ISW(NN),2).EQ.0) NAMEP(2) (4:4) = '4'
      CALL SEARCH(NAMEP(1),LENG,ISWW)
      IF(ISWW.NE.0) GO TO 3300
      CALL READ(NAMEP(1),A,LENG)
C
      IST          = 0
      DO 3100    I = 1,NEF
      LGV          = IA(IST+2)
      SIGTHM(I,NN) =  A(IST+6)
      IST          = IST+10+LGV
 3100 CONTINUE
 3300 CONTINUE
C
      IF(IPATH.EQ.1) THEN
                     DO 200 I = 1 , NEF
                     SIGWF(I) = SIGTHM(I,MICFL)
  200                CONTINUE
                     DO 225 I = 1 , NEF
                     SIGWM(I) = SIGTHM(I,MICMOD)
  225                CONTINUE
                     DO 250 J = 1 , NISOHM
                     M        = NCODE(J,MTREPL)
                     IF(M.LE.0) GO TO 250
                     DO 240 I = 1 , NEF
                     SIGWT(I,M) = SSTHM(I,1,J)
  240                CONTINUE
  250                CONTINUE
                     ENDIF
C    *************************************************
C    *  CALCULATE MACROSCOPIC TOTAL X-SECTION USING  *
C    *  INFINTE MICROSCOPIC TOTAL X-SECTION , AND    *
C    *  CALCULATE SIGMA-0 VALUE OF EACH MATERIAL.    *
C    *************************************************
      REWIND 3
C-----LOOP OF MATERIAL
      SETINF     = 1.0000E+10
      DO  500 NN = 1,NMAT
      CALL CLEA(SIG0,LENG1,SETINF)
C-----SET SIGT(NN) FOR COLLISION PROBABILITY CAL.
      SIGT(NN)   = SIGTHM(NEF,NN)
      IF(ISW(NN).NE.3) GO TO 450
C
      MMK = NISO(NN)
      IF(MMK.LE.0)     GO TO 450
      JPOS       = 1
      IF(VOLM(NN).LE.0.0)  JPOS = 2
C-----LOOP OF NUCLIDE
      DO  400 J  = 1,NISOHM
      M          = NCODE(J,NN)
      IF(M.LE.0)           GO TO 400
      IF(IRES(M,NN).EQ.-1) GO TO 400
      DNTMP      = DN(M,NN)
CM    IF(DNTMP.LE.0.0)     GO TO 400
CMOD  IF(DNTMP.LT.1.0E-50) GO TO 400
      IF(DNTMP.LT.1.0E-30) GO TO 400
C
      DNTMP      = 1.0 / DNTMP
C-----CALCULATE SIGMA-0 VALUE
      IF(ISWF(NN).EQ.1.OR.VOLM(NN).LE.0.0) THEN
      DO 300   I = 1,NEF
      SIG0(I,M)  = SIGTHM(I,NN)*DNTMP - SSTHM(I,JPOS,J)
  300 CONTINUE
                                           ELSE
                                           DO 350 I = 1,NEF
                                           SIG0(I,M)= SIG0HM(I,J)
  350                                      CONTINUE
                                           ENDIF
  400 CONTINUE
  450 WRITE(3) NN,SIG0
  500 CONTINUE
      REWIND 3
C    *************************************************
C    * PRINT-OUT FOR DOUBLE HETRO. TREATMENT         *
C    *************************************************
      IF(IPATH.EQ.1)    THEN
      WRITE(NOUT2,4810) MTNAME(1,MTREPL),MTNAME(2,MTREPL),MTREPL,
     *                  MTNAME(1,MICFL ),MTNAME(2,MICFL ),MICFL ,
     *                  MTNAME(1,MICMOD),MTNAME(2,MICMOD),MICMOD,VCELL,
     *                  VF,VM,SVF,SVM,IGEOMS,SXLL,SDAN,SGAMMA,SBELL
C
      IF(IOPT(19).GT.1) THEN
                        WRITE(NOUT2,4820) SIGWF
                        WRITE(NOUT2,4830) SIGWM
                        DO 4805 M = 1 , NISO(MTREPL)
      WRITE(NOUT2,4840) M,IDENT(1,M,MTREPL),IDENT(2,M,MTREPL)
      WRITE(NOUT2,4851) (SIGWT(I,M),I=1,NEF)
 4805                   CONTINUE
                        ENDIF
                        ENDIF
C    *************************************************
C    * CALCULATE DANCOFF FACTOR ACCORDING TO IOPT(3) *
C    *************************************************
      IF(IOPT(3).EQ.0)  RETURN
C    *************************************************
C    *DANCOFF CORRECTION FACTOR BY EMPIRICAL FORMULA *
C    *************************************************
      IF(IOPT(3).NE.1.AND.IOPT(3).NE.2) THEN
                       WRITE(NOUT1,998)
                       STOP
                       ENDIF
C     *********************************************************
C     * CALCULATE NUCLIDE-WISE DANCOFF FACTOR.                *
C     * THIS FORMULATION IS ADDED SINCE 4/10/1984.            *
C     *********************************************************
      CALL ICLEA( MCODE , LENG3 ,   0 )
      CALL ICLEA( IND   , LENG3 ,   0 )
      CALL  CLEA( DENRES, LENG3 , 0.0 )
      CALL ICLEA( KCODE , LENG3 ,   0 )
C-----SET MCODE & DENRES
      DO 1400 K = 1 , NMP
      NN        = MATD(K)
CMOD  3/12/1991 BY JAIS K.KANEKO
CM    CALL  CLEA( DANCOF(1,NN) , KNMAX , 1.0   )
      CALL  CLEA( DANCOF(1,NN) , KNMAX , 0.0   )
      IF(ISWF(NN).EQ.0)   GO TO 1400
C
      CALL  CLEA( DANCOF(1,NN) , KNMAX , 1.0   )
      MMK       = NISO(NN)
      IF(MMK.LE.0)        GO TO 1400
      XLL       = XL(NN)
      IF(XLL.LE.0.0)  XLL = 1.0
C
                         DO 1370 M = 1 ,MMK
                         IF(IRES(M,NN).NE.2) GO TO 1370
                         ISAVE1 = IDENT(1,M,NN)
                         JJ        = 0
                            DO 1360 J = 1 , NORES
                            IF(ISAVE1.EQ.NAMRES(J))  THEN
                                                     MCODE(J,NN)=M
                                                     JJ         =J
                                                     ENDIF
 1360                       CONTINUE
                            IF(JJ.GT.0) THEN
                                        DO 1365 J = 1 , NISOHM
                            IF(NCODE(J,NN).EQ.M)  KCODE(JJ,NN)=J
 1365                                   CONTINUE
                                        DENRES(JJ,NN) = DN(M,NN)
                                        ENDIF
 1370                    CONTINUE
C
             DO 1380 M = 1 ,NORES
             IF(MCODE(M,NN).EQ.0) GO TO 1380
CM           DNTMP = DN(MCODE(M,NN),NN)
             DNTMP = DENRES(M,NN)
             SAVE  = DNTMP*XLL
C            MEAN CHORD LENGTH 1.-4  1988/8/22
CKSK (1996/2/9) IND=1:USE CALCULATED DANCOFF
CKSK         IF(SAVE.GT.1.0E-4)  IND(M,NN) = 1
             IF(DNTMP.GT.1.0E-6) IND(M,NN) = 1
             IF((DNTMP.LE.1.0E-6).AND.(SAVE.GT.1.0E-4))  IND(M,NN) = 1
CKSK
CM           DENRES(M,NN) = DNTMP
 1380        CONTINUE
 1400 CONTINUE
C@ADD
      IF(NORES.LE.0) GO TO 4801
C@END
C
C     LOOP OF RESONANCE NUCLIDE FOR CALCULATION OF DANCOFF FACTOR
C
      SIGINF = 100.0
      IPASS  = 0
      CALL CLEA(SIGOLD , NMP  , 0.0 )
C
      DO  4000 LOP = 1 , NORES
      ISUM = 0
      DO 2100 K=1,NMP
      NN = MATD(K)
      ISUM = ISUM + IND(LOP,NN)
 2100 CONTINUE
      IF(ISUM.EQ.0)  GO TO 4000
C-----SET TOTAL CROSS SECTION FOR LOP-TH RESONANT NUCLIDE
      SIGRES    = 1.0000E+8
      DO 2200 K = 1 , NMP
      NN        = MATD(K)
      IF(IND(LOP,NN).EQ.0) THEN
                           SIGT0J(K) = SIGT(NN)
                           GO TO 2200
                           ENDIF
      DNTMP     = DENRES(LOP,NN)
      SIGT0J(K) = SIGT(NN) - DNTMP*SSTHM(NEF,1,KCODE(LOP,NN))
      XLL       = XL(NN)
      IF(XLL.LE.0) XLL = 1.0
      SIGTMP    = (SIGINF/XLL - SIGT0J(K))/DNTMP
      IF(SIGTMP.LT.SIGRES)  SIGRES = SIGTMP
 2200 CONTINUE
C
      DO 2300 K = 1 , NMP
      NN        = MATD(K)
      IF(IND(LOP,NN).EQ.0) GO TO 2300
      DNTMP     = DENRES(LOP,NN)
      SIGT0J(K) = SIGT0J(K) +  DNTMP*SIGRES
 2300 CONTINUE
C
      ISKIP     = 0
      IF(IPASS.GT.0) THEN
                     ISKIP = 1
                     DO 2350 K = 1 , NMP
                     RATIO     = SIGT0J(K)/SIGOLD(K) - 1.0
                     IF(ABS(RATIO).GT.1.000E-4) ISKIP = 0
 2350                CONTINUE
                     ENDIF
C
C-----CALCULATE COLLISION PROBABILITY
C
      IF(ISKIP.EQ.1) GO TO 2501
C
*     WRITE(6,*) ' ** LOP NEF NAMRES(LOP) SIGINF SIGRES ** ',
*    +                LOP,NEF,NAMRES(LOP),SIGINF,SIGRES
*     WRITE(6,*) ' ** MATD(K)  ** ',(MATD(K),K=1,NMP)
*     WRITE(6,*) ' ** IND(LOP) ** ',(IND(LOP,K),K=1,NMAT)
*     WRITE(6,*) ' ** DENRES(K)** ',(DENRES(LOP,K),K=1,NMAT)
*     WRITE(6,*) ' ** SIGT(K)  ** ',(SIGT(K),K=1,NMAT)
*     WRITE(6,*) ' ** SIGTHM(K)** ',(SIGTHM(NEF,K),K=1,NMAT)
*     WRITE(6,*) ' ** SIGT0J(K)** ',(SIGT0J(K),K=1,NMP)
C
      NG = 1
      REWIND NSIGTF
      DO 2500  J=1,NMP
 2500 WRITE(NSIGTF)  SIGT0J(J)
      REWIND NSIGTF
C
      CALL PIJ2(NG,3)
C
      REWIND 21
      READ(21) ((PIJ(I,J),I=1,NMP),J=1,NMP)
      REWIND 21
C-----CALCULATE DANCOFF FACTOR
 2501 CALL  CLEA(GAMMA,LENG2,0.0)
C
      DO 2700 I = 1 , NMP
      NN     = MATD(I)
      XLL    = XL(NN)
      IF(XLL.LE.0)  XLL = 1.0
      SIGTOT = SIGT0J(I)
      SAVEDC = 1.0
C
             DO 2600 J = 1 , NMP
             GAMMA(I,J) = - PIJ(I,J)*SIGTOT*XLL
             IF(I.EQ.J) GAMMA(I,J) = (1.0-PIJ(I,J))*SIGTOT*XLL
             MM  = MATD(J)
             IF(IND(LOP,MM).EQ.0)  SAVEDC = SAVEDC + GAMMA(I,J)
 2600        CONTINUE
C
      IF(IND(LOP,NN).EQ.0) GO TO 2700
      MM = MCODE(LOP,NN)
      IF( MM        .EQ.0) GO TO 2700
      DANCOF(MM,NN) = SAVEDC
C
      IF(IOPT(19).GT.0) THEN
      WRITE(NOUT2,4004) NAMRES(LOP)
      WRITE(NOUT2,4001) MTNAME(1,NN),MTNAME(2,NN),
     *                  SAVEDC,XL(NN),PIJ(I,I),SIGRES,
     *                  DENRES(LOP,NN),NEF,I,NN,LOP,NORES,MM,
     *                  KCODE(LOP,NN)
                        ENDIF
 2700 CONTINUE
C
      IPASS     = IPASS + 1
      DO 2750 K = 1 , NMP
      SIGOLD(K) = SIGT0J(K)
 2750 CONTINUE
C
      IF(IOPT(19).GT.0) THEN
      WRITE(NOUT2,2003) (MATD(J),SIGT0J(J),J=1,NMP)
                        ENDIF
C
      IF(IOPT(19).GT.1) THEN
                        WRITE(NOUT2,4002) NAMRES(LOP)
                        DO 2900 I = 1 , NMP
                        SUM = 0.0
                            DO 2800 J = 1 , NMP
                            SUM = SUM + GAMMA(I,J)
 2800                       CONTINUE
                        WRITE(NOUT2,4003) I,SUM
                        WRITE(NOUT2,4006) (J,GAMMA(I,J),J=1,NMP)
 2900                   CONTINUE
                        ENDIF
C
 4000 CONTINUE
C-----PRINT OUT CALCLULATED DANCOFF FACTOR
      WRITE(NOUT2,4501)
      DO 4800 NN = 1,NMP
      K          = MATD(NN)
      IF(ISWF(K).NE.1) GO TO 4800
      MMK        = NISO(K)
      IF(MMK.LE.0)     GO TO 4800
      WRITE(NOUT2,4502)  K,(MTNAME(I,K),I=1,2)
      WRITE(NOUT2,4503) ((IDENT(J,M,K),J=1,2),M=1,MMK)
      WRITE(NOUT2,4504) (DANCOF(M,K),M=1,MMK)
 4800 CONTINUE
C-----MOVE DANCOF TO DANNEW ARRAY
 4801 CONTINUE
      LISO       = 0
      DO 5000 NN = 1 , NMAT
      MMK        = NISO(NN)
      IF(MMK.LE.0) GO TO 5000
      DO 4850  M = 1 , MMK
      LISO       = LISO + 1
      DANNEW(LISO) = DANCOF(M,NN)
 4850 CONTINUE
 5000 CONTINUE
C
      RETURN
C    *************************************************
C    * ERROR CASE                                    *
C    *************************************************
 9999 CONTINUE
      WRITE(NOUT1,9991) NEF,NGF
      STOP
C
 4501 FORMAT(///1H ,35X,'******************************************',
     +         /1H ,35X,'* CALCULATED NUCLIDE-WISE DANCOFF FACTOR *',
     +         /1H ,35X,'******************************************'/)
 4502 FORMAT(/1H ,30X,'## MATERIAL NAME ----- ',I2,3X,2A4,' ##'/)
 4503 FORMAT(1H ,10X,'IDENTIFICATION OF NUCLIDE ---- ',5(2X,2A4,2X)
     +    /,(1H ,41X,5(2X,2A4,2X)))
 4504 FORMAT(1H ,10X,'NUCLIDE-WISE DANCOFF FACTOR---',5F12.6,
     +    /,(1H ,40X,5F12.6)  )
C
 2001 FORMAT(//1H ,10X,' ## DANCOFF FACTOR CALCULATION (BY COLLISION PRO
     +BABILITY ) ## '/)
 2002 FORMAT(1H ,15X,'REGION NAME -------------------- ',1X,2A4,
     +      /1H ,15X,'DANCOFF FACTOR ----------------- ',E12.5,
     +      /1H ,15X,'MEAN CHORD LENGTH -------------- ',E12.5,
     +      /1H ,15X,'PIJ(K,K) ----------------------- ',E12.5,
     +      /1H ,15X,'ENERGY GROUP NO ---------------- ',I4)
 2003 FORMAT(1H ,15X,'TOTAL X-SECTION FOR PIJ2-ROUTINE :',/,
     +(1H ,20X,5(I3,2X,1PE12.5)))
 4810 FORMAT(////1H ,20X,' ## DOUBLE HETEROGENIETY INFORMATION ## ',
     *//1H ,17X,'MACROSCOPIC FUEL MATERIAL NAME     : ',2A4,'(',I2,')',
     * /1H ,17X,'MICROSCOPIC FUEL MATERIAL NAME     : ',2A4,'(',I2,')',
     * /1H ,17X,'MICROSCOPIC MOD. MATERIAL NAME     : ',2A4,'(',I2,')',
     * /1H ,17X,'VOLUME OF MICROSCOPIC CELL         : ',E12.5,
     * /1H ,17X,'VOLUME OF MICROSCOPIC FUEL         : ',E12.5,
     * /1H ,17X,'VOLUME OF MICROSCOPIC MODERATOR    : ',E12.5,
     * /1H ,17X,'VOLUME FRACTION OF MICROSCOPIC FUEL: ',E12.5,
     * /1H ,17X,'VOLUME FRACTION OF MICROSCOPIC MOD.: ',E12.5,
     * /1H ,17X,'MICROSCOPIC GEOMETRY               : ',I12,
     * /1H ,17X,'MICROSCOPIC MEAN CHORD LENGTH      : ',E12.5,
     * /1H ,17X,'MICROSCOPIC DANCOFF FACTOR         : ',E12.5,
     * /1H ,17X,'MICROSCOPIC GAMMA   FACTOR         : ',E12.5,
     * /1H ,17X,'MICROSCOPIC BELL    FACTOR         : ',E12.5///)
 4820 FORMAT(1H ,' ## SIGWF ## ',1P10E11.4)
 4830 FORMAT(1H ,' ## SIGWM ## ',1P10E11.4)
 4840 FORMAT(1H ,10X,' ## ',I2,'-TH NUCLIDE TOTAL X-SECTION(',2A4,')##')
 4851 FORMAT(1H ,' ## SIGWT ## ',1P10E11.4)
 4001 FORMAT(1H ,15X,'REGION NAME -------------------- ',1X,2A4,
     +      /1H ,15X,'DANCOFF FACTOR ----------------- ',E12.5,
     +      /1H ,15X,'MEAN CHORD LENGTH -------------- ',E12.5,
     +      /1H ,15X,'PIJ(K,K) ----------------------- ',E12.5,
     +      /1H ,15X,'TOTAL X-SEXTION FOR THIS RESON.- ',E12.5,
     +      /1H ,15X,'NUMBER DENSITY  ---------------- ',E12.5,
     +      /1H ,15X,'ENERGY GROUP NO ---------------- ',I4,
     +      /1H ,15X,'REGION NUMBER   ---------------- ',I4,
     +      /1H ,15X,'REGION POSITION NUMBER---------- ',I4,
     +      /1H ,15X,'RESONANT NUMBER ---------------- ',I4,' OF ',I4,
     +      /1H ,15X,'RESONANT POSITON --------------- ',I4,
     +      /1H ,15X,'RESONANT POSITON IN IDENTH ----- ',I4)
 4002 FORMAT(//1H ,20X,' ********************************** ',
     +        /1H ,20X,' *     << GAMMA(I,J) TABLE >>     * ',
     +        /1H ,20X,' ********************************** ',
     +        /1H ,20X,' *  RESONANT NAME = ',A4,'          * ',
     +        /1H ,20X,' ********************************** '/)
 4003 FORMAT(1H ,10X,'<< SOURCE REGION NUMBER = ',I3,
     +               '     (SUM = ',1PE12.5,')  >>')
 4004 FORMAT(//1H ,20X,' ********************************** ',
     +        /1H ,20X,' *  NUCLIDE-WISE DANCOFF FACTOR   * ',
     +        /1H ,20X,' ********************************** ',
     +        /1H ,20X,' *  NUCLIDE NAME = ',A4,'           * ',
     +        /1H ,20X,' ********************************** '/)
 4005 FORMAT(1H ,15X,'TOTAL-0J X-SECTION FOR PIJ2-ROUTINE :',/,
     +(1H ,20X,5(I3,2X,1PE12.5)))
 4006 FORMAT(1H ,' GAMMA(I,J) :',10(I2,F10.6),/,(1H ,12X,10(I2,F10.6)) )
C
  998 FORMAT(/1H ,10X,'ERROR STOP (SUB. MAFSIG) ---- EMPIRICAL FORMULA
     +IS NOT GIVEN , SET IC3.NE.3 AND RERUN ]]]')
  999 FORMAT(/1H ,10X,'WARNING (SUB. MAFSIG) --- NODE = ',2A4,
     +'.MATRIX.TR  IS NOT FOUND ')
 9991 FORMAT(1H1///1H ,10X,'ERROR STOP AT (SUB. MAFSIG)',
     +            /1H ,10X,'NEF(',I3,') IS NOT EQUAL TO LIB. GROUP NO.('
     +             ,I3,') ]] ')
C
      END
