      SUBROUTINE PCOAVG
     1 (VOLM  ,VOLR  ,MAR   ,MTNAME,BL    ,TEMP  ,NISO  ,IDENT ,IRES  ,
     2  DEN   ,FLUXS ,NCOR  ,ISWF  ,MCODE ,NCODEL,AMU   ,SIGF  ,SIGNU ,
     3  SIGS  ,SIGA  ,VCAP  ,SSFEFC,IRCONT,ISWFIS,BETA  ,UMAX  ,ADEN  ,
     4  SIG   ,RATD  ,SUM   ,PHI   ,S     ,SA    ,SF    ,SS    ,PIJ   ,
     5  PHIRR ,PHIXR ,RIAR  ,RIFR  ,RISR  ,XSECAB,XSECFI,XSECSS,XSECFN,
     6  SIGMA ,C     ,X     ,NX    ,FLUX  ,SCATH ,FLUXSS,QQ    ,DANX  ,
     7  DANY  ,DAN   ,SIGWW ,SAA   ,SFF   ,SSS   ,SUMA  ,SUMF  ,SUMS  ,
     8  RIAS  ,RIFS  ,RISS  ,QIJ   ,IDRREG,VOLR0 ,MAR0  ,RATIO ,SIGS0 ,
     9  NGFLX ,RIER  ,XSECER,RIES  ,RFLUX  )
C
      REAL*8  BETA,UMAX,PHI,S,C,FLUX,SCATH,TMP1,TMP3,U,UIGP,UFGP,
     +        UEXP,DTMP,SAVE,RATIO8,BUFGP,BUIGP
      REAL*8  DSAVE,TMP4,TMP5,TMP6,TMP7,TMP8,TMP9,UMINUS,UPLUS,UHALF
      REAL*8  UBEF,TMP2,UMID,UDOWN
C
      REAL*8          EBOUND,UBGP,UUFGP,UUIGP
      CHARACTER*4     NFILE,IDTMP
C
      COMMON /PCOWK2/ KCOMP,KCOMPF,DELBEF,KSREG,KMAT,KNMAX,KRES,NPROB,
     1                NDOUBL,NOUT1,NOUT2,NBB,NBH,MAXN,MAXP,KDAN,
     3                KPIJ1,KPIJ2,ESCAPA,ESCAPF,GUZAI,IPLOT,MAIN(2)
C
      COMMON /UMC001/ LIBTYP,NEF,ISTART,NG,NOMESH,KFGP,NGMAX,MAXINT,NSET
      COMMON /UMC002/ ENERGY(75),EE(47),NI(46),INTNO(46),
     +                NXG(10),NFI(10),NOIG(10),MST(46),MEND(46)
      COMMON /UMC003/ EBOUND(11),UUIGP(10),UUFGP(10),UBGP(46)
      COMMON /UMC004/ INTBL(2000),ENGD(2000)
      COMMON /UMCTMP/ NTEMP,TMPSET(40),IDTMP(40)
C
      COMMON /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,TEMPPP
C
      COMMON /PCODBL/ LCOMP,LSREG,MTREPL,MICFL,MICMOD,IPATH,METHOD,
     +                IGEOM,RF,RM,XLL,VF,VM,VCELL,RHO,GAMMA,LENFLX
C
      COMMON /MAINC / NNOPT(500)
C
      EQUIVALENCE  (LINTOT,NNOPT(95))
C
C     FLUX CALCULATION  USING RECCURSION FORMULA
C
      DIMENSION
     1  VOLM(KCOMP) ,VOLR(KSREG) ,MAR(KSREG)    ,MTNAME(2,KCOMP) ,
     2  BL(KCOMP)   ,TEMP(KCOMP) ,NISO(KCOMP)   ,IDENT(2,KNMAX,KCOMP) ,
     3  IRES(KNMAX,KCOMP)        ,DEN(KNMAX,KCOMP) ,
     4  FLUXS(NGMAX,KSREG)       ,NCOR(KCOMP)   ,ISWF(KCOMP) ,
     5  MCODE(KNMAX,KCOMP)       ,AMU(KMAT)     ,
     6  SIGF(KMAT,NG)            ,SIGNU(KMAT,NG),
     6  SIGS(KMAT,NG)            ,SIGA (KMAT,NG),
     7  VCAP(KMAT)               ,SIGS0(KMAT)   ,
     8  ISWFIS(KRES),BETA(KMAT)  ,UMAX(KMAT)    ,ADEN(KMAT)   ,
     9  SIG(KCOMP)  ,RATD(KCOMP) ,SUM(KCOMP)    ,PHI(KSREG)   ,
     A  S(KSREG)    ,SA(KMAT,KCOMP)             ,SF(KMAT,KCOMP) ,
     B  SS(KMAT,KCOMP)           ,PIJ(KSREG,KSREG)
      DIMENSION
     1  PHIRR(KSREG,NG) ,PHIXR(KCOMP,NG) ,RIAR(KRES,KCOMP,NG),
     2  RIFR(KRES,KCOMP,NG)              ,RISR(KRES,KCOMP,NG),
     3  XSECAB(KCOMP,NG),XSECFI(KCOMP,NG),XSECFN(KCOMP,NG)   ,
     4  XSECSS(KCOMP,NG),SIGMA(KCOMP)    ,C(KSREG)           ,
     5  X(KCOMPF)       ,NX(KCOMPF)      ,FLUX(KSREG,KFGP,NBB) ,
     6  SCATH(KSREG,KRES,KFGP,NBH)       ,FLUXSS(KSREG)
      DIMENSION
     1  QQ(KPIJ1,MAXN,MAXP)              ,DANX(MAXN,KDAN)    ,
     2  DANY(MAXN,KDAN) ,DAN(MAXP)       ,SIGWW(KPIJ2,KCOMP) ,
     3  SAA(KRES,MAXINT,KFGP)     ,SFF(KRES,MAXINT,KFGP)     ,
     4  SSS(KRES,MAXINT,KFGP)            ,SUMA(KRES)         ,
     5  SUMF(KRES)      ,SUMS(KRES)      ,RIAS(KMAT,KSREG)   ,
     6  RIFS(KMAT,KSREG),RISS(KMAT,KSREG),QIJ(LSREG,LSREG)
      DIMENSION
     1  IDRREG(KSREG)   ,VOLR0(LSREG)    ,MAR0(LSREG)     ,RATIO(KSREG)
      DIMENSION
     1  NGFLX(NBB),RIER(KRES,KCOMP,NG),XSECER(KCOMP,NG),RIES(KMAT,KSREG)
C
      DIMENSION       RFLUX(KSREG,KFGP,MAXINT)
      DIMENSION       SSFEFC(3,NG,KRES,KCOMP)
      DIMENSION       IRCONT(6,KRES)
      CHARACTER*8     NCODEL(KMAT)
C
C ---- CALCULATION START
C
C     TMP1=DEXP(-UFGP)
C
      IF(NDOUBL.NE.0)   REWIND 32
      REWIND 96
      WRITE(96) NOMESH,KFGP,NG,NGMAX,KSREG,(NOIG(I),I=1,NOMESH),
     +          (UUFGP(I),I=1,NOMESH),(EBOUND(I),I=1,NOMESH+1)
C
      IXG    = 1
      UIGP   = UUIGP(IXG)
      UFGP   = UUFGP(IXG)
      NFII   = KFGP
      TMP1   = DEXP(-UFGP)
      TMP3   = 1.0 /UIGP
      TMP5   = 1.0 /UFGP
      UHALF  = 0.5*UFGP
      U      = 0.
      TMPZAI = 1.0
      IF(GUZAI.NE.0.0) TMPZAI=1.0/GUZAI
      ESCAPA = 0.
      ESCAPF = 0.
      INTL   = 0
      KSAVE  = 1
      LUB    = 0
      LENG1  = KRES*KCOMP*NG
      LENG2  = KCOMP*NG
      LENG3  = NGMAX*KSREG
C
      CALL  CLEA(RIAR  ,LENG1 ,0.0)
      CALL  CLEA(RIFR  ,LENG1 ,0.0)
      CALL  CLEA(RISR  ,LENG1 ,0.0)
      CALL  CLEA(RIER  ,LENG1 ,0.0)
      CALL  CLEA(XSECAB,LENG2 ,0.0)
      CALL  CLEA(XSECFI,LENG2 ,0.0)
      CALL  CLEA(XSECSS,LENG2 ,0.0)
      CALL  CLEA(XSECFN,LENG2 ,0.0)
      CALL  CLEA(XSECER,LENG2 ,0.0)
      CALL  CLEA(FLUXS ,LENG3 ,0.0)
C
      LENG1  = KSREG*NG
      LENG2  = KCOMP*NG
      CALL  CLEA(PHIRR ,LENG1 ,0.0)
      CALL  CLEA(PHIXR ,LENG2 ,0.0)
      LENG1  = KRES*MAXINT*KFGP
      LENG2  = KMAT*KSREG
      LENG3  = KSREG*MAXINT*KFGP
C
      IF(KSREG.EQ.1) GO TO 12
      IF(NPROB.EQ.3) GO TO 12
      IF(NPROB.EQ.4) GO TO 12
C     CALCULATES CILLISION PROBABILTY INTERPOLATION TABLE
      IF(IPATH.NE.2)
     *CALL    PCOPIJ(PIJ   ,QQ    ,DANX  ,DANY  ,SIGWW ,BL    ,SIG   ,
     *               RATD  ,SIGMA ,DAN   ,NCOR  ,MAR )
C
      IF(IPATH.EQ.2)
     *CALL    PCOQIJ(PIJ   ,QQ    ,DANX  ,DANY  ,SIGWW ,BL    ,SIG   ,
     *               RATD  ,SIGMA ,DAN   ,NCOR  ,MAR   ,QIJ   ,IDRREG,
     *               VOLR0 ,MAR0 )
   12 CONTINUE
C ---- SET DD NAME OF PDS-FILE
      IFLSW    = 1
CKSK  NFILE(1) = 4HUMCR
      NFILE(1) = 'UMCR'
CKSK  NFILE(2) = 4HOSS
      NFILE(2) = 'OSS '
      ITCNT    = 0
C
C ------- BROAD GROUP LOOP
C
      DO 100 IBG=1,NG
C --- FINE GROUP CONSTANTS READ
      KINT=INTNO(IBG)
      IOUT= 0
      IF(IBG.LT.NG) THEN
                    IF(INTNO(IBG+1).GT.0) IOUT = 1
                    ELSE
                    IOUT = 1
                    ENDIF
C
      IF(KINT.GT.0) THEN
                    ITCNT = ITCNT + 1
                    KINTK = KINT
                    CALL  CLEA(SAA   ,LENG1 ,0.0)
                    CALL  CLEA(SFF   ,LENG1 ,0.0)
                    CALL  CLEA(SSS   ,LENG1 ,0.0)
                    CALL  CLEA(RFLUX ,LENG3 ,0.0)
                    DO 9000     KRESP   = 1,KRES
                    IPOS                = IRCONT(6,KRESP)
                    NCODEL(KRESP) (5:5) = IDTMP(ITCNT) (1:1)
                    NCODEL(KRESP) (8:8) = IDTMP(IPOS ) (1:1)
                    CALL PCOFIN(NCODEL(KRESP)    , ISWFIS(KRESP) ,
     +                          SAA  ,   SFF     , SSS   ,KINTK  ,
     +                          KFGP ,MAXINT     ,KRESP  ,KRES   )
 9000               CONTINUE
                    ENDIF
C ------------- READ END
      CALL  CLEA(RIAS  ,LENG2 ,0.0)
      CALL  CLEA(RIFS  ,LENG2 ,0.0)
      CALL  CLEA(RISS  ,LENG2 ,0.0)
      CALL  CLEA(RIES  ,LENG2 ,0.0)
      CALL  CLEA(SIG   ,KCOMP ,0.0)
C
      DO 770 J = 1,KCOMP
      MMK      = NISO(J)
      IF(MMK.LE.0)     GO TO 770
      DO 760 I = 1,MMK
      MM       = MCODE(I,J)
      IF(MM.LE.0)                       GO TO 760
      IF(IRES(I,J).EQ.2.OR.MM.LE.KRES)  GO TO 760
      SAVE     = DEN(I,J)*SIGA(MM,IBG)
      IF(ISWF(J).EQ.2.AND.IRES(I,J).EQ.4)  SAVE=0.0
      IF(ISWF(J).EQ.3.AND.IRES(I,J).EQ.4)  SAVE=0.0
      SA(MM,J) = SAVE
      SS(MM,J) = DEN(I,J)*SIGS(MM,IBG)
      SF(MM,J) = DEN(I,J)*SIGF(MM,IBG)
      SIG(J)   = SIG(J) + SA(MM,J) + SS(MM,J) + SF(MM,J)
  760 CONTINUE
  770 CONTINUE
C
      IF(KSREG.LE.1) GO TO 13
C
      IF(NPROB.EQ.3.OR.NPROB.EQ.4) THEN
      IF(IPATH.NE.2)
     *CALL    PCOPIJ(PIJ   ,QQ    ,DANX  ,DANY  ,SIGWW ,BL    ,SIG   ,
     *               RATD  ,SIGMA ,DAN   ,NCOR  ,MAR )
C
      IF(IPATH.EQ.2)
     *CALL    PCOQIJ(PIJ   ,QQ    ,DANX  ,DANY  ,SIGWW ,BL    ,SIG   ,
     *               RATD  ,SIGMA ,DAN   ,NCOR  ,MAR   ,QIJ   ,IDRREG,
     *               VOLR0 ,MAR0 )
                                   ENDIF
C
   13 CONTINUE
      INTF = INTL + 1
      INTL = NI(IBG)
*     WRITE(6,14) IBG,INTF,INTL
C
      IF(INTF.GT.NXG(IXG)) THEN
                           BUFGP  = UFGP
                           BUIGP  = UIGP
                           DUXG   = U
                           IXG    = IXG + 1
                           UIGP   = UUIGP(IXG)
                           UFGP   = UUFGP(IXG)
                           UHALF  = 0.5*UFGP
                           TMP1   = DEXP(-UFGP)
                           TMP3   = 1.0 /UIGP
                           TMP5   = 1.0 /UFGP
                           TMP7   = 1.0 /BUIGP
                           TMP9   = 1.0 /BUFGP
                           KSAVE  = UFGP/BUFGP + 0.01
                           LUB    = 1
*                          WRITE(6,83) IXG,IBGP,KSAVE,U,UIGP,UFGP
                           ENDIF
C
   14 FORMAT(1H ,' ## IGB INTF INTL ## ',3I10)
   83 FORMAT(1H ,' ## IXG IBGP KSAVE U UIGP UFGP ## ',3I6,1P3E12.5)
   84 FORMAT(1H ,' ## K ISWPOS ISW ISWSSF ## ',4I6)
C
      ISWSSF   = 0
      DO 88  K = 1 , KRES
      ISWPOS   = IRCONT(5,K)
      ISW      = (ISWPOS+NFII-1)/NFII
      IF(ISW.GE.INTF.AND.ISW.LE.INTL) ISWSSF = 1
*     WRITE(6,84) K,ISWPOS,ISW,ISWSSF
   88 CONTINUE
C
C     LOOP OF INTERMEDIATE ENERGY GROUP
C
      DO 200 IBGP= INTF,INTL
      MU         = (IBGP-1)/NBB
      NU         = IBGP-NBB*MU
      MH         = (IBGP-1)/NBH
      NH         = IBGP-NBH*MH
      NGFLX(NU)  = IBG
C
      DO 15    J = 1,KSREG
      FLUXSS(J)  = 0.
      DO 15    L = 1,KFGP
      FLUX(J,L,NU) = 0.
      DO 16     K= 1,KRES
   16 SCATH(J,K,L,NH) = 0.
   15 CONTINUE
C-------------------- RESET CAPTURE CROSS SECTION FOR 1/V NUCLIDE
                     TMP     = SQRT(0.0256/ENGD(IBGP))
                     DO 82 J = 1,KCOMP
                     IF(ISWF(J).NE.2.AND.ISWF(J).NE.3)  GO TO 82
                     MMK     = NISO(J)
                     IF(MMK.LE.0)      GO TO 82
                     SIG(J)  = 0.0
                     DO 81 I = 1,MMK
                     MM      = MCODE(I,J)
                     IF(MM.LE.0)   GO TO 81
                     IF(IRES(I,J).EQ.4)  SA(MM,J)=VCAP(MM)*TMP*DEN(I,J)
                     SIG(J)  = SIG(J)+SA(MM,J)+SF(MM,J)+SS(MM,J)
   81                CONTINUE
   82                CONTINUE
C
      UEXP    = DEXP(-U)
      UBEF    = U
      ISEQ    = NFII*IBGP
C
C-----LOOP OF FINE ENERGY GROUP
C
      DO 300 IFGP=1,NFII
      ISEQ    = ISEQ + 1
      SUMPHV  = 0.
C
      DO 17 J = 1,KRES
      SUMA(J) = 0.
      SUMF(J) = 0.
      SUMS(J) = 0.
   17 CONTINUE
C
      U       = U    + UFGP
      UEXP    = UEXP*TMP1
      UMID    = U    - UHALF
      TMP2    = UMID - UBEF
C
      DO 199 I=1,KSREG
      K    = MAR(I)
      MMK  = NISO(K)
      IF(MMK.LE.0)   GO TO 199
C
      DO 99 J=1,MMK
      MK   = MCODE(J,K)
      IF(MK.LE.0)          GO TO 99
      IF(DEN(J,K).LE.0.0)  GO TO 99
      S(I) = S(I) + SS(MK,K)*BETA(MK)*PHI(I)
      IF(UMAX(MK).GT.19.0) GO TO 99
C
      DTMP = UMID - UMAX(MK)
      TMP4 = DTMP
C
      IF(DTMP.LE.0.0) THEN
      DSAVE  = DEXP(DTMP-UHALF)*UFGP*DEN(J,K)*VOLR(I)*BETA(MK)*SIGS0(MK)
      RATIO8 = RATIO(I)
      S(I)   = S(I) - DSAVE*RATIO8
                      GO TO 99
                      ENDIF
C
      IF(DTMP.GE.UFGP) GO TO 98
C
      DSAVE = UFGP-DTMP
      DSAVE = DEXP(-DSAVE*0.5)*DSAVE*DEN(J,K)*VOLR(I)*BETA(MK)*SIGS0(MK)
      RATIO8= RATIO(I)
      S(I)  = S(I) - DSAVE*RATIO8
C
      IF(KSAVE.EQ.1) THEN
                     IF(MK.LE.KRES) THEN
                     DSAVE = SCATH(I,MK,1,1)
                                    ELSE
                     DSAVE = FLUX(I,1,1)*BETA(MK)*DEN(J,K)*SIGS(MK,1)
                                    ENDIF
                     S(I)  = S(I) - DSAVE*DTMP*TMP5
                     ENDIF
C
      IF(KSAVE.GT.1) THEN
                     LSAVE = DTMP*TMP9 + 1
                     DSAVE = 0.0
C
                        IF(MK.LE.KRES) THEN
                        DO 30 LOP = 1 , LSAVE
                        TMP4  = 1.0
                        IF(LOP.EQ.LSAVE)
     +                  TMP4  = (DTMP-DFLOAT(LOP-1)*BUFGP)*TMP9
                        DSAVE = DSAVE + SCATH(I,MK,LOP,1)*TMP4
   30                   CONTINUE
                        S(I)  = S(I) - DSAVE
C
                                       ELSE
                        DO 35 LOP = 1 , LSAVE
                        TMP4  = 1.0
                        IF(LOP.EQ.LSAVE)
     +                  TMP4  = (DTMP-DFLOAT(LOP-1)*BUFGP)*TMP9
                        DSAVE = DSAVE + FLUX(I,LOP,1)*TMP4
   35                   CONTINUE
                        S(I)  = S(I) -DSAVE*BETA(MK)*DEN(J,K)*SIGS(MK,1)
                                       ENDIF
                     ENDIF
      GO TO 99
C
   98 IPASS  = 0
      IF(IXG.GT.1) THEN
                   DSAVE = DUXG  - DTMP
                   IF(DSAVE.GT.-UFGP) IPASS = 2
                   IF(DSAVE.GE.  0.0) IPASS = 1
                   ENDIF
C ------------------- SAME LETAHRGY WIDTH ENERGY RANGE
      IF(IPASS.EQ.0) THEN
                     MX1   = 0
                     IF(IXG.GT.1) THEN
                                  MX1  = NXG(IXG-1)
                                  TMP4 = TMP4 - DUXG
                                  ENDIF
                     MX2   = TMP4*TMP3
                     MX    = MX1 + MX2
                     TMP6  = TMP4 - DFLOAT(MX2)*UIGP
                     LX    = TMP6*TMP5
                     TMP8  = TMP6 - DFLOAT(LX)*UFGP
                     UPLUS = (UFGP-TMP8)*TMP5
                     UMINUS= TMP8*TMP5
                     ISAVE = 2
                     ENDIF
C ------------------- PREVIOUS LETHARGY WIDTH ENERGY RANGE
      IF(IPASS.EQ.1) THEN
                     MX    = TMP4*TMP7
                     TMP6  = TMP4 - DFLOAT(MX)*BUIGP
                     LX    = TMP6*TMP9
                     TMP8  = TMP6 - DFLOAT(LX)*BUFGP
                     TMP6  = UFGP - TMP8
                     IF(LUB.EQ.1) TMP6 = 0.5*(UFGP+BUFGP) - TMP8
                     LSAVE = TMP6*TMP9
                     UPLUS = (TMP6-DFLOAT(LSAVE)*BUFGP)*TMP9
                     UMINUS= TMP8*TMP9
                     ISAVE = LSAVE + 2
                     ENDIF
C ------------------- ACROSS TWO ENERGY RANGE
      IF(IPASS.EQ.2) THEN
                     MX    = NXG(IXG-1)
                     TMP6  = TMP4 - DUXG
                     LX    = TMP6*TMP5
                     TMP8  = TMP6 - DFLOAT(LX)*UFGP
                     UMINUS= TMP8*TMP5
                     UPLUS = UFGP - TMP8
                     LSAVE = UPLUS*TMP9
                     UPLUS = (UPLUS-DFLOAT(LSAVE)*BUFGP)*TMP9
                     ISAVE = 2 + LSAVE
                     ENDIF
C-----SET LM
      LM   = MX + 1
      LX   = LX + 1
C-----RESONANT TERM
      IF(MK.LE.KRES) THEN
                     MM = (LM-1)/NBH
                     M  =  LM - NBH*MM
                     DSAVE = SCATH(I,MK,LX,M)*UMINUS
                     LX = LX - 1
                     IF(LX.LE.0) THEN
                                 LX = NFII
                                 M  = M - 1
                                 IF(M.LE.0) M = NBH
                                 ENDIF
                     DO 50 LOP = 2 , ISAVE
                     TMP4      = 1.0
                     IF(LOP.EQ.ISAVE) TMP4 = UPLUS
                     DSAVE     = DSAVE + SCATH(I,MK,LX,M)*TMP4
                     IF(LOP.EQ.ISAVE) GO TO 50
                     LX        = LX    - 1
                     IF(LX.LE.0) THEN
                                 LX = NFII
                                 M  = M - 1
                                 IF(M.LE.0) M = NBH
                                 ENDIF
   50                CONTINUE
                     S(I)       = S(I) - DSAVE
                     ENDIF
C-----DILUENT TERM
      IF(MK.GT.KRES) THEN
                     MM = (LM-1)/NBB
                     M  =  LM - NBB*MM
                     MG =  NGFLX(M)
                     DSAVE = FLUX(I,LX,M)*UMINUS *
     *                                    BETA(MK)*DEN(J,K)*SIGS(MK,MG)
                     LX = LX - 1
                     IF(LX.LE.0) THEN
                                 LX = NFII
                                 M  = M - 1
                                 IF(M.LE.0) M = NBB
                                 MG =  NGFLX(M)
                                 ENDIF
                     DO 60 LOP = 2 , ISAVE
                     TMP4      = 1.0
                     IF(LOP.EQ.ISAVE) TMP4 = UPLUS
                     DSAVE     = DSAVE + FLUX(I,LX,M)*TMP4*
     *                                   BETA(MK)*DEN(J,K)*SIGS(MK,MG)
                     IF(LOP.EQ.ISAVE) GO TO 60
                     LX        = LX    - 1
                     IF(LX.LE.0) THEN
                                 LX = NFII
                                 M  = M - 1
C2004                            IF(M.LE.0) M = NBH
                                 IF(M.LE.0) M = NBB
                                 MG =  NGFLX(M)
                                 ENDIF
   60                CONTINUE
                     S(I)       = S(I) - DSAVE
                     ENDIF
   99 CONTINUE
  199 CONTINUE
C
      MR      = INTBL(IBGP)
      LUB     = 0
C----- RESET SELF-SHEILDING FACTOR FOR RESONANCE NUCLIDE AFTER EULOW
      IF(ISWSSF.EQ.1) THEN
                      DO 223 K = 1 , KRES
                      IPOS     = IRCONT(5,K)
                      IF(IPOS.NE.ISEQ) GO TO 223
                                       DO 222 J = 1 , KCOMP
                                       SSFEFC(1,IBG,K,J) = 1.0
                                       SSFEFC(2,IBG,K,J) = 1.0
                                       SSFEFC(3,IBG,K,J) = 1.0
  222                                  CONTINUE
  223                                  CONTINUE
                      ENDIF
C
      DO 20 J = 1,KCOMP
      L       = NCOR(J)
      IF(L.LE.0.OR.L.GT.KCOMPF) GO TO 20
      IF(ISWF(J).EQ.2)          GO TO 123
      SIG(J)  = 0.
      MMK     = NISO(J)
      IF(MMK.LE.0)              GO TO 20
      DO 23 I = 1,MMK
      MK      = MCODE(I,J)
      IF(MK.LE.0)   GO TO 23
      IF(MK.GT.KRES.OR.IRES(I,J).NE.2) GO TO 22
      SA(MK,J)= SAA(MK,MR,IFGP)*DEN(I,J)*SSFEFC(3,IBG,MK,J)
      SF(MK,J)= SFF(MK,MR,IFGP)*DEN(I,J)*SSFEFC(2,IBG,MK,J)
      SS(MK,J)= SSS(MK,MR,IFGP)*DEN(I,J)*SSFEFC(1,IBG,MK,J)
   22 CONTINUE
      SIG(J)  = SIG(J)+SA(MK,J)+SF(MK,J)+SS(MK,J)
   23 CONTINUE
C
  123 CONTINUE
C
      IF(J.EQ.MAIN(L)) THEN
                       TMP     = 1.0
                       IF(BL(J).NE.0.0) TMP=BL(J)
                       TMP     = TMP*SIG(J)
                       X(L)    = TMP/(TMP+1.)
                       ENDIF
   20 CONTINUE
C ------- FLUX CALCULATION
      IF(KSREG.LE.1) THEN
                     C(1)=S(1)*UFGP
                     ENDIF
C
      IF(KSREG.GT.1) THEN
                     IF(IPATH.NE.2)
     +               CALL PCOINT(PIJ   ,QQ    ,DANX  ,DANY  ,DAN   ,X ,
     +                           NX    ,SIG   ,NCOR  ,MAR   ,VOLR )
                     IF(IPATH.EQ.2)
     +               CALL PCOQIN(PIJ   ,QQ    ,DANX  ,DANY  ,DAN   ,X ,
     +                           NX    ,SIG   ,NCOR  ,MAR   ,VOLR )
C
                     DO 26 I=1,KSREG
                     C(I) = 0.
                     DO 27 J=1,KSREG
   27                C(I) = C(I) + PIJ(J,I)*S(J)
   26                C(I) = UFGP*C(I)
                     ENDIF
C
      SUMPHV    = 0.
C
      DO 29   I = 1,KSREG
      M         = MAR(I)
      PHI(I)    = C(I) / SIG(M)
      FLUX(I,IFGP,NU) = PHI(I)
      PHIEXP    = UEXP * PHI(I)
      FLUXSS(I) = FLUXSS(I)  + PHIEXP
      RFLUX(I,IFGP,MR) = PHIEXP
      MMK       = NISO(M)
      IF(MMK.LE.0)  GO TO 29
C
      DO 25 JJ=1,MMK
      IF(DEN(JJ,M).LE.0.0)      GO TO 25
      J   = MCODE(JJ,M)
      IF(J.LE.0.OR.J.GT.KRES)   GO TO 25
      IF(IRES(JJ,M).NE.2    )   GO TO 25
      SCATH(I,J,IFGP,NH) =  BETA(J)*SS(J,M)*PHI(I)
   25 CONTINUE
C
      DO 28   JJ = 1,MMK
      J          = MCODE(JJ,M)
      IF(J.LE.0) GO TO 28
      RIAS(J,I)  = RIAS(J,I) + PHIEXP*SA(J,M)
      RIFS(J,I)  = RIFS(J,I) + PHIEXP*SF(J,M)
      RISS(J,I)  = RISS(J,I) + PHIEXP*SS(J,M)
C-----CALCULATE ELASTIC REMOVAL CROSS SECTION ADDED BY K.KANEKO 8/1/86
      IF(AMU(J).GT.1.1) THEN
               UDOWN = U + UMAX(J) - UBGP(IBG)
               IF(UDOWN.GT.0.0) THEN
                                DSAVE = DEXP(UDOWN) - 1.0
                                DSAVE =(BETA(J)-1.0)*DBLE(SS(J,M))*DSAVE
                                RIES(J,I) = RIES(J,I) + PHIEXP*DSAVE
                                ENDIF
               ELSE
               UDOWN     = U - UBGP(IBG)
               DSAVE     = DBLE(SS(J,M))*DEXP(UDOWN)
               RIES(J,I) = RIES(J,I) + PHIEXP*DSAVE
               ENDIF
   28 CONTINUE
          TMP    = PHI(I)
          SUMPHV = SUMPHV + TMP
          IF(NDOUBL .EQ.0)  GO TO 29
          IF(ISWF(M).NE.3)  GO TO 29
C
                 DO 73 J = 1,KRES
                 SUMA(J) = SUMA(J)+SA(J,M)*TMP
                 SUMF(J) = SUMF(J)+SF(J,M)*TMP
                 SUMS(J) = SUMS(J)+SS(J,M)*TMP
   73            CONTINUE
   29 CONTINUE
C
C     IF(IBGP.GT.230.AND.IBGP.LE.250)
C    +WRITE(NOUT1,7776) (PHI(I),I=1,KSREG)
C
                IF(NDOUBL.EQ.0) GO TO 300
                MR      = INTBL(IBGP)
                DO 74 J = 1,KRES
                TMP     = ADEN(J)*SUMPHV
                IF(TMP.LE.0.0)  GO TO  74
                SAA(J,MR,IFGP) = SUMA(J)/TMP
                SFF(J,MR,IFGP) = SUMF(J)/TMP
                SSS(J,MR,IFGP) = SUMS(J)/TMP
   74           CONTINUE
  300 CONTINUE
C
C ------ FINE GROUP LOOP END
C
      DO 31      I  = 1,KSREG
      PHIRR(I,IBG)  = PHIRR(I,IBG) + FLUXSS(I)
      M             = MAR(I)
      IF(M.NE.0) PHIXR(M,IBG) = PHIXR(M,IBG) + FLUXSS(I)
      FLUXS(NGMAX+1-IBGP,I)   = FLUXSS(I) / ( UIGP*VOLR(I) )
   31 CONTINUE
C
  200 CONTINUE
C
C ------ INTERMEDIATE GROUP LOOP END
C
      IF(IOUT.EQ.1) THEN
                    IST2    = KFGP*INTL
                    IST1    = KFGP*(INTL-KINTK) + 1
                    WRITE(96) IST1,IST2
          WRITE(96) (((RFLUX(K,I,J),I=1,KFGP),J=1,KINTK),K=1,KSREG)
                    IF(NDOUBL.GT.0)  THEN
                                     WRITE(32) IST1,IST2
                                     DO 33 K = 1,KRES
                           WRITE(32) ((SAA(K,J,I),I=1,KFGP),J=1,KINTK),
     +                               ((SFF(K,J,I),I=1,KFGP),J=1,KINTK),
     +                               ((SSS(K,J,I),I=1,KFGP),J=1,KINTK)
   33                                CONTINUE
                                     ENDIF
                     ENDIF
C
      DO 140 I = 1,KSREG
      M        = MAR(I)
      SAVEA    = 0.0
      SAVEF    = 0.0
      SAVES    = 0.0
      SAVEFN   = 0.0
      SAVER    = 0.0
      DO 130 J =1,KMAT
      ESCAPA   = ESCAPA + RIAS(J,I)*TMPZAI
      ESCAPF   = ESCAPF + RIFS(J,I)*TMPZAI
      SAVEA    = SAVEA  + RIAS(J,I)
      SAVEF    = SAVEF  + RIFS(J,I)
      SAVES    = SAVES  + RISS(J,I)
      SAVER    = SAVER  + RIES(J,I)
      SAVEFN   = SAVEFN + RIFS(J,I)*SIGNU(J,IBG)
  130 CONTINUE
      XSECAB(M,IBG) = XSECAB(M,IBG) + SAVEA
      XSECFI(M,IBG) = XSECFI(M,IBG) + SAVEF
      XSECSS(M,IBG) = XSECSS(M,IBG) + SAVES
      XSECFN(M,IBG) = XSECFN(M,IBG) + SAVEFN
      XSECER(M,IBG) = XSECER(M,IBG) + SAVER
  140 CONTINUE
C
      DO 150 I = 1,KSREG
      M        = MAR(I)
      DO 150 J = 1,KRES
      RIAR(J,M,IBG) = RIAR(J,M,IBG) + RIAS(J,I)
      RIFR(J,M,IBG) = RIFR(J,M,IBG) + RIFS(J,I)
      RISR(J,M,IBG) = RISR(J,M,IBG) + RISS(J,I)
      RIER(J,M,IBG) = RIER(J,M,IBG) + RIES(J,I)
  150 CONTINUE
C
             IF(IPATH.NE.2)  GO TO 100
             DO 160 I = 1,KSREG
             J        = IDRREG(I)
             IF(J.GE.0)  GO TO  160
             M        = MAR0(-J)
             SAVEA    = 0.0
             SAVEF    = 0.0
             SAVES    = 0.0
             SAVEFN   = 0.0
             SAVER    = 0.0
             DO 155 J = 1,KMAT
             SAVEA    = SAVEA  + RIAS(J,I)
             SAVEF    = SAVEF  + RIFS(J,I)
             SAVES    = SAVES  + RISS(J,I)
             SAVEFN   = SAVEFN + RIFS(J,I)*SIGNU(J,IBG)
             SAVER    = SAVER  + RIES(J,I)
C
                IF(J.GT.KRES)  GO TO 155
                RIAR(J,M,IBG) = RIAR(J,M,IBG) + RIAS(J,I)
                RIFR(J,M,IBG) = RIFR(J,M,IBG) + RIFS(J,I)
                RISR(J,M,IBG) = RISR(J,M,IBG) + RISS(J,I)
                RIER(J,M,IBG) = RIER(J,M,IBG) + RIES(J,I)
  155        CONTINUE
             XSECAB(M,IBG) = XSECAB(M,IBG) + SAVEA
             XSECFI(M,IBG) = XSECFI(M,IBG) + SAVEF
             XSECSS(M,IBG) = XSECSS(M,IBG) + SAVES
             XSECFN(M,IBG) = XSECFN(M,IBG) + SAVEFN
             XSECER(M,IBG) = XSECER(M,IBG) + SAVER
             PHIXR(M,IBG)  = PHIXR (M,IBG) + PHIRR(I,IBG)
  160        CONTINUE
C
  100 CONTINUE
C
C ------ BROAD GROUP LOOP END
C
      IF(NDOUBL.NE.0)  REWIND 32
                       REWIND 96
C
      DO 177 I = 1,KCOMP
      MMK      = NISO(I)
      IF(MMK.LE.0) GO TO 177
             DO 175 IBG = 1,NG
             TMP        = PHIXR(I,IBG)
             DO 170 JJ  = 1,MMK
             J          = MCODE(JJ,I)
             IF(J.LE.0)  GO TO 170
             IF(J.GT.KRES.OR.IRES(JJ,I).NE.2)  GO TO 170
             TMP2       = TMP*DEN(JJ,I)
             IF(TMP2.LE.0.)                    GO TO 170
CADD
CMOD               TMP2 = 1.0 / TMP2
                   TMP2 = 1.00000 / TMP
                   TMP3 = 1.00000 / DEN(JJ,I)
                   RIAR(J,I,IBG) = RIAR(J,I,IBG)*TMP2*TMP3
                   RIFR(J,I,IBG) = RIFR(J,I,IBG)*TMP2*TMP3
                   RISR(J,I,IBG) = RISR(J,I,IBG)*TMP2*TMP3
                   RIER(J,I,IBG) = RIER(J,I,IBG)*TMP2*TMP3
  170        CONTINUE
  175        CONTINUE
  177 CONTINUE
C
      DO 180 I   = 1,KCOMP
      DO 180 IBG = 1,NG
      TMP        = PHIXR(I,IBG)
      IF(TMP.LE.0.0)  GO TO 180
      TMP        = 1.0 / TMP
      XSECAB(I,IBG) = XSECAB(I,IBG)*TMP
      XSECFI(I,IBG) = XSECFI(I,IBG)*TMP
      XSECSS(I,IBG) = XSECSS(I,IBG)*TMP
      XSECFN(I,IBG) = XSECFN(I,IBG)*TMP
      XSECER(I,IBG) = XSECER(I,IBG)*TMP
  180 CONTINUE
C
      RETURN
      END
