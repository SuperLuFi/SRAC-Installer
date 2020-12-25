      SUBROUTINE PCODAT
     1 (VOLR  ,MAR   ,BL    ,NISO  ,IRES  ,DN    ,NCOR  ,ISWF  ,MCODE  ,
     2  NCODEL,AMU   ,SIGF  ,SIGS  ,SIGA  ,BETA  ,UMAX  ,ADEN  ,
     3  SIG   ,RATD  ,SUM   ,PHI   ,S     ,SA    ,SF    ,SS    ,
     4  PIJ   ,IDRREG,VOLR0 ,DEN   ,RATIO ,PDSFLX,SIGT  ,SIGS0  )
C
      REAL*8          BETA,UMAX,PHI,S,USAVE
      REAL*8          EBOUND,UBGP,UFGP,UIGP
      CHARACTER*4     TIL,ID,NFILE
      CHARACTER*4     NAMEP(2),IDTEMP
C
      COMMON /PCOWK1/ TIL(18),ID(2)
      COMMON /PCOWK2/ KCOMP,KCOMPF,DELBEF,KSREG,KMAT,KNMAX,KRES,NPROB,
     1                NDOUBL,NOUT1,NOUT2,NBB,NBH,MAXN,MAXP,KDAN,
     3                KPIJ1,KPIJ2,ESCAPA,ESCAPF,GUZAI,IPLOT,MAIN(2)
      COMMON /PCOWK3/ A(14500)
C
      COMMON /UMC001/ LIBTYP,NEF,ISTART,NG,NOMESH,KFGP,NGMAX,MAXINT,NSET
      COMMON /UMC002/ ENERGY(75),EE(47),NI(46),INTNO(46),
     +                NXG(10),NFI(10),NOIG(10),MST(46),MEND(46)
      COMMON /UMC003/ EBOUND(11),UIGP(10),UFGP(10),UBGP(46)
C
      COMMON /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,TEMPPP
      COMMON /MAINC / IOPT(1000)
C
      COMMON /PCODBL/ LCOMP,LSREG,MTREPL,MICFL,MICMOD,IPATH,METHOD,
     +                IGEOM,RF,RM,XLL,VF,VM,VCELL,RHO,GAMMA,LENFLX
      COMMON /TMPSET/ STND(35),IDTEMP(61),NTDUMY
C
      DIMENSION
     1  VOLR(KSREG),NCOR(KCOMP),ISWF(KCOMP),MAR(KSREG),BL(KCOMP),
     2  NISO(KCOMP),IRES(KNMAX,KCOMP),DN(KNMAX,KCOMP),NCODEL(2,KMAT),
     3  AMU(KMAT),SIGF(KMAT,NG),SIGS(KMAT,NG),
     4  SIGA(KMAT,NG),BETA(KMAT),SIGS0(KMAT),
     5  UMAX(KMAT),ADEN(KMAT),SIG(KCOMP),RATD(KCOMP),SUM(KCOMP),
     6  PHI(KSREG),S(KSREG),SA(KMAT,KCOMP),SF(KMAT,KCOMP),
     7  SS(KMAT,KCOMP),PIJ(KSREG,KSREG),MCODE(KNMAX,KCOMP)
      DIMENSION  SIGT(NG,KCOMP)
C
      DIMENSION
     1  IDRREG(KSREG)  ,VOLR0(LSREG),
     2  DEN(KRES,KCOMP),RATIO(KSREG),PDSFLX(LENFLX)
C
      USAVE  = 1.0E+10
      DO  3 N=1,NOMESH
      IF(UIGP(N).LT.USAVE)  USAVE = UIGP(N)
    3 CONTINUE
C
      DO 10 J=1,KMAT
      IF(AMU(J).LE.1.1) THEN
                        UMAX(J)=20.
                        BETA(J)=1.
                        ELSE
                        UMAX(J)=2.*ALOG(1.+2./(AMU(J)-1.))
                        BETA(J)=1./(1.-DEXP(-UMAX(J)))
                        ENDIF
   10 CONTINUE
C
      NBB=UMAX(NBB)/USAVE + 2
      NBH=UMAX(NBH)/USAVE + 2
C
      LENG1=KMAT*KCOMP
C
      CALL   CLEA(SIG   ,KCOMP ,0.0)
      CALL   CLEA(SA    ,LENG1 ,0.0)
      CALL   CLEA(SF    ,LENG1 ,0.0)
      CALL   CLEA(SS    ,LENG1 ,0.0)
      CALL   CLEA(RATD  ,KCOMP ,0.0)
      CALL   CLEA(SIGT  ,NG*KCOMP ,0.0)
C
      DO 111 J=1,KCOMP
      MMK = NISO(J)
      IF(MMK.LE.0)  GO TO 111
      DO 12 I=1,MMK
      M   = MCODE(I,J)
      IF(M.LE.0)   GO TO 12
      IF(IRES(I,J).EQ.2.OR.M.LE.KRES)  GO TO 12
      DO 11 N = 1,NG
      SAVEF   = DN(I,J)*SIGF(M,N)
      SAVES   = DN(I,J)*SIGS(M,N)
      SAVEA   = DN(I,J)*SIGA(M,N)
      IF(IRES(I,J).EQ.4.AND.ISWF(J).EQ.2) SAVEA  = 0.0
      IF(IRES(I,J).EQ.4.AND.ISWF(J).EQ.3) SAVEA  = 0.0
      SIGT(N,J) = SIGT(N,J) + SAVEF + SAVES + SAVEA
   11 CONTINUE
   12 CONTINUE
  111 CONTINUE
C
      DO 15 J = 1,KCOMP
      MMK     = NISO(J)
      IF(MMK.LE.0)  GO TO 15
      DO 14 I = 1,MMK
      M       = MCODE(I,J)
      IF(M.LE.0)   GO TO 14
      SF(M,J) = DN(I,J)*SIGF(M,1)
      SS(M,J) = DN(I,J)*SIGS(M,1)
      SA(M,J) = DN(I,J)*SIGA(M,1)
      IF(IRES(I,J).EQ.4.AND.ISWF(J).EQ.2) SA(M,J) = 0.0
      IF(IRES(I,J).EQ.4.AND.ISWF(J).EQ.3) SA(M,J) = 0.0
   14 CONTINUE
   15 CONTINUE
C
      DO 17 J=1,KCOMP
      IF(ISWF(J).NE.3) THEN
                       SIG(J) = SIGT(NG,J)
                       ELSE
                       SAVE   = 1.0E+20
                       DO 16 N=1,NG
                       IF(SIGT(N,J).LT.SAVE) THEN
                                             NGPOS = N
                                             SAVE  = SIGT(N,J)
                                             ENDIF
   16                  CONTINUE
                       SIG(J) = SIGT(NGPOS,J)
                       ENDIF
   17 CONTINUE
C
CM    WRITE(NOUT2,771)
CM    DO 18 N=1,NG
CM 18 WRITE(NOUT2,772) N,(SIGT(N,J),J=1,KCOMP)
CM    WRITE(NOUT2,773)   (SIG(J),J=1,KCOMP)
CM    WRITE(NOUT2,774)   (SIGS0(J),J=1,KMAT)
CM
CM771 FORMAT(//1H ,20X,' << MATERIAL-WIZE TOTAL X-SECTION >> '/)
CM772 FORMAT(1H ,5X,3HNG=,I2,3X,1P10E11.4)
CM773 FORMAT(1H ,5X,3HSIG,5X,1P10E11.4)
CM774 FORMAT(1H ,' ## SIGS0 ## ',1P10E11.4)
C
      DO 20 I = 1,KCOMPF
      M       = MAIN(I)
      MIN     = 0
      VMIN    = 100000.
      DO 21 J = 1,KCOMP
      SUM(J)  = 1000000.
      IF(NCOR(J).NE.I) GO TO 21
      IF(MIN.EQ.0) MIN=J
      IF(DEN(M,J).GE.0.00001) SUM(J) = SIG(J)/DEN(M,J)
      IF(SUM(J).LT.VMIN) MIN  = J
      IF(SUM(J).LT.VMIN) VMIN = SUM(J)
   21 CONTINUE
C
      MAIN(I)=MIN
             DO 22 J=1,KCOMP
             IF(NCOR(J).NE.I) GO TO 22
             RATD(J)=1.0
             IF(DEN(M,MIN).GE.0.00001) RATD(J)=DEN(M,J)/DEN(M,MIN)
   22        CONTINUE
   20 CONTINUE
C
      IF(KSREG.GT.1) GO TO 1
      VOLR(1) = 1.0
      TVOLR   = 1.0
      MAR(1)  = 1
      BL(1)   = 1.0
      PIJ(1,1)= 1.0
      MAIN(1) = 1
      NCOR(1) = 1
      RATD(1) = 1.0
      ISWF(1) = 3
      GO TO 2
C
    1 CONTINUE
      TVOLR   = 0.0
      DO 33 I = 1,KSREG
      TVOLR   = TVOLR + VOLR(I)
   33 CONTINUE
C
      CALL  CLEA(ADEN  ,KMAT  ,0.0)
C
      DO 44 I = 1,KSREG
      MM      = MAR(I)
      MMK     = NISO(MM)
      IF(MMK.LE.0)     GO TO 44
      DO 88 M = 1,MMK
      J       = MCODE(M,MM)
      IF(J.LE.0)  GO TO 88
      TMP     = DN(M,MM)*VOLR(I)/TVOLR
      ADEN(J) = ADEN(J) + TMP
   88 CONTINUE
   44 CONTINUE
C
    2 CONTINUE
      IFLSW   = 1
CKSK  NFILE(1) = 4HFLUX
      NFILE(1) = 'FLUX'
CKSK  NFILE(2) = 4H
      NFILE(2) = '    '
      NAMEP(1) = ID(1)
CKSK  NAMEP(2) = 4HF002
      NAMEP(2) = 'F002'
      IBURN    = IOPT(79)
      IF(IBURN.GT.0) NAMEP(2) (2:2) = IDTEMP(IBURN) (4:4)
C
CM    WRITE(6,'(9H NAMEP = ,2A4)') NAMEP
C
      CALL CLEA  (PDSFLX ,LENFLX ,1.0 )
      CALL CLEA  (RATIO  ,KSREG  ,1.0 )
      CALL SEARCH(NAMEP(1),LENG,ISW)
C
CM    WRITE(6,'(9H ISW   = ,I6)') ISW
CM    WRITE(6,'(9H LENG  = ,I6)') LENG
CM    WRITE(6,'(9H LENFLX= ,I6)') LENFLX
C
      IF(ISW.EQ.1)       GO TO 101
      IF(LENG.GT.LENFLX) GO TO 101
      CALL READ(NAMEP(1),PDSFLX,LENG)
C
      DELBEF   = LOG(ENERGY(ISTART-1)/ENERGY(ISTART))
*     WRITE(6,'(10H ISTART = ,I12)')   ISTART
*     WRITE(6,'(10H DELBEF = ,E12.5)') DELBEF
C
      IST      = LSREG*(ISTART-2)
      DO  95 I = 1,LSREG
      A(I)     = PDSFLX(IST+I)/VOLR0(I)/DELBEF
   95 CONTINUE
C
      DO 100 I = 1 , KSREG
      ISW      = IABS(IDRREG(I))
      RATIO(I) = A(ISW)
  100 CONTINUE
C
  101 CONTINUE
      GUZAI   = 0.0
      DENOM   = 0.0
      DO 24 I =1,KSREG
      K       = MAR(I)
      MMK     = NISO(K)
      IF(MMK.LE.0)  GO TO 24
      RFLUX   = RATIO(I)
      DO 23 M = 1,MMK
      J       = MCODE(M,K)
      IF(J.LE.0)    GO TO 23
      TMP     = VOLR(I)*DN(M,K)*SIGS(J,NG)*RFLUX
      GUZAI   = GUZAI + TMP*( 1.0 - (BETA(J)-1.0)*UMAX(J) )
      DENOM   = DENOM + TMP
   23 CONTINUE
   24 CONTINUE
C
*     WRITE(NOUT1,25) GUZAI,DENOM,GUZAI/DENOM
      GUZAI   = GUZAI/DENOM
C
   25 FORMAT(1H ,' ## GUZAI DENOM GUZAI/DENOM ## ',1P3E12.5)
C
      DO 27 I = 1,KSREG
      S(I)    = 0.
      PHI(I)  = VOLR(I)*UFGP(1)*RATIO(I)
      K       = MAR(I)
      MMK     = NISO(K)
      IF(MMK.LE.0)  GO TO 27
      DO 26 M = 1,MMK
      J       = MCODE(M,K)
      IF(J.LE.0)  GO TO  26
      S(I)    = S(I) + VOLR(I)*SIGS0(J)*DN(M,K)*RATIO(I)
   26 CONTINUE
   27 CONTINUE
C
      IFLSW   = 1
CKSK  NFILE(1)= 4HUMCR
      NFILE(1)= 'UMCR'
CKSK  NFILE(2)= 4HOSS
      NFILE(2)= 'OSS '
      DELBEF  = RATIO(1)
C
      IF(IPLOT.EQ.0) RETURN
C
      WRITE(NOUT2,600)
      DO 6000 I = 1,KMAT
      TMPZ      = (1.0-2.0/(AMU(I)+1.0))**2
      TGUZAI    =  1.0 - (BETA(I)-1.0)*UMAX(I)
      WRITE(NOUT2,610) I,NCODEL(1,I),NCODEL(2,I),TMPZ,UMAX(I),BETA(I),
     +                   TGUZAI
 6000 CONTINUE
C
      WRITE(NOUT2,620) NBB,NBH
      WRITE(NOUT2,625) (I,RATIO(I),I=1,KSREG)
      WRITE(NOUT2,630) (I,PHI(I),I=1,KSREG)
      WRITE(NOUT2,640) (I,S(I),I=1,KSREG)
      WRITE(NOUT2,645) (I,RATD(I),I=1,KCOMP)
      WRITE(NOUT2,655) (I,MAIN(I),I=1,KCOMPF)
      IF(NDOUBL.EQ.1) WRITE(NOUT2,650) (I,ADEN(I),I=1,KMAT)
C
  600 FORMAT(//1H0,25X,'*** SLOWING DOWN PROPERTY ***'/1H0,9X,
     +'NO.   NUCLIDE       ALPHA      UMAX(I)     1/(1.-ALP.)   GUZAI')
  610 FORMAT(1H ,9X,I2,4X,2A4,2X,2F11.5,2X,F12.4,F12.5)
  620 FORMAT(///1H ,10X,'*** MAXIMUM SLOWING-DOWN I.M. GROUP NO.',
     1 '(NBB,NBH) ***'/1H ,16X,'BY LIGHTEST AND HEAVIEST',
     2 /1H0,12X,2(I9,2H G))
  625 FORMAT(1H0,10X,'*** UPPER FLUX VALUES FOR R-REGION ***'//,
     + (1H ,10X,5(I3,2X,E12.5,3X)/))
  630 FORMAT(1H0,10X,'*** INITAL FLUX   VALUES FOR R-REGION ***'//,
     + (1H ,10X,5(I3,2X,E12.5,3X)/))
  640 FORMAT(1H0,10X,'*** INITAL SOURCE VALUES FOR R-REGION ***'//,
     + (1H ,10X,5(I3,2X,E12.5,3X)/))
  645 FORMAT(1H0,10X,'*** DENSITY RATIO TO MAIN FUEL MATERIAL FOR EACH M
     +ATERIAL *** '//,
     + (1H ,10X,5(I3,2X,E12.5,3X)/))
  655 FORMAT(1H0,10X,'*** MAIN FUEL MATERIAL NUMBER FOR EACH FUEL ***',
     +//,1H ,10X,2(I3,' ---> ',I3,5X))
  650 FORMAT(1H0,10X,'*** HOMOGENIZED ATOMIC DENSITY IN THIS CELL ***',
     +//,(1H ,10X,5(I3,2X,E12.5,3X)/))
C
      RETURN
      END
