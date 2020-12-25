      SUBROUTINE PCOCHK(PIJ   ,QQ    ,DANX  ,DANY  ,SIGWW ,BL    ,SIG ,
     +                  RATD  ,SIGMA ,DAN   ,NCOR  ,MAR   ,QIJ ,IDRREG,
     +                  VOLR0 ,MAR0  ,ISKIP )
C***********************************************************************
C    THIS ROUTINE WAS PRODUCED FOR COLLISION PROBABILITY TABLE CHECKING.
C    DUMMY ROUTINE FOR GENERAL USER.
C***********************************************************************
CDEL  PARAMETER ( MXDBHT =   450 )
      INCLUDE  'DBHETINC'
      COMMON /MAINC / NNOPT(500)
C
      COMMON /PCOWK2/ KCOMP,KCOMPF,DELBEF,KSREG,KMAT,KNMAX,KRES,NPROB,
     1                NDOUBL,NOUT1,NOUT2,NBB,NBH,MAXN,MAXP,KDAN,
     3                KPIJ1,KPIJ2,ESCAPA,ESCAPF,GUZAI,IPLOT,MAIN(2)
C
      COMMON /PCODBL/ LCOMP,LSREG,MTREPL,MICFL,MICMOD,IPATH,METHOD,
     +                IGEOM,RF,RM,XLL,VF,VM,VCELL,RHO,GAMMA,LENFLX
CKSK  COMMON /DOUBLE/ LDOUBL(50),ISWRES(450),C
      COMMON /DOUBLE/ LDOUBL(50),ISWRES(MXDBHT),C
C
      EQUIVALENCE  (LINTOT,NNOPT(95))
C
      DIMENSION   PIJ(KSREG,KSREG),QQ(KPIJ1,MAXN,MAXP),DANX(MAXN,KDAN),
     1            DANY(MAXN,KDAN),SIG(KCOMP),SIGWW(KPIJ2,KCOMP),
     2            BL(KCOMP),SIGMA(KCOMP),RATD(KCOMP),IDRREG(KSREG),
     3            DAN(MAXP),NCOR(KCOMP),MAR(KSREG),QIJ(LSREG,LSREG),
     4            VOLR0(LSREG),MAR0(LSREG)
C
      DIMENSION   XL(20)
      DIMENSION   SIGF(20),SIGE(20),PFF(20),PFM(20),PMF(20),PMM(20)
      DIMENSION   ALPHAF(20),ALPHAM(20),SIGFE(20)
C
      EXTERNAL FFFF
C
      DATA PI/ 3.141592 /
C
      FFX(X) = 2.0*(1.0-(1.0+X)*EXP(-X))/X/X
C
      IF(ISKIP.EQ.1)  RETURN
C
      ICON   = 0
      AAA    = RHO**(-0.333333)
      C      = 0.0
C
      SIGF( 1) = 6.64184E-1
      SIGF( 2) = 6.93137E-1
      SIGF( 3) = 3.82700
      SIGF( 4) = 6.21891
      SIGF( 5) = 6.14842E-1
      SIGF( 6) = 6.60349E-1
      SIGF( 7) = 6.96555E-1
      SIGF( 8) = 7.36167E-1
      SIGF( 9) = 7.92291E-1
      SIGF(10) = 8.90080E-1
      SIGF(11) = 1.10285
      SIGF(12) = 1.73348
      SIGF(13) = 5.14306
      SIGF(14) = 495.388
      SIGF(15) = 11.4773
      SIGF(16) = 3.84936
      SIGF(17) = 2.52433
      SIGF(18) = 2.08761
      SIGF(19) = 1.91977
      SIGF(20) = 1.86897
C
      WRITE(6,506) SIGF
C
      DO 1000 MMM=1,20
      MM       = 21 - MMM
      SIGM     = SIG(MICMOD)
      PEP      = PE( IGEOM , SIGF(MM)*RF )
      PFM (MM) = PEP / ( 1.0 + (1.0/GAMMA-1.0)*SIGF(MM)*XLL*PEP )
      PFF (MM) = 1.0 - PFM(MM)
      PMF (MM) = PFM(MM)*VF*SIGF(MM) / VM / SIGM
      PMM (MM) = 1.0 - PMF(MM)
      SIGINF   =(SIGF(MM)*VF + SIGM*VM) / VCELL
      SIGFV    = SIGF(MM)*VF
C
      SIGE(MM) = SIGINF
      VFSF     = SIGFV
      IPASS    = 0
      IF(SIGF(MM).LT.25.0)  IPASS = 1
C
      GO TO (200,300,400,500),METHOD
C
C----METHOD 1
C
  200         CONTINUE
              IF(IPASS.EQ.1)  GO TO600
              ARG = 2.0*RF*(SIGF(MM)-SIGM)
              FFE = 1.0 - FFX(ARG)
              IF(ABS(ARG).LT.0.01)  FFE = 0.66666*ARG
              RP  = 1.5*RF
              C   = 2.0*RP*RHO*PI*RF**2*FFE
C
                       IF(C.GE.0.0)
     *                 CALL UTSD1(C,1.0,FFFF,C/10000.,XXXX,ICON)
CKSK *                 CALL TSD1(C,1.0,FFFF,C/10000.,XXXX,ICON)
C
                       IF(C.LT.0.0)
     *                 CALL UTSD1(C,0.0,FFFF,ABS(C/10000.),XXXX,ICON)
CKSK *                 CALL TSD1(C,0.0,FFFF,ABS(C/10000.),XXXX,ICON)
C
                       IF(ABS(C).LT.0.001)  XXXX = C
C
                       SIGE(MM) = XXXX/2.0/RP + SIGM
C                      VFSF     = SIGE(MM)*VCELL - SIGM*VM
                       VFSF     = XXXX/2.0/RP*VCELL + SIGM*VF
              WRITE(6,507) ICON,C,VFSF
              IF(VFSF.LT.0.0)   SIGE(MM) = SIGINF
              IF(VFSF.LT.0.0)   VFSF     = SIGFV
              GO TO 600
C
C----METHOD 2
C
  300         CONTINUE
              VFSF   = VM*SIGM*PMF(MM)/PMM(MM)
              SIGE(MM) = (VFSF + SIGM*VM) / VCELL
              GO TO 600
C
C----METHOD 3
C
  400         CONTINUE
              SHILDS = (1.0/GAMMA - 1.0)*XLL*SIGM*VM/VF*PEP
              VFSF   = SIGF(MM)*SHILDS*VF
              SIGE(MM) = (VFSF + SIGM*VM) / VCELL
              GO TO 600
C
C----METHOD 4
C
  500         CONTINUE
              IF(IPASS.EQ.1)  GO TO 600
              ARG = 2.0*RF*(SIGF(MM)-SIGM)
              FFE = 1.0 - FFX(ARG)
              IF(ABS(ARG).LT.0.01)  FFE = 0.66666*ARG
              CI  = AAA*RHO*PI*RF**2*FFE
              XI  = -ALOG(1.0-CI)
              IF(ABS(CI).LT.0.001)  XI = CI
              SIGE(MM) = XI/AAA + SIGM
C             VFSF      = SIGE(MM)*VCELL - SIGM*VM
              VFSF      = XI/AAA*VCELL + SIGM*VF
              WRITE(6,507) ICON,CI,XI,VFSF
              IF(VFSF.LT.0.0)   SIGE(MM) = SIGINF
              IF(VFSF.LT.0.0)   VFSF     = SIGFV
              GO TO 600
C
  600 CONTINUE
      SAVEDA = SIGE(MM)
      IF(VFSF.GT.SIGFV)  SIGE(MM) = SIGINF
      IF(VFSF.GT.SIGFV)  VFSF     = SIGFV
C
      ALPHAF(MM) = VFSF/SIGE(MM)/VCELL
      ALPHAM(MM) = 1.0 - ALPHAF(MM)
      SIGFE (MM) = VFSF/VF
      RATIOR     = ALPHAF(MM)/ALPHAM(MM)
      SHIELD     = VFSF/SIGFV
C
           WRITE(6,501) MM,SIGF(MM),SIGM,PEP,SAVEDA,SIGINF
           WRITE(6,502) SIGFE(MM),PFM(MM),PFF(MM),PMF(MM),PMM(MM)
           WRITE(6,503) SIGE(MM),ALPHAF(MM),ALPHAM(MM),RATIOR
           WRITE(6,508) METHOD,VFSF,SIGFV,SHIELD
 1000 CONTINUE
C
  501 FORMAT(1H0,' ## NG SIGF SIGM PEP SIGE SIGINF ## ',I3,1P6E12.5)
  502 FORMAT(1H ,' ## SIGFE PFM PFF PMF PM         ## ',1P5E12.5)
  503 FORMAT(1H ,' ##SIGE ALPHAF ALPHAM RATIO(F/M) ## ',1P4E12.5)
  504 FORMAT(1H ,' ## I       ## ',I6)
  505 FORMAT(1H ,' ## PIJ     ## ',1P10E12.5)
  506 FORMAT(1H1,1H ,20X,' ##  CHECK WRITE AT PCOCHK ## ',
     */1H ,' ## SIGF ## ',1P10E12.5,
     */1H ,' ## SIGF ## ',1P10E12.5/)
  507 FORMAT(1H0,' ## ICON C (XI) VFSF ## ',I6,2X,1P5E12.5)
  508 FORMAT(1H ,' ## METHOD VFSF SIGFV SHIELD ## ',I6,2X,1P3E12.5)
C
C
C
      DO 1100 M=1,20
      SIG(MTREPL)=SIGE(M)
      DO 1010 I=1,LCOMP
      SIGWW(M,I)=SIG(I)
 1010 CONTINUE
 1100 CONTINUE
C
      REWIND LINTOT
      DO 1110 JZ=1,LCOMP
      WRITE(LINTOT) (SIGWW(IZ,JZ),IZ=1,KPIJ2)
 1110 CONTINUE
      REWIND LINTOT
C
      CALL PIJ2(KPIJ2,2)
C
      REWIND 21
      DO 1200 M=1,20
      READ(21) ((QIJ(I,J),I=1,LSREG),J=1,LSREG)
C-----PIJ EXPANDING
      CALL  PCOEXP(LSREG   ,KSREG ,QIJ,PIJ,ISWRES   ,SIGF(M)  ,SIGM  ,
     *             SIGFE(M),VF    ,VM     ,ALPHAF(M),ALPHAM(M),PFF(M),
     *             PFM(M)  ,PMF(M),PMM(M) ,SIG      ,MAR0     ,VOLR0 ,
     *             IDRREG  ,KCOMP )
C
      IJ = 0
      DO 1400 I=1,KSREG
      WRITE(6,504) I
      WRITE(6,505) (PIJ(I,J),J=1,KSREG)
 1400 CONTINUE
 1200 CONTINUE
      REWIND 21
C
C
      RETURN
C
      END
