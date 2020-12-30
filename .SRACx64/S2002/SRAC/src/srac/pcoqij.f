      SUBROUTINE PCOQIJ(PIJ   ,QQ    ,DANX  ,DANY  ,SIGWW ,BL    ,SIG ,
     +                  RATD  ,SIGMA ,DAN   ,NCOR  ,MAR   ,QIJ ,IDRREG,
     +                  VOLR0 ,MAR0  )
C
CDEL  PARAMETER (MAXMAT=100)
CDEL  PARAMETER (MXDBHT=450)
      INCLUDE  'MATDTINC'
      INCLUDE  'DBHETINC'
C
      CHARACTER*4     TIL,ID
C
      COMMON /MAINC / NNOPT(500)
C
      COMMON /PCOWK1/ TIL(18),ID(2)
      COMMON /PCOWK2/ KCOMP,KCOMPF,DELBEF,KSREG,KMAT,KNMAX,KRES,NPROB,
     1                NDOUBL,NOUT1,NOUT2,NBB,NBH,MAXN,MAXP,KDAN,
     3                KPIJ1,KPIJ2,ESCAPA,ESCAPF,GUZAI,IPLOT,MAIN(2)
      COMMON /PCOWK5/ IBURN,KEEPIJ,IPCNT,NXRB,KCOREK(MAXMAT)
C
      COMMON /PCODBL/ LCOMP,LSREG,MTREPL,MICFL,MICMOD,IPATH,METHOD,
     +                IGEOM,RF,RM,XLL,VF,VM,VCELL,RHO,GAMMA,LENFLX,
     +                TABLAG(20)
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
      CHARACTER*8 MEMBER
C
      EXTERNAL FFFF
C
      DATA PI/ 3.141592 /
C
      FFX(X) = 2.0*(1.0-(1.0+X)*EXP(-X))/X/X
C
C     START OF PROCESS
C
      IF(KCOMPF.GT.1) GO TO 999
C
      ICON   = 0
      AAA    = RHO**(-0.333333)
      C      = 0.0
      PRESLD = -1.0
      IPASS  = 0
C
      TABLAG( 1) = 0.00001
      TABLAG( 2) = 0.0001
      TABLAG( 3) = 0.001
      TABLAG( 4) = 0.005
      TABLAG( 5) = 0.01
      TABLAG( 6) = 0.02
      TABLAG( 7) = 0.03
      TABLAG( 8) = 0.04
      TABLAG( 9) = 0.06
      TABLAG(10) = 0.08
      TABLAG(11) = 0.10
      TABLAG(12) = 0.20
      TABLAG(13) = 0.30
      TABLAG(14) = 0.40
      TABLAG(15) = 0.50
      TABLAG(16) = 0.60
      TABLAG(17) = 0.70
      TABLAG(18) = 0.80
      TABLAG(19) = 0.90
      TABLAG(20) = 1.00
C
                         XL( 1) = 1.000010E-5
                         XL( 2) = 1.000100E-4
                         XL( 3) = 1.001001E-3
                         XL(20) = 1.000000E+5
                         DO 10 I = 4 , 19
                         XL( I) = TABLAG(I)/(1.0-TABLAG(I))
   10                    CONTINUE
C
      DO 1000 MMM=1,20
      MM       = 21 - MMM
      SIGF(MM) = XL(MM) / XLL
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
      IF(IPASS.EQ.1)  GO TO600
C
      GO TO (200,300,400,500),METHOD
C
C----METHOD 1
C
  200         CONTINUE
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
C             WRITE(6,507) ICON,C,VFSF
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
              ARG = 2.0*RF*(SIGF(MM)-SIGM)
              FFE = 1.0 - FFX(ARG)
              IF(ABS(ARG).LT.0.01)  FFE = 0.66666*ARG
              CI  = AAA*RHO*PI*RF**2*FFE
              XI  = -ALOG(1.0-CI)
              IF(ABS(CI).LT.0.001)  XI = CI
              SIGE(MM) = XI/AAA + SIGM
C             VFSF      = SIGE(MM)*VCELL - SIGM*VM
              VFSF      = XI/AAA*VCELL + SIGM*VF
C             WRITE(6,507) ICON,CI,XI,VFSF
              IF(VFSF.LT.0.0)   SIGE(MM) = SIGINF
              IF(VFSF.LT.0.0)   VFSF     = SIGFV
              GO TO 600
C
  600 CONTINUE
      SAVEDA = SIGE(MM)
      IF(IPASS.EQ.1)       GO TO 610
      SHIELD = VFSF/SIGFV
      IF(SHIELD.GT.PRESLD) GO TO 610
C
              IPASS    = 1
              SIGE(MM) = SIGINF
              VFSF     = SIGFV
C
  610 IF(VFSF.GT.SIGFV)  SIGE(MM) = SIGINF
      IF(VFSF.GT.SIGFV)  VFSF     = SIGFV
C
      ALPHAF(MM) = VFSF/SIGE(MM)/VCELL
      ALPHAM(MM) = 1.0 - ALPHAF(MM)
      SIGFE (MM) = VFSF/VF
      RATIOR     = ALPHAF(MM)/ALPHAM(MM)
      SHIELD     = VFSF/SIGFV
      PRESLD     = SHIELD
C
C          WRITE(6,501) MM,SIGF(MM),SIGM,PEP,SAVEDA,SIGINF
C          WRITE(6,502) SIGFE(MM),PFM(MM),PFF(MM),PMF(MM),PMM(MM)
C          WRITE(6,503) SIGE(MM),ALPHAF(MM),ALPHAM(MM),RATIOR
C          WRITE(6,508) METHOD,VFSF,SIGFV,SHIELD
 1000 CONTINUE
C
C 501 FORMAT(1H0,' ## NG SIGF SIGM PEP SIGE SIGINF ## ',I3,1P6E12.5)
C 502 FORMAT(1H ,' ## SIGFE PFM PFF PMF PM         ## ',1P5E12.5)
C 503 FORMAT(1H ,' ##SIGE ALPHAF ALPHAM RATIO(F/M) ## ',1P4E12.5)
C 507 FORMAT(1H0,' ## ICON C (XI) VFSF ## ',I6,2X,1P5E12.5)
C 508 FORMAT(1H ,' ## METHOD VFSF SIGFV SHIELD ## ',I6,2X,1P3E12.5)
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
      IF(IBURN .LE.2) GO TO 1155
      IF(IPCNT .LE.1) GO TO 1155
      IF(KEEPIJ.LE.1) GO TO 1155
C
      ISW = MOD(IBURN-1,KEEPIJ)
      WRITE(6,*) ' **  IBURN ISW KEEPIJ : ',IBURN,ISW,KEEPIJ
      IF(ISW.NE.1) THEN
CKSK               MEMBER = 8HPEACOPIJ
                   MEMBER = 'PEACOPIJ'
            CALL   BURNRW ( MEMBER , QQ  , KPIJ1 , MAXN , MAXP ,
     &                      KPIJ1 , MAXN , MAXP  , 'READ')
                   RETURN
                   ENDIF
C
 1155 CONTINUE
C
      REWIND LINTOT
      DO 1110 JZ=1,LCOMP
      WRITE(LINTOT) (SIGWW(IZ,JZ),IZ=1,KPIJ2)
 1110 CONTINUE
      REWIND LINTOT
C
      CALL PIJ2(KPIJ2,2)
      LIN  = 21
C
 1120 CONTINUE
C
      REWIND LIN
      DO 1200 M=1,20
      READ(LIN) ((QIJ(I,J),I=1,LSREG),J=1,LSREG)
C-----PIJ EXPANDING
      CALL  PCOEXP(LSREG   ,KSREG ,QIJ,PIJ,ISWRES   ,SIGF(M)  ,SIGM  ,
     *             SIGFE(M),VF    ,VM     ,ALPHAF(M),ALPHAM(M),PFF(M),
     *             PFM(M)  ,PMF(M),PMM(M) ,SIG      ,MAR0     ,VOLR0 ,
     *             IDRREG  ,KCOMP )
C
              IJ = 0
              DO 1400 I=1,KSREG
              DO 1400 J=1,KSREG
              IJ = IJ + 1
 1400         QQ(M,1,IJ)=PIJ(I,J)
 1200 CONTINUE
      REWIND LIN
C
C
      IF(IBURN.GT.0.AND.KEEPIJ.GT.1) THEN
CKSK                  MEMBER = 8HPEACOPIJ
                      MEMBER = 'PEACOPIJ'
               CALL   BURNRW ( MEMBER , QQ  , KPIJ1 , MAXN , MAXP ,
     &                         KPIJ1 , MAXN , MAXP  , 'OVER' )
                      ENDIF
C
C
   15 CALL CLEA( DAN , MAXP , 0.0 )
C
      IJ      = 0
      DO 17 I = 1,KSREG
      IM      = MAR(I)
      IM      = NCOR(IM)
           DO 16 J= 1,KSREG
           IJ     = IJ + 1
           IF(IM.EQ.1)  DAN(IJ)=(QQ(20,1,IJ)-QQ(19,1,IJ))*XL(19)
   16      CONTINUE
   17 CONTINUE
C
C     DO 2100 N=1,MAXP
C     WRITE(NOUT1,991) N,DAN(N)
C     WRITE(NOUT1,992) (QQ(I,1,N),I=1,KPIJ1)
C2100 CONTINUE
C
      CALL       PCOCHK(PIJ   ,QQ    ,DANX  ,DANY  ,SIGWW ,BL    ,SIG ,
     +                  RATD  ,SIGMA ,DAN   ,NCOR  ,MAR   ,QIJ ,IDRREG,
     +                  VOLR0 ,MAR0  , 1 )
C
      RETURN
C
  999 CONTINUE
      WRITE(NOUT1,993)
      WRITE(NOUT2,993)
      STOP
C
C 991 FORMAT(/1H ,10X,' *** CHECK WRITE FROM PCOPIJ ***(CASE=',I3,')',
C    *'    *DAN** ',1PE12.5)
C 992 FORMAT(1H ,' **QQ** ',1P10E12.5)
C
  993 FORMAT(1H0,'  ERROR STOP ST SUBR(PCOQIJ). KCOMPF MUST BE ONE WHEN
     *DOUBLE HETEROGENEOUS PROBREM IS TREATED.'/)
C
      END
