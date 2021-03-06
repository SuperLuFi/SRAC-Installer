C             S807                LEVEL=2        DATE=82.09.10
      SUBROUTINE S807(CRX,Q,MB,MC,XMD,IH1,IGM1)
C   *** S807    MIXES CROSS SECTION
      DIMENSION CRX(IH1,IGM1,1),Q(1),MB(1),MC(1),XMD(1)
      COMMON /SN1C/
     &              D(1),LIM1,LR,LW,LDSN,LMA,LMZ,LMB,LMC,LXMD,LFIX,LFLT,
     &       LJ5,LRM,LDF,LJ3,LJ4,LIGT,LART,LALFT,
     &LFGP,LFGG,LEND,LV,LAA,LWD,LMR,LPNC,
     &ID,ITH,ISCT,ISN,IGE,IBL,IBR,IZM,IM,IEVT,IGM,IHT,IHS,IHM,MS,MCR,MTP
     &,MT,IDFM,IPVT,IQM,IPM,IPP,IIM,ID1,ID2,ID3,ID4,ICM,IDAT1,IDAT2,IFG,
     &IFLU,IFN,IPRT,IXTR,
     &EV,EVM,EPS,BF,DY,DZ,DFM1,XNF,PV,RYF,XLAL,XLAH,EQL,XNPM,
     &T(12),NIN,NOU,NT1,NT2,NT3,NT4,NT5,NT6,NT7
      COMMON /WORK/ Z(1),LIM2,LXKI,LFD,LXN,LVE,LMTT,LCRX,LQ,LPA,
     &       LXJ,LCH,LCA,LCF,LCT,LCS,LTAB,
     &LXND,LSA,LSAT,LRAV,LRA,LXNN,LXNE,LXNR,LXNA,LSR,LST,LQG,LFG,LSG,
     &LXKE,LXNI,LXNO,LT3,LT5,LDA,LDB,LDC,LDS,LB,IGMP,IGMM,IIGG,NERR,IMJT
     &,IHG,IMP,MP,NDS,NUS,SDG,SCG,AG,XNLGG,XNLG,SNG,ALA,ASR,EAM,EPG,EQ,
     &E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12,E13,E14,E15,E16,E17,E18,E19
     &,E20,ESC,ESM,EVP,EVPP,FTP,IC,ICVT,IGP,IG,IHP,IIC,IIG,IP,IZP,I01,
     &I02,I03,I04,I05,I06,I07,I08,I09,I00,JT,LC,MG,MI,ML,MM,NFN,XITR,
     &XLAP,XLAPP,XLAR,XLA,XNIO,XNII,ZZ1,ZZ2,ZZ3,XNB,XKEP,XKIP,IH,I,K,L,
     &M,J,N,NN,ISV
      IQM1 = 0
      IF (IQM.GT.0) IQM1 = 1
      I09=IQM1*IM + IPM*MM
      I08=1
      IF(IDAT1.NE.0)I08=IGM
      DO 1 IIG=1,I08
      IF(IDAT1.EQ.0)GO TO 2
      READ (NT3)  ((CRX(IH,M,1  ),IH=1,IHP),M=1,ML)
      IF(I09.NE.0) READ (NT3) (Q(I),I=1,I09)
      IF(IIG.EQ.IGM)REWIND NT3
    2 IF(MS.EQ.0)GO TO 8
      DO 3 M=1,MS
      I=MB(M)
      K=MC(M)
      DO 3 IH=1,IHG
      IF(K.NE.0)GO TO 4
      CRX(IH,1,I  )=CRX(IH,1,I  )*XMD(M)
      GO TO 3
    4 IF(I.EQ.K)GO TO 5
      CRX(IH,1,I  )=CRX(IH,1,I  ) + CRX(IH,1,K  )*XMD(M)
      GO TO 3
    5 CRX(IH,1,I  )=CRX(IH,1,I  )*EV
    3 CONTINUE
    8 IF(IDAT1.EQ.0)GO TO 1
      WRITE (NT4) ((CRX(IH,M,1  ),IH=1,IHP),M=1,MT)
      IF(I09.NE.0) WRITE (NT4) (Q(I),I=1,I09)
      IF(IC.NE.0 .OR. IPRT.EQ.1)GO TO 1
      WRITE (NOU,10) IIG
      CALL WOT(CRX,MT,IHP,1,'POS.','MAT.',0)
   10 FORMAT('1 CROSS SECTIONS FOR GROUP ',I3)
    1 CONTINUE
      IF(IDAT1.NE.0)REWIND NT4
    7 ML=MT
      I02=NT3
      NT3=NT4
      NT4=I02
      IF(IDAT1.NE.0 .OR. IC.NE.0 .OR. IPRT.EQ.1)GO TO 6
      WRITE (NOU,20)
   20 FORMAT('1 CROSS SECTIONS')
      CALL WOT(CRX,IGM,IHP,MT,'POS.','GRP.','MAT.')
    6 RETURN
      END
