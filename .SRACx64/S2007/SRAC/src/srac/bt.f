C             BT                  LEVEL=1        DATE=80.09.29
      SUBROUTINE BT
      COMMON /SN1C/
     &              D(1),LIM1,LR,LW,LDSN,LMA,LMZ,LMB,LMC,LXMD,LFIX,LFLT,
     &       LJ5,LRM,LDF,LJ3,LJ4,LIGT,LART,LALFT,
     &LFGP,LFGG,LEND,LV,LAA,LWD,LMR,LPNC,
     &ID,ITH,ISCT,ISN,IGE,IBL,IBR,IZM,IM,IEVT,IGM,IHT,IHS,IHM,MS,MCR,MTP
     &,MT,IDFM,IPVT,IQM,IPM,IPP,IIM,ID1,ID2,ID3,ID4,ICM,IDAT1,IDAT2,IFG,
     &IFLU,IFN,IPRT,IXTR,
     &EV,EVM,EPS,BF,DY,DZ,DFM1,XNF,PV,RYF,XLAL,XLAH,EQL,XNPM,
     &T(3),LCMAC,T1(8),NIN,NOU,NT1,NT2,NT3,NT4,NT5,NT6,NT7
      COMMON /WORK/ Z(1),LIM2,LXKI,LFD,LXN,LVE,LMTT,LCRX,LQ,LPA,
     &                 LXJ,LCH,LCA,LCF,LCT,LCS,LTAB,
     &LXND,LSA,LSAT,LRAV,LRA,LXNN,LXNE,LXNR,LXNA,LSR,LST,LQG,LFG,LSG,
     &LXKE,LXNI,LXNO,LT3,LT5,LDA,LDB,LDC,LDS,LB,IGMP,IGMM,IIGG,NERR,IMJT
     &,IHG,IMP,MP,NDS,NUS,SDG,SCG,AG,XNLGG,XNLG,SNG,ALA,ASR,EAM,EPG,EQ,
     &E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12,E13,E14,E15,E16,E17,E18,E19
     &,E20,ESC,ESM,EVP,EVPP,FTP,IC,ICVT,IGP,IG,IHP,IIC,IIG,IP,IZP,I01,
     &I02,I03,I04,I05,I06,I07,I08,I09,I00,JT,LC,MG,MI,ML,MM,NFN,XITR,
     &XLAP,XLAPP,XLAR,XLA,XNIO,XNII,ZZ1,ZZ2,ZZ3,XNB,XKEP,XKIP,IH,I,K,L,
     &M,J,N,NN,ISV
      DIMENSION LZ(1)
      EQUIVALENCE (Z(1),LZ(1))
      ISAV1=IZM
      ISAV2=IZP
    4 I01=IZP*IGP
C   *** DETERMINE STORAGE FOR BALANCE TABLES
      DO 1 I=20,35
    1 LZ(I)=LZ(I-1) + I01
      CALL ADDR(45,61,DUM,DUM)
      IF(LT3 .LE.LIM2)GO TO 3
      IF(IZM.EQ.1)GO TO 999
      IZM=1
      IZP=2
      GO TO 4
    3 IF(IZM.NE.ISAV1)WRITE (NOU,10)
   10 FORMAT('0SUMMARY TABLE STORAGE INSUFFICIENT   SYSTEM TREATED AS',
     &' ONE ZONE')
      CALL CLEAR(0.0,Z(LSA),LT3-LSA)
      CALL SUMARY(Z(LXKI),Z(LFD),Z(LXN),      Z(LVE),D(LW),D(LDSN),
     &D(LMA),D(LCMAC),Z(LCRX),Z(LQ),Z(LPA),D(LDF),IP     ,Z(LCA),Z(LCF),
     &Z(LCT),Z(LCS),D(LV),D(LAA),Z(LTAB),D(LWD),Z(LXND),       Z(LSA),
     &Z(LSAT),Z(LRAV),Z(LRA),Z(LXNN),Z(LXNE),Z(LXNR),Z(LXNA),Z(LSR),
     &Z(LST),Z(LQG),Z(LFG),Z(LSG),Z(LXKE),Z(LXNI),Z(LXNO),
     &IGP,IM,IHP,IGMM,MP,IMP)
  999 IZM=ISAV1
      IZP=ISAV2
      RETURN
      END
