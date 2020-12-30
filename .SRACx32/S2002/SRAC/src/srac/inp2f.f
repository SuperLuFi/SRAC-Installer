C             INP2F               LEVEL=6        DATE=85.06.20
C *********************************************************************
C                            INP2F
C *********************************************************************
      SUBROUTINE INP2F(NGMAX,LOC,LSS,LGV,AAA,LGXEC,III,MTNAME,MATD)
      DIMENSION LOC(NGMAX,1),LSS(NGMAX,1),LGV(NGMAX,1),RANGE(3),MATD(1)
      DIMENSION AAA(1000),IDENT(2),III(1),MTNAME(2,1)
      COMMON / PIJ2C / IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,LCOUNT,IEDPIJ,
     &                 IFORM,ID11,ID12,ID13,ID14,
     &                 NGLAST,NGUP,ITAPE,NGKMAX,
     &                 IEDFLX,ITMINN,ITMOUT,ITBG,LCMX,ITDM,JPT,
     &                 EPSI,EPSO,EPSG,RELC,OVERX,FACTOR,ICOOD,
     &                 NMP,NO1(7),
     &                 LCNREG,LCIRR,LCIXR,LCMAR,LCMAT,LCVOL,
     &                 LCVOLR,LCVOLX,LCVOLM,LCMATD,IIAA(950)
      COMMON / MAINC / IOPT(63),NOUT1,NOUT2,
     &                DUM66(8),ISOIN,IFIN,IFOUT,DUM77(21),
     &                IRANGE,ICF,I100,CASENM(2),TITLE(18),DUM3(380)
      EQUIVALENCE (IOPT(55),NEF),(IOPT(57),NERF)
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      CHARACTER *4 IDENT,FILENM,RANGE,MTNAME,CASENM,TITLE,ICF
      DATA RANGE/'FAST','THER','ALL '/
C     IEDFLX=IPTF+2*IPTXEC+4*IPTPIJ+8*IPTS
      IPTXEC=MOD(IEDFLX,4)/2
      NGUP=NGMAX
      NGKMAX=0
      IFLSW=1
      FILENM(1)='MACR'
      FILENM(2)='O   '
      FILENM(3)='    '
      IF(ICF.EQ.'0002') FILENM(2)='OWRK'
C *** CROSS-SECTION ***
      K2=0
      DO 117 NM=1,NMP
      MM=MATD(NM)
      IDENT(1)=MTNAME(1,MM)
      IDENT(2)=MTNAME(2,MM)
      CALL PACKX(IDENT(2),1,RANGE(IRANGE+1),1)
      CALL PACK (IDENT(2),4,ICF)
      IFLAG=0
      CALL SEARCH(IDENT,LENGTH,ISW)
      IF(ISW.EQ.1) THEN
      IDENT(2)(1:1)='A'
      CALL SEARCH(IDENT,LENGTH,ISW)
      IF(ISW.EQ.1) GO TO 1111
      IF(IRANGE.EQ.1) IFLAG=1
                   ENDIF
      CALL READ(IDENT,AAA(K2+1),LENGTH)
                   IF(IFLAG.EQ.1) THEN
                    K2S=K2
                    IREAD=NERF
                    IF(ICF.EQ.'0002') IREAD=NEF
                    DO 100 NG=1,IREAD
                    K2=K2+III(K2+2)+10
  100               CONTINUE
                    IDIFF=K2-K2S
                    J=0
                    DO 105 I=IDIFF+1,LENGTH
                    J=J+1
                    AAA(K2S+J)=AAA(K2S+I)
  105               CONTINUE
                    K2=K2S
                                ENDIF
      DO 117 NG=1,NGMAX
       K1 = K2+1
      LOC(NG,NM)= K1
      LSS(NG,NM)=III(K1)
      LGV(NG,NM)=III(K1+1)
      AAA(K1+1)=AAA(K1+9)
      IF(LSS(NG,NM).GT.1) NGUP=MIN0(NGUP,NG-LSS(NG,NM)+1)
      IF(AAA(K1+6).NE.0.) NGKMAX=MAX0(NGKMAX,NG)
      K1=K1+10
      K2 = K1+LGV(NG,NM)-1
      J=LGV(NG,NM)+NG-LSS(NG,NM)-NGMAX
      AAA(K1-10)=0.0
      IF(J .LE. 0) GO TO 115
      J1=K2-J+1
      T=0.
      DO 114 K=J1,K2
      T=T+AAA(K)
  114 CONTINUE
      AAA(K1-10)=T
      LGV(NG,NM)=LGV(NG,NM)-J
  115 CONTINUE
      IF(LSS(NG,NM).LE.NG) GO TO 116
      AAA(K1-10)=AAA(K1-10)+AAA(K1)
  116 CONTINUE
      AAA(K1-1)=AAA(K1-1)+AAA(K1-10)
  117 CONTINUE
      LGXEC=K2
C
      IF(IPTXEC.LE.0)  GO TO 119
      WRITE(NOUT2,15) CASENM,TITLE
   15 FORMAT(1H1,20A4,20X,'*** CROSS-SECTION ***'//15X,'LSS   LGV',3X,
     &       'SIG.ACT',5X,'SIG.F',4X,'NU*SIG.F',7X,'SIG.T',7X,' X   ',
     &       7X,' D1   ',6X,' D2   ',6X,'SIG.AB',6X,'SIG OUT '//)
      DO 118 NM=1,NMP
      MM=MATD(NM)
      WRITE(NOUT2,16) NM,MM,MTNAME(1,MM),MTNAME(2,MM)
   16 FORMAT('0 ** MATERIAL REGION',I3,'  ** MATERIAL NUMBER',I3,
     &  '** MAT NAME ',2A4,'**')
      DO 118 NG=1,NGMAX
      K=LOC(NG,NM)
      WRITE(NOUT2,17)NG,LSS(NG,NM),LGV(NG,NM),AAA(K+2),AAA(K+3),AAA(K+4)
     &        ,AAA(K+5),AAA(K+6),AAA(K+7),AAA(K+8),AAA(K+1),AAA(K)
C  17 FORMAT('   GROUP ',I3,3X2I5,1P9E12.5)
   17 FORMAT('   GROUP ',I3,3X,2I5,1P,9E12.5)
      K1=K+10
      K2=K1+LGV(NG,NM)-1
      WRITE(NOUT2,18) (AAA(K),K=K1,K2)
   18 FORMAT(25X,1P9E12.5)
  118 CONTINUE
  119 RETURN
 1111 WRITE(NOUT1,1112) IDENT(1),IDENT(2),RANGE(IRANGE+1)
 1112 FORMAT(1H0,'*** MATERIAL *',2A4,'* NOT FOUND IN ',A4,
     &        ' RANGE OF MACRO X-SECTION FILE *** PIJ3 STEP ***')
      STOP
      END
