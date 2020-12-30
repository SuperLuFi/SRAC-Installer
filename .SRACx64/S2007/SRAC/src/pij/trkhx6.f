C *****************************************************************
                 SUBROUTINE TRKHX6
C                  CALLED BY TRACE
C   ***************************************************************
C         PATH TABLE FOR HEXAGONAL CELL OF TRIANGULAR DIVISION
C  *****************************************************************
C         ARGUMENTS
     *(RX,TY,RXPIN,TYPIN,RDPIN,LCPIN,NDPIN,NTPIN,ANG,LL,TT,IM,IP)
C
      COMMON / PIJ1C / NX,NY,IDUM1,NAPIN,NCELL,NM,NGR,NGA,IDUM2,IDIVP,
     1                 BETM,NX1,NY1,WEIGHT,WEIGH2,WEIGH3,IXP,
     2  IYP,IZP,NDPIN1,NDR,NDA,IDUM3,L0,RO1,DRO,FVOL,LOCAL1,LOCAL2,RRR,
     3                 IDUM4,LOCAL3,LOCAL4,SINB,COSB,LOCAL5
      COMMON / PIJ2C /
     1         IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,LCOUNT,IEDPIJ,IFORM,
     2         NTTAB,NUTAB,SZ,ITYPE,NGLAST,LOCAL6(3),
     3         IEDFLX,ITMINN,ITMOUT,ITBG,LCMX,ITDM,IPT,
     4         EPSI,EPSO,EPSG,RELC,OVERX,FACTOR,ICOOD,MM,NO1(6),
     5         LCMMR,LCNREG,LCIRR,LCIXR,LCMAR,LCMAT,LCVOL,
     6         LCVOLR,LCVOLX,LCVOLM,LCMATD,AAA(1200)
      COMMON /IGEOM/
     * NTPINZ, NGRZM ,NGRZP,
     * NGRM,   NGRP  ,PITCH,
     * XLENG  ,NPINRT, NPINZ
C    *******************************************************************
      DIMENSION  TT(1),IM(1),IP(1)
C    *******************************************************************
      DIMENSION AA(1700),II(1700)
      EQUIVALENCE (AA(1),II(1),ID)
      DIMENSION DX(100) ,DY(100) ,DZ(100)
      DIMENSION TT2(100),IM2(100),IP2(100)
      DIMENSION SS(4,6)
      DIMENSION RX(1),TY(1),RXPIN(1),TYPIN(1),RDPIN(NDPIN+1,*),
     *   LCPIN(1)
      DATA PI,CRT3/3.141592,1.7320508/
C     DIMENSION SS(4,6)
      DATA SS/ 1.7320508,1. , 1.  , 0.       ,
     *         0.       ,1. , 0.5 , 0.8660254,
     *        -1.7320508,1. ,-0.5 , 0.8660254,
     *         1.7320508,1. ,-1.0 , 0.       ,
     *         0.       ,1. ,-0.5 ,-0.8660254,
     *        -1.7320508,1. , 0.5 ,-0.8660254/
C*****************************************************************
      NRXZ=NX
      NTYZ=NY
      JRPH=NAPIN
      RHO2=RRR
      SINA=SINB
      COSA=COSB
C********************************
      IER = 0
      LL=0
      CALL TRKCRH(XLENG,HI,HO,JSHIP,JSHOP,ANG,IER)
      IF(IER.NE.0) GOTO 250
C1    ANGI=ANG+PI/3. *  C2    ANGI=ANG *******   C3    ANGI=ANG-PI/3. *
C4    ANGI=ANG+PI/3. *  C5    ANGI=ANG *******   C6    ANGI=ANG-PI/3. *
      ANGI=ANG+FLOAT(2-MOD(JSHIP ,3))*PI/3.
      WEIGH2=WEIGH2*WEIGHT
C1    ANGO=ANG+PI/3.    C2    ANGO=ANG           C3    ANGO=ANG-PI/3.
C4    ANGO=ANG+PI/3.    C5    ANGO=ANG           C6    ANGO=ANG-PI/3.
      ANGO=ANG+FLOAT(2-MOD(JSHOP,3))*PI/3.
      WEIGH3=WEIGH3*WEIGHT
      IF (NRXZ.EQ.JRPH) GO TO 101
C     PREVIOUS TY MODE OF THE INITIAL & FINAL VOLUME REGION
      CALL TRKSM(SS(1,JSHIP),SS(2,JSHIP),SS(3,JSHIP)*XLENG,
     *         SS(4,JSHIP)*XLENG,XLENG,NTYZ,TY,JSMIP,ANG,IER)
      IF(IER.NE.0) GOTO 99
      CALL TRKSM(SS(1,JSHOP),SS(2,JSHOP),SS(3,JSHOP)*XLENG,
     *         SS(4,JSHOP)*XLENG,XLENG,NTYZ,TY,JSMOP,ANG,IER)
      IF(IER.NE.0) GOTO 99
      IRING=NRXZ+1
C     ENTRY & EXIT VOLUME REGIONS
      JRGI=NGRZM+NTYZ*(IRING-NRXZ-1)-(NTYZ-JSMIP)
      JRGO=NGRZM+NTYZ*(IRING-NRXZ-1)-(NTYZ-JSMOP)
      JRGI=MOD(JRGI-1,NGRZM)+1
      JRGO=MOD(JRGO-1,NGRZM)+1
      CALL TRKINS(HI,JRGI,JRGO,HO,TT,IM,IP,LL)
                DO 100 IRING=NRXZ,JRPH+1,-1
C     CURRENT SURFACE NUMBER OF IN-OUT PERIPHERAL REGIONS
      IER70 = 0
      CALL TRKCRH(RX(IRING),HI,HO,JSHIC,JSHOC,ANG,IER70)
      IF(IER70.NE.0) GOTO 70
C     CURRENT TY MODE OF IN-OUT PERIPHERAL REGIONS
      CALL TRKSM(SS(1,JSHIC),SS(2,JSHIC),
     *           SS(3,JSHIC)*RX(IRING),SS(4,JSHIC)*RX(IRING)
     *          ,RX(IRING),NTYZ,TY,JSMIC,ANG,IER)
      IF(IER.NE.0) GOTO 99  
      CALL TRKSM(SS(1,JSHOC),SS(2,JSHOC),
     *           SS(3,JSHOC)*RX(IRING),SS(4,JSHOC)*RX(IRING)
     *          ,RX(IRING),NTYZ,TY,JSMOC,ANG,IER)
      IF(IER.NE.0) GOTO 99  
C     ENTRY & EXIT VOLUME REGIONS
      JRGI=NGRZM+NTYZ*(IRING-NRXZ-1)-(NTYZ-JSMIC)
      JRGO=NGRZM+NTYZ*(IRING-NRXZ-1)-(NTYZ-JSMOC)
      JRGI=MOD(JRGI-1,NGRZM)+1
      JRGO=MOD(JRGO-1,NGRZM)+1
      CALL TRKINS(HI,JRGI,JRGO,HO,TT,IM,IP,LL)
C     WRITE(6,22) JRGI,JRGO, LL,(TT(I),IM(I),I=1,LL)
C     WRITE(6,23) (IP(K),K=2,LL)
C  22 FORMAT(3X,3(I3,2X)/('TT(LL)',F12.5,2X,'IM(LL)',I5))
C  23 FORMAT(('IP(LL)=',I5,2X))
C     WRITE(6,*)'AFT TRKINS2 HI=',HI,'JRGI=',JRGI,'JRGO=',JRGO,
C    *  'LL=',LL,'TT=',TT,'IM=',IM,'IP=',IP
C     IF(IRING.EQ.JRPH+1) GO TO 100
C     IF CROSSED BY TY AZIMUTHAL LINE
       IF(JSHIP.EQ.JSHIC .AND. JSMIP.EQ.JSMIC) THEN
                  GO TO 40
                                               ELSE
            IF(RHO2.GT.0.) THEN
      JCCCC=JSHIC
      IF(JCCCC.GT.JSHIP) JCCCC=JCCCC-6
      J111=NTYZ*(JSHIP-1)+JSMIP-1
      J222=NTYZ*(JCCCC-1)+JSMIC
      IFLAG=-1
                          ELSE
      IF(JSHIP.GT.JSHIC) JSHIP=JSHIP-6
      J111=NTYZ*(JSHIP-1)+JSMIP
      J222=NTYZ*(JSHIC-1)+JSMIC-1
      IFLAG=+1
                          ENDIF
            DO 30 JSM=J111,J222,IFLAG
      HSM=HTY(JSM,NTYZ,TY,NGRZM,NRXZ+1-IRING,IFLAG,IPP,IMM,ANG)
      CALL TRKDVD(IPP,IMM,HSM,TT,IM,IP,LL)
   30 CONTINUE
                                               ENDIF
   40  IF(JSHOP.EQ.JSHOC .AND. JSMOP.EQ.JSMOC) THEN
                  GO TO 90
                                               ELSE
            IF(RHO2.GT.0.) THEN
      IF(JSHOC.LT.JSHOP) JSHOP=JSHOP-6
      J111=NTYZ*(JSHOC-1)+JSMOC-1
      J222=NTYZ*(JSHOP-1)+JSMOP
      IFLAG=-1
                          ELSE
      JCCCC=JSHOC
      IF(JSHOP.LT.JCCCC) JCCCC=JCCCC-6
      J111=NTYZ*(JCCCC-1)+JSMOC
      J222=NTYZ*(JSHOP-1)+JSMOP-1
      IFLAG=+1
                          ENDIF
            DO 60 JSM=J111,J222,IFLAG
      HSM=HTY(JSM,NTYZ,TY,NGRZM,NRXZ+1-IRING,IFLAG,IPP,IMM,ANG)
      CALL TRKDVD(IPP,IMM,HSM,TT,IM,IP,LL)
   60 CONTINUE
                                               ENDIF
                  GO TO 90
C     IF NOT FURTHER CROSSED BY THE INNER HEXAGON
   70 CONTINUE
            IF(RHO2.GT.0.) THEN
      IF(JSHOP.GT.JSHIP) JSHOP=JSHOP-6
      J111=NTYZ*(JSHIP-1)+JSMIP-1
      J222=NTYZ*(JSHOP-1)+JSMOP
      IFLAG=-1
                          ELSE
      IF(JSHIP.GT.JSHOP) JSHIP=JSHIP-6
      J111=NTYZ*(JSHIP-1)+JSMIP
      J222=NTYZ*(JSHOP-1)+JSMOP-1
      IFLAG=+1
                   ENDIF
            DO 80 JSM=J111,J222,IFLAG
      HSM=HTY(JSM,NTYZ,TY,NGRZM,NRXZ+1-IRING,IFLAG,IPP,IMM,ANG)
      CALL TRKDVD(IPP,IMM,HSM,TT,IM,IP,LL)
   80 CONTINUE
C     WRITE(6,404) IP(LL)
      GO TO 180
C     **************************************
   90 JSHIP=JSHIC
      JSHOP=JSHOC
      JSMIP=JSMIC
      JSMOP=JSMOC
  100 CONTINUE
C    ***************************************
C     WRITE(6,401) LL,(IM(L),TT(L),L=1,LL)
  401 FORMAT(7X,    'TRKHX6 PHERI-- LL=',I3,
C    1              ' IN-S=',I3,' OUT-S=',I3,
C    1 ' IN-SMODE=',I3,' OUT-SMODE=',I3,' IN-AMODE=',I3,
C    1 ' OUT-AMODE=',I3/
     2   /(3X,' IM(L)=',I3,' TT(L)=',F7.4
     2   ,2X,' IM(L)=',I3,' TT(L)=',F7.4
     2   ,2X,' IM(L)=',I3,' TT(L)=',F7.4
     2   ,2X,' IM(L)=',I3,' TT(L)=',F7.4
     2   ,2X,' IM(L)=',I3,' TT(L)=',F7.4))
C     WRITE(6,404)(IP(L),L=2,LL)
  404 FORMAT(2X,' IP(L)=',I3)
C           INTERSECTION IN TRIANGULAR MESH
  101 CONTINUE
      LL1=LL
      IDY=1
      IF(ANG.GT.PI/3.)IDY=-1
      IDX=1
      IF(ANG.GT.2.*PI/3.)IDX=-1
      IDZ=1
C     IF(ANG.GT.0. .AND. ANG.LT.PI) ALWAYS IDZ=1
C * CROSS WITH X-MESH 2*JRPH+1 POINTS IN ASCENDING ORDER INTO DX ARRAY
      DO 110 I=1,2*JRPH+1
      I1=I
      IF(IDX.EQ.-1) I1=2*JRPH+2-I
      A=CRT3
      B=1.0
      C=CRT3*RX(JRPH+1)*FLOAT(JRPH+1-I1)/FLOAT(JRPH)
      CALL TRKCRS(A,B,C,DX(I),ANG)
  110 CONTINUE
C * * CROSS WITH Y-MESH 2*JRPH+1 POINTS IN ASCENDING ORDER INTO DY ARRAY
      DO 115 I=1,2*JRPH+1
      I1=I
      IF(IDY.EQ.-1) I1=2*JRPH+2-I
      A=-CRT3
      B=1.0
      C=CRT3*RX(JRPH+1)*FLOAT(I1-JRPH-1)/FLOAT(JRPH)
      CALL TRKCRS(A,B,C,DY(I),ANG)
  115 CONTINUE
C * * CROSS WITH Z-MESH 2*JRPH+1 POINTS IN ASCENDING ORDER INTO DY ARRAY
      DO 120 I=1,2*JRPH+1
      I1=I
      IF(IDZ.EQ.-1) I1=2*JRPH+2-I
      A=0.
      B=1.
      C=CRT3*RX(JRPH+1)*FLOAT(JRPH+1-I1)/FLOAT(2*JRPH)
      CALL TRKCRS(A,B,C,DZ(I),ANG)
  120 CONTINUE
      GO TO 121
   99 WRITE(6,*) ' DX(I)=',(DX(I),I=1,2*JRPH+1)
      WRITE(6,*) ' DY(I)=',(DY(I),I=1,2*JRPH+1)
      WRITE(6,*) ' DZ(I)=',(DZ(I),I=1,2*JRPH+1)
      WRITE(6,*) '  L=',L,' IX=',IX,' IY=',IY,' IZ=',IZ
      STOP
C **********************************************************************
C  ARRANGE DX DY DZ IN ASCENDING ORDER INTO D ARRAY WITH S-REG NUMBER
C     POSITION ON ARRAY IX, IY, IZ
  121 CONTINUE
      IX=1
      IY=1
      IZ=1
C     TABLE POSITION L
      L=1
C     ACTUAL MESH NUMBER IXB, IYB, IZB
      IXB=0
      IF(IDX.EQ.-1) IXB=2*JRPH+1
      IYB=0
      IF(IDY.EQ.-1) IYB=2*JRPH+1
      IZB=0
      IF(IDZ.EQ.-1) IZB=2*JRPH+1
C     INITIAL VALUE OF IXB, IYB, IZB = IXC, IYC, IZC
      IXC=IXB
      IYC=IYB
      IZC=IZB
  135 IF(DX(IX).EQ.MIN(DX(IX),DY(IY),DZ(IZ))) GO TO 140
      IF(DY(IY).EQ.MIN(DX(IX),DY(IY),DZ(IZ))) GO TO 150
      IF(DZ(IZ).EQ.MIN(DX(IX),DY(IY),DZ(IZ))) GO TO 160
      WRITE(6,*) ' **** MIN OF DX,DY,DZ NOT FOUND'
      STOP
  140 TT2(L)=DX(IX)
      IXB=IXB+IDX
C     IM2(L)=IXB**2-2*(IYB-1)-MOD(IXB+IYB+IZB+1,2)
      CALL MODE6(JRPH,NGRZM,IXB,IYB,IZB,IM2(L))
      IM2(L)=MOD(IM2(L)-1,NGRZM)+1
      IP2(L)=IM2(L-1)
      IX=IX+1
      IF(IX.GT.2*JRPH+1) GO TO 170
      IF(IYB.EQ.IYC) GO TO 135
      IF(IZB.EQ.IZC) GO TO 135
      L=L+1
      GO TO 135
  150 TT2(L)=DY(IY)
      IYB=IYB+IDY
C     IM2(L)=IXB**2-2*(IYB-1)-MOD(IXB+IYB+IZB+1,2)
      CALL MODE6(JRPH,NGRZM,IXB,IYB,IZB,IM2(L))
      IM2(L)=MOD(IM2(L)-1,NGRZM)+1
      IP2(L)=IM2(L-1)
      IY=IY+1
      IF(IY.GT.2*JRPH+1) GO TO 170
      IF(IXB.EQ.IXC) GO TO 135
      IF(IZB.EQ.IZC) GO TO 135
      L=L+1
      GO TO 135
  160 TT2(L)=DZ(IZ)
      IZB=IZB+IDZ
C     IM2(L)=IXB**2-2*(IYB-1)-MOD(IXB+IYB+IZB+1,2)
      CALL MODE6(JRPH,NGRZM,IXB,IYB,IZB,IM2(L))
      IM2(L)=MOD(IM2(L)-1,NGRZM)+1
      IP2(L)=IM2(L-1)
      IZ=IZ+1
      IF(IZ.GT.2*JRPH+1) GO TO 170
      IF(IXB.EQ.IXC) GO TO 135
      IF(IYB.EQ.IYC) GO TO 135
      L=L+1
      GO TO 135
  170 LL2=L
C
C     WRITE(6,402)       LL2,(IM2(I),TT2(I),I=1,LL2)
  402 FORMAT(3X,'HX6TRI - LL2=',I3/
     2   (2X,' IM2(L)=',I3,' TT2(L)=',F7.4
     2   ,2X,' IM2(L)=',I3,' TT2(L)=',F7.4
     2   ,2X,' IM2(L)=',I3,' TT2(L)=',F7.4
     2   ,2X,' IM2(L)=',I3,' TT2(L)=',F7.4))
C     WRITE(6,403) (IP2(I),I=2,LL2)
  403 FORMAT('   IP2(L)=',I3)
C     COMBINE TWO SETS IF ARRAYS TT,IM,IP
      IF(NRXZ.GT.JRPH) THEN
      IF(LL2.EQ.1) GO TO 180
      CALL COMB(LL1,LL2,TT,TT2,IM,IM2,IP,IP2,IER)
      IF(IER.NE.0) GOTO 399 
      LL=LL1
                       ELSE
C     IF(L.EQ.1) GO TO 190
                       LL=LL2
                    DO 175  I=1,LL
                    TT(I) = TT2(I)
                    IM(I) = IM2(I)
                    IP(I) = IP2(I)
  175               CONTINUE
                       ENDIF
  180 IF(NTPIN.EQ.0) GO TO 190
C     IF(IDIVP.NE.0) GO TO 180
C
C   SCAN NTPIN RODS IF THE RAY TRACES
      DO 186 NP=1,NTPIN
      PA=RXPIN(NP)*COS(TYPIN(NP))
      PB=RXPIN(NP)*SIN(TYPIN(NP))
      DAB=ABS(PB*COSA-PA*SINA-RHO2)
      DMID=PA*COSA+PB*SINA
      NDP=NDPIN+1
  185 RR=RDPIN(NDP,NP)
      IF(RR.LE.DAB) GO TO 186
      DET=SQRT(RR**2-DAB**2)
      DPA=DMID-DET
      DPB=DMID+DET
C     IF(IDPIN(NP).EQ.0) THEN
      IZN=NGRZM+NDP-2+LCPIN(NP)
      CALL TRKFIL(DPA,IZN,DPB,TT,IM,IP,LL)
C     CALL TRKINS(DPA,IZN,IZN,DPB,TT,IM,IP,LL)
C                        ENDIF
      NDP=NDP-1
      IF(NDP.GE.2) GO TO  185
  186 CONTINUE
  190 LL=LL-1
      IF(LL.EQ.0) RETURN
      IERR=0
      DO 200 L=1,LL
C     IF(IM(L).LT.NGRZM)THEN
C        IM(L)=MOD(IM(L)-1,NGRZM)+1
C                      ELSE
C        IM(L)=MOD(IM(L)-1-NGRZM,NGRZP)+1+NGRZM
C                      ENDIF
C     IF(IM(L).EQ.0) IERR=1
      TT(L)=TT(L+1)-TT(L)
  200 CONTINUE
      IF(IERR.EQ.1) GO TO 399
  250 RETURN
  300 WRITE(6,301) LL
      STOP
  301 FORMAT(1H0,9X,'TABLE OVERFLOW BY', I6,' FROM 200')
  399 CONTINUE
C     WRITE(6,404) IP(LL)
      WRITE(6,402)       LL2,(IM2(I),TT2(I),I=1,LL2)
      WRITE(6,403)       IP2(LL2)
      STOP
       END
