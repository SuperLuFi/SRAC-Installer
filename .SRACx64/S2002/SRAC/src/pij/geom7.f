C**********************  GEOM7 *****************************************
C       **** 7*7 BUNDLE CLUSTERED ASSEMBLY
C**********************  GEOM7 *****************************************
      SUBROUTINE GEOM7(D,IM,IP,RX,RPP,RDP,IPX,MESHP,MESHX,NCUT,DPP,DPM,
     1                 DX,DY,IXA,IYA,X)
      DIMENSION RX(*),RDP(*),RPP(*),IPX(*),D(*),IM(*),IP(*)
     1         ,NCUT(*),DPP(*),DPM(*),MESHP(*),MESHX(*)
     2         ,DX(*),DY(*),IXA(*),IYA(*),X(*)
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGD,NDPIN,
     1               IDIVP,BETM,NX1,NY1,I14,I15,I16,IXP,IYP,I19,NDPIN1,
     2                 NDR,NDA,LL,L0,RO1,DRO,FVOL,NA,NA1,RO,BETA,
     3                 NNR,IAXIS,CS,CC,NMESH
      COMMON / PIJ2C / IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,ICOUNT,IEDPIJ,
     1                 IFORM,NTTAB,NUTAB,SZ
      COMMON /MAINC/ DUMMY1(63),NOUT1,NOUT2,DUMMY2(435)
C     CS=SIN(BETA)
C     CC=COS(BETA)
C * * CROSS X=RX(I)  2*NX+1 POINTS
      DXZ=RO*CC/CS
      DYZ=-RO*CS/CC
      DO 10 I=1,NX
      I1=NX+I+IAXIS
      I2=NX1-I
      DX(I1)=DXZ-RX(I+1)/CS
      DX(I2)=DXZ+RX(I+1)/CS
   10 CONTINUE
      IF(IAXIS.EQ.0) GO TO 11
      DX(NX1)=DXZ
C * * CROSS Y=RX(I)  2*NX+1 POINTS
   11 DO 20 I=1,NX
      I1=NX1-I
      I2=NX+I+IAXIS
      DY(I1)= RX(I+1)/CC + DYZ
      DY(I2)=-RX(I+1)/CC + DYZ
   20 CONTINUE
      IF(IAXIS.EQ.0) GO TO 21
      DY(NX1)=DYZ
C * * ARRANGE DX DY IN ASSENDING ORDER INTO D
   21 IX=1
      IY=1
      L=1
      IXB=-NX1
      IYB= NX1
   35 IJK=1
      IF(DX(IX).LT.DY(IY)) IJK=2
      GO TO (40,50),IJK
   40 D(L)=DX(IX)
      IXB=IXB+1
      IF(IXB.EQ.0) IXB=2-IAXIS
      IXA(L)=IXB
      IYA(L)=IYB
      IX=IX+1
      IF(IXB .GT. NX) GO TO 60
      L=L+1
      GO TO 35
   50 D(L)=DY(IY)
      IYB=IYB-1
      IF(IYB.EQ.0) IYB=IAXIS-2
      IYA(L)=IYB
      IXA(L)=IXB
      IY=IY+1
      IF(IYB+NX .LT.0) GO TO 60
      L=L+1
      GO TO 35
C * * CUT OFF TABLE
   60 L1=0
      DO 70 J=1,L
      L1=L1+1
      IF(IXA(J)+NX .GE. 0 .AND. IYA(J)-NX .LE. 0) GO TO 75
   70 CONTINUE
   75 LL=L-L1
C
      IF(LL.EQ.0) RETURN
C     IF(NAPIN.EQ.0) GO TO 91
      IF(IDIVP.NE.0) GO TO 120
C * * SEARCH CROSSING PIN ROD
C
      NP1=0
      DO 90 J=L1,L
      NCUT(J)=0
      IX=IABS(IXA(J))
      IY=IABS(IYA(J))
      MESHX(J) = LOCF(IX,IY,NX,NNR)
      IF(NAPIN.EQ.0) GO TO 90
      NPX = IPX(IX)
      NPY = IPX(IY)
      IF(NPX.EQ.0 .OR.NPY.EQ.0) GO TO 90
      PA= SIGN(RPP(NPX),FLOAT(IXA(J)) )
      PB= SIGN(RPP(NPY),FLOAT(IYA(J)))
      DAB= PA*CC+PB*CS-RO
      DMID=PB*CC-PA*CS
      NDP=NDPIN1
   85 RR=RDP(NDP)
      IF( RR .LE. ABS(DAB) ) GO TO 90
      NP1=NP1+1
      DET=SQRT(RR*RR-DAB*DAB)
      NCUT(J)=NCUT(J)+1
      DPP(NP1)=DMID+DET
      DPM(NP1)=DMID-DET
      MESHP(NP1)=(LOCF(NPX,NPY,NAPIN,NTPIN)-1)*NDPIN + NDP -1 + NNR
      NDP=NDP-1
      IF(NDP .GE. 2) GO TO 85
   90 CONTINUE
C
C * * ARRANGE PATH TABLE  D,IM,IP FROMM D,MESHX,NCUT,DPP,DPM,MESHP
C
   91 IJ=0
      NP1=0
      DO 100 J=L1,L
      IJ=IJ+1
      X(IJ)=D(J)
      NM1=MESHX(J)
      IM(IJ)=NM1
      IF(NAPIN.EQ.0) GO TO 100
      NC=NCUT(J)
      IF(NC.EQ.0) GO TO 100
   92 IJ=IJ+1
      NP1=NP1+1
      X(IJ)=DPP(NP1)
      NMP=MESHP(NP1)
      IM(IJ)=NMP
      NC=NC-1
      IF(NC.EQ.0) GO TO 95
      GO TO  92
   95 IJP=IJ
      NC=NCUT(J)
   96 IJ=IJ+1
      NC=NC-1
      X(IJ)=DPM(NP1)
      NP1=NP1-1
      IJP=IJP-1
      IM(IJ)=IM(IJP)
      IF(NC.GT.0) GO TO 96
      NP1=NP1+NCUT(J)
  100 CONTINUE
      LL=IJ
      GO TO 200
C
  120 IJ=0
      IMM=0
      DO 130 J=L1,L
      IJ=IJ+1
      IX=IABS(IXA(J))
      IY=IABS(IYA(J))
      X(IJ)=D(J)
      IM(IJ)=LOCF(IX,IY,NX,NNR)
      IP(IJ)=IMM
      IMM=IM(IJ)
  130 CONTINUE
C
      LL=LL+1
      NPY= NAPIN+1
      DO 150 NYY=1,NA
      NPY=NPY-1
      IF(NPY.EQ.0) NPY= -1-NA1
      NPPY=IABS(NPY)
      NPX=-NAPIN-1
      DO 140 NXX=1,NA
      NPX=NPX+1
      IF(NPX.EQ.0) NPX=  1+NA1
      NPPX=IABS(NPX)
      PA= SIGN(RPP(NPPX),FLOAT(NPX) )
      PB= SIGN(RPP(NPPY),FLOAT(NPY) )
      DAB= PA*CC+PB*CS-RO
      DMID=PB*CC-PA*CS
      NDP=NDPIN1
  135 RR=RDP(NDP)
      IF( RR .LE. ABS(DAB)) GO TO 140
      DET = SQRT(RR**2-DAB**2)
      DPA =DMID+DET
      DPB =DMID-DET
      IZP= (LOCF(NPPX,NPPY,NAPIN,NTPIN)-1)*NDPIN+NDP-1+NNR
      CALL INSET7(DPA,IZP,IZP,DPB,   X,IM,IP)
C     IF(MOD(ICOUNT,10).EQ.0)
C    *WRITE(6,950) ICOUNT,LL,(X(L),IP(L),IM(L),L=1,LL)
      NDP=NDP-1
      IF(NDP.GE.2) GO TO 135
  140 CONTINUE
  150 CONTINUE
C
  200 LL=LL-1
      IF(LL.EQ.0) RETURN
      XP=X(1)
      DO 250 L=1,LL
      D(L)=XP-X(L+1)
      XP=X(L+1)
  250 CONTINUE
C
      IF(LL.GT.NTTAB) GO TO 300
  950 FORMAT(1H0,2I5/(10X,E12.5,2I10))
      GO TO (260,270),IJK
  260 IXP=IXP+1
      RO=RO-2.*RX(NX1)*CC
      RETURN
  270 IYP=IYP+1
      RO=2.*RX(NX1)*CS+RO
      RETURN
  300 WRITE(NOUT1,201) LL,NTTAB
      STOP
  201 FORMAT('0        TABLE OVERFLOW',2I10)
      END
