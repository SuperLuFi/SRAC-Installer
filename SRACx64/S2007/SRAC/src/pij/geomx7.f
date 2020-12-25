      SUBROUTINE GEOMX7(RX,RPP,RDP,IM,IP,D,DX,DY,NSRPIN)
C *** GENERAL X-Y TWO DIMENSIONAL LATTICE CELL OF REFLECTIVE CONDITION
C *** EXCLUSIVE USE FOR IGT=09 OCTANT SYMMETRIC & IDIV=0,1
C     AT AXISESC *** X AND Y OUTER BOUND. COND. IS ISO, REFL, AND BLACK
      DIMENSION RX(*),D(*),IM(*),IP(*),DX(*),DY(*),RPP(*)
     *         ,RDP(NDPIN1,NTPIN),NSRPIN(NTPIN+1)
      COMMON /PIJ1C/ NX,NY,NTPIN,NAPIN,IDUM5(4),
     &              NDPIN,IDIVP,BETM,NX1,NY1
     &             ,IDUM14( 3),IXP,IYP,IZP,NDPIN1,IDUM21(2),LL,IDUM24(6)
     &             ,RO,ANG,IDUM32(2),SINB,COSB,IBASE
      COMMON /PIJ2C/ IGT,NZ,IDUM1(8),NTTAB
      EQUIVALENCE (IDUM1(7),IEDPIJ)
      COMMON /MAINC/ DUMY1(63),NOUT1,NOUT2,DUMMY2(435)
      IF(IEDPIJ.GE.3)
     &WRITE(6,*) ' NSRP=',(NSRPIN(I),I=1,NTPIN)
      NX2=(NX*NX1)/2
      NAPIN2=(NAPIN*(NAPIN+1))/2
      IDY=1
C     ANGULAR RANGE IS ASSUMED WITHIN (0,90) DEG
C     IF(SINB.LT.0.)IDY=-1
      IDX=1
C     IF(COSB.LT.0.)IDX=-1
C * * CROSS WITH X=RX(I) 2*NX+1 POINTS IN DESCENDING ORDER INTO DX ARRAY
      DXZ=RO*SINB/COSB
      DYZ=-RO*COSB/SINB
      DO 10 I=1,NX
C     I1 POSITION OF POSITIVE RX(I+1)
      I1=I+NX+1
      I2=NX-I+1
C     I2 POSITION OF NEGATIVE RX(I+1)
      DX(I1)=DXZ-RX(I+1)/COSB
      DX(I2)=DXZ+RX(I+1)/COSB
   10 CONTINUE
      DX(NX1)=DXZ
*     IF(IEDPIJ.GE.3)
*    &WRITE(6,*) '  1DX=',(DX(I),I=1,2*NX+1)
C * * CROSS WITH Y=RX(I) 2*NX+1 POINTS IN DESCENDING ORDER INTO DY ARR
   11 DO 20 I=1,NX
      I1=I+NX+1
      I2=NX-I+1
      DY(I1)=-RX(I+1)/SINB + DYZ
      DY(I2)= RX(I+1)/SINB + DYZ
   20 CONTINUE
      DY(NX1)=DYZ
*     IF(IEDPIJ.GE.3)
*    &WRITE(6,*) '  1DY=',(DY(I),I=1,2*NX+1)
C
C * * ARRANGE DX DY IN DESCENDING ORDER INTO D ARRAY WITH S-REG NUMBER
   21 IX=1
      IY=1
      L=1
      IXB=-NX1
      IYB=-NX1
      IXC=IXB
      IYC=IYB
   35 IJK=1
      IF(DX(IX).LT.DY(IY)) IJK=2
      GO TO (40,50),IJK
   40 D(L)=DX(IX)
      IXB=IXB+1
      IF(IXB.EQ.0) IXB=1
C     IM(L)=(IABS(IYB)-1)*NX+IABS(IXB)
      IM(L)=LOCF(IABS(IXB),IABS(IYB),NX,NX2)
      IP(L)=IM(L-1)
      IX=IX+1
      IF(IX.GT.2*NX+1) GO TO 55
C     Y POSITION STILL REMAIN AT INITIAL
      IF(IYB.EQ.IYC)GOTO 35
      L=L+1
      GO TO 35
   50 D(L)=DY(IY)
      IYB=IYB+1
      IF(IYB.EQ.0) IYB=1
C     IM(L)=(IABS(IYB)-1)*NX+IABS(IXB)
      IM(L)=LOCF(IABS(IXB),IABS(IYB),NX,NX2)
      IP(L)=IM(L-1)
      IY=IY+1
      IF(IY.GT.2*NX+1) GO TO 55
C     X POSITION STILL REMAIN AT INITIAL
      IF(IXB.EQ.IXC) GO TO 35
      L=L+1
      GO TO 35
   55 LL=L
      IF(LL.EQ.1) GO TO 90
*     IF(IEDPIJ.GE.3) THEN
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  2LL=',LL,'IM=',(IM(L),L=1,LL-1)
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  IJK=',IJK,'IP=',(IP(L),L=2,LL)
*     WRITE(6,'(A,10F12.5/(11X,10F12.5))')
*    &'        D(L)=',(D(L),L=1,LL)
*                 ENDIF
      IF(NTPIN.EQ.0) GO TO 90
C
C   SCAN NTPIN RODS IF THE RAY TRACES
C     DO  86 NT=1,NTPIN
      DO  86 NTX=1,NAPIN
      DO  86 NTY=1,NAPIN
      LOCP=LOCF(NTX,NTY,NAPIN,NAPIN2)
      PA=RPP(NTX)
CXR   PA X POSITION OF PIN NAPIN
      PB=RPP(NTY)
CXR   PB Y POSITION OF PIN NAPIN
CXZ   NXR=2
CXR   REPEAT TWICE FOR MIRROR REFLECTIVE GEOMETRY
CXZ   NYR=2
      DO  85 II=1,2
      IZ=II
      IF(ABS(PA).LT.1.E-5)         IZ=0
C     IF(ABS(PA-RX(NX1)).LT.1.E-5) IZ=3
C     IF(ABS(PA+RX(NX1)).LT.1.E-5) IZ=4
      DO  83 JJ=1,2
         JZ=JJ
      IF(ABS(PB).LT.1.E-5)         JZ=0
C     IF(ABS(PB-RX(NX1)).LT.1.E-5) JZ=3
C     IF(ABS(PB+RX(NX1)).LT.1.E-5) JZ=4
         DAB=ABS(PB*COSB-PA*SINB+RO)
         DMID=-PA*COSB-PB*SINB
CXZ
         DXX=DXZ-PA/COSB
         DYY=DYZ-PB/SINB
      DO 81 NDP=NDPIN1,2,-1
         RR=RDP(NDP,LOCP)
         IF(RR.LE.DAB) GO TO  82
         DET=SQRT(RR**2-DAB**2)
         DPA=DMID+DET
         DPB=DMID-DET
CXZ
      IF(DXX.GT.DYY)         THEN
        IF(DXX.GT.DPA)     THEN
          IF(DPB.GT.DYY) THEN
CXZ    PATTERN 2 TO 2
                         ELSE
CXZ    PATTERN 2 TO 4
                         ENDIF
                           ELSE
          IF(DPB.GT.DYY) THEN
CXZ    PATTERN 1 TO 2
                         ELSE
CXZ    PATTERN 1 TO 2 TO 4
            IZN=NDP-1+NSRPIN(LOCP)
            CALL INSERT(DXX,IZN,IZN,DYY,D,IM,IP)
                         ENDIF
                           ENDIF
                               ELSE
        IF(DYY.GT.DPA)     THEN
          IF(DPB.GT.DXX) THEN
CXZ    PATTERN 3 TO 3
                         ELSE
CXZ    PATTERN 3 TO 4
                     ENDIF
                           ELSE
          IF(DPB.GT.DXX) THEN
CXZ    PATTERN 1 TO 3
                         ELSE
CXZ    PATTERN 1 TO 3 TO 4
            IZN=NDP-1+NSRPIN(LOCP)
            CALL INSERT(DYY,IZN,IZN,DXX,D,IM,IP)
                         ENDIF
                           ENDIF
                             ENDIF
CXZ   IZN=IBASE+NDP-1+NDPIN*(NT-1)
   70 IZN=NDP-1+NSRPIN(LOCP)
      CALL INSERT(DPA,IZN,IZN,DPB,D,IM,IP)
*     IF(IEDPIJ.GE.3) THEN
*     WRITE(6,*) '   NTX=',NTX,'   NTY=',NTY
*    &, '  IZN=',IZN,
*    &, ' PA=',PA,' PB=',PB,' PP1=',PP1,' PP2=',PP2
*    &, ' DPA=',DPA,' DPB=',DPB,' DXX=',DXX,' DYY=',DYY
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  4LL=',LL,'IM=',(IM(L),L=1,LL-1)
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  IJK=',IJK,'IP=',(IP(L),L=2,LL)
*     WRITE(6,'(A,10F12.5/(11X,10F12.5))')
*    &'    D(L)=',(D(L),L=1,LL)
*                     ENDIF
   81 CONTINUE
   82 IF(JZ.EQ.0) GO TO 84
CXR   IF ON Y-AXIS
      PB=-PB
   83 CONTINUE
   84 IF(IZ.EQ.0) GO TO 86
CXR   IF ON X-AXIS
      PA=-PA
   85 CONTINUE
   86 CONTINUE
   90 LL=LL-1
      IF(LL.EQ.0) RETURN
      IF(IEDPIJ.GE.3) THEN
      WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
     & '  3LL=',LL,'IM=',(IM(L),L=1,LL)
      WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
     & '  IJK=',IJK,'IP=',(IP(L),L=2,LL+1)
      WRITE(6,'(A,10F12.5/(11X,10F12.5))')
     &'      D(L)=',(D(L),L=1,LL+1)
                      ENDIF
      DO 100 L=1,LL
      D(L)=D(L)-D(L+1)
  100 CONTINUE
      IF(IEDPIJ.GE.3) THEN
      WRITE(6,'(A,10F12.5/(11X,10F12.5))')
     &'     XD(L)=',(D(L),L=1,LL)
                      ENDIF
      IF(LL.GT.NTTAB) GO TO 300
      GO TO (110,120),IJK
  110 IXP=IXP+1
CXY   RO=RO-SIGN(2.*SINB,SINB*COSB)*RX(NX1)
      RO=RO-     2.*SINB           *RX(NX1)
      RETURN
  120 IYP=IYP+1
CXY   RO=RO+SIGN(2.*COSB,SINB*COSB)*TY(NY1)
      RO=RO+     2.*COSB           *RX(NX1)
      RETURN
  300 WRITE(NOUT1,301) LL,NTTAB
      STOP
  301 FORMAT(1H0,9X,15HTABLE OVERFLOW   ,2I10)
      END
