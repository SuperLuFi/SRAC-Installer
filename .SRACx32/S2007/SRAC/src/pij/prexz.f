C     PREXZ FOR GEOMETRY #16 REFLECTIVE BOUNDARY CONDITION IDIVP=2
      SUBROUTINE PREXZ(RX,RDP,NPTX,NPTY,TY,VMESH,MESHX,MESHP,
     1  IIJJ,NSRPIN,NZONE,IXR,MMR,IRR)
      DIMENSION NZONE(1),RX(1),TY(1),MESHX(NX,NY),IXR(1),
     1        VMESH(1),IRR(1),MMR(1),NPTX(NTPIN)
     2       ,NPTY(NTPIN),RDP(NDPIN1,NTPIN),MESHP(4,NDPIN,NX1,NY1)
     3       ,NSRPIN(NTPIN)
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGD,NDPIN,
     1                 IDIVP,BETM,NX1,NY1,DUM(6),NDPIN1,
     2                 NDR,NDA,LL,L0,RO1,DRO,FVOL,IDUM28(8),NMESH
      COMMON / PIJ2C / IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,ICOUNT,IEDPIJ,
     1                 IFORM,NTTAB,NUTAB,SZ
      COMMON /MAINC/   DUMMY1(63),NOUT1,NOUT2
      COMMON /IGEOM/   DUMMY2( 5),PINR,RANGE,DUMMY3,NPINZ
      DIMENSION IIJJ(4,NTPIN)
      DATA PI/3.1415927/
C
      IJ=0
      DO 100 J=1,NY
      DO 100 I=1,NX
      IJ=IJ+1
      MESHX(I,J)=IJ
      VMESH(IJ )=(TY(J+1)-TY(J))*(RX(I+1)-RX(I))
  100 CONTINUE
      NMESH=IJ
CXZ   IF(NTPIN.EQ.0) GO TO 350
      CALL ICLEA(MESHP,4*NDPIN*NX1*NY1,0)
      CALL ICLEA(IIJJ,4*NTPIN,0)
CXZ   PIN REGION COUNT  NSRPIN
      DO 300 N=1,NTPIN
      NSRPIN(N)=IJ
CXZ   FACT=0.
CXZ   IIJJ(3) IIJJ(4) REGION AROUND PIN N  LEFT UPPER,  RIGHT UPPER
CXZ   IIJJ(1) IIJJ(2 )                     LEFT LOWER,  RIGHT UPPER
      VP=PI*RDP(NDPIN1,N)**2/4.
C
      IF(NPTX(N).GT.1   .AND. NPTY(N).GT.1  ) THEN
CXZ   FACT=FACT+0.25
      IIJJ(1,N)=NPTX(N)-1+NX*(NPTY(N)-2)
CXZ   IF(IDIVP.EQ.0) IIJJ(1)=1
                                VMESH(IIJJ(1,N))=VMESH(IIJJ(1,N))-VP
                                              ENDIF
      IF(NPTX(N).LT.NX1 .AND. NPTY(N).GT.1  ) THEN
CXZ   FACT=FACT+0.25
      IIJJ(2,N)=NPTX(N) +NX*(NPTY(N)-2)
CXZ   IF(IDIVP.EQ.0) IIJJ(2)=1
                                VMESH(IIJJ(2,N))=VMESH(IIJJ(2,N))-VP
                                              ENDIF
      IF(NPTX(N).GT.1   .AND. NPTY(N).LT.NY1) THEN
CXZ   FACT=FACT+0.25
      IIJJ(3,N)=NPTX(N)-1+NX*(NPTY(N)-1)
CXZ   IF(IDIVP.EQ.0) IIJJ(3)=1
                                VMESH(IIJJ(3,N))=VMESH(IIJJ(3,N))-VP
                                              ENDIF
      IF(NPTX(N).LT.NX1 .AND. NPTY(N).LT.NY1) THEN
CXZ   FACT=FACT+0.25
      IIJJ(4,N)=NPTX(N) +NX*(NPTY(N)-1)
CXZ   IF(IDIVP.EQ.0) IIJJ(4)=1
                                VMESH(IIJJ(4,N))=VMESH(IIJJ(4,N))-VP
                                              ENDIF
      RR=0.
      DO 210 NP=1,NDPIN
      RP=RDP(NP+1,N)**2
      DO 200 I=1,4
      IF(IIJJ(I,N).NE.0) THEN
      IJ=IJ+1
      VMESH(IJ)=PI*(RP-RR)*0.25
                       ENDIF
  200 MESHP(I,NP,NPTX(N),NPTY(N))=IJ
  210 RR=RP
  300 CONTINUE
      NSRPIN(NTPIN+1)=IJ
  350 IF(NZ.NE.IJ) THEN
                   WRITE(NOUT1,9100) NZ,IJ
                   STOP
                   ENDIF
C
CXX   SEVERAL QUANTITIES FOR NUMERICAL INTEGRATION
               NPINZ=0
CXZ   SCAN WHICH IS THE PIN LOCATED AT THE CENTER
       DO 400 N=1,NTPIN
      IF(NPTX(N).EQ.1 .AND. NPTY(N).EQ.1) THEN
               PINR=RDP(NDPIN1,N)
               NPINZ=1
                                          ENDIF
  400 CONTINUE
      NDR=NGR*((NX+NY)/2)
      IF(NDR     .GT. 2000) WRITE(NOUT1,9101)
CXY   RANGEGE : HALF OF DIAGONAL
      RANGE =SQRT(RX(NX1)**2+TY(NY1)**2)
C     DRO =RANGE/FLOAT(NDR)
      RO1= RANGE
CXY   FVOL=1.0 NO CHANGE IN ANALYTICAL VOL FROM #13 TO #16
      FVOL=0.25
CXY   SZ=2.*(RX(NX1)+TY(NY1))
      SZ=    RX(NX1)+TY(NY1)
       IF(BETM.GT.PI/2.)THEN
      WRITE(6,*) ' ** ANGULAR INTEGRATION RANGE IS MODIFIED TO 90 DEG '
       BETM=PI/2.
                        ENDIF
C
C     DO 80 NT=1,NTPIN
C     RPP(NT)=RX(NPTX(NT))
C     THETA(NT)=TY(NPTY(NT))
   80 CONTINUE
C
C
C * * PRINT MESH NUMBER AND REGION NUMBER
CXZ   IF(NTPIN.EQ.0) CALL IPRTX('S-RE','GION',NX,NY,MESHX)
                     CALL IPRTXZ('S-REGION',MESHX,MESHP)
      IF(NR.EQ.NZ) GOTO 140
      IJ=0
      DO 135 J=1,NY
      DO 135 I=1,NX
      IJ=IJ+1
      IT=NZONE(IJ)
      MESHX(I,J) = IT
  135 CONTINUE
CXZ   IF(NTPIN.EQ.0) GO TO 140
      IJ=NMESH
      DO 136 NT=1,NTPIN
      DO 136 ND=1,NDPIN
      DO 136 I=1,4
      IF(IIJJ(I,NT).NE.0) THEN
      IJ=IJ+1
      IT=NZONE(IJ)
      MESHP(I,ND,NPTX(NT),NPTY(NT))=IT
                          ENDIF
  136 CONTINUE
  140 CONTINUE
C     IF(NTPIN.EQ.0) CALL IPRTX('S-RE','GION',NX,NY,MESHX)
                     CALL IPRTXZ('T-REGION',MESHX,MESHP)
C
      IF(NRR.EQ.NR) GOTO 151
      IJ=0
      DO 145 J=1,NY
      DO 145 I=1,NX
      IJ=IJ+1
      IT=NZONE(IJ)
      MESHX(I,J) = IRR(IT)
CXZ   IF(NTPIN.EQ.0 .OR. IDIVP.NE.0)  IJ=IJ+1
  145 CONTINUE
CXZ   IF(NTPIN.EQ.0) GO TO 150
      IJ=NMESH
      DO 146 NT=1,NTPIN
      DO 146 ND=1,NDPIN
      DO 146 I=1,4
      IF(IIJJ(I,NT).NE.0) THEN
      IJ=IJ+1
      IT=NZONE(IJ)
      MESHP(I,ND,NPTX(NT),NPTY(NT))=IRR(IT)
                          ENDIF
  146 CONTINUE
  150 CONTINUE
CXZ   IF(NTPIN.EQ.0) CALL IPRTX('R-RE','GION',NX,NY,MESHX)
                     CALL IPRTXZ('R-REGION',MESHX,MESHP)
  151 IF(NXR.LT.2 .OR. NXR.EQ.NRR) GOTO 171
      IJ=0
      DO 155 J=1,NY
      DO 155 I=1,NX
      IJ=IJ+1
      IT=NZONE(IJ)
      MESHX(I,J) = IXR(IRR(IT))
CXZ   IF(NTPIN.EQ.0 .OR. IDIVP.NE.0)  IJ=IJ+1
  155 CONTINUE
CXZ   IF(NTPIN.EQ.0) GO TO 170
      IJ=NMESH
      DO 160 NT=1,NTPIN
      DO 160 ND=1,NDPIN
      DO 160 I=1,4
      IF(IIJJ(I,NT).NE.0) THEN
      IJ=IJ+1
      IT=NZONE(IJ)
      MESHP(I,ND,NPTX(NT),NPTY(NT))=IXR(IRR(IT))
                          ENDIF
  160 CONTINUE
  170 CONTINUE
CXZ   IF(NTPIN.EQ.0) CALL IPRTX('X-RE','GION',NX,NY,MESHX)
                     CALL IPRTXZ('X-REGION',MESHX,MESHP)
  171 IF(NM.EQ.NRR) GO TO 9999
      IF(NM.EQ.  1) GO TO 9999
      IJ=0
      DO 180 J=1,NY
      DO 180 I=1,NX
      IJ=IJ+1
      IREG=NZONE(IJ)
      MESHX(I,J) = MMR(IRR(IREG))
CXZ   IF(NTPIN.EQ.0 .OR. IDIVP.NE.0)  IJ=IJ+1
  180 CONTINUE
CXZ   IF(NTPIN.EQ.0) GO TO 195
      IJ=NMESH
      DO 190 NT=1,NTPIN
      DO 190 ND=1,NDPIN
      DO 190 I=1,4
      IF(IIJJ(I,NT).NE.0) THEN
      IJ=IJ+1
      IT=NZONE(IJ)
      IR=IRR(IT)
      MESHP(I,ND,NPTX(NT),NPTY(NT))=MMR(IR)
                         ENDIF
  190 CONTINUE
      CALL IPRTXZ('M-REGION',MESHX,MESHP)
 9999 RETURN
 9100 FORMAT(' **** UNMATCH IN TOTAL S-REGION NUMBER ***',2I10)
 9101 FORMAT(30H**** TIME CONSUMING CASE       )
      END
