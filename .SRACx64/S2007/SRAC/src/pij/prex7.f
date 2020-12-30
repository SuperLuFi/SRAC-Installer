C     PREX7 FOR OCTANT SYMMETRIC GEOMETRY #09  IDIVP=0,1
      SUBROUTINE PREX7(RX,RPP,RDP,NPTX,VMESH,MESHX,
     1  MESHP,NSRPIN,NZONE,IRR,IXR,MMR)
      DIMENSION NZONE(1),RX(1),MESHX(NX,NX),IXR(1),
     1        VMESH(1),IRR(1),MMR(1),NPTX(NAPIN)
     2       ,RPP(NAPIN),RDP(NDPIN1,NTPIN),MESHP(NDPIN,NX1,NX1)
     3       ,NSRPIN(NTPIN+1)
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGD,NDPIN,
     1                 IDIVP,BETM,NX1,NY1,DUM(6),NDPIN1,
     2                 NDR,NDA,LL,L0,RO1,DRO,FVOL,IDUM28(8),NMESH
      COMMON / PIJ2C / IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,ICOUNT,IEDPIJ,
     1                 IFORM,NTTAB,NUTAB,SZ
      COMMON /MAINC/   DUMMY1(63),NOUT1,NOUT2
      COMMON /IGEOM/   DUMMY2( 5),PINR,RANGE,DUMMY3,NPINZ
      DIMENSION IIJJ(4)
      DATA PI/3.1415927/
CXX          MESH NUMBER OF MODERATOR REGION
      NX2=(NX1*NX)/2
      CALL ICLEA(MESHX,NX*NX,0)
      IJ=0
      DO 100 J=1,NX
      DO 100 I=J,NX
      IJ=IJ+1
      MESHX(I,J)=IJ
      VMESH(IJ )=(RX(J+1)-RX(J))*(RX(I+1)-RX(I))
      IF(I.NE.J) VMESH(IJ)=2.*VMESH(IJ)
  100 CONTINUE
      NMESH=IJ
      IF(NTPIN.EQ.0) GO TO 350
      CALL ICLEA(MESHP,NDPIN*NX1*NX1,0)
CXZ   PIN REGION COUNT  NSRPIN
      L=0
CXX   DO 300 N=1,NTPIN
      DO 300 N=1,NAPIN
      FACT2=1.
      DO 300 K=N,NAPIN
      CALL ICLEA(IIJJ,4,0)
      L=L+1
      NSRPIN(L)=IJ
CXZ   FACT=0.
      VP=PI*RDP(NDPIN1,L )**2/4.
      IF(N.NE.K) VP=2.*VP
C
                 IF(IDIVP.EQ.1) THEN
      IF(NPTX(K).GT.1   .AND. NPTX(N).GT.1  ) THEN
      IIJJ(1)=LOCF(NPTX(K)-1,NPTX(N)-1,NX,NMESH)
                  VMESH(IIJJ(1))=VMESH(IIJJ(1))-VP
      IIJJ(1)=1
                                              ENDIF
      IF(NPTX(K).LT.NX1 .AND. NPTX(N).GT.1  ) THEN
      IIJJ(2)=LOCF(NPTX(K)  ,NPTX(N)-1,NX,NMESH)
                  VMESH(IIJJ(2))=VMESH(IIJJ(2))-VP
      IIJJ(2)=1
                                              ENDIF
      IF(NPTX(K).GT.1   .AND. NPTX(N).LT.NX1) THEN
      IIJJ(3)=LOCF(NPTX(K)-1,NPTX(N)  ,NX,NMESH)
                  VMESH(IIJJ(3))=VMESH(IIJJ(3))-VP
      IIJJ(3)=1
                                              ENDIF
      IF(NPTX(K).LT.NX1 .AND. NPTX(N).LT.NX1) THEN
      IIJJ(4)=LOCF(NPTX(K)  ,NPTX(N)  ,NX,NMESH)
                  VMESH(IIJJ(4))=VMESH(IIJJ(4))-VP
      IIJJ(4)=1
                                              ENDIF
                                 ELSE
CX7   CASE FOR IDIVP=0
      IF(RPP(N).EQ.0.)                   THEN
          IF(RPP(K).EQ.0.)            THEN
                  IIJJ(4)=1
                  VMESH(1      )=VMESH(1      )-VP
          ELSEIF(RPP(K).EQ.RX(NX1))   THEN
                  IIJJ(4)=1
                  VMESH(NX     )=VMESH(NX     )-VP
                                      ELSE
                  IIJJ(4)=2
                  VMESH(NPTX(K))=VMESH(NPTX(K))-2.*VP
                                      ENDIF
      ELSEIF(RPP(N).EQ.RX(NX1))          THEN
C           (RPP(K).EQ.RX(NX1))       THEN
                  IIJJ(4)=1
                  VMESH(NMESH  )=VMESH(NMESH  )-VP
C                                     ENDIF
                                         ELSE
C    0< RPP(N) < RX(NX1)
          IF(RPP(K).EQ.RX(NX1))       THEN
                  IIJJ(4)=LOCF(NPTX(K)-1,NPTX(N),NX,NMESH)
                  VMESH(IIJJ(4))=VMESH(IIJJ(4))-2.*VP
                  IIJJ(4)=2
                                      ELSE
                  IIJJ(4)=LOCF(NPTX(K),NPTX(N),NX,NMESH)
                  VMESH(IIJJ(4))=VMESH(IIJJ(4))-4.*VP
                  IIJJ(4)=4
                                      ENDIF
                                         ENDIF
                                        ENDIF
C
      RR=0.
      FACT4=FLOAT(IIJJ(1)+IIJJ(2)+IIJJ(3)+IIJJ(4))
      DO 210 NP=1,NDPIN
      RP=RDP(NP+1,L)**2/4.
CXXR  RP=RDP(NP+1,N)**2/4.
      IJ=IJ+1
      VMESH(IJ)=PI*FACT2*FACT4*(RP-RR)
      MESHP(NP,NPTX(K),NPTX(N))=IJ
  200 CONTINUE
  210 RR=RP
      FACT2=2.
  300 CONTINUE
      NSRPIN(NTPIN+1)=IJ
  350 IF(NZ.NE.IJ) THEN
                   WRITE(NOUT1,9100) NZ,IJ
                   STOP
                   ENDIF
C
CXX   SEVERAL QUANTITIES FOR NUMERICAL INTEGRATION
      IF(NPTX(1) .EQ.1) THEN
      NPINZ=1
      PINR=RDP(NDPIN1,1)
                        ELSE
      NPINZ=0
                        ENDIF
CXY   NDR=NGR*(NX+NY)/2
      NDR=  NGR*NX
      IF(NDR     .GT. 2000) WRITE(NOUT1,9101)
CXY   RANGE : HALF OF DIAGONAL
      RANGE =   SQRT(2.*RX(NX1)**2)
C     DRO =RANGE/FLOAT(NDR)
      RO1= RANGE
CXY
      FVOL=0.25
CXY   SZ : QUARTER OF SURFACE AREA BY FVOL
      SZ=2.*RX(NX1)
       IF(BETM.GT.PI/4.)THEN
      WRITE(6,*) ' ** ANGULAR INTEGRATION RANGE IS MODIFIED TO 45 DEG '
       BETM=PI/4.
                        ENDIF
CXX   DO 80 NT=1,NTPIN
CXX   RPP(NT)=RX(NPTX(NT))
CXX   THETA(NT)=TY(NPTY(NT))
CXX80 CONTINUE
C
C
C * * PRINT MESH NUMBER AND REGION NUMBER
      IF(IDIVP.LE.1) THEN
           CALL IPRTX7('S-REGION',MESHX,MESHP)
                     ELSE
           CALL IPRTXX('S-REGION',MESHX,MESHP)
                     ENDIF
      IF(NR.EQ.NZ) GOTO 140
      IJ=0
      DO 135 J=1,NX
      DO 135 I=J,NX
      IJ=IJ+1
      IT=NZONE(IJ)
      MESHX(I,J) = IT
  135 CONTINUE
      IF(NTPIN.EQ.0) GO TO 140
      IJ=NMESH
CXX   DO 136 NT=1,NTPIN
      NT=0
      DO 136 N =1,NAPIN
      DO 136 K =N,NAPIN
      NT=NT+1
      DO 136 ND=1,NDPIN
      IJ=IJ+1
      IT=NZONE(IJ)
      MESHP(ND,NPTX(K),NPTX(N))=IT
  136 CONTINUE
  140 CONTINUE
      IF(IDIVP.LE.1) THEN
           CALL IPRTX7('T-REGION',MESHX,MESHP)
                     ELSE
           CALL IPRTXX('T-REGION',MESHX,MESHP)
                     ENDIF
C
      IF(NRR.EQ.NR) GO TO 151
      IJ=0
      DO 145 J=1,NX
      DO 145 I=J,NX
      IJ=IJ+1
      IT=NZONE(IJ)
      MESHX(I,J) = IRR(IT)
  145 CONTINUE
      IF(NTPIN.EQ.0) GO TO 150
      IJ=NMESH
CXX   DO 146 NT=1,NTPIN
      NT=0
      DO 146 N =1,NAPIN
      DO 146 K =N,NAPIN
      NT=NT+1
      DO 146 ND=1,NDPIN
      IJ=IJ+1
      IT=NZONE(IJ)
      MESHP(ND,NPTX(K),NPTX(N))=IRR(IT)
  146 CONTINUE
  150 CONTINUE
      IF(IDIVP.LE.1) THEN
           CALL IPRTX7('R-REGION',MESHX,MESHP)
                     ELSE
           CALL IPRTXX('R-REGION',MESHX,MESHP)
                     ENDIF
  151 IF(NXR.LT.2 .OR. NXR.EQ.NRR) GOTO 171
      IJ=0
      DO 155 J=1,NX
      DO 155 I=1,NX
      IJ=IJ+1
      IT=NZONE(IJ)
      MESHX(I,J) = IXR(IRR(IT))
  155 CONTINUE
      IF(NTPIN.EQ.0) GO TO 170
      IJ=NMESH
CXX   DO 160 NT=1,NTPIN
      NT=0
      DO 160 N =1,NAPIN
      DO 160 K =N,NAPIN
      NT=NT+1
      DO 160 ND=1,NDPIN
      IJ=IJ+1
      IT=NZONE(IJ)
      MESHP(ND,NPTX(K ),NPTX(N ))=IXR(IRR(IT))
  160 CONTINUE
  170 CONTINUE
      IF(IDIVP.LE.1) THEN
           CALL IPRTX7('X-REGION',MESHX,MESHP)
                     ELSE
           CALL IPRTXX('X-REGION',MESHX,MESHP)
                     ENDIF
  171 IF(NM.EQ.NRR) GO TO 9999
      IF(NM.EQ.  1) GO TO 9999
      IJ=0
      DO 180 J=1,NX
      DO 180 I=J,NX
      IJ=IJ+1
      IREG=NZONE(IJ)
      MESHX(I,J) = MMR(IRR(IREG))
  180 CONTINUE
      IF(NTPIN.EQ.0) GO TO 195
      IJ=NMESH
CXX   DO 190 NT=1,NTPIN
      NT=0
      DO 190 N =1,NAPIN
      DO 190 K =N,NAPIN
      NT=NT+1
      DO 190 ND=1,NDPIN
      IJ=IJ+1
      IT=NZONE(IJ)
      IR=IRR(IT)
      MESHP(ND,NPTX(K ),NPTX(N ))=MMR(IR)
  190 CONTINUE
  195 IF(IDIVP.LE.1) THEN
           CALL IPRTX7('M-REGION',MESHX,MESHP)
                     ELSE
           CALL IPRTXX('M-REGION',MESHX,MESHP)
                     ENDIF
 9999 RETURN
 9100 FORMAT(' **** UNMATCH IN TOTAL S-REGION NUMBER ***',2I10)
 9101 FORMAT(30H**** TIME CONSUMING CASE       )
      END
