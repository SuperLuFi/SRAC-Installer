      SUBROUTINE GEOMXZ(RX,RDP,NPTX,NPTY,TY,IM,IP,D,DX,DY,NSRPIN)
C *** GENERAL X-Y TWO DIMENSIONAL LATTICE CELL OF REFLECTIVE CONDITION
C *** EXCLUSIVE USE FOR IGT=16 & IDIV=2
C     AT AXISESC *** X AND Y OUTER BOUND. COND. IS ISO, REFL, AND BLACK
      DIMENSION RX(1),TY(1),D(1),IM(1),IP(1),DX(1),DY(1)
     *         ,NPTX(NTPIN),NPTY(NTPIN),RDP(NDPIN1,NTPIN),NSRPIN(NTPIN)
      COMMON /PIJ1C/ NX,NY,NTPIN,IDUM4(5),NDPIN,IDIVP,BETM,NX1,NY1
     1             ,IDUM14( 3),IXP,IYP,IZP,NDPIN1,IDUM21(2),LL,IDUM24(6)
     *             ,RO,ANG,IDUM32(2),SINB,COSB,IBASE
      COMMON /PIJ2C/ IGT,NZ,IDUM1(8),NTTAB
      EQUIVALENCE (IDUM1(7),IEDPIJ)
      COMMON /MAINC/ DUMY1(63),NOUT1,NOUT2,DUMMY2(435)
      DIMENSION IREFL(4,25)
      DATA IREFL/1,1,1,1, 1,2,1,2, 2,1,2,1, 1,0,1,0, 0,1,0,1
     &          ,1,1,2,2, 1,2,3,4, 2,1,4,3, 1,0,2,0, 0,1,0,2
     &          ,2,2,1,1, 3,4,1,2, 4,3,2,1, 2,0,1,0, 0,2,0,1
     &          ,1,1,0,0, 1,2,0,0, 2,1,0,0, 1,0,0,0, 0,1,0,0
     &          ,0,0,1,1, 0,0,1,2, 0,0,2,1, 0,0,1,0, 0,0,0,1/
CXZ   NUMBERING IN A PIN ON EACH QUADRANT
CXZ          IZ=4 IZ=2  IZ=0 IZ=1  IZ=2  IZ=3
CXZ
CXZ  JZ=3   0*0   0*0    0*0   0*0    0*0
CXZ         0*1   2*1    1*1   1*2    1*0
CXZ
CXZ  JZ=1   0*2    4 3   2*2   3 4    2*0
CXZ         0*1    2 1   1*1   1 2    1*0
CXZ
CXZ         0*1    2*1   1*1   1 2    1*0
CXZ *JZ=0******************************************
CXZ         0*1    2*1   1*1   1 2    1*0
CXZ
CXZ  JZ=2   0*1    2 1   1*1   1 2    1*0
CXZ         0*2    4 3   2*2   3 4    2*0
CXZ
CXZ  JZ=4   0*1    2*1   1*1   1*2    1*0
CXZ         0*0    0*0   0*0   0*0    0*0
CXZ POSITION IN IREFL
CXZ    JZ    *  0   1   2   3   4
CXZ *****************************
CXZ    IZ  0 *  1   6  11  16  21
CXZ        1 *  2   7  12  17  22
CXZ        2 *  3   8  13  18  23
CXZ        3 *  4   9  14  19  24
CXZ        4 *  5  10  15  20  25
CXZ
*     IF(IEDPIJ.GE.3)
*    &WRITE(6,*) ' RO=',RO,' ANG=',ANG,' IBASE=',IBASE
C     IBASE=NX*NY BUT IF(IDIVP.EQ.0 .AND. NTPIN.NE.0) IBASE=1
C     IBASE : LARGEST S-REGION NUMBER OF XY DIVISION
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
C * * CROSS WITH Y=TY(I) 2*NY1+1 POINTS IN DESCENDING ORDER INTO DY ARR
   11 DO 20 I=1,NY
      I1=I+NY+1
      I2=NY-I+1
      DY(I1)=-TY(I+1)/SINB + DYZ
      DY(I2)= TY(I+1)/SINB + DYZ
   20 CONTINUE
      DY(NY1)=DYZ
*     IF(IEDPIJ.GE.3)
*    &WRITE(6,*) '  1DY=',(DY(I),I=1,2*NY+1)
C
C * * ARRANGE DX DY IN DESCENDING ORDER INTO D ARRAY WITH S-REG NUMBER
   21 IX=1
      IY=1
      L=1
      IXB=-NX1
      IYB=-NY1
      IXC=IXB
      IYC=IYB
   35 IJK=1
      IF(DX(IX).LT.DY(IY)) IJK=2
      GO TO (40,50),IJK
   40 D(L)=DX(IX)
      IXB=IXB+1
      IF(IXB.EQ.0) IXB=1
      IM(L)=(IABS(IYB)-1)*NX+IABS(IXB)
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
      IM(L)=(IABS(IYB)-1)*NX+IABS(IXB)
      IP(L)=IM(L-1)
      IY=IY+1
      IF(IY.GT.2*NY+1) GO TO 55
C     X POSITION STILL REMAIN AT INITIAL
      IF(IXB.EQ.IXC) GO TO 35
      L=L+1
      GO TO 35
   55 LL=L
*     IF(IEDPIJ.GE.3) THEN
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  2LL=',LL,'IM=',(IM(L),L=1,LL-1)
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  IJK=',IJK,'IP=',(IP(L),L=2,LL)
*     WRITE(6,'(A,10F12.5/(11X,10F12.5))')
*    &'        D(L)=',(D(L),L=1,LL)
*                 ENDIF
      IF(NTPIN.EQ.0) GO TO 90
      IF(L.EQ.1) GO TO 90
C     IF(IDIVP.EQ.0) NO XY DIVISION
CXZ   IF(IDIVP.NE.0) GO TO 80
CXZ   D(2)=D(L)
CXZ   IM(1)=1
CXZ   IP(2)=1
CXZ   LL=2
C
C   SCAN NTPIN RODS IF THE RAY TRACES
   80 DO  86 NT=1,NTPIN
      FACT4=(NSRPIN(NT+1)-NSRPIN(NT))/NDPIN
      PA=RX(NPTX(NT))
CXR   PA X POSITION OF PIN NT
      PB=TY(NPTY(NT))
CXR   PB Y POSITION OF PIN NT
CXZ   NXR=2
CXR   REPEAT TWICE FOR MIRROR REFLECTIVE GEOMETRY
CXZ   NYR=2
      DO  85 II=1,2
      IZ=II
C     IF(PA.EQ.0.)IZ=0
C     IF(PA.EQ. RX(NX+1))IZ=3
C     IF(PA.EQ.-RX(NX+1))IZ=4
      IF(NPTX(NT).EQ.1)         THEN
         IZ=0
      ELSEIF(NPTX(NT).EQ. NX+1) THEN
        IF(II.EQ.1)  THEN
          IZ=3
                     ELSE
          IZ=4
                     ENDIF
                                ENDIF
      DO  83 JJ=1,2
         JZ=JJ
C        IF(PB.EQ.0.)JZ=0
         IF(NPTY(NT).EQ.1)         THEN
             JZ=0
         ELSEIF(NPTY(NT).EQ. NY+1) THEN
          IF(JJ.EQ.1)  THEN
             JZ=3
                        ELSE
             JZ=4
                        ENDIF
                                   ENDIF
         DAB=ABS(PB*COSB-PA*SINB+RO)
         DMID=-PA*COSB-PB*SINB
CXZ
         DXX=DXZ-PA/COSB
         DYY=DYZ-PB/SINB
      DO 81 NDP=NDPIN1,2,-1
         RR=RDP(NDP,NT)
         IF(RR.LE.DAB) GO TO  82
         DET=SQRT(RR**2-DAB**2)
         DPA=DMID+DET
         DPB=DMID-DET
         PP1=DPA
         PP2=DPB
CXZ
      IF(DXX.GT.DYY)         THEN
        IF(DXX.GT.DPA)     THEN
          IF(DPB.GT.DYY) THEN
CXZ    PATTERN 2 TO 2
            IIJJ1=IREFL(2,5*JZ+IZ+1)
            IIJJ2=IIJJ1
            GO TO 70
                         ELSE
CXZ    PATTERN 2 TO 4
            IIJJ1=IREFL(2,5*JZ+IZ+1)
            IIJJ2=IREFL(4,5*JZ+IZ+1)
            GO TO 60
                         ENDIF
                           ELSE
          IF(DPB.GT.DYY) THEN
CXZ    PATTERN 1 TO 2
            IIJJ1=IREFL(1,5*JZ+IZ+1)
            IIJJ2=IREFL(2,5*JZ+IZ+1)
            GO TO 65
                         ELSE
CXZ    PATTERN 1 TO 2 TO 4
            IIJJ1=IREFL(2,5*JZ+IZ+1)
            IZN1=IIJJ1+FACT4*(NDP-2)+NSRPIN(NT)
            CALL INSERT(DXX,IZN1,IZN1,DYY,D,IM,IP)
            IIJJ1=IREFL(1,5*JZ+IZ+1)
            IIJJ2=IREFL(4,5*JZ+IZ+1)
CXZ   RIGHT EDGE
               IF(IZ.EQ.3)     THEN
            PP2=DXX
            IIJJ2=IIJJ1
CXZ   LEFT EDGE
                ELSEIF(IZ.EQ.4)THEN
            PP1=DXX
            IIJJ1=IREFL(2,5*JZ+IZ+1)
                               ENDIF
CXZ   UPPER EDGE
               IF(JZ.EQ.3)     THEN
            PP2=DYY
            IIJJ2=IREFL(2,5*JZ+IZ+1)
CXZ   LOWER EDGE
               ELSEIF(JZ.EQ.4) THEN
            PP1=DYY
            IIJJ1=IIJJ2
                               ENDIF
CXZ   CORNER RIGHT UPPER
              IF(JZ.EQ.3 .AND. IZ.EQ.3)     THEN
            PP2=DXX
            IIJJ2=IIJJ1
CXZ   CORNER LEFT LOWER
              ELSEIF(JZ.EQ.4 .AND. IZ.EQ.4) THEN
            PP1=DYY
            IIJJ1=IIJJ2
                                            ENDIF
            GO TO 70
                         ENDIF
                           ENDIF
                               ELSE
        IF(DYY.GT.DPA)     THEN
          IF(DPB.GT.DXX) THEN
CXZ    PATTERN 3 TO 3
            IIJJ1=IREFL(3,5*JZ+IZ+1)
            IIJJ2=IIJJ1
            GO TO 70
                         ELSE
CXZ    PATTERN 3 TO 4
            IIJJ1=IREFL(3,5*JZ+IZ+1)
            IIJJ2=IREFL(4,5*JZ+IZ+1)
            GO TO 65
                     ENDIF
                           ELSE
          IF(DPB.GT.DXX) THEN
CXZ    PATTERN 1 TO 3
            IIJJ1=IREFL(1,5*JZ+IZ+1)
            IIJJ2=IREFL(3,5*JZ+IZ+1)
            GO TO 60
                         ELSE
CXZ    PATTERN 1 TO 3 TO 4
            IIJJ1=IREFL(3,5*JZ+IZ+1)
            IZN1=IIJJ1+FACT4*(NDP-2)+NSRPIN(NT)
            CALL INSERT(DYY,IZN1,IZN1,DXX,D,IM,IP)
            IIJJ1=IREFL(1,5*JZ+IZ+1)
            IIJJ2=IREFL(4,5*JZ+IZ+1)
CXZ   RIGHT EDGE
             IF(IZ.EQ.3)            THEN
         PP2=DXX
            IIJJ2=IREFL(3,5*JZ+IZ+1)
CXZ   LEFT  EDGE
             ELSEIF(IZ.EQ.4)        THEN
            PP1=DXX
            IIJJ1=IIJJ2
                                    ENDIF
CXZ   UPPER EDGE
              IF(JZ.EQ.3)            THEN
             PP2=DYY
             IIJJ2=IIJJ1
CXZ   LOWER EDGE
              ELSEIF(JZ.EQ.4)        THEN
             PP1=DYY
             IIJJ1=IREFL(3,5*JZ+IZ+1)
                                     ENDIF
CXZ   CORNER RIGHT UPPER
              IF(JZ.EQ.3 .AND. IZ.EQ.3)               THEN
             PP2=DYY
             IIJJ2=IIJJ1
CXZ   CORNER LEFT LOWER
              ELSEIF(JZ.EQ.4 .AND. IZ.EQ.4)           THEN
             PP1=DXX
             IIJJ1=IIJJ2
                                                      ENDIF

             GO TO 70
                         ENDIF
                           ENDIF
                             ENDIF
CXZ   UPPER EDGE
   60 IF(JZ.EQ.3)            THEN
         PP2=DYY
         IIJJ2=IIJJ1
CXZ   LOWER EDGE
      ELSEIF(JZ.EQ.4)        THEN
         PP1=DYY
         IIJJ1=IIJJ2
                             ENDIF
      GO TO 70
CXZ   RIGHT EDGE
   65 IF(IZ.EQ.3)            THEN
         PP2=DXX
         IIJJ2=IIJJ1
CXZ   LEFT  EDGE
      ELSEIF(IZ.EQ.4)        THEN
         PP1=DXX
         IIJJ1=IIJJ2
                             ENDIF
      GO TO 70
CXZ   IZN=IBASE+NDP-1+NDPIN*(NT-1)
   70 IZN1=IIJJ1+FACT4*(NDP-2)+NSRPIN(NT)
      IZN2=IIJJ2+FACT4*(NDP-2)+NSRPIN(NT)
      CALL INSERT(PP1,IZN1,IZN2,PP2,D,IM,IP)
*     IF(IEDPIJ.GE.3) THEN
*     WRITE(6,*) '  JZ=',JZ,'  IZ=',IZ,'   NT=',NT
*    &, '  IJ1=',IIJJ1,'  IJ2=',IIJJ2,'  IZN1=',IZN1,'  IZN2=',IZN2
*    &, '  F4=',FACT4,' PA=',PA,' PB=',PB,' PP1=',PP1,' PP2=',PP2
*    &, ' DPA=',DPA,' DPB=',DPB,' DXX=',DXX,' DYY=',DYY
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  4LL=',LL,'IM=',(IM(L),L=1,LL-1)
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  IJK=',IJK,'IP=',(IP(L),L=2,LL)
*     WRITE(6,'(A,10F12.5/(11X,10F12.5))')
*    &'    D(L)=',(D(L),L=1,LL)
*                     ENDIF
   81 CONTINUE
   82 IF(ABS(PB).LT.1.E-3) GO TO 84
CXR   IF ON Y-AXIS
      PB=-PB
   83 CONTINUE
   84 IF(ABS(PA).LT.1.E-3) GO TO 86
CXR   IF ON X-AXIS
      PA=-PA
   85 CONTINUE
   86 CONTINUE
   90 LL=LL-1
      IF(LL.EQ.0) RETURN
*     IF(IEDPIJ.GE.3) THEN
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  3LL=',LL,'IM=',(IM(L),L=1,LL)
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  IJK=',IJK,'IP=',(IP(L),L=2,LL+1)
*     WRITE(6,'(A,10F12.5/(11X,10F12.5))')
*    &'      D(L)=',(D(L),L=1,LL+1)
*                     ENDIF
      DO 100 L=1,LL
      D(L)=D(L)-D(L+1)
  100 CONTINUE
*     IF(IEDPIJ.GE.3) THEN
*     WRITE(6,'(A,10F12.5/(11X,10F12.5))')
*    &'     XD(L)=',(D(L),L=1,LL)
*                     ENDIF
      IF(LL.GT.NTTAB) GO TO 300
      GO TO (110,120),IJK
  110 IXP=IXP+1
CXY   RO=RO-SIGN(2.*SINB,SINB*COSB)*RX(NX1)
      RO=RO-     2.*SINB           *RX(NX1)
      RETURN
  120 IYP=IYP+1
CXY   RO=RO+SIGN(2.*COSB,SINB*COSB)*TY(NY1)
      RO=RO+     2.*COSB           *TY(NY1)
      RETURN
  300 WRITE(NOUT1,301) LL,NTTAB
      STOP
  301 FORMAT(1H0,9X,15HTABLE OVERFLOW   ,2I10)
      END
