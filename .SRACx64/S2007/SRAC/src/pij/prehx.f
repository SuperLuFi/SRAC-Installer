C **********************************************************************
                      SUBROUTINE PREHX
C **********************************************************************
C             THE ARGUMENTS OF THE ROUTINE
     * (RX      ,TY        ,
     * IXPIN     ,IYPIN     ,RXPIN     ,TYPIN     ,
     *  RDPIN   ,LCPIN     , VLGRP     ,VLGRM     )
C  IN RX, TY, IDPIN, IXPIN, IYPIN, AND RDPIN,
C  THE GEOMETRICAL INFORMATION TRANSFERRED CONTAINS THAT FOR A SUB-UNIT.
C  THAT WILL BE EXTENDED OWING TO THE SYMMETRIC CONDITION.
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGA,NDPIN,IDIVP,
     1                 BETM,NX1,NY1,LOCAL(3),IXP,IYP,IZP,NDPIN1,
     2                 NDR,NDA,LL,L0,RO1,DRO,FVOL,LOCAL1,LOCAL2,RR,
     3                 ANG,LOCAL3,LOCAL4,SINB,COSB,LOCAL5
C     COMMON / PIJ2C /
C    1         IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,LCOUNT,IEDPIJ,IFORM,
C    2         NTTAB,NUTAB,SZ,ITYPE,NGLAST,LOCAL6(3),
C    3         IEDFLX,ITMINN,ITMOUT,ITBG,LCMX,ITDM,IPT,
C    4         EPSI,EPSO,EPSG,RELC,OVERX,FACTOR,ICOOD,MM,NO1(6),
C    5         LCMMR,LCNREG,LCIRR,LCIXR,LCMAR,LCMAT,LCVOL,
C    6         LCVOLR,LCVOLX,LCVOLM,LCMATD,AA(950)
      COMMON  /MAINC/IDUM0(64),NOUT2
      COMMON /WORK/A(60000)
      COMMON /IGEOM/
     * NTPINZ, NGRZM ,
     * NGRZP ,
C              PARTIAL SPACE(30)
     *  NGRM  , NGRP  , PITCH,
     * XLENG ,  NPINRT, NPINZ
C    *******************************************************************
      DIMENSION
     * RX   (1),TY   (1),
     * IXPIN(1),IYPIN(1),RXPIN(1),TYPIN(1),
     * RDPIN(NDPIN+1,1),LCPIN(1),VLGRM(1),VLGRP(1)
      DIMENSION IBND(3,5),JCOMB(10),FINOUT(10,3)
      DIMENSION    LOC(24),FCT(24)
      EQUIVALENCE (LOC(1),L1),(LOC(2),L2),(LOC(3),L3)
     *           ,(LOC(4),L4),(LOC(5),L5),(LOC(6),L6)
C        10 IN JCOMB COVERS POSSIBLE COMBINATION OF BOUNDARY CONDITIONS
C        10 IN FINOUT LIMITS MAXIMUM VALUE OF NDPIN
      DATA PI,CRT3 /3.1415926,1.7320508/
*     CHARACTER *2 NUMR(4)
*     DATA NUMR/'ST','ND','RD','TH'/
      DATA IBND/
C
C        HEX-T-T
C          60
C       IB IP IH
C **********************************************************************
     1   0, 0, 0,
     2   0, 2, 1,
     3   8, 0, 0,
     4   0, 0, 0,
     5   0, 1, 1/
C **********************************************************************
C               BOUNDARY CONDITION    *  CONDITION 1 IX=0
C        RECTANGULAR       TRIANGULAR *            2 IY=0
C +1         LEFT            LEFT     *            3 IX=MAX
C +2         RIGHT          RIGHT     *            4 IY=MAX
C +4        BOTTOM          BOTTOM    *            5 IX=IY
C +8         TOP                      *
      DATA JCOMB/
C  FACTOR FOR PIN ROD REGION BY THE COMBINATION OF BOUNDARY CONDITION
C           1     2     3     4     5     6     7     8     9    10
C **********************************************************************
     *      2 ,   2 ,   6 ,   6 ,   0 ,   0 ,   0 ,   2 ,   6 ,  6/
C **********************************************************************
         NRXZ=NX
         NTYZ=NY
         JRPH=NAPIN
         NPR = 6
         NROT = 6
         NPL = 6
C **********************************************************************
                       IERR=0
C **********************************************************************
           PITCH=RX(2)-RX(1)
C     IF ISHAPE= 6  (HEXAGONAL) T  & PERIPHERIC 60 DEG ROTATIONAL
                 UNITV=CRT3*PITCH*PITCH/4.
C        NGRZM = 1 + 3 + 5 + ... + (2*JRPH-1) + (NRXZ-JRPH)*NTYZ
         NGRZM =  JRPH*JRPH + (NRXZ-JRPH)*NTYZ
                         IGRM=0
                       DO 520 J=1,JRPH
                        IMAX=2*J-1
                       DO 510 I=1,IMAX
                        IGRM=IGRM+1
                     VLGRM(IGRM)=UNITV
  510                    CONTINUE
  520                    CONTINUE
C
                      IF(NRXZ.GT.JRPH)
     *                    THEN
                       DO 550 J=JRPH+1,NRXZ
                  DYY=(RX(J+1)-RX(J))*SQRT(3.)/4.
                      PYY= RX(J+1)+RX(J)
                        DO 540 I=1,NTYZ
                          IGRM=IGRM+1
                 VLGRM(IGRM)=DYY*(TY(I+1)-TY(I))*PYY
  540                     CONTINUE
  550                     CONTINUE
                           ENDIF
                         MZ=IGRM
C                   END PERIPHERIC REGION
C    *******************************************************************
                         NGRM=NGRZM*NPR
C **********************************************************************
                            IGRP=0
                   IF(NTPINZ.EQ.0) GO TO 950
C **********************************************************************
                          VPR=0.
                     DO 900 NP=1,NTPINZ
                        JGRP=IGRP+1
                                   MDCH=NP
                    IF(MDCH.GT.20) MDCH=MOD(MDCH,10)
                    IF(MDCH.GT.3)  MDCH=4
C                   WRITE(6,575) NP,NUMR(MDCH),JGRP
                    LCPIN(NP)=JGRP
  575 FORMAT('  *** GEOMETRICAL REGION OF ',I3,A2
     *,' PIN ROD STARTS BY ' ,I3,' ***')
C   IB : INDEX WHETHER IF THE PIN IS ON THE OUTER BOUNDARY ***
C   IB = 0  INCLUDED IN THE SUB-UNIT
C      = 1  ON THE HORIZONTAL BOUNDARY
C      = 2  ON THE VERTICAL BOUNDARY
C      = 3  ON THE CELL CENTER
C   IP : INDEX WHETHER IF THE PIN IS ON THE SUB-UNIT BOUNDARY ***
C   IP = 0  INCLUDED IN THE SUB-UNIT
C      = 1  ON THE HORIZONTAL BOUNDARY
C      = 2  ON THE VERTICAL BOUNDARY
C      = 3  ON THE CELL CENTER
C   IH : INDEX WHETHER IF THE PIN IS ON THE CORNER OF HEXAGON ***
C   IH = 0  ON THE SIDE
C      = 1  ON THE CORNER
                           IB=0
                           IP=0
                           IH=0
                           ICOND=1
             IF(IXPIN(NP).EQ.0)         GO TO 620
  580                      ICOND=2
             IF(IYPIN(NP).EQ.0)         GO TO 620
  590                      ICOND=3
             IF(IXPIN(NP).EQ.NRXZ)      GO TO 620
  600                      ICOND=4
             IF(IYPIN(NP).EQ.NTYZ)      GO TO 620
  610                      ICOND=5
        IF(IXPIN(NP).EQ.IYPIN(NP).AND. IXPIN(NP).LE.JRPH)  GO TO 620
        IF(IYPIN(NP).EQ.NTYZ     .AND. IXPIN(NP).GT.JRPH)  GO TO 620
                   GO TO 630
  620             IB=IB+IBND(1,ICOND)
                  IP=IP+IBND(2,ICOND)
                  IH=IH+IBND(3,ICOND)
                  GO TO (580,590,600,610,630),ICOND
  630                      IC=IB+IP
*     WRITE(6,*) '     IB=',IB,'   IP=',IP,'  IC=',IC,'  IH=',IH
                           FSELF=1.0
                           FBOND=1.0
               IF(IB.GT.0) FSELF=1./FLOAT(JCOMB(IB))
               IF(IC.GT.0) FBOND=1./FLOAT(JCOMB(IC))
C
C **********************************************************************
C
C         *****  VOLUME OF SOLID PIN ROD  IN FINOUT ********
C
                    RR=RXPIN(NP)**2
C         HF : FACTOR OF THE INNER PART OF A PIN ROD
                                                    HF=0.5
C             ON THE CORNER OF A HEXAGONAL SHELL
              IF(IH.EQ.1)                           HF=0.3333333
C         IF THE PIN LOCATED AT THE CELL CENTER
        IF(RR.EQ.0)     HF=0.
C         IF THE PIN LOCATED AT THE OUTER CORNER IN R-T COORDINATES
C      IF(IXPIN(NP).EQ.NRXZ)      HF=0.25
C      IF(IXPIN(NP).EQ.NRXZ)      HF=0.3333333
C         OF : FACTOR OF THE OUTER PART OF A PIN ROD
                                                    OF=1.0-HF
        IF(RR.EQ.0)     OF=FBOND
                      DO 640  ND=1,NDPIN+1
                       RS=RDPIN(ND,NP)**2
                       FINOUT(ND,1)=PI*RS
                    FINOUT(ND,2)=HF*FINOUT(ND,1)
                    FINOUT(ND,3)=OF*FINOUT(ND,1)
C         IF R-THETA DIVIDED SQUARE OR HEX   EXCEPT THE CENTER ROD
  640                      CONTINUE
C
C             ***** VPIN   VPINI   VPINO *****
C
                     VPIN =FINOUT(NDPIN+1,1)
                     VPINI=FINOUT(NDPIN+1,2)
                     VPINO=FINOUT(NDPIN+1,3)
C
                        FCT(1)=VPINO/VPIN/2.
                        FCT(2)=FCT(1)
                        FCT(3)=VPINI/VPIN/2.
                        FCT(4)=FCT(3)
      IF(IXPIN(NP).GE.JRPH .AND.IH.EQ.0)
     *                         THEN
                    FCT(1)=TYPIN(NP)/2./PI+0.16666666
                        FCT(2)=0.5-FCT(1)
                        IF(IXPIN(NP).GT.JRPH)
     *                          THEN
                        FCT(3)=FCT(1)
                        FCT(4)=FCT(2)
                                ELSE
                        LOC(3)=0
                        LOC(4)=0
                        FCT(3)=0.
                        FCT(4)=0.
                                ENDIF
                        LOC(5)=0
                        LOC(6)=0
                        FCT(5)=0.
                        FCT(6)=0.
                                ENDIF
C **********************************************************************
C         FIND MODERATOR REGION NUMBERS AROUND THE PIN ROD
C
           IF(RDPIN(NDPIN+1,NP).GT.CRT3*PITCH/2.) THEN
      CALL GMNJNT(JRPH,IXPIN(NP),IYPIN(NP),
     *            RDPIN(NDPIN+1,NP),PITCH,LOC,FCT)
                                           JIANT=1
                                                ELSE
                                           JIANT=0
      CALL GMNAME(NRXZ,NTYZ,IXPIN(NP),IYPIN(NP),LOC,FCT,JRPH,TY)
                                                ENDIF
C
C **********************************************************************
C    ISKIP :   SKIP IF ON THE LEFT BOUND OF ROTATIONAL SYMMETRY
                               ISKIP=0
      IF(MOD(IP,4).EQ.1)       ISKIP=1
C
C     ******  VOLUME OF ANNULAR PIN ROD REGION INTO VLGRP ********
C
C                 IF(IDPIN(NP).EQ.0 .AND. ISKIP.EQ.0) THEN
                  IF(ISKIP.EQ.0)                      THEN
                      DO 650 ND=1,NDPIN
                         IGRP=IGRP+1
              VLGRP(IGRP)=FSELF*(FINOUT(ND+1,1)-FINOUT(ND,1))
  650                     CONTINUE
                                                      ENDIF
C **************************** END  IDPIN=0 *************************
C ****************************START IDPIN=1 *************************
C
C                 IF(IDPIN(NP).EQ.1 .AND. ISKIP.EQ.0) THEN
C                    FACTOR=1.0
C                  IF(MOD(IB,4).GT.0) FACTOR=0.5
C                  INNER HALF **** SKIP IF ON THE BOTTOM
C                  IF(MOD(IC,8).GE.4) GO TO 670
C                    DO 660 ND=1,NDPIN
C                      IGRP=IGRP+1
C             VLGRP(IGRP)=FACTOR*(FINOUT(ND+1,2)-FINOUT(ND,2))
C 660                   CONTINUE
C                  OUTER HALF    SKIP IF ON THE TOP
C 670              IF(    IC   .GE.8) GO TO 690
C                    DO 680 ND=1,NDPIN
C                       IGRP=IGRP+1
C             VLGRP(IGRP)=FACTOR*(FINOUT(ND+1,3)-FINOUT(ND,3))
C 680                   CONTINUE
C 690                   CONTINUE
C                                                    ENDIF
C **************************** END  IDPIN=1 ****************************
      IF(IXPIN(NP).LE.JRPH) GO TO 820
C ****************************START IDPIN=3 *************************
C                  INNER RIGHT QUARTER
                  IF(L4.EQ.0) GO TO 750
                  VPR=VPR+FCT(4)*VPIN
              VLGRM(L4)=VLGRM(L4)-FCT(4)*VPIN
C                  INNER LEFT QUARTER
  750              IF(L3.EQ.0) GO TO 770
                  VPR=VPR+FCT(3)*VPIN
              VLGRM(L3)=VLGRM(L3)-FCT(3)*VPIN
C                  OUTER RIGHT QUARTER
  770             IF(L1.EQ.0) GO TO 790
                  VPR=VPR+FCT(1)*VPIN
              VLGRM(L1)=VLGRM(L1)-FCT(1)*VPIN
C                  OUTER LEFT QUARTER
  790              IF(L2.EQ.0) GO TO 810
  805              CONTINUE
C                                                  ELSE
              VLGRM(L2)=VLGRM(L2)-FCT(2)*VPIN
C                                                  ENDIF
                  VPR=VPR+FCT(2)*VPIN
  810                 CONTINUE
C ******************************* END IDPIN=3 **************************
                           GO TO 900
C               TRIANGULAR MESH AND IDPIN=3
C 820              IF(IDPIN(NP).EQ.3 .AND. ISKIP.EQ.0)
C    *                LCPIN(NP)=IGRP+1
  820              IF(JIANT.EQ.1) GO TO 890
                   IF(IXPIN(NP).LE.JRPH) THEN
C                                  2
C                               3     1
C                               4     6
C                                  5
                         DO 850 L=1,6
                  IF(LOC(L).EQ.0) GO TO 850
                  VPR=VPR+FCT(L)*VPIN
              VLGRM(LOC(L))=VLGRM(LOC(L))-FCT(L)*VPIN
  850                       CONTINUE
                                   ELSE
                         DO 880 L=1,4
                  IF(LOC(L).EQ.0) GO TO 880
                  VPR=VPR+FCT(L)*VPIN
              VLGRM(LOC(L))=VLGRM(LOC(L))-FCT(L)*VPIN
  880                       CONTINUE
                                    ENDIF
                           GO TO 900
C  **   FOR JIANT PIN ROD IN TRIANGULAR MESH **
  890                     DO 895 L=1,24
               IF(LOC(L).EQ.0) GO TO 895
                  VPR=VPR+FCT(L)
              VLGRM(LOC(L))=VLGRM(LOC(L))-FCT(L)
C             WRITE(6,911) IGRP,NP,NUMR(MDCH),LOC(L),VLGRM(LOC(L))
  895                    CONTINUE
C                       END JIANT PIN ROD
  900                       CONTINUE
C                       END NTPINZ LOOP
  910  FORMAT('      PIN ROD REGION ',I3,' OF ',I3,A2,' PIN ROD CONTACTS
     * WITH THE MODERATOR REGION ',I3)
  911  FORMAT('      PIN ROD REGION ',I3,' OF ',I3,A2,' PIN ROD CONTACTS
     * WITH THE MODERATOR REGION ',I3,' MOD VOL REMAIN ',E12.5)
C      WRITE(6,*)'NTPIN=',NTPIN
                       NGRZP=IGRP
                       NGRP=NGRZP*NPR
C **  PREPARATION IF THE ROTATIONAL SYMMETRY FOR TRIANGLE SPECIFIED
C **********************************************************************
C     **   EXPAND VOLUMES  BY ANGULAR PERIOD
  950               CONTINUE
                       DO 960 I=1,NGRZM
                      VLGRM(I)=VLGRM(I)*6
  960                     CONTINUE
  980                     CONTINUE
C **********************************************************************
 1000                     CONTINUE
 1175 FORMAT(' ****** DIAGONAL SYMMETRY NOT SATISFIED ***',2F10.2,
     * ' *** IN AZIMUTHAL DIVISION')
C
C***************** EXPAND  RXPIN & TYPIN ***********************
C
C *** EXPAND PIN ROD ARRAY OWING TO THE ROTATIONAL SYMMETRY
C
C   ******** COMPRESS THE PIN ROD INFORMATION *****************
C
              IF(NTPINZ.EQ.0) GO TO 1370
              AROT=2.*PI/FLOAT(NROT)
              NPINRT=(NTPIN-NPINZ)/6
C            FOLLOWING IS VALID IF IDPIN=0
C            ROTATIONAL CENTER : NON-REPEATED NPFIX
C            ROTATIONAL PERIOD : REPEATED     NPRZ
                   NPFIX=NPINZ*NDPIN
                   NPRZ=NGRZP-NPFIX
                   NGRP=NROT*NPRZ+NPFIX
                      IJ=NPFIX
                 DO 1225 I=1,NPRZ
                      IJ=IJ+1
                 VLGRP(IJ)=VLGRP(IJ)*6
 1225            CONTINUE
C                  NTPIN = NPINZ + NROT*NPINRT
       IF(NPINRT.EQ.0) GO TO 1370
             DO 1250 I=NPINZ+1,NPINRT+NPINZ
                      IDX=IXPIN(I)
                      IDY=IYPIN(I)
                    DO 1240 J=1,NROT-1
                     LPIN=J*NPINRT+I
                      IDDX=IDX
                      IDDY=IDY
                     IF(IXPIN(I).LE.JRPH)
     *                   THEN
                      IDX=IDDX-IDDY
                      IDY=IDDX
                    IXPIN(LPIN)=IDX
                    IYPIN(LPIN)=IDY
                         ELSE
                    IXPIN(LPIN)=IXPIN(I)
                    IYPIN(LPIN)=IYPIN(I)+J*NTYZ
                         ENDIF
                    LCPIN(LPIN)=LCPIN(I)
                    RXPIN(LPIN)=RXPIN(I)
                    TYPIN(LPIN)=TYPIN(I) + J*AROT
                    DO 1230 K=1,NDPIN+1
                    RDPIN(K,LPIN)=RDPIN(K,I)
 1230                    CONTINUE
 1240                    CONTINUE
 1250                    CONTINUE
 1330               IF(NTPIN.EQ.1) GO TO 1370
C    *                  J,RXPIN(J),TYPIN(J)
  133 FORMAT(' **** OVERLAPPING OF PIN ROD POSITION MET BETWEEN ',
     * I3,'-TH PIN X=',E12.5,' Y=',E12.5,' WITH ',
     * I3,'-TH PIN X=',E12.5,' Y=',E12.5/' **** ERROR STOP ')
C  **           TEST PIN OVERLAPPING EACH OTHER
C  **           FOR THE GEOMETRIES WHERE PIN POSITION BY R THETA
                    DO 1360 I=1,NTPIN-1
                    DO 1360 J=I+1,NTPIN
C
                  DT=ABS(TYPIN(I)-TYPIN(J))
                  IF(DT.GT.PI) DT=2.*PI-DT
           IF((RDPIN(NDPIN+1,I)+RDPIN(NDPIN+1,J))**2 .GT.
     *   RXPIN(I)**2 + RXPIN(J)**2 -2.*RXPIN(I)*RXPIN(J)*COS(DT))
     *                     THEN
            WRITE(6,13) I,RXPIN(I),TYPIN(I),
     *                  J,RXPIN(J),TYPIN(J)
   13 FORMAT(' **** OVERLAPPING OF PIN ROD POSITION MET BETWEEN ',
     * I3,'-TH PIN R=',E12.5,' ANGLE=',F8.5,' WITH ',
     * I3,'-TH PIN R=',E12.5,' ANGLE=',F8.5/' **** ERROR STOP ')
                          IERR=IERR+1
                           ENDIF
 1360                     CONTINUE
 1370                      ISN=0
C                  LGLAST = LVLGRP + NGRP
C                    WRITE(6,1470) (VLGRM(I),I=1,NGRZM)
C1470 FORMAT(' ** G-REGION VOLUME FOR MODERATOR ** '/ (10X,10E12.5))
C     IF(NTPINZ.NE.0) WRITE(6,1480) (VLGRP(I),I=1,NGRZP)
C1480 FORMAT(' ** G-REGION VOLUME FOR PIN-ROD ** '/ (10X,10E12.5))
                          VMD=0.
                      DO 1490 I=1,NGRZM
C                     VMD=VMD+VLGRM(I)
                      VMD=VMD+VLGRM(I)/6.0
 1490                    CONTINUE
                         IF(NTPINZ.NE.0)
     *                    THEN
                    FP=VPR/VPIN
      WRITE(NOUT2,1500) FP , NGRZP , NGRP , NTPIN, VPR
 1500 FORMAT(
     *2X,' **  AVERAGE NO. OF PIN RODS IN A SUB UNIT  (',F12.3,')'/
     *2X,' **  NO. OF PIN-ROD REGIONS  IN A SUB-UNIT          (',I4,')'/
     *2X,' **  NO. OF PIN-ROD REGIONS  IN THE WHOLE CELL      (',I4,')'/
     *2X,' **  TOTAL NUMBER OF PIN-RODS IN THE WHOLE CELL     (',I4,')'/
     *2X,' **  VOLUME OF PIN-ROD IN A SUB-UNIT        (',E12.5,')')
C      WRITE(6,1520) '   **  LCPIN=',(LCPIN(I),I=1,NTPIN)
C     WRITE(6,1520) '   ** IDPIN=',(IDPIN(I),I=1,NTPIN)
                          ENDIF
                       VSU=VMD+VPR
C                    VTOT=VSU*FLOAT(NPR)
C**** WRITE(6,1510) NGRZM , NGRM , VMD   , VSU  , VTOT
      WRITE(NOUT2,1510) NGRZM , NGRM , VMD   , VSU
 1510 FORMAT(
     *2X,' **  NO. OF MODERATOR REGIONS IN A SUB UNIT         (',I4,')'/
     *2X,' **  NO. OF MODERATOR REGIONS IN THE WHOLE CELL     (',I4,')'/
     *2X,' **  VOLUME OF MODERATOR IN A SUB-UNIT      (',E12.5,')'/
     *2X,' **  VOLUME OF A SUB-UNIT                   (',E12.5,')')
C*** *2X,' **  VOLUME OF THE WHOLE CELL               (',E12.5,')')
 1520 FORMAT(A,40I3/(13X,40I3))
                      IF(IERR.GT.0) STOP
                            RETURN
                             END
