C **********************************************************************
                      SUBROUTINE PATHHX
C **********************************************************************
C  ARRANGE AND REFORM THE INFORMATION ON GEOMETRY
C **********************************************************************
C             THE ARGUMENTS OF THE ROUTINE
     * (RX      ,TY        ,
     *  IXPIN     ,IYPIN     ,RXPIN     ,TYPIN     )
C  IN RX, TY, IDPIN, IXPIN, IYPIN
C  THE GEOMETRICAL INFORMATION TRANSFERRED CONTAINS THAT FOR A SUB-UNIT.
C  THAT WILL BE EXTENDED OWING TO THE SYMMETRIC CONDITION.
C **********************************************************************
      COMMON /MAINC/ DUM(95),MEMO
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGA,NDPIN,IDIVP,
     1                 BETM,NX1,NY1,LOCAL(3),IXP,IYP,IZP,NDPIN1,
     2                 NDR,NDA,LL,L0,RO1,DRO,FVOL,LOCAL1,LOCAL2,RR,
     3                 ANG,LOCAL3,LOCAL4,SINB,COSB,LOCAL5
      COMMON / PIJ2C /
     1         IGT,NZ,NR,NRR,NXR,IBOUND,IDRECT,LCOUNT,IEDPIJ,IFORM,
     2         NTTAB,NUTAB,SZ,ITYPE,NGLAST,LOCAL6(3),
     3         IEDFLX,ITMINN,ITMOUT,ITBG,LCMX,ITDM,IPT,
     4         EPSI,EPSO,EPSG,RELC,OVERX,FACTOR,ICOOD,MM,NO1(6),
     5         LCMMR,LCNREG,LCIRR,LCIXR,LCMAR,LCMAT,LCVOL,
     6         LCVOLR,LCVOLX,LCVOLM,LCMATD,AA(950)
      COMMON / PIJC / L00,L01,L02,L03,L04,L05,L06,L07,L08,L09,
     1                L10,L11,L12,L13,L14,L15,L16,L17,L18,L19,L20,L21,
     2                L22,L23,L24,L25,L26,L27,L28,L29,L30,L31,L32,L33,
     3                L34,L35,DD(65)
      COMMON /WORK/A(60000)
C-----ADDED BY JAIS K.KANEKO  6/14/84 ----------------------------------
C
      COMMON /IGEOM/
     * NTPINZ, NGRZM ,
     * NGRZP ,
C              PARTIAL SPACE(30)
     *  NGRM  , NGRP  , PITCH,
     *  XLENG ,  NPINRT, NPINZ
     *
C              PARTIAL SPACE(30)
     *
C    *******************************************************************
C
      DIMENSION
     * RX   (*),TY   (*), IXPIN(*),IYPIN(*),RXPIN(*),TYPIN(*)
C     DIMENSION IBND(3,7,5)
      DIMENSION    LOC(6)
      EQUIVALENCE (LOC(1),L1),(LOC(2),L2),(LOC(3),L3)
     *           ,(LOC(4),L4),(LOC(5),L5),(LOC(6),L6)
C
      DATA PI  /3.1415926/
C*****************************
      NRXZ=NX
      NTYZ=NY
      JRPH=NAPIN
C **********************************************************************
        XLENG=A(L01+NRXZ)
        SZ=XLENG*6
                       IERR=0
C **********************************************************************
C     L01=1
C 1   RX  BY L01
C     L02 =L01 + NX1+NAPIN
C 2   RPP BY L02
C     L03 = L02 + NTPIN
C 3   RDP BY L03
C     L04 = L03 + NDPIN1*NTPIN
C 4   NPIN  BY L04
C     L05=L04+2*NTPIN
C 5   THETA BY L05
C     L06 = L05 + NTPIN
C 6   TY    BY L06
C     L08 = L06 + NY1
C 8   D     BY L08
C     L09 = L08 + NTTAB
C 9   IM    BY L09
C     L10 = L09 + NTTAB
C 10  IP    BY L10
C     L11 = L10 + NTTAB
C 11  II    BY L11
C     L12 = L11 + NTTAB
C 12  XX    BY L12
C     L14 = L12 + NTTAB
C 14  S     BY L14NR
C     L25 = L14 + NR
C 25  XX    BY L25
C     L26 = L25 + NUTAB
C 26  III   BY L26
C     L27 = L26 + NUTAB
C 27  LCPIN
      L28 = L27 + NTPIN*(NDPIN+1)
C 28  NGRM+NGRP
      L29 = L28 + NZ
      IF(L29.GT.MEMO) THEN
      WRITE(6,*) ' LAST MEMORY L29=',L29,'MEMO=',MEMO
      STOP
                      ENDIF
C  ***** OBTAIN  R-THETA PIN POSITION GIVEN BY GRID NUMBER ************
                     IF(NTPIN.GT.0) THEN
       IF(IXPIN(1).EQ.0.AND.IYPIN(1).EQ.0) THEN
         NPINZ=1
                                           ELSE
         NPINZ=0
                                           ENDIF
       NTPINZ=(NTPIN-NPINZ)/6+NPINZ
       NNTDPI=NTPINZ*NDPIN
       NNTDMO=NZ-NNTDPI
                       DO 200 NP=1,NTPINZ
                    IF(IXPIN(NP).LE.JRPH)
     *                      THEN
                            ISM=5
        CALL GRIDRT(RX(IXPIN(NP)+1),RX(IYPIN(NP)+1),
     *                          RXPIN(NP),TYPIN(NP),ISM)
                            ELSE
                            ISM=4
                    IF(IYPIN(NP).GT.NTYZ)
     *                      THEN
        CALL GRIDRT(RX(IXPIN(NP)+1),TY(IYPIN(NP)-NTYZ+1),
     *                          RXPIN(NP),TYPIN(NP),ISM)
                        TYPIN(NP)=TYPIN(NP)+PI/3.
                            ELSE
        CALL GRIDRT(RX(IXPIN(NP)+1),TY(IYPIN(NP)+1),
     *                          RXPIN(NP),TYPIN(NP),ISM)
                            ENDIF
                            ENDIF
C  *********************************************************************
C  ** MODIFY PIN DIVISION CONTROL ACCORDING TO THE BOUNDARY CONDITION **
C  ** AND TEST IF THE PIN LOCATION IS OUT-OF-BOUNDARY **
C                        IF ON THE CENTER
  160 FORMAT(' *** PIN POSTION OUT-OF-BOUNDARY FOR ',I3,'-TH PIN '
     *,'SPECIFIED BY IX=',I2,' IY=',I2)
  200                     CONTINUE
                                   ENDIF
C **********************************************************************
C                  FIGURE G-REGION MAP
C **********************************************************************
  250 CALL GFHEX6(NRXZ,JRPH,NTYZ,NTPINZ,IXPIN,IYPIN)
      CALL PREHX(RX,TY,IXPIN,IYPIN,RXPIN,TYPIN
     *             ,A(L03),A(L27),A(L28+NNTDMO),A(L28))
      CALL VOLPIJ(AA(LCNREG),AA(LCIRR),AA(LCIXR),
     *  AA(LCMMR),A(L28),AA(LCVOL),AA(LCVOLR),AA(LCVOLX),AA(LCVOLM))
      CALL MAKEHX(A(L14),RX ,RXPIN ,TY ,TYPIN,A(L08),A(L09),A(L10))
                            RETURN
                            END
