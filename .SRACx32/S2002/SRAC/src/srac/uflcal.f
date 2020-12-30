CMOD  SUBROUTINE UFLCAL(NEGF)
      SUBROUTINE UFLCAL( NEGF,  IPATH, NUCMAX,KIDENT )
C
CMOD  PARAMETER ( MXLISO = 2000 )
      INCLUDE 'MATDTINC'
C
      COMMON /MAINC/   IOPT(20),       JNFSTL(2),      FNFSTL(2),
     &        JNTHEL(2),       FNTHEL(2),      JNEFST(2),
     &        FNEFST(2),       JNETHE(2),      FNETHE(2),
     &        JNMACR(2),       FNMACR(2),      JNMCRS(2),
     &        FNMCRS(2),       JNEMIC(2),      FNEMIC(2),
     &        JNFLUX(2),       FNFLUX(2),      NEFL,   NETL,   NEF,
     &        NET,     NERF,   NERT,   NMAT,   NETL1,  BSQ,    NIN1,
     &        NIN2,    NOUT1,  NOUT2,  IT0,    NBFL(3),        NBF(3),
     &        DUMMY1(8),       LCNEGF, LCNEGT, LCNECF, LCNECT, LCMTNM,
     &        LCNISO,  LCTEMP, LCXL,   LCXCDC, LCLISO, LCIDNT, LCDN,
     &        LCIRES,  LCIXMC, DUMMY2(6),      CASEID(2),
     &        TITLE(18),       AAAA(380)
C ----COMMON ## USERIX ## IS PASSED TO USERTL ROUTINE
C
      COMMON /TMPSET/  STND(35),       IDTEMP(61),     NTDUMY
CMSASA
      CHARACTER*4 IDTEMP
C
      COMMON /USERIX/  NUCLID(2,MXLISO),       NUCLNO
CMSASA
      CHARACTER*4 NUCLID
C
C
C
      COMMON /PDSPDS/  BUFFER(540),    IFLSW,  NFILE(3),       ECODE,
     &        ITMP
CMSASA
C
      COMMON /UFLCNT/  ICAPT,  IFISS,  IRES,   LTOT,   LTH(4), LA(4),
     &        LD(4),   IFS,    IFTR,   IFC,    IFF,    IFE,    IFER,
     &        NGMIN,   NGMAX,  NSIG,   NTEMP,  AMASS,  SIGP,   SIGC0,
     &        TEMP(4), SIG0(8),        IPL
C
      COMMON /WORK/    FSIGC(74),      FSIGF(74),      FUNU(74),
     &        FCHI(74),        FSIGT(74),      WF(74), FSIGE(74),
     &        FSIGB(74),       BSIGC(74),      BSIGF(74),      BUNU(74),
     &        BCHI(74),        BSIGT(74),      WR(74), BSIGE(74),
     &        BSIGB(74),       AF(75), AR(75), IDENT(2),
     &        NREG(107),       BB(107,74),     NAMEP(2),
     &        LOCAM(11),       LOCAF(7),       FACT(8,4),
     &        FCROS(74,8,4),   BCROS(74,8,4),  FINE(74,8,4),
     &        WTF(74,8,4),     WTB(74,8,4),    FTE(74,8,4),
     &        FTB(74,8,4),     STRE(107,74),   STR(107,74),
     &        STRB(107,74),    DCHI(74,15),    FCHIMT(74,74),
     &        BCHIMT(74,74)
CMOD +                NAMEP(2),LOCAM(11),LOCAF(6),
CMSASA
      CHARACTER*4 NAMEP
C
C ---- ADDED  BY JAIS K.KANEKO  1/26/1983 ------------------------------
C ---- ADDED  BY JAIS K.KANEKO 10/19/1989 ------------------------------
C ----------------------------------------------------------------------
      DIMENSION NEGF(1), II(17000), IA(41), SIG1D(74,8), AA(17000),
     &        IDPQ(10)
      DIMENSION NMYLD(2), NMTEMP(2)
      EQUIVALENCE(AA(1),II(1)), (IA(1),ICAPT), (SIG1D(1,1),FSIGC(1))
C
      DIMENSION KIDENT(2,1)
CMSASA
      CHARACTER*4 NFILE,IBLNK,IDZERO,IDXX,IDPQ,NMYLD,NMTEMP
      CHARACTER*4 IDENT, KIDENT
C
C
      DATA IBLNK /'    '/, IDZERO /'0000'/
      DATA IDXX /'   X'/
      DATA IDPQ /'   C', '   M', '   F', '   R', '   P', '   B', '   C',
     &        '   F', '   E', '   1'/
      DATA NMYLD /'FISS', 'YILD'/
      DATA NMTEMP /'TEMP', 'SET '/
C
CMSASA
C     DIMENSION     KIDENT(2,1)
C
C     USERFL START
C
      NUCLNO  = 0
      DO 100 I = 1, MXLISO
         NUCLID(1,I) = '    '
         NUCLID(2,I) = '    '
  100 CONTINUE
C
      IFLSW   = 1
      NFILE(1)    = 'FAST'
      NFILE(2)    = 'P   '
C
      IF ( NETL.EQ.0 ) WRITE(NOUT1,7000)
 7000 FORMAT('  *** CAUTION *** THERMAL LIBRARY NOT USED ***')
C
      K       = 0
      DO 120 N = 1, NEF + NET
         DO 110 I = 1, NEGF(N)
            K       = K + 1
  110    NREG(K) = N
         IF ( N.EQ.NEF ) NEFL4   = K
  120 CONTINUE
      NETL1   = NETL + NEFL4 - 106
C     WRITE(NOUT2,7007) NETL1,NEFL4,NEFL
C7007 FORMAT(1H ,' ##NETL1 NEFL4 NEFL## ',3I6)
      IF ( NET.GT.0 .OR. NEFL4.EQ.NEFL ) GO TO 140
      KEEP    = NEF
      DO 130 I = NEFL4 + 1, NEFL
         KEEP    = KEEP + 1
         NREG(I) = KEEP
  130 CONTINUE
  140 CONTINUE
C
      NAMEP(1)    = 'FAST'
      NAMEP(2)    = 'LIB '
      CALL READ( NAMEP(1), II, 4 )
      NEFL    = II(1)
      NBFL(1) = II(2)
      NBFL(2) = II(3)
      NBFL(3) = II(4)
C     WRITE(NOUT2,7001) NEFL,NBFL
C7001 FORMAT(1H ,' ##NEFL NBFL## ',4I5)
      LENG    = 5 + 2*NEFL
C
      CALL READ( NAMEP(1), AA, LENG )
      CALL READ( NMYLD(1), FCHI, NEFL )
      DO 150 I = 1, NEFL
         WF(I)   = AA(4+I)
         AF(I)   = AA(4+NEFL+I)
  150 CONTINUE
      AF(NEFL+1)  = AA(LENG)
C
      NFILE(1)    = 'FAST'
      NFILE(2)    = 'U   '
C
      CALL SEARCH( NAMEP(1), LENG, ISW )
      IF ( ISW.EQ.0 ) GO TO 210
C
C  ===BOUNDARY ENERGIES===
  160 AR(1)   = AF(1)
      NJ      = 0
      DO 170 N = 1, NEF
         NJ      = NJ + NEGF(N)
         AR(N+1) = AF(NJ+1)
         BCHI(N) = 0.0
  170 CONTINUE
      CALL ENTAPR( 'FINE   ', 'FAST   ', NEF, AR )
      DO 180 N = 1, 3
         NBF(N)  = NREG(NBFL(N))
  180 CONTINUE
C
C  ===WEIGHTS FOR INFINITE DILUTION===
      CALL COLLAP( NEFL4, NREG, 0, WF, WR, AF, AF, AF )
C  ===FISSION SPECTRUM ===
      CALL COLLAP( NEFL4, NREG, 0, FCHI, BCHI, AF, AF, AF )
C
      CALL ICLEA( II, 300, 0 )
      II(1)   = NEF
      II(2)   = NBF(1)
      II(3)   = NBF(2)
      II(4)   = NBF(3)
      ISW     = 4
      DO 190 I = 1, NEF
         ISW     = ISW + 1
         AA(ISW) = WR(I)
  190 CONTINUE
      DO 200 I = 1, NEF + 1
         ISW     = ISW + 1
         AA(ISW) = AR(I)
  200 CONTINUE
      LENG    = ISW
C
      CALL WRITE( NAMEP(1), II, LENG )
      CALL WRITE( NMYLD(1), BCHI, NEF )
C
C  ===READ IN NUCLIDE IDENTIFICATION===
  210 CONTINUE
C
C 100 WRITE(NOUT1,101)
C 101 FORMAT(' ENTER NUCLIDE I.D. (8H)' )
C
  220 CONTINUE
      IF ( IPATH.EQ.0 ) THEN
         CALL REAM( IDENT, II, A, 2, 0, 0 )
C
C     WRITE(NOUT1,115) IDENT
C 115 FORMAT(1H+,T41,2A4)
CTFREEIF (IDENT(1).EQ.IBLNK) RETURN
         IF ( IDENT(1).EQ.IBLNK ) THEN
            NFILE(1)    = 'FAST'
            NFILE(2)    = 'U   '
C
            CALL SEARCH( NMTEMP(1), LENG, ISW )
            IF ( ISW.EQ.0 ) CALL READ( NMTEMP(1), STND, 71 )
            IF ( ISW.EQ.1 ) CALL WRITE( NMTEMP(1), STND, 71 )
            RETURN
         END IF
C
      ELSE
         ISW     = NUCLNO + 1
         IF ( ISW.GT.NUCMAX ) RETURN
         IDENT(1)    = KIDENT(1,ISW)
         IDENT(2)    = KIDENT(2,ISW)
      END IF
C===== TEMPSET MEMBER CHECK
      IF ( IDENT(1).EQ.NMTEMP(1).AND.IDENT(2).EQ.NMTEMP(2) ) GO TO 1140
C
      NUCLNO  = NUCLNO + 1
      IF ( NUCLNO.GT.MXLISO ) GO TO 1160
      NUCLID(1,NUCLNO)    = IDENT(1)
      NUCLID(2,NUCLNO)    = IDENT(2)
C     MASK 1-ST,5-TH,6-TH,7-TH,8-TH CHARACTER
      NAMEP(1)    = IDENT(1)
      NAMEP(2)    = IDZERO
      CALL PACK( NAMEP(1), 1, IBLNK )
C
      NFILE(1)    = 'FAST'
      NFILE(2)    = 'P   '
C
      CALL PACK( NAMEP(1), 1, IDPQ(1) )
      CALL SEARCH( NAMEP(1), LENG, ISW )
      IF ( ISW.EQ.0 ) GO TO 240
C
C
  230 WRITE(NOUT1,7020) IDENT
 7020 FORMAT(' *** READ ERROR ENCOUNTERED AT ID=',2A4,'  RETRY ]')
      GO TO 220
C
  240 CONTINUE
C
      CALL UFLCON( NAMEP, LOCAM, LOCAF, NOUT2, NEFL )
      CALL PACK( NAMEP(1), 1, IDPQ(2) )
      CALL CLEA( AA, 17000, 0.0 )
C
      CALL READ( NAMEP(1), AA, LTOT )
      CALL PACK( NAMEP(1), 1, IDPQ(1) )
C
      NFILE(1)    = 'FAST'
      NFILE(2)    = 'U   '
      CALL SEARCH( NAMEP(1), LENG, ISW )
C
      IF ( ISW.EQ.0 ) THEN
C@MOD               WRITE(NOUT1,7008) IDENT
         IF ( IDENT(2).EQ.IDZERO ) WRITE(NOUT1,7040) IDENT
         GO TO 210
      END IF
C
 7040 FORMAT(' ','  CAUTION --- ',2A4,' ALREADY EXISTS IN USER-FILE ')
C
C     INFINITE DILUTION X-SECTION COLLAPSE START
C
      CALL CLEA( SIG1D, 592, 0.0 )
      CALL CLEA( BSIGC, 592, 0.0 )
      DO 260 MT = 1, 7
         IST     = LOCAM(MT)
         IF ( IST.LE.0 ) GO TO 260
         DO 250 I = 1, NEFL
            SIG1D(I,MT) = AA(IST)
            IST     = IST + 1
  250    CONTINUE
  260 CONTINUE
C
C ----WEIGHTING
      CALL COLLAP( NEFL4, NREG, 0, WF, WR, AF, AF, AF )
C ----CAPTURE
      ISW     = 0
      IF ( ICAPT.EQ.0 ) GO TO 280
C
      CALL COLLAP( NEFL4, NREG, 1, WF, WR, FSIGC, BSIGC, AF )
      DO 270 I = 1, NEF
         ISW     = ISW + 1
  270 AA(ISW) = BSIGC(I)
C ----FISSION
  280 CONTINUE
      IF ( IFISS.EQ.0 ) GO TO 320
C
      CALL COLLAP( NEFL4, NREG, 1, WF, WR, FSIGF, BSIGF, AF )
      DO 290 I = 1, NEF
         ISW     = ISW + 1
  290 AA(ISW) = BSIGF(I)
C ----NU
      CALL COLLAP( NEFL4, NREG, 2, WF, AR, FSIGF, BUNU, FUNU )
      DO 300 I = 1, NEF
         ISW     = ISW + 1
  300 AA(ISW) = BUNU(I)
C ----SPECTRUM
      CALL COLLAP( NEFL4, NREG, 0, FCHI, BCHI, AF, AF, AF )
      DO 310 I = 1, NEF
         ISW     = ISW + 1
  310 AA(ISW) = BCHI(I)
C -----TOTAL
  320 CONTINUE
      CALL COLLAP( NEFL4, NREG, 1, WF, WR, FSIGT, BSIGT, AF )
      DO 330 I = 1, NEF
         ISW     = ISW + 1
  330 AA(ISW) = BSIGT(I)
C ----WEIGHTING
      DO 340 I = 1, NEF
         ISW     = ISW + 1
  340 AA(ISW) = WR(I)
C ----ELASTIC
      CALL COLLAP( NEFL4, NREG, 1, WF, WR, FSIGE, BSIGE, AF )
      DO 350 I = 1, NEF
         ISW     = ISW + 1
  350 AA(ISW) = BSIGE(I)
C
      LDKEEP  = LD(3)
      LAKEEP  = LA(3)
C ---- MATRIX DATA
      LTOT    = 0
      NEG     = NEFL4 + NETL - NETL1 + 1
      IF ( NETL.EQ.0 ) NEG    = NEFL4
CADD
      IF ( NET.EQ.0 ) NEG = NEFL4
*     WRITE(NOUT2,7009) NEG
*7009 FORMAT(1H ,' ##NEG## ',I6)
C     MATRICES
      DO 400 J = 1, 4
         IF ( LTH(J).LE.0 ) GO TO 400
C     AA TO BB
         CALL CLEA( BB, 7918, 0.0 )
C----- ADDED BY JAIS K.KANEKO   1/26/1983 ------------------------------
         IF ( J.EQ.3 ) CALL CLEA( STRE, 7918, 0.0 )
C-----------------------------------------------------------------------
         LTHR    = 0
         LOC     = LOCAM(7+J)
         LLEXT   = LA(J)
         IF ( NETL.EQ.0.AND.LLEXT.GT.NEFL4 ) LLEXT   = NEFL4
         DO 370 N = 1, LLEXT
            NJ      = NREG(N)
            NK      = NJ
            DO 360 K = N, N + LD(J)
               SAVE    = AA(LOC)
C -----  ADDED BY JAIS K.KANEKO 1/26/1983 ------------------------------
CM    IF( J .EQ. 3 ) STRE( K,N ) = SAVE
               IF ( J.EQ.3.AND.K.LE.NEG ) STRE(K,N)    = SAVE
               IF ( J.EQ.3.AND.K.GT.NEG ) STRE(NEG,N)  = STRE(NEG,N)
     &            + SAVE
C     IF(J.EQ.3.AND.K.EQ.N+1) FSIGB(N)=SAVE
               IF ( J.EQ.3.AND.K.EQ.N ) FSIGB(N)   = FSIGE(N) - SAVE
C ----------------------------------------------------------------------
               LOC     = LOC + 1
               IF ( SAVE.EQ.0.0 ) GO TO 360
               KK      = K
               IF ( K.GT.NEG ) KK  = NEG
               NK      = NREG(KK)
               BB(NK,NJ)   = BB(NK,NJ) + SAVE*WF(N)
  360       CONTINUE
            LTHR    = MAX0(LTHR,NK-NJ)
  370    CONTINUE
C
C     BB TO AA
C
         IF ( NJ.GT.NEF ) NJ = NEF
         LA(J)   = NJ
         LD(J)   = LTHR
C     WRITE(NOUT2,7004) NJ,LTHR
 7060    FORMAT(' ',' ##NJ LTHR## ',2I6)
         DO 390 NJ = 1, LA(J)
            DO 380 NK = NJ, NJ + LTHR
               ISW     = ISW + 1
               AA(ISW) = 0.0
               IF ( NK.GT.NEF+NET ) GO TO 380
               SAVE    = BB(NK,NJ) /WR(NJ)
               AA(ISW) = SAVE
C ----- CHANGED BY JAIS K.KANEKO  1/26/1983 ----------------------------
C     IF(J.EQ.3.AND.NK.EQ.NJ+1) BSIGB(NJ)=SAVE
               IF ( J.EQ.3.AND.NK.EQ.NJ ) BSIGB(NJ)    = BSIGE(NJ) -
     &            SAVE
C ----------------------------------------------------------------------
  380       CONTINUE
  390    CONTINUE
C
         LTH(J)  = LA(J)*(LTHR+1)
         LTOT    = LTOT + LTH(J)
  400 CONTINUE
C ----OUTPUT TO PDS-FILE
      LTOT    = LTOT + NEF*(3*IFISS+ICAPT+3)
      KEEP1   = NGMIN
      KEEP2   = NGMAX
      NGMIN   = 0
      NGMAX   = 0
      IF ( KEEP1.GT.0 ) NGMIN = NREG(KEEP1)
      IF ( KEEP2.GT.0 ) NGMAX = NREG(KEEP2)
      IF ( NGMAX.GT.NEF ) NGMAX   = NEF
      IF ( NGMIN.LE.NEF ) GO TO 410
      NGMIN   = 0
      NGMAX   = 0
      IFS     = 0
      IFTR    = 0
      IFC     = 0
      IFF     = 0
      IFE     = 0
      IFER    = 0
C     WRITE(NOUT2,7002) ISW,LTOT,NTEMP,NSIG,KEEP1,KEEP2,NGMIN,NGMAX
C7002 FORMAT(1H ,' ##ISW LTOT NTEMP NSIG KEEP1 KEEP2 NGMIN NGMAX ## ',
C    +8I5)
C
C
  410 CALL PACK( NAMEP(1), 1, IDPQ(1) )
C ----CONT
C     CALL   WRITE(NAMEP(1),IA,41)
C
      CALL WRITE( NAMEP(1), IA, 42 )
C
      CALL PACK( NAMEP(1), 1, IDPQ(2) )
C ----MATRIX
C
      CALL WRITE( NAMEP(1), AA, ISW )
C
C     COLLPASE FOR CHI-MATRIX
C
      IF ( IFISS.EQ.0 ) GO TO 480
      NFILE(1)    = 'FAST'
      NFILE(2)    = 'P   '
C
      CALL PACK( NAMEP(1), 1, IDXX )
      CALL SEARCH( NAMEP(1), LENG, ISW )
C
      IF ( ISW.EQ.1 ) GO TO 480
      CALL CLEA( FCHIMT, 5476, 0.0 )
      CALL CLEA( BCHIMT, 5476, 0.0 )
C
      CALL READ( NAMEP(1), AA, LENG )
      ISW     = 0
      DO 420 I = 1, NEFL
         DO 420 J = 1, NEFL
            ISW     = ISW + 1
            FCHIMT(J,I) = AA(ISW)
  420 CONTINUE
C
      CALL CLEA( AA, 17000, 0.0 )
      JSW     = 0
      DO 470 I = 1, NEFL4
         CALL COLLAP( NEFL4, NREG, 0, FCHIMT(1,I), BCHI, AF, AF, AF )
         WTNOW   = FSIGF(I)*FUNU(I)*WF(I)
         NOW     = NREG(I)
         IF ( I.LT.NEFL4 ) NEXT  = NREG(I+1)
         DO 430 J = 1, NEF
            BCHIMT(J,NOW)   = BCHIMT(J,NOW) + WTNOW*BCHI(J)
  430    CONTINUE
*     IF(I.EQ.1) THEN
*                WRITE(6,*) ' ** NEFL NEFL4 NEF ** ',NEFL,NEFL4,NEF
*                WRITE(6,7195) (NREG(J),J=1,NEFL4)
*                ENDIF
*     WRITE(6,*) ' ** I NEXT NOW JSW WTNOW ** ',I,NEXT,NOW,JSW,WTNOW
*     IF(NOW.EQ.NEXT.AND.I.NE.NEFL4) GO TO 7200
C
         SUMCHI  = 0.0
         DO 440 J = 1, NEF
            SUMCHI  = SUMCHI + BCHIMT(J,NOW)
  440    CONTINUE
         IF ( SUMCHI.GT.0.0 ) THEN
            DO 450 J = 1, NEF
               BCHIMT(J,NOW)   = BCHIMT(J,NOW) /SUMCHI
               JSW     = JSW + 1
               AA(JSW) = BCHIMT(J,NOW)
  450       CONTINUE
         ELSE
            DO 460 J = 1, NEF
               BCHIMT(J,NOW)   = BCHI(J)
               JSW     = JSW + 1
               AA(JSW) = BCHIMT(J,NOW)
  460       CONTINUE
         END IF
  470 CONTINUE
C-----OUTPUT
      NFILE(1)    = 'FAST'
      NFILE(2)    = 'U   '
      CALL WRITE( NAMEP(1), AA, JSW )
C
*     WRITE(6,'(A,2A4)') ' ** CHI MATRIX OF ',NAMEP
*     DO 7180 I = 1 ,NEF
*     IF(MOD(I,10).NE.1.AND.I.NE.NEF) GO TO 7180
*     WRITE(6,*) ' ** SOURCE GROUCP NO => ',I
*     WRITE(6,7190) (BCHIMT(J,I),J=1,NEF)
*7180 CONTINUE
C
 7080 FORMAT(' ',' ## BCHIMT ## ',10F10.5)
 7100 FORMAT(' ',' ## NEG ## ',20I6)
C
C     COLLAPSE FOR F-TABLE
C
  480 CONTINUE
      IF ( IFS.EQ.0 ) GO TO 990
      CALL CLEA( AA, 17000, 0.0 )
C
      NFILE(1)    = 'FAST'
      NFILE(2)    = 'P   '
C
C
      CALL PACK( NAMEP(1), 1, IDPQ(3) )
      CALL SEARCH( NAMEP(1), LENG, ISW )
      IF ( ISW.EQ.1 ) GO TO 990
C
      ICNT    = 0
      CALL CLEA( AA, 17000, 0.0 )
CADD
C     WRITE(6,239) ' ** LENGTH OF ',NAMEP,' IS ',LENG
C 239 FORMAT(1H ,5X,A,2A4,A,I10)
C
      IF ( LENG.NE.LOCAF(7) ) THEN
         WRITE(6,*) ' ** LENGTH OF ', NAMEP, ' IS UNMATCH  ]] '
         WRITE(6,*) ' ** LENGTH OF DEFINITION IS ', LOCAF(7)
         WRITE(6,*) ' ** LENGTH FROM SEARCH   IS ', LENG
         LENG    = LOCAF(7)
      END IF
CEND
C
      CALL READ( NAMEP(1), AA, LENG )
C
C ----WEIGHTING PRODUCTION
C
      CALL CLEA( FACT, 32, 1.0 )
C
      IF ( IRES.EQ.0 ) THEN
         DO 490 I = 1, NEFL
            DO 490 K = 1, NTEMP
               DO 490 J = 1, NSIG
                  WTF(I,J,K)  = WF(I) /(FSIGT(I)+SIG0(J))
  490    CONTINUE
C
      ELSE
         DO 500 I = 1, NEFL
            DO 500 K = 1, NTEMP
               DO 500 J = 1, NSIG
                  WTF(I,J,K)  = WF(I)
  500    CONTINUE
C
         ISW     = LOCAF(6) - 1
         DO 520 K = 1, NTEMP
            DO 510 J = 1, NSIG
               ISW     = ISW + 1
               RSAVE   = 1.000
               IF ( AA(ISW).GT.0.0 ) RSAVE = 1.0000/AA(ISW)
CM                  FACT(J,K)=WF(KEEP1)/AA(ISW)
               FACT(J,K)   = WF(KEEP1)*RSAVE
  510       CONTINUE
  520    CONTINUE
      END IF
C
C     WRITE(NOUT2,7003)  FACT
C7003 FORMAT(1H ,' ##FACT## ',8F8.5)
C
      ISW     = LOCAF(6)
      DO 530 I = KEEP1, KEEP2
         DO 530 K = 1, NTEMP
            DO 530 J = 1, NSIG
               WTF(I,J,K)  = FACT(J,K)*AA(ISW)
  530 ISW     = ISW + 1
C ----TRANSPORT
      IF ( IFTR.EQ.0 ) GO TO 600
      CALL CLEA( FCROS, 2368, 1.0 )
      ISW     = LOCAF(1)
      IF ( ISW.LE.0 ) GO TO 600
      DO 540 I = KEEP1, KEEP2
         DO 540 K = 1, NTEMP
            DO 540 J = 1, NSIG
               FCROS(I,J,K)    = AA(ISW)
  540 ISW     = ISW + 1
C
      DO 550 I = 1, NEFL
         DO 550 K = 1, NTEMP
            DO 550 J = 1, NSIG
               SAVE    = FCROS(I,J,K)*FSIGT(I)
               FCROS(I,J,K)    = SAVE
               FINE(I,J,K) = WTF(I,J,K) /(SAVE+SIG0(J))
  550 CONTINUE
C
      DO 560 K = 1, NTEMP
         DO 560 J = 1, NSIG
            CALL COLLAP( NEFL4, NREG, 0, FINE(1,J,K), WTB(1,J,K), AF,
     &              AF, AF )
            CALL COLLAP( NEFL4, NREG, 1, FINE(1,J,K), WTB(1,J,K),
     &              FCROS(1,J,K), BCROS(1,J,K), AF )
  560 CONTINUE
C
      DO 590 I = NGMIN, NGMAX
         SAVE    = BSIGT(I)
         IF ( SAVE.GT.0.0 ) THEN
            DO 570 K = 1, NTEMP
               DO 570 J = 1, NSIG
                  ICNT    = ICNT + 1
                  AA(ICNT)    = BCROS(I,J,K) /SAVE
  570       CONTINUE
         ELSE
            DO 580 K = 1, NTEMP
               DO 580 J = 1, NSIG
                  ICNT    = ICNT + 1
                  AA(ICNT)    = 1.00
  580       CONTINUE
         END IF
  590 CONTINUE
C-----WEIGHTING
  600 CONTINUE
      DO 610 K = 1, NTEMP
         DO 610 J = 1, NSIG
            CALL COLLAP( NEFL4, NREG, 0, WTF(1,J,K), WTB(1,J,K), AF, AF,
     &              AF )
  610 CONTINUE
C ----CAPTURE
      IF ( IFC.EQ.0 ) GO TO 680
      CALL CLEA( FCROS, 2368, 1.0 )
      ISW     = LOCAF(2)
      IF ( ISW.LE.0 ) GO TO 680
      DO 620 I = KEEP1, KEEP2
         DO 620 K = 1, NTEMP
            DO 620 J = 1, NSIG
               FCROS(I,J,K)    = AA(ISW)
  620 ISW     = ISW + 1
C
      DO 630 I = 1, NEFL
         DO 630 K = 1, NTEMP
            DO 630 J = 1, NSIG
               FCROS(I,J,K)    = FCROS(I,J,K)*FSIGC(I)
  630 CONTINUE
C
      DO 640 K = 1, NTEMP
         DO 640 J = 1, NSIG
            CALL COLLAP( NEFL4, NREG, 1, WTF(1,J,K), WTB(1,J,K),
     &              FCROS(1,J,K), BCROS(1,J,K), AF )
  640 CONTINUE
C
      DO 670 I = NGMIN, NGMAX
         SAVE    = BSIGC(I)
         IF ( SAVE.GT.0.0 ) THEN
            DO 650 K = 1, NTEMP
               DO 650 J = 1, NSIG
                  ICNT    = ICNT + 1
                  AA(ICNT)    = BCROS(I,J,K) /SAVE
  650       CONTINUE
         ELSE
            DO 660 K = 1, NTEMP
               DO 660 J = 1, NSIG
                  ICNT    = ICNT + 1
                  AA(ICNT)    = 1.0
  660       CONTINUE
         END IF
  670 CONTINUE
C ----FISSION
  680 IF ( IFF.EQ.0 ) GO TO 750
      CALL CLEA( FCROS, 2368, 1.0 )
      ISW     = LOCAF(3)
      IF ( ISW.LE.0 ) GO TO 750
      DO 690 I = KEEP1, KEEP2
         DO 690 K = 1, NTEMP
            DO 690 J = 1, NSIG
               FCROS(I,J,K)    = AA(ISW)
  690 ISW     = ISW + 1
C
      DO 700 I = 1, NEFL
         DO 700 K = 1, NTEMP
            DO 700 J = 1, NSIG
               FCROS(I,J,K)    = FCROS(I,J,K)*FSIGF(I)
  700 CONTINUE
C
      DO 710 K = 1, NTEMP
         DO 710 J = 1, NSIG
            CALL COLLAP( NEFL4, NREG, 1, WTF(1,J,K), WTB(1,J,K),
     &              FCROS(1,J,K), BCROS(1,J,K), AF )
  710 CONTINUE
C
      DO 740 I = NGMIN, NGMAX
         SAVE    = BSIGF(I)
         IF ( SAVE.GT.0.0 ) THEN
            DO 720 K = 1, NTEMP
               DO 720 J = 1, NSIG
                  ICNT    = ICNT + 1
                  AA(ICNT)    = BCROS(I,J,K) /SAVE
  720       CONTINUE
         ELSE
            DO 730 K = 1, NTEMP
               DO 730 J = 1, NSIG
                  ICNT    = ICNT + 1
                  AA(ICNT)    = 1.0
  730       CONTINUE
         END IF
  740 CONTINUE
C ----- ADDED BY JAIS K.KANEKO  1/26/1983  -----------------------------
  750 CALL CLEA( FTE, 2368, 1.0 )
C ----------------------------------------------------------------------
C ----ELASTIC
      IF ( IFE.EQ.0 ) GO TO 820
      CALL CLEA( FCROS, 2368, 1.0 )
      ISW     = LOCAF(4)
      IF ( ISW.LE.0 ) GO TO 820
      DO 760 I = KEEP1, KEEP2
         DO 760 K = 1, NTEMP
            DO 760 J = 1, NSIG
               FCROS(I,J,K)    = AA(ISW)
C ----- ADDED BY JAIS K.KANEKO  1/26/1983  -----------------------------
               FTE(I,J,K)  = AA(ISW)
C ----------------------------------------------------------------------
  760 ISW     = ISW + 1
C
      DO 770 I = 1, NEFL
         DO 770 K = 1, NTEMP
            DO 770 J = 1, NSIG
               FCROS(I,J,K)    = FCROS(I,J,K)*FSIGE(I)
  770 CONTINUE
C
      DO 780 K = 1, NTEMP
         DO 780 J = 1, NSIG
            CALL COLLAP( NEFL4, NREG, 1, WTF(1,J,K), WTB(1,J,K),
     &              FCROS(1,J,K), BCROS(1,J,K), AF )
  780 CONTINUE
C
      DO 810 I = NGMIN, NGMAX
         SAVE    = BSIGE(I)
         IF ( SAVE.GT.0.0 ) THEN
            DO 790 K = 1, NTEMP
               DO 790 J = 1, NSIG
                  ICNT    = ICNT + 1
                  AA(ICNT)    = BCROS(I,J,K) /SAVE
  790       CONTINUE
         ELSE
            DO 800 K = 1, NTEMP
               DO 800 J = 1, NSIG
                  ICNT    = ICNT + 1
                  AA(ICNT)    = 1.0
  800       CONTINUE
         END IF
  810 CONTINUE
C ----ELASTIC REMOVAL
  820 IF ( IFER.EQ.0 ) GO TO 970
      CALL CLEA( FCROS, 2368, 1.0 )
C ----- ADDED BY JAIS K.KANEKO  1/26/1983  -----------------------------
      CALL CLEA( FTB, 2368, 1.0 )
C ----------------------------------------------------------------------
C     WRITE(NOUT2,7005) FSIGB
C     WRITE(NOUT2,7006) BSIGB
 7120 FORMAT(' ',' ##FSIGB## ',1P10E11.4)
 7140 FORMAT(' ',' ##BSIGB## ',1P10E11.4)
C
      ISW     = LOCAF(5)
      IF ( ISW.LE.0 ) GO TO 970
      DO 830 I = KEEP1, KEEP2
         DO 830 K = 1, NTEMP
            DO 830 J = 1, NSIG
               FCROS(I,J,K)    = AA(ISW)
C ----- ADDED BY JAIS K.KANEKO  1/26/1983  -----------------------------
               FTB(I,J,K)  = AA(ISW)
C ----------------------------------------------------------------------
  830 ISW     = ISW + 1
C
      DO 840 I = 1, NEFL
         DO 840 K = 1, NTEMP
            DO 840 J = 1, NSIG
               FCROS(I,J,K)    = FCROS(I,J,K)*FSIGB(I)
  840 CONTINUE
C
      DO 860 K = 1, NTEMP
         DO 860 J = 1, NSIG
            DO 850 I = 1, NEFL4 - 1
               NJ      = NREG(I)
               NJ1     = NREG(I+1)
               IF ( NJ1.EQ.NJ ) GO TO 850
               BCROS(NJ,J,K)   = FCROS(I,J,K)*WTF(I,J,K)
  850       CONTINUE
            BCROS(NEF,J,K)  = FCROS(NEFL4,J,K)*WTF(NEFL4,J,K)
            DO 860 I = 1, NEF
               BCROS(I,J,K)    = BCROS(I,J,K) /WTB(I,J,K)
  860 CONTINUE
C ----- ADDED BY JAIS K.KANEKO  1/26/1983  -----------------------------
C     WRITE(6,317) NTEMP,NSIG,LLEXT,LDKEEP,NEG,(NREG(IA1),IA1=1,107)
C 317 FORMAT(1H ,100('-'),//,11X,'NTEMP NSIG LLEXT LDKEEP NEG=',5I11,//,
C    *                     11(3X,'NREG =',10I12,/))
      IF ( LDKEEP.LE.1 ) GO TO 930
      DO 920 K = 1, NTEMP
         DO 920 N = 1, NSIG
            CALL CLEA( STR, 7918, 0.0 )
CMOVE CALL CLEA( STRB, 7918, 0.0 )
            DO 880 J = 1, LAKEEP
               STR(J,J)    = FSIGE(J)*FTE(J,N,K) - FSIGB(J)*FTB(J,N,K)
CADD
               MXDOWN  = J + LDKEEP
               IF ( MXDOWN.GT.NEG ) MXDOWN = NEG
CMODJ          DO 311 I = J+1 , J+LDKEEP
               DO 870 I = J + 1, MXDOWN
                  STR(I,J)    = STRE(I,J)*FTB(J,N,K)
  870          CONTINUE
  880       CONTINUE
C
            LLEXT   = LAKEEP
            IF ( NETL.EQ.0.AND.LLEXT.GT.NEFL4 ) LLEXT   = NEFL4
CMOVE
            CALL CLEA( STRB, 7918, 0.0 )
C
            DO 910 J = 1, LLEXT
               NJ      = NREG(J)
CADD
               MXDOWN  = J + LDKEEP
               IF ( MXDOWN.GT.NEG ) MXDOWN = NEG
               WWWW    = WTF(J,N,K)
CMODJ          DO 313 I = J , J+LDKEEP
               DO 890 I = J, MXDOWN
                  SAVE    = STR(I,J)
                  IF ( SAVE.EQ.0.0 ) GO TO 890
                  JJ      = I
CDELETE           IF(I    .GT. NEG ) JJ = NEG
                  NK      = NREG(JJ)
CMODJ             STRB(NK,NJ) = STRB(NK,NJ) + SAVE*WTF(J,N,K)
                  STRB(NK,NJ) = STRB(NK,NJ) + SAVE*WWWW
  890          CONTINUE
               IF ( J.LT.LLEXT.AND.NREG(J+1).EQ.NJ ) GO TO 910
               IF ( NJ.GT.NEF ) GO TO 910
               SAVE    = 0.0
               DO 900 I = NJ + 1, NK
                  SAVE    = SAVE + STRB(I,NJ)
  900          CONTINUE
               BCROS(NJ,N,K)   = SAVE/WTB(NJ,N,K)
C
  910       CONTINUE
C
  920 CONTINUE
  930 CONTINUE
C ----- ADDITION ENDED -------------------------------------------------
      DO 960 I = NGMIN, NGMAX
         SAVE    = BSIGB(I)
         IF ( SAVE.GT.0.0 ) THEN
            DO 940 K = 1, NTEMP
               DO 940 J = 1, NSIG
                  ICNT    = ICNT + 1
                  AA(ICNT)    = BCROS(I,J,K) /SAVE
CM    IF(I.EQ.NGMAX.AND.AA(ICNT).EQ.0.0)   AA(ICNT) = 1.0
                  IF ( AA(ICNT).LE.0.0 ) AA(ICNT) = 1.0
  940       CONTINUE
         ELSE
            DO 950 K = 1, NTEMP
               DO 950 J = 1, NSIG
                  ICNT    = ICNT + 1
                  AA(ICNT)    = 1.0
  950       CONTINUE
         END IF
  960 CONTINUE
C ----WEIGHTING STORE
  970 CONTINUE
      DO 980 I = NGMIN, NGMAX
         DO 980 K = 1, NTEMP
            DO 980 J = 1, NSIG
               ICNT    = ICNT + 1
               AA(ICNT)    = WTB(I,J,K)
  980 CONTINUE
C
      NFILE(1)    = 'FAST'
      NFILE(2)    = 'U   '
C
      CALL WRITE( NAMEP(1), AA, ICNT )
C
  990 CONTINUE
C     IF(IRES.EQ.0)  GO TO 100
      IF ( IRES.EQ.0 ) GO TO 1080
C ---- RESONANCE PARAMETER
      DO 1070 JJ = 1, 6
         NFILE(1)    = 'FAST'
         NFILE(2)    = 'P   '
C
         GO TO(1000,1010,1020,1030,1040,1050), JJ
 1000    CALL PACK( NAMEP(1), 1, IDPQ(4) )
         GO TO 1060
 1010    CALL PACK( NAMEP(1), 1, IDPQ(5) )
         GO TO 1060
 1020    CALL PACK( NAMEP(1), 1, IDPQ(5) )
         CALL PACK( NAMEP(2), 4, IDPQ(10) )
         GO TO 1060
 1030    CALL PACK( NAMEP(1), 1, IDPQ(6) )
         CALL PACK( NAMEP(2), 4, IDPQ(7) )
         GO TO 1060
 1040    CALL PACK( NAMEP(1), 1, IDPQ(6) )
         CALL PACK( NAMEP(2), 4, IDPQ(8) )
         GO TO 1060
 1050    CALL PACK( NAMEP(1), 1, IDPQ(6) )
         CALL PACK( NAMEP(2), 4, IDPQ(9) )
C
 1060    CALL SEARCH( NAMEP(1), LENG, ISW )
         IF ( ISW.EQ.1 ) GO TO 1070
C
         CALL READ( NAMEP(1), AA, LENG )
         NFILE(1)    = 'FAST'
         NFILE(2)    = 'U   '
         CALL WRITE( NAMEP(1), AA, LENG )
 1070 CONTINUE
C
C ---- DELAYED NEUTRON DATA  (YZZM0000)  IFISS=1 ONLY
C
 1080 CONTINUE
C     IF (IFISS.EQ.0) GO TO 601
      NFILE(1)    = 'FAST'
      NFILE(2)    = 'P   '
C
      CALL PACK( NAMEP(1), 1, '   Y' )
      NAMEP(2)    = IDZERO
      CALL SEARCH( NAMEP(1), LENG, ISW )
      IF ( ISW.EQ.1 ) GO TO 1130
C
      CALL READ( NAMEP(1), AA, LENG )
CIBM  IF(NAMEP(1).EQ.'YD02') THEN
CMSASA
      IF ( NAMEP(1).EQ.'YD02' ) THEN
         NFAMLY  = 9
         CALL IVALUE( BUNU, NEF, 0.0 )
         CALL IVALUE( DCHI, NEF*NFAMLY, 0.0 )
      ELSE
         NFAMLY  = 6
C ---- DELAYED NEU VALUE
         CALL COLLAP( NEFL4, NREG, 2, WF, AR, FSIGF, BUNU, AA(15) )
C ---- DELAYED FISSION YIELD
         DO 1090 I = 1, NFAMLY
            LOC     = I*NEFL + 4 + NFAMLY*2
            CALL COLLAP( NEFL4, NREG, 0, AA(LOC), DCHI(1,I), AF, AF, AF
     &              )
 1090    CONTINUE
      END IF
C -- STORE ARAY AA
      DO 1100 I = 1, NEF
         AA(2+NFAMLY*2+I)    = BUNU(I)
 1100 CONTINUE
      AA(NEF+3+NFAMLY*2)  = AA(NEFL+3+NFAMLY*2)
      LOC     = NEF + 3 + NFAMLY*2
      DO 1120 I = 1, NFAMLY
         DO 1110 J = 1, NEF
            LOC     = LOC + 1
            AA(LOC) = DCHI(J,I)
 1110    CONTINUE
 1120 CONTINUE
      NFILE(1)    = 'FAST'
      NFILE(2)    = 'U   '
      CALL WRITE( NAMEP, AA, LOC )
C
 1130 CONTINUE
      IF ( IPL.GT.1 ) CALL UFLMPL( NAMEP, AA, WF, NREG, BB, WR, IPL,
     &   LAKEEP, LDKEEP, NEG, NETL, NEFL4, LA(3), LD(3), NEF, NET )
C
      GO TO 210
C
C==== STANDARD TEMPERATURE ARRAY DEFINITION === ADDED 4/14/1983
C
 1140 CONTINUE
      WRITE(NOUT1,7160)
      CALL REAG( STND, 35, 'STND', ' TMP' )
      NTDUMY  = 0
      DO 1150 N = 1, 35
         IF ( STND(N).GT.0.0 ) NTDUMY    = N
 1150 CONTINUE
C
      NFILE(1)    = 'FAST'
      NFILE(2)    = 'U   '
      CALL SEARCH( NMTEMP(1), LENG, ISW )
      IF ( ISW.EQ.0 ) CALL DELETE( NMTEMP(1) )
      CALL WRITE( NMTEMP(1), STND, 71 )
C
 7160 FORMAT(' ENTER STND(35) IN FREE FORMAT.',/' ',
     &      '  ( STND: STANDARD TEMPERATURE ARRAY(LOW TO HIGH ORDER) )')
C
      GO TO 210
C
 1160 WRITE(NOUT1,7180) MXLISO
      STOP
 7180 FORMAT('0','***** TOO MANY NUCLIDES MORE THAN ',I4,'*****',
     &        ' STOP EXECUTION ')
      END
