      SUBROUTINE CVMASW(B,IB,AB,IMX,MTNAME,NM,IGM,IHT,IHS,IHM,LIMB,
     *                  NT,NOU,MTP,MTT,ISCT1,NMP)
C
C  ***  CVMASN CONVERTS DATA POOL MACRO TO SN MACRO LIB.
C
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECOOD,TEMPRY
      COMMON /MAINC / III(1000)
C
      DIMENSION       B(LIMB),IB(LIMB),IMX(1),MTT(1)
      CHARACTER*4     AB(LIMB)
      CHARACTER*4     MTNAME(2,NM),IDENT(2)
      CHARACTER*4     EXTRA(24),IRANGE(3)
      CHARACTER*4     BLK,ICFL,ICF,FILENM
      CHARACTER*4     N2N,IDN2N(2)
C
      EQUIVALENCE    (III(64),NOUT1),(III(65),NOUT2),
     1               (III(98),IRANG),(III(99),ICFL),(III(77),ITYPE)
C
      DATA            BLK /'    '/
      DATA            EXTRA
     1               /'SRAC','   M','ACRO','SC  ','OPIC',' C  ','ROSS',
     2                'SE  ','CTIO','N   ',' FOR',' T  ','WOTR','AN  ',
     3                    10*'    '                            /
      DATA           IRANGE/'FAST','THER','ALL '/
C
C     START OF PROCESS
C
      NERR      = 0
C     WRITE(NOU,6000) MANNO,FILENM,ERANG,IGM,NM
      FILENM(1) = 'MACR'
      FILENM(2) = 'O '
      ICF       = ICFL
      IF(ICFL.NE.'0000')  FILENM(2) = 'OWRK'
      IDENT(1)  = 'CONT'
      IDENT(2)  = 'X00X'
      IDENT(2) (1:1) = IRANGE(IRANG+1) (1:1)
      IDENT(2) (4:4) = ICF             (4:4)
      CALL GETLEN(IDENT,  LENGTH)
      CALL READ  (IDENT,B,LENGTH)
      NGR       = IB(1)
      IF(IGM.LT.NGR)  THEN
                      WRITE(NOUT1,7010) IGM,NGR
                      STOP 7
                      ENDIF
C
      NUS      = 0
      NDS      = 0
      MTP      = 0
      I00      = 0
      ISW      = 0
C
      DO 130 I = 1,NMP
      MM1      = IMX(I)
      MM       = IABS(MM1)
      IF(ICFL.EQ.'0000') THEN
                         ICF='0000'
                         ELSE
                         IF(MM1 .GT. 0) ICF='0002'
                         IF(MM1 .LT. 0) ICF='0004'
                         ENDIF
C
      IDENT(1) = MTNAME(1,MM)
      IDENT(2) = MTNAME(2,MM)
      IDENT(2) (1:1) = IRANGE(IRANG+1) (1:1)
      MTP      = MTP + 1
      MTT(MTP) = MTP
C
      LPMAX    = ISCT1
      IF(LPMAX.GT.2) LPMAX = 2
C
CMOD  DO 120 J = 1,ISCT1
      DO 120 J = 1,LPMAX
      IF(J .NE. 1 ) THEN
                    IF( MM1. GT. 0) GO TO 120
                    IF( ICFL.EQ.'0000') THEN
                                        ICF='0001'
                                        ELSE
                                        ICF='0003'
                                        ENDIF
                    ENDIF
C
      IDENT(2) (4:4) = ICF (4:4)
      CALL SEARCH(IDENT,LENGTH,ISW)
      IF(ISW.EQ.1) THEN
                   WRITE(NOU,7030) IDENT,J-1
                   NERR = NERR + 1
                   GO TO 120
                   ENDIF
C
      CALL READ(IDENT,B,LENGTH)
      LTH      = LENGTH
      I00      = MAX0(I00,LTH)
      IF(LTH.GT.LIMB) THEN
                      WRITE(NOUT1,7020) LIMB,LTH
                      STOP 7
                      ENDIF
C
      IBP      = 0
      DO 115 L = 1,IGM
      NUS      = MAX0(NUS,IB(IBP+1)-1)
      NDS      = MAX0(NDS,IB(IBP+2)-IB(IBP+1))
      IBP      = IBP+IB(IBP+2)+10
  115 CONTINUE
  120 CONTINUE
  130 CONTINUE
C
C     END OF MATERIAL LOOP
C
      IF(NERR.GT.0) STOP 7
      IHT    = 6
      IHM    = IHT + NUS + NDS + 1
      IHS    = IHT + NUS +       1
CMSASA A GIMICK TO WALK AROUND A BUG IN HP FORTRAN 9.X/10.0
C      WHICH PRINTS VALUES OF IHT,IHM BEFORE DATA SUBSTITUTION ABOVE
C      WHEN COMPILED IN +O2 OPTIMIZATION OR HIGHER WITH +E7 OPTION
C      (SAVE DUMMY ARGUMENT VALUE).
C
C     WRITE(NOU,6030) IHT,IHM,IHS
      IHT00 = IHT
      IHM00 = IHM
      IHS00 = IHS
      WRITE(NOU,6030) IHT00,IHM00,IHS00
CEND
C
C ===   MACRO LIB. WRITE
C
      IZERO  = 0
      IONE   = 1
      ICRX   = I00 + 1
      ISCX   = ICRX + IGM * 7
      I01    = ISCX +(NUS+NDS+1)*IGM*ISCT1
      ISTN2N = I01 + 2*NMP
      I02    = ISTN2N + I00
      JBAND  = NUS + NDS + 1
      IF (I02.GT.LIMB) THEN
                       WRITE(NOUT1,7020) LIMB,I02
                       STOP 7
                       ENDIF
C
      REWIND NT
      WRITE (NT) ((FILENM(I),I=1,2),J=1,3),IZERO
      WRITE (NT) IGM,NMP,NUS,NDS,IZERO,IZERO,IONE,IONE,IONE
C === MATERIAL LOOP
      DO 160 I = 1,NMP
      MM1      = IMX(I)
      MM       = IABS(MM1)
      IF (ICFL.EQ.'0000') THEN
                          ICF = '0000'
                          N2N = '000N'
                          ELSE
                          N2N = '000M'
                          IF (MM1.LT.0) ICF = '0004'
                          IF (MM1.GT.0) ICF = '0002'
                          ENDIF
C
      IDENT(1) = MTNAME(1,MM)
      IDENT(2) = MTNAME(2,MM)
      IDENT(2) (1:1)    =  IRANGE(IRANG+1) (1:1)
      IDENT(2) (4:4)    = ICF (4:4)
      AB(I01+(I-1)*2  ) = IDENT(1)
      AB(I01+ I   *2-1) = IDENT(2)
  160 CONTINUE
      WRITE (NT) EXTRA,(AB(I),I=I01,I01+2*NMP-1)
C
      DO 290 I = 1,NMP
      CALL GETLEN(IB(I01+(I-1)*2),  LENGTH)
      CALL READ  (IB(I01+(I-1)*2),B,LENGTH)
C === READ (N.2N) DATA
      ISWN2N   = 0
      LENN2N   = 0
      IDN2N(1) = AB(I01+(I-1)*2)
      IDN2N(2) = AB(I01+(I-1)*2+1)
      IDN2N(2) (4:4) = N2N (4:4)
      CALL SEARCH (IDN2N,LENN2N,ISW)
*     WRITE(6,171) IDN2N,LENN2N,ISW
      IF(ISW.EQ.0) THEN
                   CALL CLEA ( B(ISTN2N), LENN2N    , 0.0    )
                   CALL READ ( IDN2N    , B(ISTN2N) , LENN2N )
                   ISWN2N = 1
                   ENDIF
      KN2N  = ISTN2N - 1
C
      IBP      = 0
      ISCXX    = ISCX - 1
C
C === LOOP OF ENERGY GROUP
C
      DO 210 L = 1,IGM
C **  FISSION CROSS SECTION
      B(I00+L+4*IGM) = B(IBP+ 4)
C **  NU
      B(I00+L+5*IGM) = 0.0
      IF(B(IBP+4).NE.0.0) B(I00+L+5*IGM)=B(IBP+5)/B(IBP+4)
C **  N2N
      TN2N     = 0.0
CM    IF(ISWN2N.GT.0) THEN
      IF(ISWN2N.GT.0.AND.LENN2N.GT.0) THEN
                      TN2N = B(KN2N+6)
                      IDEL = 10 + IB(KN2N+2)
*                     WRITE(6,172) L,KN2N,LENN2N,IDEL,TN2N
                      KN2N = KN2N + IDEL
                      LENN2N = LENN2N - IDEL
                      ENDIF
CTTT  B(I00+L+6*IGM) = TN2N
      B(I00+L+6*IGM) = 0.0
C     LENGTH OF SCATTERING VECTOR
      LGV11    = IB(IBP+2)+10
      SIGS     = 0.0
      DO 170 M = 11,LGV11
      SIGS     = SIGS + B(IBP+M)
  170 CONTINUE
C
C **  TRANSPORT CROSS SECTION  TEMPORARILY FILLED BY TOTAL
CMOD  B(I00+L)       = B(IBP+ 6)
      B(I00+L)       = B(IBP+10) + SIGS - TN2N
      B(I00+L+IGM)   = 0.0
C **  TOTAL
CMOD  B(I00+L+2*IGM) = B(IBP+ 6)
      B(I00+L+2*IGM) = B(IBP+10) + SIGS - TN2N
C     CAPTURE FILLED BY ABSORPTION -FISSION + N2N
CMOD  B(I00+L+3*IGM) = B(I00+L+3*IGM)-B(I00+L+4*IGM)+B(I00+L+6*IGM)
CTTT  B(I00+L+3*IGM) = B(IBP+10) - B(IBP+ 4) + TN2N
      B(I00+L+3*IGM) = B(IBP+10) - B(IBP+ 4) - TN2N
C
      DO 180  M  = 1,JBAND
      B(ISCXX+M) = 0.0
  180 CONTINUE
      IBPP       = 0
      DO 200   N = 1,IGM
      LSS        = IB(IBPP+1)
      LGV        = IB(IBPP+2)
      M1         = N-LSS+1
      M2         = N+LGV-LSS
      IF (L.LT.M1.OR.L.GT.M2) GO TO 190
      B(ISCXX+L-N+NUS+1) =B(IBPP+LSS+L-N+10)
  190 CONTINUE
      IBPP       = IBPP +IB(IBPP+2)+10
  200 CONTINUE
      IBP        = IBP+IB(IBP+2)+10
      ISCXX      = ISCXX + JBAND
  210 CONTINUE
C
      LORD  = 1
      IF(ISCT1.EQ.1.OR.IMX(I).GT.0) GO TO 280
C
C === P1-COMPONENT
C
      IF(ICFL.EQ.'0000') THEN
                         CALL PACK(IB(I01+I*2-1),4,'0001')
                         ELSE
                         CALL PACK(IB(I01+I*2-1),4,'0003')
                         ENDIF
C
      CALL GETLEN(IB(I01+(I-1)*2),  LENGTH)
      CALL READ  (IB(I01+(I-1)*2),B,LENGTH)
      ISCX1    = ISCX + JBAND*IGM
      ISCXX    = ISCX1 - 1
      DO 250 L = 1,IGM
      IBPP     = 0
      DO 220   M = 1,JBAND
      B(ISCXX+M) = 0.0
  220 CONTINUE
      DO 240 N = 1,IGM
      LSS      = IB(IBPP+1)
      LGV      = IB(IBPP+2)
      M1       = N-LSS+1
      M2       = N+LGV-LSS
      IF (L.LT.M1.OR.L.GT.M2) GO TO 230
      B(ISCXX+L-N+NUS+1) = B(IBPP+LSS+L-N+10)*0.3333333333
  230 CONTINUE
      IBPP     = IBPP + IB(IBPP+2)+10
  240 CONTINUE
      ISCXX    = ISCXX + JBAND
  250 CONTINUE
C
      IBPP     = 0
      DO 260 L = 1,IGM
      LSS      = IB(IBPP+1)
      LGV      = IB(IBPP+2)
C **  TRANSPORT CROSS SECTION FILLED BY TOTAL - P1_TOTAL
      B(I00+L) = B(I00+L) - B(IBPP+6)*0.3333333333
      IBPP     = IBPP + IB(IBPP+2)+10
  260 CONTINUE
C
C === HIGH ORDER-COMPONENT (PL>1)
C
      LORD  = 2
C
      IF(ISCT1.GT.2) THEN
                     IF(ICFL.EQ.'0000') THEN
                                        WRITE(NOUT1,7040) ISCT1-1
                                        WRITE(NOUT2,7040) ISCT1-1
                                        STOP 913
                                        ENDIF
C
                     DO 1280 LOP = 3 , ISCT1
                     IF(LOP.EQ.3) CALL PACK(IB(I01+I*2-1),4,'0005')
                     IF(LOP.EQ.4) CALL PACK(IB(I01+I*2-1),4,'0006')
                     IF(LOP.EQ.5) CALL PACK(IB(I01+I*2-1),4,'0007')
                     IF(LOP.EQ.6) CALL PACK(IB(I01+I*2-1),4,'0008')
                     IF(LOP.GT.6) CALL PACK(IB(I01+I*2-1),4,'000?')
                     CALL SEARCH(IDENT,LENGTH,ISW)
                     IF(ISW.EQ.1) GO TO 1290
                     LORD  = LOP
                     CALL READ  (IB(I01+(I-1)*2),B,LENGTH)
C
                     DO 1250 L = 1,IGM
                     IBPP      = 0
                     DO 1220  M = 1,JBAND
                     B(ISCXX+M) = 0.0
 1220                CONTINUE
                     FACT     = 1.000 / FLOAT(2*LOP-1)
                     DO 1240 N = 1,IGM
                     LSS      = IB(IBPP+1)
                     LGV      = IB(IBPP+2)
                     M1       = N-LSS+1
                     M2       = N+LGV-LSS
                     IF (L.LT.M1.OR.L.GT.M2) GO TO 1230
                     B(ISCXX+L-N+NUS+1) = B(IBPP+LSS+L-N+10)*FACT
 1230                CONTINUE
                     IBPP     = IBPP + IB(IBPP+2)+10
 1240                CONTINUE
                     ISCXX    = ISCXX + JBAND
 1250                CONTINUE
 1280                CONTINUE
 1290                CONTINUE
                     ENDIF
C
C     ISOTOPE RECORD WRITE
C
  280 CONTINUE
CMOD  LORD = 1
CMOD  IF (ISCT1.GT.1.AND.IMX(I).LT.0) LORD=2
      IJJ  = NUS + 1
      WRITE (NT) (BLK,J=1,4),(IZERO,J=1,4),IONE,IZERO,IONE,IZERO,IZERO,
     *       IONE,IZERO,IZERO,IZERO,LORD,(JBAND,J=1,IGM),(IJJ,J=1,IGM)
      WRITE (NT) (B(J),J=ICRX,ISCX-1)
      WRITE (NT) (B(J),J=ISCX,ISCX+LORD*JBAND*IGM-1)
      WRITE (NOU,6020) I,(IB(I01+(I-1)*2+J-1),J=1,2)
  290 CONTINUE
C
C     END OF PROCESS
C
      REWIND NT
      RETURN
C
  171 FORMAT(1H ,' ## MEMBER LENGTH ISW ## ',2A4,2X,2I6)
  172 FORMAT(1H ,' ## NG KN2N LENN2N IDEL T2N2 ## ',4I6,1PE12.5)
 6000 FORMAT('0  MACRO LIB. CONVERSION START FROM MACRO FILE ( ',
     1       A4,A1,'.',2A4,'.MACR.',2A4,' )'/6X,
     2       'NUMBER OF GROUPS',18X,I6/6X,
     3       'NUMBER OF MATERIALS',15X,I6/           )
 6020 FORMAT(I10,2X,2A4,' CONVERTED' )
 6030 FORMAT(6X,'POSITION SIGMA-T',18X,I6/6X,
     +      'TABLE LENGTH',22X,I6/6X,
     +      'POSITION SIGMA-GG',17X,I6/        )
 7010 FORMAT('0*** ERROR  IGM IS GREATER THAN NGR ( DATA POOL )',2I7)
 7020 FORMAT('0*** ERROR ALLOCATED ARRAY ( B ) ',I10,' REQUIRED LENGTH',
     *       ' OF MATRIX',I10 )
 7030 FORMAT('0*** ERROR  MATERIAL (',2A4,') IS NOT FOUND IN ',
     *       'DATA POOL OR READ ERROR. PL ORDER IS ',I2)
 7040 FORMAT('0*** ERROR  INPUT LEGENDRE ORDER IS ',I2,
     *   '. BUT IT MUST BE LESS THAN 2 IN BROAD GROUP CALCULATION ]] '/)
C
      END
