      SUBROUTINE   CVMASN
     *    (TOT,B,IB,IMX,MTNAME,NM,IGM,IHT,IHS,IHM,LIMB,
     *     NT,NOU,MTP,MTT,ISCT1,MZ,IZM,IM,MA,WT,FISG,FISGI,NMP)
C
C  ***  CVMASN CONVERTS DATA POOL MACRO TO SN MACRO LIB.
C
      CHARACTER*4 MTNAME,IRANGE,IDENT,BLK,NZERO,ICF,LCF,FILENM
C
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECOOD,TEMPRY
      COMMON /MAINC / III(1000)
C
      DIMENSION     TOT(2,IGM,NMP),B(LIMB),IB(LIMB),IMX(1),IRANGE(3),
     *              MTNAME(2,NM),IDENT(2),MTT(1),MZ(IZM),
     *              WT(1),FISG(1),FISGI(1),MA(1)
C
      EQUIVALENCE   (III(64),NOUT1),(III(65),NOUT2),
     1              (III(98),IRANG),(III(99),ICF),(III(77),ITYPE)
C
      DATA BLK       /'    '/
      DATA NZERO     /'0000'/
      DATA IRANGE    /'FAST','THER','ALL '/
C
C     START OF PROCESS
C
C     WRITE(NOU,6000) MANNO,FILENM,ERANG,IGM,NM
      WRITE(6,*) ' **** CVMASN **** '
      WRITE(6,*) ' **** ISCT1,IGM** ',ISCT1,IGM
      WRITE(6,*) ' **** NM,NMP **** ',NM,NMP
      WRITE(6,*) ' **** IMX    **** ',(IMX(M),M=1,NMP)
      NERR     = 0
      ISW      = 0
C
      FILENM(1)= 'MACR'
      FILENM(2)= 'OWRK'
      IF(ICF.EQ.NZERO) FILENM(2)='O   '
      IDENT(1) = 'CONT'
      IDENT(2) = 'X00X'
      CALL PACKX(IDENT(2),1,IRANGE(IRANG+1),1)
      CALL PACK (IDENT(2),4,ICF)
      CALL GETLEN(IDENT,LENGTH)
      CALL READ(IDENT,B,LENGTH)
      LCF  = ICF
      NGR  = IB(1)
      IF(ITYPE.EQ.0) THEN
                     DO 10 I=1,NGR
                     WT(I)=B(I+1)
   10                CONTINUE
                     ENDIF
C
      IF(IGM.EQ.NGR) GO TO 100
      WRITE(NOUT1,7010) IGM,NGR
      STOP 7
C
  100 CONTINUE
      NUS    = 0
      NDS    = 0
      MTP    = 0
      I00    = 0
      CALL CLEA (  TOT , 2*IGM*NMP , 0.0 )
C
      DO 130 I = 1,NMP
      MM1      = IMX(I)
      MM       = IABS(MM1)
      IF(ICF .EQ. NZERO) THEN
                         LCF='0000'
                         ELSE
                         IF(MM1 .GT. 0) LCF='0002'
                         IF(MM1 .LT. 0) LCF='0004'
                         ENDIF
      IDENT(1)=MTNAME(1,MM)
      IDENT(2)=MTNAME(2,MM)
      CALL PACKX(IDENT(2),1,IRANGE(IRANG+1),1)
C
      DO 120 J=1,ISCT1
      IF(J .GT. 1 ) THEN
CM           IF( MM1 .GT. 0) GO TO 120
             IF( MM1 .GT. 0) GO TO 125
             IF(ICF.EQ.NZERO ) THEN
                               LCF='0001'
                               IF(J.GE.3)  LCF ='000?'
                               ELSE
                               LCF='0003'
                               IF(J.EQ.3)  LCF ='0005'
                               IF(J.EQ.4)  LCF ='0006'
                               IF(J.EQ.5)  LCF ='0007'
                               IF(J.EQ.6)  LCF ='0008'
                               IF(J.GT.6)  LCF ='000?'
                               ENDIF
                    ENDIF
C
CM    MTP      = MTP + 1
CM    MTT(MTP) = MTP
C
      CALL PACK  (IDENT(2),     4,LCF)
      CALL SEARCH(IDENT   ,LENGTH,ISW)
      IF (ISW .EQ. 1)  THEN
                       WRITE (NOU,7030) IDENT,J-1
CM                     NERR = NERR + 1
                       IF(J.LE.2)  NERR = NERR + 1
CM                     GO TO 120
                       GO TO 125
                       ENDIF
C
      MTP      = MTP + 1
      MTT(MTP) = MTP
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
      IF(L+1-IB(IBP+1).GT.0)           NUS=MAX0(NUS,IB(IBP+1)-1)
      IF(L-IB(IBP+1)+IB(IBP+2).LE.IGM) NDS=MAX0(NDS,IB(IBP+2)-IB(IBP+1))
      IF(J.LE.2) TOT(J,L,I) = B(IBP+6)
      IBP      = IBP+IB(IBP+2)+10
  115 CONTINUE
  120 CONTINUE
  125 CONTINUE
  130 CONTINUE
C
 7010 FORMAT('0*** ERROR  IGM IS NOT EQUAL TO NGR ( MACRO FILE)',2I7)
 7020 FORMAT('0*** ERROR ALLOCATED ARRAY ( B ) ',I10,' REQUIRED LENGTH',
     *       ' OF MATRIX',I10 )
 7030 FORMAT('0*** ERROR  MATERIAL (',2A4,') IS NOT FOUND IN ',
     *       'DATA POOL OR READ ERROR. PL ORDER IS ',I2)
 9000 FORMAT(10X,'DO 1N 190 ','MM=',I3)
 9100 FORMAT(10X,'DO IN 180 J=',I5)
C
      IF(NERR.GT.0) STOP 7
C
      IHT    = 5
      IHM    = IHT + NUS + NDS + 1
      IHS    = IHT + NUS + 1
      I03    = NUS + NDS + 1
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
C    MACRO LIB. WRITE
C
      I01   = I00+IHM*IGM
      IF(I01.GT.LIMB) THEN
                      WRITE(NOUT1,7020) LIMB,I01
                      STOP 7
                      ENDIF
C
      CALL CLEA(FISG  , NMP*ISCT1 , 0.0 )
      CALL CLEA(FISGI , IM        , 0.0 )
      IH1      = IHM*IGM
      IC       = 0
C
      REWIND NT
C
C *** LOOP OF MATERIAL
C
      DO 190 I = 1,NMP
      CALL CLEA(B(I00+1),IH1,0.0)
      MM1      = IMX(I)
      MM       = IABS(MM1)
      IF(ICF .EQ. NZERO) THEN
                         LCF='0000'
                         ELSE
                         IF(MM1 .GT. 0) LCF='0002'
                         IF(MM1 .LT. 0) LCF='0004'
                         ENDIF
      IDENT(1) = MTNAME(1,MM)
      IDENT(2) = MTNAME(2,MM)
      CALL PACKX(IDENT(2),1,IRANGE(IRANG+1),1)
C      WRITE(99,9000) MM
      DO 180 J=1,ISCT1
      IF(J .GT. 1 ) THEN
                    IF( MM1 .GT. 0) GO TO 185
                    IF(ICF.EQ.NZERO ) THEN
                                      LCF = '0001'
                                      IF(J.GT.2)  LCF ='000?'
                                      ELSE
                                      LCF = '0003'
                                      IF(J.EQ.3)  LCF ='0005'
                                      IF(J.EQ.4)  LCF ='0006'
                                      IF(J.EQ.5)  LCF ='0007'
                                      IF(J.EQ.6)  LCF ='0008'
                                      IF(J.GT.6)  LCF ='000?'
                                      ENDIF
                    ENDIF
C      WRITE(99,9100) J
      ISW   = 0
      CALL PACK(IDENT(2),4,LCF)
      CALL SEARCH(IDENT   ,LENGTH,ISW)
      IF(ISW.EQ.1) GO TO 185
C
      IC    = IC + 1
      CALL READ(IDENT,B,LENGTH)
      IBP   = 0
      I99   = I00
C
      DO 170 L = 1,IGM
      LSS      = IB(IBP+1)
      LGV      = IB(IBP+2)
C **  ACTIVATION FILLED BY 1/SQRT(E)
      B(I99+1) = B(IBP+3)
C **  TRANSPORT CROSS SECTION
      IF(J.EQ.1) THEN
                 B(I99+IHT-3) = TOT(1,L,I)  - TOT(2,L,I)*0.333333333
                 ELSE
                 B(I99+IHT-3) = 0.0
                 ENDIF
C **  ABSORPTION CROSS SECTION
      B(I99+IHT-2)=B(IBP+10)
C **  NU* FISSION CROSS SECTION
      B(I99+IHT-1)=B(IBP+ 5)
CI--- 7/10/83 NUUTRON PRODUCTION FOR FISSION GUESS
      FISG(IC)    = FISG(IC) + B(IBP+5)*WT(L)
C **  TOTAL CROSS SECTION
      B(I99+IHT)  = B(IBP+ 6)
C **  SCATTERING VECTOR
      IBP         = IBP + 10
      DO 160 LS=1,LGV
C     IG * GROUP NUMBER OF SECONDARY NEUTRON
      IG=L+LS-LSS
      IF(IG.LT.1.OR.IG.GT.IGM) THEN
C     PSEUDO ABSORPTION
      B(I99+IHT-2) = B(I99+IHT-2) + B(IBP+LS)
                               ELSE
      IPOS    = (IG-1)*IHM + IHS + LS - LSS + I00
      B(IPOS) = B(IBP+LS)
                               ENDIF
  160 CONTINUE
      IBP     = IBP + LGV
      I99     = I99 + IHM
  170 CONTINUE
  175 CONTINUE
      K = J - 1
      WRITE(NT) IGM,IHM,IC,IC,IDENT,BLK,IDENT,(BLK,L=1,7)
      WRITE(NT) (B(L),L=I00+1,I01)
      WRITE(NOU,6020) IC,IDENT(1),IDENT(2),K
  180 CONTINUE
  185 CONTINUE
  190 CONTINUE
      REWIND NT
C
      IF(ITYPE.NE.0) GO TO 300
C
      REWIND 33
      DO 250 M = 1,IM
      IMA      = MA(M)
      IIM      = IABS(MZ(IMA))
      FISGI(M) = FISG(IIM)
  250 CONTINUE
      WRITE(33) (FISGI(I),I=1,IM)
C
C *** END OF PROCESS
C
  300 CONTINUE
      RETURN
C
 6000 FORMAT('0  MACRO LIB. CONVERSION START FROM MACRO FILE ( ',
     1       A4,A1,'.',2A4,'.MACR.',2A4,' )'/6X,
     2       'NUMBER OF GROUPS',18X,I6/6X,
     3       'NUMBER OF MATERIALS',15X,I6            )
 6020 FORMAT(I10,2X,2A4,2X,' P-',I2,' CONVERTED' )
 6030 FORMAT(6X,'POSITION SIGMA-T',18X,I6/6X,
     +      'TABLE LENGTH',22X,I6/6X,
     +      'POSITION SIGMA-GG',17X,I6/        )
C
      END
