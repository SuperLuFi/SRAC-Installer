C     THIS ROUTINE IS USED ONLY IN SRAC-CITATION AND NOT USED IN COREBN
C     PERTURBATION CALCULATION
C
CMODI SUBROUTINE PERTC(TERM  ,P1E   ,P2E   ,IOPT  ,IXYZ  ,
      SUBROUTINE PERTC(TERM4 ,P1E   ,P2E   ,IOPT  ,IXYZ  ,
     1                 SAMPLE,SIG2  ,SCAC2 ,XI2   ,BKLE  ,
     2                 IBKLGP,SIG   ,SCAC  ,NRGNE ,NCOMP ,
     3                 PVOL  ,MTNAME,MAT   ,IDOPT ,XII   ,
     4                 BUF   ,BND   ,BBND  ,P     ,
     5                 ICASE ,JMAX  ,IMAX  ,KBMAX ,JVX   ,
     6                 IVX   ,KBVX  ,JVXP1 ,IVXP1 ,KBVXP1,
     7                 KMAX  ,KVX   ,IOUT  ,LMAX  ,MMAX  ,
     8                 JIVX  ,IO18  ,IO26  ,XKEFF ,NMT   ,
     9                 NBUF  ,IDELAY,MEMPET,IX104 ,NDIM  ,
     A                 NUAC5 ,NGC13 ,IO14                  )
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
C
      REAL * 8 XII,BIGD,ALTOTL,TOTAL,TERM,EDIT1,EDIT2
      CHARACTER *4 SAMPLE,MTNAME,SAMPLO
C
      COMMON/AMESH/BMESH(30),NREGI,NREGJ,NREGKB,XSHI(RGX),XSHJ(RGX),
     & XSHKB(RGX), MSHI(RGX),MSHJ(RGX),MSHKB(RGX),Y(MSX),YY(MSX), X(MSX)
     &  ,XX(MSX),Z(MSX),ZZ(MSX), ZONVOL(ZNEX),AVZPD(ZNEX),PDI(MSX),
     & PDJ(MSX) , PDK(MSX)
C
CMODI TERM REAL*8 SET STATIC DIMENSION ==>  TERM(10,200),200:MAX GROUP
CMODI DIMENSION TERM(10,KVX),P1E(JVX,IVX,KBVX),P2E(JIVX,KBVX,KVX),
      DIMENSION TERM(10,200),P1E(JVX,IVX,KBVX),P2E(JIVX,KBVX,KVX),
     1          IOPT(ICASE),IXYZ(2,3,ICASE),SAMPLE(2,ICASE),SIG2(KVX,12)
     2         ,SCAC2(KVX,KVX),XI2(KVX),BKLE(ICASE),IBKLGP(ICASE),
     3          SIG(KVX,MMAX,12),SCAC(KVX,MMAX,KVX),NRGNE(JVX,IVX,KBVX),
     4          NCOMP(LMAX),PVOL(LMAX),MTNAME(2,NMT),MAT(NMT),P(MEMPET),
     5          IDOPT(ICASE),XII(KVX,MMAX),BUF(NBUF),BND(6,KVX),
     6          BBND(KVX)
      DIMENSION EDIT1(6),EDIT2(5),SAMPLO(2),XX1(MSX),YY1(MSX),ZZ1(MSX)
CMODI TERM REAL*4 REPLASED TERM4(10,KVX)
CMODI ADD
      DIMENSION TERM4(10,KVX)
C
      SAMPLO(1) = '    '
      SAMPLO(2) = '    '
      IDOPTO    = -1
CUNIX BKLEO     = -1.E60
      BKLEO     = -1.E+35
      WRITE(IOUT,1000)
      ALTOTL = 0.0
      IF (NGC13.GE.0) GO TO 95
      REWIND IO14
      DO 90 K = 1,KMAX
      READ(IO14) ((SCAC(K,M,KK),KK=1,KMAX),M=1,MMAX)
   90 CONTINUE
      REWIND IO14
      CALL IVALUE(ZONVOL,MMAX,0.0)
      DO 93 KB = 1,KBMAX
      DO 92 I  = 1,IMAX
      DO 91 J  = 1,JMAX
      L = NRGNE(J,I,KB)
      M = NCOMP(L)
      ZONVOL(M) = ZONVOL(M) + PVOL(L)
   91 CONTINUE
   92 CONTINUE
   93 CONTINUE
   95 CONTINUE
C
C     ADJOINT FLUX
      IF (IDELAY.EQ.0) GO TO 125
      REWIND IO18
      DO 120 KB = 1,KBMAX
      N1 = 0
      DO 110 I = 1,IMAX
      DO 100 J = 1,JMAX
      N1 = N1 + 1
      READ(IO18) (P2E(N1,KB,K),K=1,KMAX)
  100 CONTINUE
  110 CONTINUE
  120 CONTINUE
  125 CONTINUE
      DO 400 IC = 1,ICASE
      IF (IC.EQ.1) GO TO 129
      IF (IOPT(IC-1).NE.2) GO TO 129
      REWIND IO18
      DO 128 KB = 1,KBMAX
      N1 = 0
      DO 127 I = 1,IMAX
      DO 126 J = 1,JMAX
      N1 = N1 + 1
      READ(IO18) (P2E(N1,KB,K),K=1,KMAX)
  126 CONTINUE
  127 CONTINUE
  128 CONTINUE
  129 CONTINUE
      CALL PERTM(BUF      ,BUF      ,SAMPLE(1,IC),SIG2  ,SCAC2    ,
     1           XI2      ,P        ,SAMPLO(1)   ,
     2           KMAX     ,NBUF     ,IOUT     ,BKLE(IC) ,IBKLGP(IC),
     3           IDOPT(IC),MEMPET   ,IX104    ,IDOPTO   ,BKLEO      )
C
C    TERM(1,K) = FISSION TERM
C    TERM(2,K) = ABSORPTION TERM
C    TERM(3,K) = MODERATION (SCATTERING) TERM
C    TERM(4,K) = BUCKLING TERM
C    TERM(5,K) = LEFT LEAKAGE
C    TERM(6,K) = RIGHT LEAKAGE
C    TERM(7,K) = TOP LEAKAGE
C    TERM(8,K) = BOTTOM LEAKAGE
C    TERM(9,K) = FRONT LEAKAGE
C    TERM(10,K) = BACK LEAKAGE
C
      CALL IVALUE(TERM(1,1),10*2*KMAX,0.0)
      VOLP = 0.0
C
C
      IF (IOPT(IC).GT.0) GO TO 130
      JX1 = 1
      JX2 = JMAX
      IX1 = 1
      IX2 = IMAX
      KX1 = 1
      KX2 = KBMAX
      GO TO 150
  130 CONTINUE
      IF (IOPT(IC).NE.1) GO TO 140
      JX1 = IXYZ(1,1,IC)
      JX2 = IXYZ(2,1,IC)
      IX1 = IXYZ(1,2,IC)
      IX2 = IXYZ(2,2,IC)
      KX1 = IXYZ(1,3,IC)
      KX2 = IXYZ(2,3,IC)
      GO TO 150
  140 CONTINUE
      CALL PERTC1(IXYZ(1,1,IC),XX    ,XX1   ,YY    ,YY1    ,
     1            ZZ    ,ZZ1   ,
     3            JVXP1 ,IVXP1 ,KBVXP1,NDIM ,JX1   ,JX2    ,
     4            IX1   ,IX2   ,KX1   ,KX2                  )
      GO TO 150
  150 CONTINUE
      REWIND IO26
      DO 245 K = 1,KMAX
      READ(IO26) P1E
      DO 240 KB = KX1,KX2
      DO 230 I = IX1,IX2
      N1 = (I-1)*JMAX+JX1-1
      DO 220 J = JX1,JX2
      N1 = N1 + 1
      L = NRGNE(J,I,KB)
      M = NCOMP(L)
      IF (IOPT(IC).LT.0.AND.M.NE.-IOPT(IC)) GO TO 220
      MP = M
      PV = PVOL(L)
      IF (IOPT(IC).NE.2) GO TO 202
C
      IF (J.EQ.JX1.OR.J.EQ.JX2.OR.I.EQ.IX1.OR.I.EQ.IX2.OR.KB.EQ.KX1.
     &              OR.KB.EQ.KX2)
     &CALL PERTC2(XX    ,XX1   ,YY    ,YY1   ,ZZ    ,ZZ1   ,
     &            PV    ,PVOL(L),NUAC5 ,JVXP1 ,IVXP1 ,KBVXP1,
     &            J     ,I     ,KB                          )
  202 CONTINUE
      PF = P1E(J,I,KB)
      PA = P2E(N1,KB,K)
      VVV = PF*PA*PV
      IF (K.EQ.1) VOLP = VOLP + PV
C
C.... ABSORPTION TERM
      TERM(2,K) = TERM(2,K) + (SIG2(K,3)-SIG(K,M,3))*VVV
C
C.... MODERATION TERM
      TERM(3,K) = TERM(3,K) - (SIG2(K,2)-SIG(K,M,2))*VVV
C
C.... SCATTERING TERM
      DO 210 KK = 1,KMAX
      VVV1 = PF*P2E(N1,KB,KK)*PV
      TERM(3,K) = TERM(3,K) + (SCAC2(K,KK)-SCAC(K,M,KK))*VVV1
  210 CONTINUE
C
C.... BUCKLING TERM
      TERM(4,K) = TERM(4,K) + ((SIG2(K,12)-SIG(K,M,12))*SIG(K,M,6)
     *                      + SIG(K,M,12)*(SIG2(K,6)-SIG(K,M,6)))*VVV
C
C.... LEAKAGE TERM
      CALL PERTL(TERM     ,NRGNE    ,NCOMP    ,P1E      ,P2E      ,
     1           SIG      ,SIG2     ,PVOL     ,BND      ,BBND     ,
     2           JVX      ,IVX      ,KBVX     ,KVX      ,LMAX     ,
     3           JVXP1    ,IVXP1    ,KBVXP1   ,JIVX     ,MMAX     ,
     4           J        ,I        ,KB       ,K                   )
  220 CONTINUE
  230 CONTINUE
  240 CONTINUE
  245 CONTINUE
C
      BIGD = 0.0
      TOTAL = 0.0
      DO 290 K = 1,KMAX
      REWIND IO26
      DO 280 KK = 1,KMAX
      READ(IO26) P1E
C
      DO 270 KB = 1,KBMAX
      N1 = 0
      DO 260 I = 1,IMAX
      DO 250 J = 1,JMAX
      N1 = N1 + 1
      L = NRGNE(J,I,KB)
      M = NCOMP(L)
      PV = PVOL(L)
C
      VVV = P1E(J,I,KB)*P2E(N1,KB,K)*PV
C
C... BIG DADY
      BIGD = BIGD + XII(K,M)*SIG(KK,M,4)*VVV
      IF (IOPT(IC).LT.0.AND.M.NE.-IOPT(IC)) GO TO 250
      IF (J.LT.JX1.OR.J.GT.JX2.OR.I.LT.IX1.OR.I.GT.IX2
     &            .OR.KB.LT.KX1.OR.KB.GT.KX2) GO TO 250
      IF (IOPT(IC).NE.2) GO TO 246
C
      IF (J.EQ.JX1.OR.J.EQ.JX2.OR.I.EQ.IX1.OR.I.EQ.IX2.OR.KB.EQ.KX1.
     &              OR.KB.EQ.KX2)
     &CALL PERTC2(XX    ,XX1   ,YY    ,YY1   ,ZZ    ,ZZ1   ,
     &            PV    ,PVOL(L),NUAC5 ,JVXP1 ,IVXP1 ,KBVXP1,
     &            J     ,I     ,KB                          )
      PF = P1E(J,I,KB)
      PA = P2E(N1,KB,K)
      VVV = PF*PA*PV
  246 CONTINUE
C
C... FISSION TERM (MATERIAL DEPENDENT XI )
C     TERM(1,K) = TERM(1,K) + (XII(K,M)*(SIG2(KK,4)-SIG(KK,M,4))
C    &                  + (XI2(K)*XKEFF-XII(K,M))*SIG(KK,M,4))*VVV
CORRECT 1991/6/11   ON J1480.PERT.FORT77(PERTC)
C     TERM(1,K) = TERM(1,K) + (XI2(K)*SIG2(KK,4)
C    &                 -XKEFF*XII(K,M)*SIG(KK,M,4))*VVV
CORRECTION ABOVE WAS INVALID BECAUSE XKEFF = 1/KEFF (1995/3/6)
      TERM(1,K) = TERM(1,K) + (XII(K,M)*(SIG2(KK,4)-SIG(KK,M,4))
     &                  + (XI2(K)*XKEFF-XII(K,M))*SIG(KK,M,4))*VVV
  250 CONTINUE
  260 CONTINUE
  270 CONTINUE
  280 CONTINUE
  290 CONTINUE
      DO 305 K = 1,KMAX
      TERM(1,K) = TERM(1,K)
      DO 300 II = 1,10
      TERM(II,K) = TERM(II,K) / BIGD
      IF (II.EQ.2.OR.II.GE.4) TERM(II,K) = - TERM(II,K)
      TOTAL = TOTAL + TERM(II,K)
  300 CONTINUE
  305 CONTINUE
C
      WRITE(IOUT,1010) IC,(SAMPLE(J,IC),J=1,2),VOLP,
     &                 (MTNAME(J,MAT(MP)),J=1,2),ZONVOL(MP)
      CALL IVALUE(EDIT1,5*2,0.0)
      DO 320 K = 1,KMAX
      TLEAK = TERM(5,K) + TERM(6,K) + TERM(7,K) + TERM(8,K) + TERM(9,K)
     &                  + TERM(10,K)
      T = TLEAK
      DO 310 II = 1,4
      T = T + TERM(II,K)
      EDIT1(II) = EDIT1(II) + TERM(II,K)
      EDIT2(II) = TERM(II,K) / TOTAL
  310 CONTINUE
      EDIT1(5)  = EDIT1(5) + TLEAK
      EDIT2(5)  = TLEAK / TOTAL
      TT        = T / TOTAL
      WRITE(IOUT,1020) K,(TERM(II,K),II=1,3),TLEAK,TERM(4,K),T,
     &                   (EDIT2(II),II=1,3),EDIT2(5),EDIT2(4),TT
  320 CONTINUE
      DO 330 II = 1,5
      EDIT2(II) = EDIT1(II) / TOTAL
  330 CONTINUE
      WRITE(IOUT,1030) (EDIT1(II),II=1,3),EDIT1(5),EDIT1(4),TOTAL,
     &                 (EDIT2(II),II=1,3),EDIT2(5),EDIT2(4)
      WRITE(IOUT,1040)
      TTLEAK = EDIT1(5)
      CALL IVALUE(EDIT1,6*2,0.0)
      DO 350 K = 1,KMAX
      TLEAK = 0.0
      DO 340 II = 5,10
      EDIT1(II-4) = EDIT1(II-4) + TERM(II,K)
      TLEAK = TLEAK + TERM(II,K)
  340 CONTINUE
      WRITE(IOUT,1050) K,(TERM(II,K),II=5,10),TLEAK
  350 CONTINUE
      WRITE(IOUT,1060)   (EDIT1(II),II=1,6),TTLEAK
      ALTOTL = ALTOTL + TOTAL
      IF (IOPT(IC).NE.2) GO TO 400
      ZZ(KX1) = ZZ1(KX1)
      ZZ(KX2+1) = ZZ1(KX2+1)
      YY(IX1) = YY1(IX1)
      YY(IX2+1) = YY1(IX2+1)
      XX(JX1) = XX1(JX1)
      XX(JX2+1) = XX1(JX2+1)
  400 CONTINUE
      WRITE(IOUT,1070) ALTOTL
      REWIND IO18
      REWIND IO26
CMODI TERM REAL*8 ==> TERM4 REAL*4 ( IF USE TERM ,AFTER PERTC.F CALLED)
CMODI
      DO 500 IE = 1,KMAX
         DO 500 IT = 1,10
            TERM4(IT,IE) = SNGL(TERM(IT,IE))
  500 CONTINUE
CMODI
      RETURN
 1000 FORMAT(1H0/1H0,10X,'***** PERTURBATION CALCULATION RESULTS *****')
 1010 FORMAT(1H0,2X,'CASE NO.',I3,3X,'SAMPLE MATERIAL NAME   (',2A4,')',
     1       3X,'VOLUME OF SAMPLE MATERIAL   (',1PE12.5,')'/
     2       17X,'ORIGINAL MATERIAL NAME (',2A4,')',3X,'VOLUME OF ORIG',
     3       'INAL MATERIAL (',1PE12.5,')'///
     4       6X,'PERTURBATION BY GROUP',9X,'UPPER : DELTA-K/K   LOWWER',
     5       ' : UPPER/TOTAL'//
     6       19X,'FISSION    ABSORPTION  MODERATION   LEAKAGE       B*',
     7       '*2       TOTAL'/16X,6(' -----------')/                   )
 1020 FORMAT(6X,'GROUP',I4,1X,1P6E12.5/16X,6E12.5                      )
 1030 FORMAT(6X,'TOTAL',5X,1P6E12.5/16X,6E12.5                         )
 1040 FORMAT(1H0,5X,'LEAKAGE SUMMARY'//
     1       20X,'LEFT',8X,'RIGHT',8X,'TOP',7X,'BOTTOM',7X,'FRONT',7X,
     2       'BACK',8X,'TOTAL'/16X,7(' -----------')                   )
 1050 FORMAT(6X,'GROUP',I4,1X,1P7E12.5                                 )
 1060 FORMAT(6X,'TOTAL',5X,1P7E12.5                                    )
 1070 FORMAT('0 OVERALL DELTA-K/K IS ',1PE12.5                         )
      END
