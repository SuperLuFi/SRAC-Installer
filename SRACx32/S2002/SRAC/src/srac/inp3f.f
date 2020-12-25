      SUBROUTINE INP3F(NRMAX,NGMAX,RR,RE,P,FLUX,Q
     &                ,LOC,LSS,LGV,XEC,DR,VOL,FR,MMR,FLUXZ)
C *********************************************************************
C                          INP3F
C *********************************************************************
      DIMENSION RR(*),RE(*),P(NRMAX,NRMAX,*),
     &          FLUX(NRMAX,NGMAX),Q(NRMAX,NGMAX),FLUXZ(NRMAX,NGMAX)
     &         ,XEC(*),LSS(NGMAX,*),LGV(NGMAX,*),LOC(NGMAX,*)
     &         ,DR(NRMAX,NGMAX),MMR(*),VOL(*),FR(*)
C
      COMMON / PIJ2C / DUMY(9),
     &                 IFORM,ID11,ID12,ID13,ID14,
     &                 NGLAST,NGUP,ITAPE,NGKMAX,
     &                 IEDFLX,ITMINN,ITMOUT,ITBG,LCMX,ITDM,JPT,
     &                 EPSI,EPSO,EPSG,RELCA,OVERX,FACTOR,ICOOD,
     &                 NO1(8),
     &                 LCNREG,LCIRR,LCIXR,LCMAR,LCMAT,LCVOL,
     &                 LCVOLR,LCVOLX,LCVOLM,NO2,AA(950)
      COMMON / MAINC / DUM(39),IFIXS,DUM2(23) ,NOUT1,NOUT2,DUM66(8)
     &                ,ISOIN,IFIN,IFOUT, ITYPE
C
C     IEDFLX=IPTF+2*IPTXEC+4*IPTPIJ+8*IPTS
C
      IPTPIJ = MOD(IEDFLX,8)/4
      IPTS   = IEDFLX/8
C
C *** READ & PRINT COLLISION PROBABILITIES ***
C
      REWIND 21
      IF(IPTPIJ.GT.0) WRITE(NOUT2,20)
      CALL CLEA( DR , NRMAX*NGMAX , 1.0 )
C
      DO 160 NG =1,NGMAX
      K1        = NG
      IF(ITAPE .EQ.2) K1=1
      IF(IPTPIJ.GT.0) WRITE(NOUT2,25) NG
C *** TOTAL CROSS SECTION BY REGION TO YIELD MODIFIED PROBABILITIES
      DO 120 NR = 1,NRMAX
      NM        = MMR(NR)
      L         = LOC(NG,NM)+5
  120 RR(NR)    = XEC(L)
C
      READ(21)((P(NR,K,K1),NR=1,NRMAX),K=1,NRMAX)
C
      DO 150 NR = 1,NRMAX
      IF(IPTPIJ.NE.0) WRITE(NOUT2,30)NR,(P(NR,K,K1),K=1,NRMAX)
C     LEAKAGE PROBABILITIES DR
      DO 130 K  = 1,NRMAX
      IF(IFORM.EQ.0) THEN
      DR(NR,NG) = DR(NR,NG)-P(NR,K,K1)
C     CHANGE ORDINARY PROBABILITIES INTO MODIFIED ONES
      P(NR,K,K1)= P(NR,K,K1)/RR(K)
                     ELSE
      DR(NR,NG) = DR(NR,NG)-P(NR,K,K1)*RR(K)
                    ENDIF
  130 CONTINUE
  150 CONTINUE
  160 CONTINUE
C
C *** FLUX GUESS ***
C
      IF(IFIXS.NE.0) THEN
                     REWIND IFIN
                     READ (IFIN) ((FLUX(NR,NG),NR=1,NRMAX),NG=1,NGMAX)
                     ELSE
                     DO 165    NG = 1,NGMAX
                     DO 165    NR = 1,NRMAX
CM                   FLUX(NR,NG)  = VOL(NR)
                     FLUX(NR,NG)  = 1.000000
  165                CONTINUE
                     ENDIF
C
C *** GUESS FOR POWER DISTRIBUTION
C
      DO 170 NR = 1,NRMAX
      NM        = MMR(NR)
      FR(NR)    = 0.0
      DO 170  NG  = 1,NGMAX
      FLUX(NR,NG) = FLUX(NR,NG)*VOL(NR)
      L           = LOC(NG,NM) + 4
      FR(NR)      = FR(NR)     + XEC(L)*FLUX(NR,NG)
  170 CONTINUE
C
C *** SOURCE DENSITY ***
C
      IF(ITYPE.EQ.0) RETURN
C
      REWIND ISOIN
      IF(IFIXS.EQ.2) THEN
                     READ(ISOIN) ((FLUXZ(I,N),I=1,NRMAX),N=1,NGMAX)
                     CALL CLEA(Q,NRMAX*NGMAX,0.)
                     DO 190 NR = 1,NRMAX
                     NM        = MMR(NR)
                     DO 190 NG = 1,NGMAX
                     K1        = LOC(NG,NM)      + 10
                     K2        = K1  + LGV(NG,NM) - 1
                     NGD       = NG  - LSS(NG,NM)
                     DO 180 KD = K1,K2
                     NGD       = NGD + 1
                     Q(NR,NGD) = Q(NR,NGD) + XEC(KD)*FLUXZ(NR,NG)
  180                CONTINUE
  190                CONTINUE
C
                     ELSE
                     READ(ISOIN) ((Q(I,N),I=1,NRMAX),N=1,NGMAX)
                     ENDIF
C
      IF(IPTS.GT.0) WRITE(NOUT2,40)
      DO 220 NG = 1,NGMAX
      IF(IPTS.NE.0) WRITE(NOUT2,50) NG,(Q(NR,NG),NR=1,NRMAX)
      DO 200 NR = 1,NRMAX
      Q(NR,NG)  = Q(NR,NG)*VOL(NR)
      IF(IFIXS.EQ.2) FLUXZ(NR,NG) = FLUXZ(NR,NG)*VOL(NR)
  200 CONTINUE
  220 CONTINUE
C
      RETURN
C
C
   20 FORMAT('0*** COLLISION PROBABILITY ***  INPUT')
   25 FORMAT('0 ** ENERGY GROUP',I3)
   30 FORMAT(' *** SOURCE REGION =',  I3/(10X,10F12.6))
   40 FORMAT('0*** SOURCE DENSITY ***  INPUT'//)
   50 FORMAT('   * E.G.',I3/(10X,10E12.5))
C
      END
