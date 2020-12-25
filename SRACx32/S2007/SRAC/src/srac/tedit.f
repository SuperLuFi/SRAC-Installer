      SUBROUTINE TEDIT(NRMAX,NGMAX,RR  ,RE  ,FLUX ,Q  ,H   ,S   ,
     &                 LOC  ,LSS  ,LGV ,XEC ,FLUXM,RM ,DR  ,VOL ,MMR)
C
      DIMENSION        RR(1),RE(1),LOC(NGMAX,1)
      DIMENSION        XEC(1),FLUX(NRMAX,NGMAX)
      DIMENSION        H(NRMAX,NGMAX),S(NRMAX,NGMAX)
      DIMENSION        DR(NRMAX,NGMAX),MMR(1)
      DIMENSION        FLUXM(1),RM(1),VOL(1)
C
      COMMON / PIJ2C / DUMMY1(6),IDRECT,ID8,ID9,
     &                 IPIJ,NTTAB,NUTAB,SZ,ITYPE,
     &                 NGLAST,NGUP,ITAPE,NGKMAX,
     &                 IEDFLX,ITMINN,ITMOUT,ITBG,LCMX,ITDM,IPT,
     &                 EPSI,EPSO,EPSG,RELC,OVERX,FACTOR,ICOOD,
     &                 NMMAX,I34(7),
     &                 LCNREG,LCIRR,LCIXR,LCMAR,LCMAT,LCVOL,
     &                 LCVOLR,LCVOLX,LCVOLM,LCMATD,IIAA(950)
      COMMON / MAINC / DUM(48),JNFLUX(2),FNFLUX(2),DUM1(6),NMAT,
     &                 DUM2(5),NOUT2,DUM21(8),ISOIN,IFIN,IFOUT,
     &                 DUM20(24),IDCASE(2),TITLE(18),DUM3(380)
C
      CHARACTER*4     IDENT(8,5),ID1(8),ID2(8),ID3(8),ID4(8),ID5(8)
C
      EQUIVALENCE    (ID2(1),IDENT(1,1)),(ID1(1),IDENT(1,2))
      EQUIVALENCE    (ID3(1),IDENT(1,3))
      EQUIVALENCE    (ID4(1),IDENT(1,4)),(ID5(1),IDENT(1,5))
C
      DATA ID1/' * A','BSOR','PTIO','N  C','ROSS','-SEC','TION',' *  '/
      DATA ID2/' * S','CATT','ER  ','   O','UT  ','    ','    ',' *  '/
      DATA ID3/' * N','U*FI','SSIO','N  C','ROSS','-SEC','TION',' *  '/
      DATA ID4/' * T','OTAL','    ','   C','ROSS','-SEC','TION',' *  '/
      DATA ID5/' * R','EMOV','AL  ','   C','ROSS','-SEC','TION',' *  '/
C
C *** START OF PROCESS
C
      IF(IDRECT.EQ.2.AND.ICOOD.NE.2) F1=3.0/FLOAT(2-ICOOD)
      IF(IDRECT.EQ.2.AND.ICOOD.NE.2) F2=FLOAT(4**ICOOD)/2.0
C     IEDFLX  = IPRF  +  2*IPTXEC + 4*IPTPIJ + 8*IPTS
      IPRF    = MOD(IEDFLX,2)
      TF      = 0.0
      TD      = 0.0
      CALL   CLEA(  H(1,1) , NRMAX , 0.0 )
      CALL   CLEA(  RR     , NRMAX , 0.0 )
      CALL   CLEA(  FLUXM  , NMMAX , 0.0 )
C
      REWIND IFOUT
      REWIND 22
      WRITE(IFOUT) ((FLUX(I,J),I=1,NRMAX),J=1,NGMAX)
      WRITE(6,*)'          FLUX WRITTEN IN FT',IFOUT
C
  113 FORMAT(// 10X,'** FINAL FLUX ** MULTIPLIED BY VOLUME AND'
     &  ,' LETHARGY **' /)
  115 FORMAT('  ENERGY GROUP',I3/(15X,1P6E14.5))
C
      IF(IPRF.NE.0) WRITE(NOUT2,113)
C
C *** ENERGY LOOP
C
      DO 140 NG = 1,NGMAX
      RE(NG)    = 0.0
      S(1,NG)   = 0.0
      S(2,NG)   = 0.0
      IF(IPRF.NE.0)  THEN
                     WRITE(NOUT2,115) NG,(FLUX(NR,NG),NR=1,NRMAX)
                     ENDIF
C
      READ(22) (H(NR,2),NR=1,NRMAX)
      IF(IDRECT.EQ.2.AND.ICOOD.NE.2) READ(22)(H(NR,3),NR=1,NRMAX)
C
      DO 130 NR = 1,NRMAX
      NM        = MMR(NR)
      FLUXM(NM) = FLUXM(NM) + FLUX(NR,NG)
      RE(NG)    = RE(NG)    + FLUX(NR,NG)
      H(NR,1)   = H(NR,1)   + FLUX(NR,NG)
      RR(NR)    = RR(NR)    + FLUX(NR,NG)/VOL(NR)
      S(1,NG)   = S(1,NG)   + H(NR,2)*FLUX(NR,NG)
      IF(IDRECT.EQ.2.AND.ICOOD.NE.2)S(2,NG)=S(2,NG)+H(NR,3)*FLUX(NR,NG)
  130 CONTINUE
C
      TD        = TD   + S(1,NG)
      S(1,NG)   = S(1,NG) / RE(NG)
C
C     modification was done to avoid negative anisotropic diffusion coeff.
C
CKUNI IF(IDRECT.EQ.2.AND.ICOOD.NE.2) S(2,NG)  = S(2,NG)/RE(NG)
CKUNI IF(IDRECT.EQ.2.AND.ICOOD.NE.2) DR(1,NG) = F1*S(1,NG) - F2*S(2,NG)
      IF(IDRECT.EQ.2.AND.ICOOD.NE.2) THEN
                                     DSAVE1   = S(2,NG)/RE(NG)
CKUNI-OCT/13/2002                    DSAVE2   = F1*S(1,NG) - F2*S(2,NG)
                                     DSAVE2   = F1*S(1,NG) - F2*DSAVE1
CKUNI-OCT/13/2002
         IF(DSAVE1.GT.1.0E-5.AND.DSAVE2.GT.1.0E-5) THEN
                                                  S(2,NG)  = DSAVE1
                                                  DR(1,NG) = DSAVE2
                                                  ELSE
                                                  S(2,NG)  = S(1,NG)
                                                  DR(1,NG) = S(1,NG)
                                                  ENDIF
                                     ENDIF
CEND
      TF        = TF   + RE(NG)
  140 CONTINUE
C
C *** ENERGY LOOP END
C
      IF(IDRECT.EQ.1) THEN
                      WRITE(IFOUT) (S(1,NG),NG=1,NGMAX)
                      WRITE(6,*) '          D1   WRITTEN IN FT',IFOUT
                      ENDIF
C
      IF(IDRECT.EQ.2.AND.ICOOD.NE.2) THEN
                      WRITE(IFOUT) (S(2,NG),NG=1,NGMAX)
                      WRITE(IFOUT) (DR(1,NG),NG=1,NGMAX)
                                     ENDIF
      IF(IDRECT.EQ.2) WRITE(6,*) '           D2   WRITTEN IN FT',IFOUT
C
      IF(IPRF.EQ.0)   RETURN
C
      WRITE(NOUT2,150) IDCASE,TITLE
      WRITE(NOUT2,151)
      WRITE(NOUT2,162) (RE(NG),NG=1,NGMAX)
      WRITE(NOUT2,152)
      WRITE(NOUT2,162) (S(1,NG),NG=1,NGMAX)
C
      IF(IDRECT.EQ.2.AND.ICOOD.NE.2) THEN
                    WRITE(NOUT2,153)
                    WRITE(NOUT2,162) (S(2,NG),NG=1,NGMAX)
                    WRITE(NOUT2,165)
                    WRITE(NOUT2,162) (DR(1,NG),NG=1,NGMAX)
                                     ENDIF
C
      WRITE(NOUT2,154)
      WRITE(NOUT2,162) (H(NR,1),NR =1,NRMAX)
      WRITE(NOUT2,155)
      WRITE(NOUT2,162) (RR(NR),NR=1,NRMAX)
      WRITE(NOUT2,156)
      WRITE(NOUT2,162) (FLUXM(NM),NM=1,NMMAX)
      WRITE(NOUT2,157) TF
      D         =  TD/TF
      WRITE(NOUT2,159)  D
      DO 160 NM = 1,NMMAX
  160 RM(NM)    = 0.0
C
      WRITE(NOUT2,161)
      WRITE(NOUT2,162) (VOL(NR),NR=1,NRMAX)
      DO 200 NR = 1,NRMAX
      NM        = MMR(NR)
  200 RM(NM)    = RM(NM) + VOL(NR)
C
      WRITE(NOUT2,205)
      WRITE(NOUT2,162) (RM(NM),NM=1,NMMAX)
C
C *** REACTION LOOP
C
      DO 500 KK = 1,5
      WRITE(NOUT2,185)  KK,(IDENT(I,KK),I=1,8)
      T         = 0.0
      TT        = 0.0
      CALL  CLEA(  RM , NMMAX , 0.0 )
      CALL  CLEA(  RR , NRMAX , 0.0 )
C *** ENERGY LOOP
      DO 300  NG = 1,NGMAX
      S(1,NG)    = 0.0
      DO 290  NR = 1,NRMAX
      NM         = MMR(NR)
      IN         = LOC(NG,NM) + KK
      IF(KK.EQ.1) IN = IN - 1
      IF(KK.EQ.2) IN = IN - 1
      IF(KK.EQ.3) IN = IN + 1
      IF(KK.EQ.4) IN = IN + 1
      IF(KK.EQ.5) IN = IN + 4
      A          = XEC(IN)*FLUX(NR,NG)
      S(1,NG)    = S(1,NG)  + A
      RR(NR)     = RR(NR)   + A
      RM(NM)     = RM(NM) + A
  290 CONTINUE
      T          = T  + S(1,NG)
      IF(S(1,NG).EQ.0.) GO TO 300
      TT         = TT + RE(NG)*RE(NG)/S(1,NG)
  300 CONTINUE
      TR        = T/TF
      TT        = TT/TF/3.00
      WRITE(NOUT2,305)
      WRITE(NOUT2,162) (S(1,NG),NG=1,NGMAX)
      DO 310 NG = 1,NGMAX
      S(1,NG)   = S(1,NG)/RE(NG)
  310 CONTINUE
      WRITE(NOUT2,315)
      WRITE(NOUT2,162) (S(1,NG),NG=1,NGMAX)
      WRITE(NOUT2,316)
      WRITE(NOUT2,162) (RR(NR), NR =1,NRMAX)
      DO 320 NR = 1,NRMAX
      RR(NR)    = RR(NR)/VOL(NR)
  320 CONTINUE
      WRITE(NOUT2,325)
      WRITE(NOUT2,162) (RR(NR),NR=1,NRMAX)
      DO 330 NR = 1,NRMAX
      RR(NR)    = RR(NR)*VOL(NR)/H(NR,1)
  330 CONTINUE
      WRITE(NOUT2,335)
      WRITE(NOUT2,162) (RR(NR),NR=1,NRMAX)
      WRITE(NOUT2,336)
      WRITE(NOUT2,162) (RM(NM),NM=1,NMMAX)
      DO 340 NM=1,NMMAX
      IF(FLUXM(NM).NE.0.)  RM(NM)=RM(NM)/FLUXM(NM)
  340 CONTINUE
      WRITE(NOUT2,345)
      WRITE(NOUT2,162) (RM(NM),NM=1,NMMAX)
      WRITE(NOUT2,350) TR,T
      IF(KK.EQ.4) WRITE(NOUT2,360) TT
  500 CONTINUE
      RETURN
C
  155 FORMAT(/10X,'FLUX(I)           '                             )
  150 FORMAT(1H1/9X,20A4/10X,'**AVERAGED AND INTEGRATED DATA**')
  151 FORMAT(/10X,'FLUX(G)             ENERGY DIST. OF FLUX ')
  152 FORMAT(/10X,'D(G) BENOIST MODEL ')
  153 FORMAT(/10X,'D(G) BENOIST MODEL RADIAL ')
  165 FORMAT(/10X,'D(G) BENOIST MODEL AXIAL  ')
  154 FORMAT(/10X,'FLUX(I)*VOL(I) '                             )
  156 FORMAT(/10X,'FLUX(M)*VOL(M) ' )
  157 FORMAT(/10X,'FLUX      INTEGRAL FLUX                   = ',E15.5)
  159 FORMAT(/10X,'DIFFUSION COEFFICIENT (BENOIST MODEL)     = ',E15.5)
  161 FORMAT(1H0,10X,'VOL(I)')
  162 FORMAT(10X, 8E15.5)
  205 FORMAT(/10X,'VOL(M)    ')
  185 FORMAT('0 SIG. ',I2,2X,8A4)
  305 FORMAT(/10X,'SIG(G) *FLUX(G)     ENERGY DIST. OF REACTION ')
  315 FORMAT(/10X,'SIG(G)              ENERGY DIST. OF CROSSSECTION')
  316 FORMAT(/10X,'SIG(I)*FLUX(I)*VOL(I) '                          )
  325 FORMAT(/10X,'SIG(I)*FLUX(I)    '                              )
  335 FORMAT(/10X,'SIG(I)    AVERAGE BY REGION'                     )
  336 FORMAT(/10X,'SIG(M)*FLUX(M)*VOL(M)')
  345 FORMAT(/10X,'SIG(M) '             )
  350 FORMAT(/10X,'SIG       AVERAGE OF LATTICE CELL           =',E15.5
     &       /10X,'SIG*FLUX  TOTAL REACTION                    =',E15.5)
  360  FORMAT(/10X,'DIFFUSION COEFFICIENT (HARMONIC AVERAGE)=',E15.5)
C
      END
