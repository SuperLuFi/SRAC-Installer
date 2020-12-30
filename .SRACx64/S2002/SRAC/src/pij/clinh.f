      SUBROUTINE CLINH(RX,RPP,RDP,THETA,TY,V,NTR,NAMEP,NAMER,F
     1               ,NPINPS,NPINDV)
      DIMENSION RX(NX1),RDP(NDPIN1,NTPIN),RPP(NTPIN),
     1 THETA(NTPIN),TY(NY1),NTR(NX),V(NZ),NAMEP(2,NDPIN,NTPIN),
     2 F(NDPIN1,NTPIN),NAMER(NTPIN),NPINPS(NTPIN),NPINDV(NTPIN)
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGA,NDPIN,
     1              IDIVP,BETM,NX1,NY1,I14,I15,I16,IXP,IYP,IZP,
     2              NDPIN1,NDR,NDA,LL,L0,RO1,DRO,FVOL,RAN,
     3              PIT,RO,BETA,INV,NHEX,SINB,COSB,I36
      COMMON / PIJ2C / IGT,NZ,NR,NRN,NXR,IBOUND,IDRECT,LCOUNT,IEDPIJ,
     1                 IFORM,NTTAB,NUTAB,SZ,IDUM14(27),
     4                 LCNREG,LCIRR,LCIXR,LCMAR,LCMAT,LCVOL,
     5                 LCVOLR,LCVOLX,LCVOLM,NO2,AAA(950)
      COMMON /MAINC/ DUMMY1(63),NOUT1,NOUT2,DUMMY2(435)
      COMMON /IGEOM/   DUMMY3( 5),PINR,RANGE,DUMMY4,NPINZ
      DATA PI/3.141593/,SQ3/1.732051/
C
      PI3=PI/3.0
      PI6=PI/6.
      EPS=1.E-03
      NHEX=0
      IF(IGT.EQ.12) NHEX=1
      IF(NGR*NDA  .EQ. 0) THEN
      NGR= 8
      NDA= INT(BETM/PI*90.)
                          ENDIF
          IF(TY(1).NE.0.) THEN
         IST=2
C        JST=0
                          ELSE
         IST=1
C        JST=1
                          ENDIF
         JST=2-IST
C
      INV=0
      IF(IBOUND.LT.0) INV=1
      IBOUND=IABS(IBOUND)
      IF(NY.EQ.0) NY=1
      NADDR=0
      RPZ=0.
            IF(NTPIN.NE.0) THEN
      DO 10 J=1,NTPIN
      IF(RPP(J).LT.RPZ) THEN
      WRITE(NOUT1,333) RPP(J),RPZ,J
  333 FORMAT(10X,'ILLEGAL ORDER OF RPP VALUES ',2E12.5,' AT IP=',I2)
                       STOP
                       ENDIF
      IF(RPP(J).NE.RPZ .AND. IDIVP.NE.0)THEN
          NADDR=NADDR+1
          RPZ=RPP(J)
                                        ENDIF
  10  CONTINUE
      NAPIN=NADDR
      IF(RPP(1).EQ.0.) NAPIN=NAPIN+1
*     WRITE(NOUT2,*) ' NADDR=',NADDR
*     WRITE(NOUT2,*) ' TY=',(TY(I),I=0,NY1)
*     WRITE(NOUT2,*) ' RPP=',(RPP(I),I=1,NTPIN)
*     WRITE(NOUT2,*) ' THT=',(THETA(I),I=1,NTPIN)
C **  NPINPS: PIN AZIMUTHAL POSITION
      DO 13 IP=1,NTPIN
        NPINPS(IP)=1
        IF(NY.LE.1) GO TO 13
      IF(RPP(IP).EQ.0.) GO TO 13
      IF(TY(1).GT.0.)  THEN
C     IF THETA & TY COINCIDES, NPINPS=TY-1 POSITION
C       NPINPS(IP)=1
        DO 11 J=1,NY
        IF(THETA(IP)-EPS.GT. TY(J-1).AND.
     &     THETA(IP)-EPS.LT.TY(J)) THEN
                  NPINPS(IP)=J
                                   ENDIF
        IF(THETA(IP)+EPS.GT. TY(J).AND.
     &     THETA(IP)-EPS.LT.TY(J)) THEN
                  NPINPS(IP)=-J
                                   ENDIF
*     WRITE(NOUT2,*) '  IP1=',IP,' NPINPS=',NPINPS(IP)
   11   CONTINUE
                       ELSE
C     IF THETA & TY COINCIDES, NPINPS=TY POSITION
C       NPINPS(IP)=NY
        DO 12 J=1,NY
        IF(THETA(IP)+EPS.GT. TY(J).AND.
     &     THETA(IP)+EPS.LT.TY(J+1))
     &            NPINPS(IP)=J
        IF(THETA(IP)+EPS.GT. TY(J).AND.
     &     THETA(IP)-EPS.LT.TY(J))
     &            NPINPS(IP)=-J
*     WRITE(NOUT2,*) '  IP2=',IP,' NPINPS=',NPINPS(IP)
   12   CONTINUE
                       ENDIF
C       IF(NPINPS(IP).EQ.NY1) NPINPS(IP)=ISIGN(NPINPS(IP),1)
   13 CONTINUE
      WRITE(NOUT2,3017)
      WRITE(NOUT2,3018) (NPINPS(I),I=1,NTPIN)
 3017 FORMAT(10X,'PIN AZIMUTHAL POSITION')
 3018 FORMAT(10X,10I12)
                      ENDIF
C
      IF (NHEX.EQ.1) THEN
C       HEXAGONAL
      PIT=RX(NX1)
      RX(NX1)=PIT*2.0/SQ3
                     ENDIF
C **  RAN OUTER LIMIT ABOVE WHICH THE ANGULAR INTEGRATION IS NOT NEEDED
C     IF(NY.GT.1)  RAN=RX(NX1)
                   RAN=RX(NX1)
C
             IF(NTPIN.GT.0) THEN
C **  INSERTION OF RPP INTO RX
      IX=NX1
      IP=NTPIN
      K=IX+NADDR
   39 CONTINUE
      IF (RX(IX).GE.RPP(IP)) GO TO 45
      IF (IDIVP.EQ.0) GOTO 43
      IF (IP.EQ.NTPIN) GO TO 40
      IF (RPP(IP).EQ.RPP(IP+1) ) GO TO 41
   40 RX(K)=RPP(IP)
      NTR(IP)=K
      K=K-1
      GO TO 42
   43 NTR(IP)=K
       GO TO 42
   41 NTR(IP)=NTR(IP+1)
   42 NPINDV(IP)=IDIVP
      IP=IP-1
      IF (IP.EQ.0) GO TO 47
      GO TO 39
   45 RX(K)=RX(IX)
      K=K-1
      IX=IX-1
      GO TO 39
   47 CONTINUE
      IF(RPP(1).EQ.0.) THEN
      NPINDV(1)=0
      NTR(1)=1
                       ENDIF
      NX=NX+NADDR
      NX1=NX+1
      WRITE(NOUT2,3015)
      WRITE(NOUT2,3010) (I,RX(I),I=1,NX1)
      WRITE(NOUT2,3033)
      WRITE(NOUT2,3018) (NTR(I),I=1,NTPIN)
      WRITE(NOUT2,3019)
      WRITE(NOUT2,3018) (NPINDV(I),I=1,NTPIN)
 3019 FORMAT(1H0,9X,'PIN DIVISION CONTROL ')
 3033 FORMAT(1H0,9X,'PIN RADIAL POSITION ')
                     ENDIF
C
      IX=0
      IF(NTPIN.NE.0) THEN
C **  NZP: NUMBER OF PIN ROD REGIONS
      DO 48 IP=1,NTPIN
   48 IX=IX+NDPIN*MAX0(1,NPINDV(IP))
                     ENDIF
      NZP=IX
C **  NAMER: FIRST LOCATION OF LAYER I
      NAMER(1)=NZP+1
CMM   NAMER(2)=NZP+2
CMM   IF(NX.GE.3) THEN
CMM   DO 49 I=3,NX
      DO 49 I=2,NX
   49 NAMER(I)=NAMER(I-1)+NY
CMM               ENDIF
C **  NZA: TOTAL NUMBER OF REGIONS
      NZA=NAMER(NX)+NY-1
*     WRITE(NOUT2,*) '       NAMER=',(NAMER(I),I=1,NX),' NZA=',NZA,
*    @               ' NY=',NY,' NX=',NX
C ** VOLUME OF SUB-REGION
C  **      NZP: BASE OF MODERATOR REGION
C    **  VHEX6: 1/12 OF AREA OF HEXAGON
         VHEX6=0.125*SQ3*RX(NX1)**2
      DO 60 I=1,NX
            J=NAMER(I)-1
CMM             IF(I.EQ.1) THEN
CMM         J=J+1
CMM         V(J)=PI*RX(2)**2
CMM         GO TO 60
CMM                        ENDIF
      IF (NHEX.EQ.1 .AND. I.EQ.NX ) THEN
      V0=0.0
        DO 50 IA=JST+1,JST+NY
        IJ=INT(TY(IA)/PI6+EPS)
C       IJ: MULTIPLE OF 30DEG
        VLW=IJ*VHEX6
        ALP=TAN(TY(IA)-IJ*PI6)
C   **  VLW: AZIMUTHAL AREA OF HEXAGON MEASURED FROM 0 TO TY(IA)
        VLW=VLW+ALP*RX(NX1)**2*0.375
C   **  VLL: AZIMUTHAL AREA OF INNER CIRCLE MEASURED FROM 0 TO TY(IA)
        VLL=RX(NX)**2*TY(IA)*0.5
        J=J+1
        VL=VLW-VLL
        V(J)=VL-V0
        V0=VL
   50   CONTINUE
      VS=VHEX6*12.0-RX(NX)**2*PI-V0
C     WRITE(6,*) ' VHEX6=',VHEX6,' V0=',V0,' VS=',VS
C    &  ,' VLW=',VLW,' VLL=',VLL,' V(8)=',V(NAMER(NX))
        IF(TY(1).NE.0.)  V(NAMER(NX))=V(NAMER(NX))+VS
                                   ELSE
      IF(TY(1).NE.0.)     THEN
      ANGP=TY(NY)-2.*PI
                          ELSE
      ANGP=0.
                          ENDIF
      DO 55 IA=1,NY
      J=J+1
C     JST=2-IST
      V(J)=(RX(I+1)**2-RX(I)**2)*(TY(IA+JST)-ANGP)/2.
   55 ANGP=TY(IA+JST)
                                   ENDIF
   60 CONTINUE
*     WRITE(NOUT2,*) ' VMOD=',(V(I),I=NZP+1,NZA)
C
      IJ=0
         IF(NTPIN.NE.0)                                   THEN
      DO 80 IP=1,NTPIN
C     IF(NHEX.EQ.0 ) RAN= AMAX1(RAN,RPP(I)+RDP(NDPIN1,I))
          IF(NPINDV(IP).NE.0) THEN
C **  VOLUME FRACTION BETWEEN INNER OF A PIN FOR NPINDV(IP)=1,2
      RR =RPP(IP)
      F(1,IP)=0.
      DO 64 ND=2,NDPIN1
      XX = (RR**2+RR**2-RDP(ND,IP)**2)/2./RR
      YY = SQRT(RR**2-XX**2)
      F(ND,IP) = RR**2*ACOS(XX/RR)+RDP(ND,IP)**2*ACOS((RR-XX)/
     1           RDP(ND,IP))         -RR*YY
   64 CONTINUE
                              ENDIF
C
*     WRITE(NOUT2,*) '  PIN NUMBER=',IP
          IF(NPINDV(IP).NE.2)       THEN
C **  VOLUMES OF FUEL REGIONS FOR NPINDV(IP)=0,1
      DO 76 ND=1,NDPIN
      IJ=IJ+1
      V(IJ)=PI*(RDP(ND+1,IP)**2-RDP(ND,IP)**2)
      NAMEP(1,ND,IP)=IJ
      NAMEP(2,ND,IP)=IJ
   76 CONTINUE
                                    ENDIF
C **  STARTING REGION NUMBER NRR COMMON FOR NPINDV(IP)=1,2
      NRR=NAMER(NTR(IP))-1
*     WRITE(NOUT2,*) '    NRR=',NRR
           IF(RPP(IP).EQ.0.) THEN
      DO  70  NS=1,NY
C TY(1)=0    1 2 3 4 5 6    JST=1  TY(NY1)=2*PI
C TY(1)>     1 2 3 4 5 6    JST=0  TY(0)  =TY(NY)-2*PI
      VPD=RDP(NDPIN1,IP)**2*(TY(NS+JST)-TY(NS+JST-1))/2.
      V(NRR+NS)=V(NRR+NS)-VPD
   70 CONTINUE
                   GO TO 80
                            ENDIF
           IF(NPINDV(IP).EQ.0) THEN
C **  VOLUMES OF MODERATOR REGION FOR NPINDV(IP)=0
      NAD=IABS(NPINPS(IP))
      IF(NPINPS(IP).LT.0)  THEN
C     THETA CROSSES WITH TY
      NAD2=NAD+IST-1
      IF(NAD2.GT.NY) NAD2=1
      NAD1=NAD2-1
      IF(NAD1.EQ.0)  NAD1=NY
*     WRITE(NOUT2,*) ' -0  NAD1,NAD2,NRR=',NAD1,NAD2,NRR
      V(NRR+NAD1)=V(NRR+NAD1)-PI*RDP(NDPIN1,IP)**2/2.
      V(NRR+NAD2)=V(NRR+NAD2)-PI*RDP(NDPIN1,IP)**2/2.
                           ELSE
*     WRITE(NOUT2,*) '  0       NAD,NRR=',NAD,NRR
      V(NRR+NAD)=V(NRR+NAD)  -PI*RDP(NDPIN1,IP)**2
                           ENDIF
                    GO TO 80
                               ENDIF
          IF(NPINDV(IP).GT.0) THEN
C     VOLUMES OF MODERATOR REGIONS FOR NPINDV(IP)=1,2
      VPIN1=F(NDPIN1,IP)
      VPIN2=PI*RDP(NDPIN1,IP)**2-F(NDPIN1,IP)
      NAD=IABS(NPINPS(IP))
      IF(NPINPS(IP).LT.0)  THEN
C     THETA CROSSES WITH TY
      NAD2=NAD+IST-1
      IF(NAD2.GT.NY) NAD2=1
      NAD1=NAD2-1
      IF(NAD1.EQ.0)  NAD1=NY
CMM      IF(RPP(IP).EQ.RX(2)) THEN
CMM   V(NAMER(1)) =V(NAMER(1)) -VPIN1
CMM                           ELSE
*     WRITE(NOUT2,*) ' -1  NAD1,NAD2,NRR=',NAD1,NAD2,NRR
      V(NRR+NAD1-NY)=V(NRR+NAD1-NY)-VPIN1/2.
      V(NRR+NAD2-NY)=V(NRR+NAD2-NY)-VPIN1/2
CMM                           ENDIF
      V(NRR+NAD1)=V(NRR+NAD1)-VPIN2/2.
      V(NRR+NAD2)=V(NRR+NAD2)-VPIN2/2
                           ELSE
CMM      IF(RPP(IP).EQ.RX(2)) THEN
CMM   V(NAMER(1)) =V(NAMER(1)) -VPIN1
CMM                           ELSE
*     WRITE(NOUT2,*) '  1       NAD,NRR=',NAD,NRR
      V(NRR+NAD)=V(NRR+NAD)-VPIN2
      V(NRR+NAD-NY)=V(NRR+NAD-NY)-VPIN1
CMM                           ENDIF
                           ENDIF
                               ENDIF
              IF(NPINDV(IP).EQ.2)  THEN
C **  NUMBERING AND VOLUMES OF FUEL REGION FOR NPINDV(IP)=2
      DO 78 ND=1,NDPIN
      IJ=IJ+1
      NDI=NDPIN1-ND
      V(IJ)=(F(NDI+1,IP)-F(NDI,IP))
      NAMEP(1,NDI,IP)=IJ
   78 CONTINUE
      DO 79 ND=1,NDPIN
      IJ=IJ+1
      V(IJ)=(PI*(RDP(ND+1,IP)**2-RDP(ND,IP)**2)-(F(ND+1,IP)-F(ND,IP))
     1)
      NAMEP(2,ND ,IP)=IJ
   79 CONTINUE
                                   ENDIF
C     NPINPS(IP)=IABS(NPINPS(IP))
   80 CONTINUE
      IF(NZA.NE.NZ) THEN
      WRITE(NOUT1,3012) NZA,NZ
      WRITE(NOUT2,3012) NZA,NZ
      STOP
                    ENDIF
C
C
                                                          ENDIF
      SZ=2.*PI*RX(NX1)
      RANGE=RX(NX1)
      IF (NHEX.NE.0) SZ=RX(NX1)*6.0
      NDR=NX*NGR
C     RO1=0.
C     FVOL=2.
C     IF BETM >= PI THEN
      IF(BETM .GT. 3.14)
     &BETM=PI
C     RO1=-RX(NX1)
         FVOL=1.
         NPINZ=0
      IF(NTPIN.GT.0 .AND. RPP(1).EQ.0.) THEN
         PINR=RDP(NDPIN1,1)
         NPINZ=1
                                        ENDIF
      RETURN
 3015 FORMAT(10X,'BOUNDARIES OF RADIAL DIVISION'  )
 3010 FORMAT(10X,I3,1H),1PE11.4,I3,1H),   E11.4
     1 ,I3,1H),E11.4,I3,1H),E11.4,I3,1H),E11.4,I3,1H),E11.4,I3,1H),E11.4
     2,I3,1H),E11.4)
 3012 FORMAT(10X,'** INCONSISTENT TOT-S-REGION', 2I5,' IN CLINH')
      END
