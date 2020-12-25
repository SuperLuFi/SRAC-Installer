C    *******************************************************************
                  SUBROUTINE TRKSM
C                 CALLED BY SEVERAL GEOMETRY TRACE ROUTINES
C     SURFACE MODE  BY INTEGER TO DIVIDE XLENG
C           FOR INCURRENT OR OUT-CURRENT NEUTRON
C          MESURURED IN ANTI-CLOCKWEISE
C     ******************************************************************
C                  ARGUMENTS OF THE ROUTINE
     *             (A,B,XS,YS,XLENG,NSR,SR,J,ANG,IER)
C     A(X-XS) + B(Y-YS) = 0  THE EQUATION TO EXPRESS THE SURFACE
C     XS    STARTIOG X-POPSITION OF THE SURFACE
C     YS    STARTIOG Y-POPSITION OF THE SURFACE
C     NSR   NUMBER OF SURFACE SEGMENTS
C     SR    FRACTIONAL DIVISION OF THE SURFACE RANGING BETWEEN (0.,1.)
C     J     INTEGER TO INDICATE THE SURFACE SEGMENT
C    *******************************************************************
C    *******************************************************************
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGA,NDPIN,IDIVP,
     1                 BETM,NX1,NY1,LOCAL(3),IXP,IYP,IZP,NDPIN1,
     2                 NDR,NDA,LL,L0,RO1,DRO,FVOL,LOCAL1,LOCAL2,RR,
     3                 DUM,LOCAL3,LOCAL4,SINB,COSB,LOCAL5
C    *******************************************************************
C      COMMON /ITRACE/
C    * TT(200),IM(200),IP(200)
C    *******************************************************************
      DIMENSION SR(NSR+1)
C****************************
      RHO2=RR
      SINA=SINB
      COSA=COSB
C***************************
      IF(A.EQ.0) GO TO 100
      IF(B.EQ.0) GO TO 200
      X0=((YS+A/B*XS)*COSA-RHO2)/(SINA+A/B*COSA)
      F=ABS(X0-XS)*SQRT(1.+(A/B)**2)/XLENG
                 GO TO 300
  100 X0=(YS*COSA-RHO2)/SINA
      F=ABS(X0-XS)/XLENG
                 GO TO 300
  200 Y0=(XS*SINA+RHO2)/COSA
      F=ABS(Y0-YS)/XLENG
  300            DO 10 I=1,NSR
                    J=I
      IF(F.LT.SR(I+1)) GO TO 20
   10            CONTINUE
      IF(ABS(F-SR(NSR+1)).LT.1.E-4) GO TO 20
   15 WRITE(6,*) ' **** ILLEGAL SURFACE MODE ENCOUNTERED *** ERROR STOP'
     *, ' *** FACTOR=',F,' A=',A,' B=',B,' YS=',YS,' XS=',XS,' X0=',X0
     * ,' RHO=',RHO2,' ANG=',ANG,' SINA=',SINA,' COSA=',COSA
                   IER = 1
                   RETURN
   20              CONTINUE
                   RETURN
                    END
