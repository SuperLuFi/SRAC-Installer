C    *******************************************************************
                  SUBROUTINE TRKCRS
C            CALLED BY SEVERAL GEOMETRY TRACE ROUTINES
C    SIGHNED DISTANCE BETWEEN H(FOOT OF O ON THE LINE RHO ANG) AND
C                     P(CROSSING POINT OF THE LINE RHO ANG AND
C                       THE LINE AX+BY+C=0)
C    *******************************************************************
C              ARGUMENTS OF THE ROUTINE
     *                (A,B,C,HP,ANG)
C    *******************************************************************
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGA,NDPIN,IDIVP,
     1                 BETM,NX1,NY1,LOCAL(3),IXP,IYP,IZP,NDPIN1,
     2                 NDR,NDA,LL,L0,RO1,DRO,FVOL,LOCAL1,LOCAL2,RR,
     3                 DUM,LOCAL3,LOCAL4,SINB,COSB,LOCAL5
C    *******************************************************************
C     DIMENSION TT(1),IM(1),IP(1)
C    *******************************************************************
      DATA PI/3.1415926/
C    *******************************************************************
C    NORMAL  CASE    A*B .NE. 0
C    SPECIAL CASE    A   .EQ. 0  100
C    SPECIAL CASE    B   .EQ. 0 200
C    *******************************************************************
C****************************
      RHO2=RR
      SINA=SINB
      COSA=COSB
C****************************
      TANA=SINA/COSA
      IF(A.EQ.0.) GO TO 100
      IF(B.EQ.0.) GO TO 200
      TANT=-A/B
      THETA=ATAN(TANT)
      THETA=MOD(THETA+PI,PI)
      COST=COS(THETA)
      IF(ABS(TANT-TANA).LT.1E-4) THEN
      WRITE(6,50) ' *** PARALLEL OF NEUTRON LINE AND GEOMETRY LINE',
     * 'RHO2=',RHO2,' ANG=',ANG,' *** CHANGE NUMBER OF ANGLULAR MESH'
   50 FORMAT(A,A,E12.5,A,F8.5/A)
                         STOP
                                  ENDIF
      SB=-C/B
      HP=((SB-RHO2/COSA)/(TANA-TANT))*(COS(THETA-ANG)/COST)+SB*SINA
C     WRITE(6,*)'HP=',HP
                  GO TO 300
C                RETURN C
C      SPECIAL CASE A=0 FOLLOWS;
  100 SB=-C/B
      HP=SB/SINA-RHO2/TANA
C     WRITE(6,*)'HP2=',HP
                  GO TO 300
C                 RETURN
C     SPECIAL CASE B=0 FOLLOWS;
  200 SC=-C/A
      HP=SC/COSA+RHO2*TANA
C     WRITE(6,*)'HP3=',HP
  300             RETURN
                    END
