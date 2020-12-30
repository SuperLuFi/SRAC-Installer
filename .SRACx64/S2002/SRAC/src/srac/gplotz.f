C     SUBROUTINE    GPLOTZ
C       WRITTEN  BY   A. HASEGAWA     ' JAERI  '   1973.02.22
C
      SUBROUTINE    GPLOTZ   ( JMAX1, X   , Y   , X1   , Y1   ,ICUMN   ,
     1IMAX1   , IDA   , IDR   ,IDN   ,XTITLE   , YTITLE   ,TITLE   ,
     2JPLOT   , NNN    ,IIP   ,NNP   ,IIST   ,A   ,B   ,WITHX,WITHY,
     3 MSCALE, RATIOX,RATIOY , MAXD,NEWP)
C
C     JMAX1      ---NUMBER OF INDIVIDUAL  DATA
C     X(4003)    --- WORK  AREA  FOR  PLOT  X-DATA
C     Y(4003)    --- WORK  AREA  FOR  PLOT  Y-DATA
C     X1(20000)  --- X-DATA   PACKED
C     Y1(20000)  --- Y-DATA   PACKED
C     ICUMN(100) --- INITIAL NUMBER IN EACH DATA ARRAY FOR INDIVIDUAL DA
C     IMAX1(100) --- NUMBER OF DATA IN EACH  INDIVIDUAL  DATA
C     IDA(100)   --- DATA IDENTIFICATION -1  , DOUBLE PRECISION
C     IDR(100)   --- DATA IDENTIFICATION -2  , DOUBLE PRECISION
C     IDN(100)   --- DATA IDENTIFICATION -3  , DOUBLE PRECISION
C     XTITLE(10) ---  X-AXIS  TITLE (COMMENT)   40- ALPHAMERIC  CHARACTE
C     YTITLE(10) ---  Y-AXIS  TITLE (COMMENT)   40- ALPHAMERIC  CHARACTE
C     TITLE(10)  --- CHART  TITLE (COMMENT)     40-ALPHAMERIC CHARACTER
C     JPLOT(100) --- SUPERVISOR  PLOT-SKIP  OPTION (IF.0 THEN I-TH PLOT
C     NNN        ---  NUMBER OF  X-DATA  BOUNDARY  FOR PLOTTING  .LE.10
C     IIP (100)  --- OPTION FOR   LINE - DASHED LINE  PLOT
C     NNP (100)  --- OPTION FOR   SYMBOL PLOT
C     IIST(100)  --- OPTION FOR   SYMBOL PLOT INTERVAL
C     A(10)      --- LOWER BOUNDARY OF X-AXIS FOR  I-TH CHART
C     B(10)      --- UPPER BOUNDARY OF Y-AXIS FOR  I-TH CHART
C     WITHX      --- LENGTH FOR  X-AXIS  OF THE INDIVIDUAL CHART.
C     WITHY      --- LENGTH FOR  Y-AXIS  OF THE INDIVIDUAL CHART.
C     MSCALE     --- SCALING OPTION. USUALLY  0 = AUTOMATIC SCALING
C     RATIOX     --- LINEAR- LOG  SCALE  OPTION
C     RATIOY     --- LINEAR- LOG  SCALE  OPTION
C     MAXD       --- MAXIMUM NUMBER FOR  INDIVIDUAL DATA SETS /X,Y=WORK
C     NEWP(100)  --- SELECTION FOR  NEW PEN  NO  (1--2--3)
      DIMENSION    X(1),  Y(1),  X1(1), Y1(1),  ICUMN(1), IMAX1(1),
     1    IDA(1),  IDR(1),  IDN(1), XTITLE(1), YTITLE(1), TITLE(1),
     2    JPLOT(1), IIP(1), NNP(1), IIST(1), A(1), B(1),NEWP(1)
      DOUBLE PRECISION    IDA,IDR,IDN
      DIMENSION     IP(100),IL1(100),IU1(100),EMAX1(100),EMIN1(100),
     1 SMAX1(100),SMIN1(100)
C
      DO     1150   LL=1,NNN
C     INITIAL  SET
      DO     1000   I=1,MAXD
      X(I)= 0.
      Y(I)= 0.
 1000 CONTINUE
C
      DO     1100   I=1,JMAX1
      IL1(I)= 0
      IU1(I)= 0
      EMAX1 (I) =  0.
      EMIN1 (I) =  0.
      SMAX1 (I) =  0.
      SMIN1 (I) =  0.
      IP    (I) =  0
 1100 CONTINUE
C
      EMAX  = 0.
      EMIN= 0.
      SMAX= 0.
      SMIN= 0.
      EL  = A(LL)
      EU  = B(LL)
C
C     END OF INITIAL SETTING
C
C     ENERGY  BOUNDARY  SET  AND  MAX  MIN  SEARCH
C
      DO   1200  I= 1,JMAX1
      IF(JPLOT(I).EQ.0)  GO TO 1200
      CALL  ISERCH ( I, EL, EU, IL, IU, EMAX,EMIN, X1,ICUMN,IMAX1 )
      IL1(I) = IL
      IU1(I) = IU
      EMAX1(I) = EMAX
      EMIN1(I) = EMIN
      IF(IL.NE.0) IP(I)=1
      CALL YSERCH  ( IL, IU, SMAX, SMIN, Y1 )
      SMAX1(I)= SMAX
      SMIN1(I)= SMIN
 1200 CONTINUE
C
C
      EMIN= 1.0E+30
      SMIN= EMIN
      AA  = EMIN
      EMAX= 0.
      SMAX= 0.
C
      DO 1330  I=1,JMAX1
      IF( (EMAX1(I).NE.0.).AND.(EMAX1(I).GT.EMAX) )  EMAX = EMAX1(I)
      IF( (EMIN1(I).NE.0.).AND.(EMIN1(I).LT.EMIN) )  EMIN = EMIN1(I)
      IF( (SMAX1(I).NE.0.).AND.(SMAX1(I).GT.SMAX) )  SMAX = SMAX1(I)
      IF( (SMIN1(I).NE.0.).AND.(SMIN1(I).LT.SMIN) )  SMIN = SMIN1(I)
 1330 CONTINUE
C
      IF( (EMAX.EQ.0.).OR.(SMAX.EQ.0.) )  GO  TO  1150
      IF( (EMIN.EQ.AA).OR.(SMIN.EQ.AA) )  GO  TO  1150
C
C     FLAME  ONLY   ---  GPLOTI  /   TITLEP  MODIFIED   /TITLE/ COMMON
C
      X(1) = EMIN
      X(2) = EMAX
      Y(1) = SMIN
      Y(2) = SMAX
      CALL  GPLOTI(1,5,X,Y,WITHX,WITHY,0,0,0,NLOGX,NLOGY,XWIDE,YWIDE,
     1 IXMIN, IYMIN, AX1, AX2, AY1, AY2, MSCALE, RATIOX, RATIOY ,
     2 XTITLE,YTITLE,TITLE, MM, IDA,IDR,IDN,JMAX1 )
C     MULTI-PLE  LINE  PLOT
C
      DO   1440  I=1,JMAX1
      MM= I
      IF(IP(I).EQ.0)  GO TO 1440
      CALL  NEWPEN(NEWP(I))
      IU= IU1(I)
      IL= IL1(I)
      IMAX3= IU-IL+1+3
      DO  1460  L=IL,IU
C     IUP1= IU+1
C     DO  1460   L=IL,IUP1
      J= L-IL+1
      X(J)= X1(L)
C     IF(L.EQ.IUP1)  GO TO 1460
      Y(J)= Y1(L)
C     PRINT  30,L,X(J),Y(J)
   30 FORMAT(1H ,I12,2E15.6)
 1460 CONTINUE
      IIP1=  IIP(I)
      NNP1=  NNP(I)
      IST1= IIST(I)
      CALL GPLOTI(0,IMAX3,X,Y,WITHX,WITHY,IIP1,NNP1,IST1,NLOGX,NLOGY,
     1 XWIDE,YWIDE, IXMIN,IYMIN,AX1,AX2,AY1,AY2,MSCALE, RATIOX,RATIOY,
     2 XTITLE,YTITLE,TITLE,MM,IDA,IDR,IDN,JMAX1)
 1440 CONTINUE
 1150 CONTINUE
      RETURN
      END