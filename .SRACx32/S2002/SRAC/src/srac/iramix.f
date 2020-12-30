      SUBROUTINE  IRAMIX(AMIX,AM,S,KNMAX,NISOM,RTAKA,SSSS)
C
      COMMON /IRAPSE/ TAKA,SQGUZ,C1B,ZETT,DELTA1
      COMMON /IRACNL/ IOPT(39),NOUT1,NOUT2
C
      DIMENSION   AM(KNMAX)
C
C     FUNCTION  SET
C
      ALFA(X)=((X-1.0)/(X+1.0))**2
      FUNCMU(A1,B1,X)= 1.-A1 -ATAN(X)/X + B1*ALOG(1.+X*X)/(X*X)
C
C     PSUEDO MASS SEARCH START
C
C     WRITE(NOUT2,7771) TAKA,S,SQGUZ,C1B,ZETT,DELTA1
C7771 FORMAT(1H ,' *TAKA,S,SQGUZ,C1B,ZETT,DELTA1* ',1P6E12.5)
      A   =TAKA
      AMIX=0.0
      DO 10 N=1,NISOM
      IF(AM(N).GT.0.0)  GO TO 11
   10 CONTINUE
      RETURN
C
   11 B   =SQGUZ*ZETT/C1B
      D   =1.0-A
      C   =0.5*DELTA1/SQGUZ
      ETA =0.000001
      ISW =0
C     WRITE(NOUT2,7773) A,B,C,D
C7773 FORMAT(1H ,' **A,B,C,D(IRAMIX)** ',1P4E12.5)
C
      X1  = 0.0
      F1  = -A
      XX  = 0.0
      IF(ABS(F1).LT.ETA)  GO TO 1111
      FLIMIT=FUNCMU(A,B,C)
      IF(ABS(FLIMIT).LT.ETA) GO TO 1111
      IF(FLIMIT.GT.0.0)  GO TO 12
C
      AMIX=1.0
C=======================================================================
C                  S     = S*A / (A+FLIMIT)
                   SSSS  = S*A / (A+FLIMIT)
                   RTAKA =  A + FLIMIT
C=======================================================================
      RETURN
C
   12 CONTINUE
      DELX=0.01*C
   20 CONTINUE
      ISW =ISW + 1
C     WRITE(NOUT2,7772)  ISW,DELX
C7772 FORMAT(1H ,' **ISW,DELX(IRAMIX)** ',I6,1PE12.5)
      DO 21 II=1,100
      X2 = X1+DELX
      F2 =FUNCMU(A,B,X2)
      XX = X2
      IF(ABS(F2).LT.ETA)  GO TO 1111
      FF =F1*F2
      IF(FF.LT.0.0)       GO TO  111
      X1 = X2
      F1 = F2
   21 CONTINUE
C
   22 CONTINUE
      WRITE(NOUT1,23)
      WRITE(NOUT2,23)
      STOP
   23 FORMAT(1H ,10X,' ERROR STOP -- PSUEDO MASS IS NOT FOUND (SUBR.',
     &' IRAMIX)')
C
  111 CONTINUE
      ITR = 0
      XX1 = X1
      FF1 = F1
      DELX=(X2-X1)*0.01
  112 CONTINUE
      ITR=ITR+1
      XX =X1-F1*(X2-X1)/(F2-F1)
      FF =FUNCMU(A,B,XX)
      IF(ABS(FF-D).LE.ETA)  GO TO 22
      IF(ABS(FF).LT.ETA) GO TO 1111
      IF(ITR.LE.10)  GO TO 120
      X1 =XX1
      F1 =FF1
      GO TO 20
C
  120 CONTINUE
      IF(FF.LT.0.0)  GO TO 121
      X2 = XX
      F2 = FF
      GO TO 112
C
  121 CONTINUE
      X1 = XX
      F1 = FF
      GO TO 112
C
C     AMIX CAL
C
 1111 CONTINUE
      ALFMIX = 1.0-XX/C
      AAA    = SQRT(ALFMIX)
      AMIX   = 1.0E+5
      IF(ABS(AAA-1.0).GT.ETA)  AMIX=(1.0+AAA)/(1.0-AAA)
C=======================================================================
                      RTAKA = TAKA
                      SSSS  = S
C=======================================================================
      RETURN
      END
