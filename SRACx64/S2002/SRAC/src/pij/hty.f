C    *******************************************************************
                  FUNCTION  HTY
C                 CALLED BY TRKHX6
C    *******************************************************************
C        INTERSECTION WITH THE RADIATING LINE
C **********************************************************************
C      ARGUMENTS
     *              (JSM,NTYZ,TY,NGR,IRING,IFLAG,IP,IM,ANG)
      DIMENSION         TY(*)
C **********************************************************************
      DATA PI,CRT3/3.1415926,1.7320508/
      JSS=MOD(JSM+6*NTYZ,6*NTYZ)
      NN=MOD(JSS,NTYZ)+1
      JS=JSS/NTYZ
      THETN=ATAN(CRT3*TY(NN)/(2.-TY(NN)))
      THETA=PI/3.*FLOAT(JS)+THETN
      THETA=MOD(THETA,PI)
      IF(ABS(THETA-PI/2.).LT.1.E-3) THEN
      A=1.
      B=0.
                                    ELSE
      A=-TAN(THETA)
      B=1.0
                                    ENDIF
      C=0.
      CALL TRKCRS(A,B,C,HSM,ANG)
      HTY=HSM
      IF(NN.GT.1) THEN
      I1=NGR*(JS+1)-NTYZ*IRING+NN
      I2=I1-1
                  ELSE
      I1=NGR*(JS+1)-NTYZ*IRING+1
      I2=I1-NGR+NTYZ-1
                  ENDIF
C*    I1=MOD(I1+6*NGR,6*NGR)
C*    I2=MOD(I2+6*NGR,6*NGR)
C*    IF(I1.EQ.0) I1=6*NGR
C*    IF(I2.EQ.0) I2=6*NGR
      I1=MOD(I1,NGR)
      I2=MOD(I2,NGR)
      IF(I1.EQ.0) I1=NGR
      IF(I2.EQ.0) I2=NGR
      IF(IFLAG.EQ.1) THEN
      IP=I2
      IM=I1
                     ELSE
      IP=I1
      IM=I2
                     ENDIF
      RETURN
      END
