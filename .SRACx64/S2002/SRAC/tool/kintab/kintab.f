C
C    MAIN PROGRAM TO MAKE UP BLOCK DATA OF BICKLEY FUNCTION
C      NEW ABC 1100
      REAL*8 ERR,X,Y,A,B,C,DY,FKIN
C     ===  EXACT VALUES OF KIN(X) TABULATE ===
      COMMON ERR
      DIMENSION X(2202) ,Y(2201,5),A(1100,5),B(1100,5),C(1100,5)
      DIMENSION DY(5)
      CALL UIOSET
      IOTAB = 10
      ERR=1.D-11
      X(1)=0.0D0
C     DEL=0.005
      WRITE(6,2)
   2  FORMAT(1H1,20X,  'K I N F U N C T I O N'                //
     1'    X        KI1            KI2            KI3      '   ,
     1'      KI4            KI5      '  /)
      Y(1,1)=1.5707963267949D0
      Y(1,2)=1.0000000000000D0
      Y(1,3)=0.7853981633974D0
      Y(1,4)=0.6666666666667D0
      Y(1,5)=0.58904862256D0
      WRITE(6,5)   X(1),(Y(1,N),N=1,5)
      DO 3 I=2,2201
      X(I)=DFLOAT(I-1)/200.D0
      DO 1 N=1,5
      Y(I,N)=FKIN(X(I),N)
   1  CONTINUE
   3  CONTINUE
C    1,7X)
   5  FORMAT(F7.3,2X,1P5D16.9)
      I=1
      DO 200 J=1,1099
      I=I+1
      DO 150 N=1,5
      A(J,N)=(4.D0*Y(I,N)-3.D0*Y(I-1,N)-Y(I+1,N))/0.01D0
      B(J,N)=(Y(I+1,N)-2.D0*Y(I,N)+Y(I-1,N))/0.00005D0
      C(J,N)=Y(I-1,N)
  150 CONTINUE
      I=I+1
      WRITE(6,5)   X(I),(Y(I,N),N=1,5)
  200 CONTINUE
      DO 450 N=1,5
      A(1100,N)=0.0D0
      B(1100,N)=0.0D0
      C(1100,N)=0.0D0
  450 CONTINUE
      WRITE(IOTAB)((A(J,N),J=1,1100),N=1,5)
      WRITE(IOTAB)((B(J,N),J=1,1100),N=1,5)
      WRITE(IOTAB)((C(J,N),J=1,1100),N=1,5)
    9 FORMAT(2X,F9.3,5D12.5)
      WRITE(6,*) ' *** RELATIVE ERROR BY QUADRATIC INTERPOLATION ****'
      DX=0.005D0
      DO 400 J=1,1100
      JJ=2*J
      DO 350 N=1,5
      DY(N)=(Y(JJ,N)-(C(J,N)+DX*(A(J,N)+DX*B(J,N))))/Y(JJ,N)
  350 CONTINUE
      WRITE(6,9) X(JJ),(DY(N),N=1,5)
  400 CONTINUE
      WRITE(6,'(A)') ' ============================= END OF'
     &               //' CALCULATION ============================='
      STOP
      END
C === SIMPSON
      SUBROUTINE   SIMPS(A,B,FUNC,ANS,ERR,Y,N)
      REAL*8 PREV,SONE,STWO,DEL,X,CUR,ANS,A,B,Y,FUNC,ERR
      NDIV=1
      PREV=0.0D0
      SONE=(B-A)*(FUNC(A,Y,N)+FUNC(B,Y,N))/2.0D0
  10  NDIV=2*NDIV
      STWO=0.0D0
      DEL=(B-A)/DFLOAT(NDIV)
      DO  20  I=1,NDIV,2
      X=A+DEL*DFLOAT(I)
      STWO=STWO+FUNC(X,Y,N)
  20  CONTINUE
      CUR=SONE+4.0D0*DEL*STWO
      IF (ERR*DABS(CUR)  .GE. DABS(CUR-PREV)) GO TO 30
      PREV=CUR
      SONE=(SONE+CUR)/4.D0
      GO TO 10
  30  ANS=CUR/3.0D0
      RETURN
      END
C     === KIN  INTEGRAND  ===
      FUNCTION  XKINI(X,Y,N)
      REAL*8 XKINI,X,Y,Z,EP,ARG
      REAL*8 TMP
      Z=DCOS(X)
      IF(Z.LE.0.) THEN
      EP=0.
      GO TO 20
                  ENDIF
      IF(Y.GT.0.) THEN
      IP=1
      ARG=Y/Z
      DO 10 I=1,3
      IF(ARG.GT.150.) THEN
      ARG=ARG/2.D0
      IP=IP*2
                      ENDIF
   10 CONTINUE
      IF(ARG.GT.150.) THEN
      EP=0.
      GO TO 20
                      ENDIF
CKSK to avoid underflow stop in SX-3
CDEL  EP=DEXP(-ARG)**IP
        TMP = DEXP(-ARG)
        IF(DABS(TMP).LE.1.0D-32.AND.IP.GE.2) THEN
          EP = 0.0D0
        ELSE
          EP=DEXP(-ARG)**IP
        ENDIF
CKSK
                  ELSE
      EP=1.D0
                  ENDIF
      Z=Z**(N-1)
   20 XKINI=Z*EP
      RETURN
      END
C     === FKIN  INTEGRAND  ===
      FUNCTION FKIN(Y,N)
      REAL*8 ERR,XKINI,FKIN,Y
      COMMON ERR
      EXTERNAL XKINI
      CALL SIMPS(0.0D0,1.5707963267949D0,XKINI,FKIN,ERR,Y,N)
      RETURN
      END
C***********************************************************************
C
C  UIOUNT   : SET UNFORMATED(0) OR FORMATED(1)
C             FOR EACH I/O DEVICE
C
C***********************************************************************
C
      SUBROUTINE UIOUNT(IOFORM)
      DIMENSION IOFORM(100)
      DO 100 I=1,100
        IOFORM(I) = -1
  100 CONTINUE
      IOFORM(5)  = 1
      IOFORM(6)  = 1
      IOFORM(10) = 0
      RETURN
      END 
