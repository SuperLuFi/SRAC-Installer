      SUBROUTINE   IRASRC(FSET  ,ANS   ,FSIG0 ,X     ,Y     ,FTAB  ,
     &                    C     ,D     ,E     ,MXSIG0,LNMAX ,NSIG  )
C
C
C
      DIMENSION    FSIG0(MXSIG0),X(MXSIG0),Y(MXSIG0),FTAB(MXSIG0)
      DIMENSION    C(LNMAX)     ,D(LNMAX) ,E(LNMAX) ,DY(2)
C
      DY(1)    = 0.0
      DY(2)    = 0.0
C
C     WRITE(6,12) FSET
C  12 FORMAT(1H ,' ##  FSET ## ',1P3E12.5)
C
CKSK  CALL  INSPL( X , Y , DY , NSIG , C , D , E , ICON )
      CALL UINSPL( X , Y , DY , NSIG , C , D , E , ICON )
C
C -----SEARCH SIGMA-0
C
      I1   = NSIG
      ANS  = FSIG0(I1)
      IF(FSET.GE.FTAB(NSIG)) GO TO 101
C
      DO 30 I = 1 , NSIG
      II = I
      IF(FSET.LT.FTAB(I)) GO TO 31
   30 CONTINUE
C
   31 I1 = II
      ANS =FSIG0(1)
      IF(I1.LE.1)  GO TO 101
C
      I2 = II
      I1 = I2-1
C     WRITE(6,32) I1,I2,FSET,FTAB(I1),FTAB(I2)
C
C  32 FORMAT(1H ,' ## I1 I2 F SET F1 F2 ## ',2I4,1P4E12.5)
C
      F1 = Y(I1)
      F2 = Y(I2)
      FFF= ALOG(FSET)
C
      XSIGX = X(I1)
      DELSIG= (X(I2) - X(I1) ) * 0.1
C
            DO 40 I = 1 , 10
            XSIGX = XSIGX + DELSIG
            DELX  = XSIGX - X(I1)
            AFFF  = Y(I1) +((E(I1)*DELX +D(I1))*DELX + C(I1) )*DELX
            IF(AFFF.GT.FFF)  GO TO 41
   40       CONTINUE
C
   41 CONTINUE
      XSIGX =  XSIGX - DELSIG
      DELSIG=  DELSIG* 0.1
            DELX  = XSIGX - X(I1)
            AFFF  = Y(I1) +((E(I1)*DELX +D(I1))*DELX + C(I1) )*DELX
            YYY1  = AFFF
            XXX1  = XSIGX
C
            DO 50 I = 1 , 10
            XSIGX = XSIGX + DELSIG
            DELX  = XSIGX - X(I1)
            AFFF  = Y(I1) +((E(I1)*DELX +D(I1))*DELX + C(I1) )*DELX
            YYY2  = AFFF
            XXX2  = XSIGX
            IF(AFFF.GT.FFF)  GO TO 51
            YYY1  = AFFF
            XXX1  = XSIGX
   50       CONTINUE
C
   51 CONTINUE
      DELXXX = (FFF-YYY1) * ( XXX2 - XXX1 ) / (  YYY2 - YYY1 )
      ANS    = XXX1 + DELXXX
      ANS    = EXP(ANS)
C
  101 ANSS   = ALOG(ANS)
            DELX  = ANSS- X(I1)
            AFFF  = Y(I1) +((E(I1)*DELX +D(I1))*DELX + C(I1) )*DELX
            AFFF  = EXP(AFFF)
C
C     WRITE(6,102) ANS,AFFF,FSET
C 102 FORMAT(1H , ' ##  ANS FANS FSET ## ',1P3E15.8)
C
      RETURN
      END
