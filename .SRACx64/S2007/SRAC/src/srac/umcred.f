      SUBROUTINE   UMCRED(XIN,YIN,STORE,NPMAX,NP)
C
      REAL*4       XIN(NPMAX),YIN(NPMAX),STORE(1)
C
      CALL CLEA ( XIN , NPMAX , 0.0 )
      CALL CLEA ( XIN , NPMAX , 0.0 )
C
      IDEL     = NP
      DO 100 I = 1 , NP
      XIN(I)   = STORE(I)
      YIN(I)   = STORE(I+IDEL)
  100 CONTINUE
C
      CALL CLEA ( STORE , 2*NPMAX , 0.0 )
C
      RETURN
      END
