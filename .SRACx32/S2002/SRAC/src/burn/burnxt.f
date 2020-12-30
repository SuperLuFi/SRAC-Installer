      SUBROUTINE   BURNXT( J79    ,  NMAT   , MATDPL , LISO   , IDTEMP ,
     1                     NTNUC  ,  NTDEPL , DNARAY , DNNEW  , MTNAME ,
     2                     SRACID ,  IBEND  , NEP    , I79    , NOUT2  ,
     3                     IPBURN , MATXRG  , MXNUC )
C
      CHARACTER*8    MTNAME(NMAT)
      CHARACTER*4    IDTEMP,SRACID(MXNUC)
C
      INTEGER*4      MATDPL(NMAT),LISO(NMAT)
      REAL*4         DNARAY(1),DNNEW(MXNUC,NTDEPL)
      INTEGER*4      IPBURN(MXNUC,NTDEPL),MATXRG(NMAT)
C
C
C
      DO 100  M  = 1,NMAT
      MPOS       = MATDPL(M)
      NOXRG      = MATXRG(M)
      IF(MPOS.LE.0) GO TO 100
C
      WRITE(NOUT2,110)  M,NOXRG
      IF(NOXRG.LE.0)    THEN
                        WRITE(NOUT2,130)
       WRITE(NOUT2,120) (SRACID(N),DNNEW(N,MPOS),N=1,NTNUC)
C
                        ELSE
       WRITE(NOUT2,120) (SRACID(N),DNNEW(N,MPOS),N=1,NTNUC)
                        IST          = LISO(M) -1
                        DO  50    I  = 1 , NTNUC
                        IPOS         = IPBURN(I,MPOS)
         IF(IPOS.GT.0)  DNARAY(IST+IPOS)= DNNEW(I,MPOS)
   50                   CONTINUE
                        ENDIF
  100 CONTINUE
C
  110 FORMAT(/1H0,4X,'MATERIAL NO.=',I3,'  &  X-REGION NO.=',I3)
  120 FORMAT(/8(1X,A4,1PE11.4))
  130 FORMAT(/1H ,4X,' WARNING ... THIS FUEL IS NOT BURNUP MATERIAL.',
     1     ' ONLY USED AS A SOURCE MATERIAL FOR BURNUP CALCULATION !')
C
C ******** UPDATE I79 FOR BURNUP STEP NUMBER
C ******** PACK I79 INTO 6-TH CHARACTER OF MTNAME IN /MAINC/ *******
C
      IF(J79.GE.NEP) IBEND = 1
C
      I79  = I79 + 1
C
      DO 300 M  = 1 , NMAT
      MPOS      = MATDPL(M)
      IF(MPOS.GT.0.AND.MATXRG(M).GT.0) THEN
                  MTNAME(M) (6:6) = IDTEMP (4:4)
                  ENDIF
  300 CONTINUE
C
      RETURN
      END
