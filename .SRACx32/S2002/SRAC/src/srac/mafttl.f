      SUBROUTINE  MAFTTL(ID    ,IOPT  ,NC    ,KPAGE ,IOUT  )
C
      DIMENSION  ID(2)
C
      WRITE(IOUT,10)
      WRITE(IOUT,20)     ID
C
      IF(IOPT.EQ.1) WRITE(IOUT,30)
      IF(IOPT.EQ.2) WRITE(IOUT,40)
C
      WRITE(IOUT,50) NC,KPAGE
C
   10 FORMAT(1H1,/)
   20 FORMAT(1H ,30X,'NUCLIDE NAME = ',2A4)
   30 FORMAT(1H ,40X,'TABLE OF SCATTERING MATRICES')
   40 FORMAT(1H ,40X,'TABLE OF ELASTIC MATRICES')
   50 FORMAT(1H ,100X,'PAGE ',I2,' OF ',I2)
C
      RETURN
      END
