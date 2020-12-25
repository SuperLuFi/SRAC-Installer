      SUBROUTINE  BURNEG( ICOLP , IMAX , LAPSE , NECF , NREG )
C
      DIMENSION    NECF(LAPSE),NREG(IMAX)
C
C *** NREG ARRAY TO ASSOCIATE FINE GROUP TO COARSE GROUP
C
      IF(ICOLP.EQ.0) THEN
C ****************** NO CONDENSED GROUP STRUCTURE
                     DO 10 NG = 1,IMAX
                     NREG(NG) = NG
   10                CONTINUE
C ****************** CONDENSED  GROUP STRUCTURE
                     ELSE
                     K         = 0
                     DO 20 NG  = 1,LAPSE
                     DO 20 KG  = 1,NECF(NG)
                     K         = K+1
                     NREG(K)   = NG
   20                CONTINUE
                     ENDIF
C
CM    WRITE(6,*) ' **** CHECK WRITE AT SUB(BURNEG) *** '
CM    WRITE(6,*) '  ICOLP IMAX LAPSE : ',ICOLP,IMAX,LAPSE
CM    WRITE(6,*) '  REG : ',(NREG(I),I=1,IMAX)
C
      RETURN
      END
