C***********************************************************************
                   SUBROUTINE GSHIFT
C                  CALLED BY GMNHEX2
C      ENTRY 1  GSCYL   FOR CYCLIC SHIFT
C      ENTRY 2  GSVECT  FOR SHIFT
C      ENTRY 3  GSADD   FOR ADDITION IN ELEMENTS
C***********************************************************************
     *              (N,LOC,FCT,KK)
             DIMENSION LOC(*),FCT(*),LL(*),FF(*)
C***********************************************************************
   10        LN=LOC(N)
             FN=FCT(N)
   20        DO 30 I=1,N-LIMIT
             IA=N-I+1
             IF(LOC(IA-ISHIFT).NE.0)
     *            THEN
             LOC(IA)=LOC(IA-ISHIFT)+KK
             FCT(IA)=FCT(IA-ISHIFT)
                  ELSE
             LOC(IA)=0
             FCT(IA)=0.
                  ENDIF
   30        CONTINUE
C***********************************************************************
             IF(MODE.EQ.3) THEN
             IF(LN.NE.0) THEN
             LOC(1)=LN+KK
             FCT(1)=FN
                         ELSE
             LOC(1)=0
             FCT(1)=0.
                         ENDIF
                           ENDIF
             IF(MODE.EQ.2) THEN
             LOC(0)=0
             FCT(0)=0.
                           ENDIF
             RETURN
C***********************************************************************
             ENTRY GSADD (N,LOC,FCT,KK)
             MODE=1
             ISHIFT=0
             LIMIT=0
             GO TO 20
C***********************************************************************
             ENTRY GSVECT(N,LOC,FCT,KK)
             MODE=2
             ISHIFT=1
             LIMIT=0
             GO TO 20
C***********************************************************************
             ENTRY GSCYL (N,LOC,FCT,KK)
             MODE=3
             ISHIFT=1
             LIMIT=1
             GO TO 10
C***********************************************************************
             ENTRY GTRNS (N,LOC,FCT,LL,FF)
             DO 40 I=1,N
             IF(LL(I).NE.0) THEN
             LOC(I)=LL(I)
             FCT(I)=FF(I)
                            ELSE
             LOC(I)=0
             FCT(I)=0.
                            ENDIF
   40        CONTINUE
             RETURN
C***********************************************************************
                         END
