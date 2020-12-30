      SUBROUTINE PDSCON
C
*INCLUDE READPINC
C
      CHARACTER*8     CZMEMB,SCMEMB,FLMODE,DATAKP
      CHARACTER*68    PATHNM
C
      COMMON /PDSWK3/ PATHNM(15),FLMODE(15),DATAKP(15),CZMEMB(MAXMEM),
     1                SCMEMB(MAXMEM)
      COMMON /PDSWK2/ IZWRIT,IZMXDT,IZWCNT,IZDWTL,ICNTMX,
     1                LENPAT(15),INCORE(15),ICNTSC,
     2                IZDTLN(MAXMEM),IZDTTL(MAXMEM),IDUMZ,
     3                IPOSDD(MAXMEM),IPOSSC(MAXMEM),
     4                RZDATA(MXWORK)
C
C  *** CONDENSE  COMMON ARRAY FOR PDS I/O
C
          IMAX      = ICNTMX
          DO  500 I = IMAX , 1 , -1
          IF(IPOSDD(I).LE.0) THEN
                            NOW = IZDTTL(I)
                            DO 300  J =  I+1 ,ICNTMX
                            IPOSDD(J-1) = IPOSDD(J)
                            CZMEMB(J-1) = CZMEMB(J)
                            LENG        = IZDTLN(J)
                            IZDTLN(J-1) = LENG
                            IOLD        = IZDTTL(J) + 1
                       CALL COPYDT( RZDATA(IOLD) , RZDATA(NOW+1) ,LENG )
                            NOW         = NOW  + LENG
                            IZDTTL(J)   = NOW
  300                       CONTINUE
                            IPOSDD(ICNTMX) = 0
                            CZMEMB(ICNTMX) = 8H
                            IZDTLN(ICNTMX) = 0
                            IZDTTL(ICNTMX) = NOW
                            ICNTMX    = ICNTMX -1
                            IZDWTL    = NOW
                            IZDCNT    = ICNTMX
                            ENDIF
  500    CONTINUE
C
       RETURN
       END
