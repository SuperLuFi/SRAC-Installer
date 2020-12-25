      SUBROUTINE WRITE(MEMBER,RAREA,N)
C
*INCLUDE READPINC
C
      CHARACTER*8     CZMEMB,SCMEMB,FLMODE,DATAKP,MEMBER
      CHARACTER*12    DDNAME,FILENM
      CHARACTER*68    PATHNM
      INTEGER*4       ECODE,TEMP,PATH
C
      COMMON /PDSPDS/ DDNAME(125),IST(15),IRW(15),IOS(35),NC(5,20),
     &                IFLSW,FILENM,ECODE,TEMP
C
      COMMON /PDSWK3/ PATHNM(15),FLMODE(15),DATAKP(15),CZMEMB(MAXMEM),
     1                SCMEMB(MAXMEM)
      COMMON /PDSWK2/ IZWRIT,IZMXDT,IZWCNT,IZDWTL,ICNTMX,
     1                LENPAT(15),INCORE(15),ICNTSC,
     2                IZDTLN(MAXMEM),IZDTTL(MAXMEM),IDUMZ,
     3                IPOSDD(MAXMEM),IPOSSC(MAXMEM),
     4                RZDATA(MXWORK)
C
      DIMENSION       RAREA(1)
C
C * WRITE
C
      PATH  = 4
      ECODE = 0
      CALL FILSRC(NFILE,FILENM)
C
      IF(N.EQ.0)   THEN
       WRITE(6,*) ' **** MEMBER LENGTH 0 ORDERED TO BE WRITTEN FOR ',
     *             MEMBER,' ON DD=',DDNAME(NFILE),' ***'
                   ECODE=4
                   GO TO 100
                   END IF
C
      IF(IOS(NFILE).EQ.0) THEN
                    WRITE(6,*) ' *** FILE NOT OPENED DD=',DDNAME(NFILE)
                    STOP
                    ENDIF
C
C
C
      IF(  INCORE(NFILE) .EQ. 1 ) THEN
        IZWCNT = IZWCNT + 1
        IZDWTL = IZDWTL + N
C
        IF(IZWCNT .GT. IZWRIT .OR. IZDWTL .GT. IZMXDT) THEN
C
          ELSE
          ICNTMX           = ICNTMX + 1
          IPOSDD(ICNTMX)   = NFILE
          CZMEMB(ICNTMX)   = MEMBER
          IZDTLN(ICNTMX)   = N
          IZDTTL(ICNTMX+1) = IZDTTL(ICNTMX) + N
          NOW              = IZDTTL(ICNTMX)
C
          CALL COPYDT( RAREA , RZDATA(NOW+1) , N )
          IF(IST(NFILE).EQ.3) THEN
                         WRITE(6,11) MEMBER,N,DDNAME(NFILE)(1:8)
                         NC(3,NFILE)=NC(3,NFILE)+1
                         RETURN
                         ENDIF
          ENDIF
      ENDIF
C
CFACOMS
      CALL PDSWRT(DDNAME(NFILE),MEMBER,RAREA,4*N,ECODE)
CFACOME
CUNIXS
CM    CALL PDSWRT(PATHNM(NFILE),LENPAT(NFILE),MEMBER,RAREA,4*N,ECODE )
CUNIXE
C
      IF(ECODE.EQ.0) THEN
                     WRITE(6,11) MEMBER,N,DDNAME(NFILE)(1:8)
                     NC(3,NFILE)=NC(3,NFILE)+1
                     ENDIF
C
      IF(IST(NFILE).EQ.3) THEN
                          ICNTNW = ICNTSC + 1
              IF(ICNTNW.LE.MAXMEM) THEN
                                   ICNTSC = ICNTNW
                                   SCMEMB(ICNTSC) = MEMBER
                                   IPOSSC(ICNTSC) = NFILE
                                   ENDIF
                           ENDIF
C
  100 IF(ECODE.EQ.0) RETURN
      CALL PDSERR(ECODE,MEMBER,PATH,DDNAME(NFILE))
      RETURN
C
C
C
   11 FORMAT(10X,'MEMBER ',A8,' OF LENGTH ',I5,' IS STORED IN '
     * ,A8,' FILE')
C
      END
