      SUBROUTINE OPNPDS(IO)
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
      CHARACTER*14    FLDISP(5)
C
      DATA FLDISP / 14H UNKNOWN FILE  ,  14H NEW FILE          ,
     @              14H OLD FILE      ,  14H SCRATCH FILE      ,
     @              14H UNKNOWN FILE     /
C
C * OPEN
C
C
      PATH  = 1
      ECODE = 0
C
      IF(IO.LT.1 .OR. IO.GT.3) THEN
               WRITE(6,*) ' *** ILLEGAL IO-MODE SPECIFIED ***',IO
               ECODE  = 8
               GO TO 300
               ENDIF
C
      CALL FILSRC(NFILE,FILENM)
CFACOMS
      CALL PDSOPN(DDNAME(NFILE),IO,ECODE)
CFACOME
CUNIXS
C     NO STATEMENT FOR UNIX MACHINE
CUNIXE
      IF(ECODE.EQ.0) THEN
                     IOS   (NFILE) = IO
                     INCORE(NFILE) = 0
                     IST   (NFILE) = 0
                     IF(IO.EQ.1)  FLMODE(NFILE) = 8HOLD
                     IF(DATAKP(NFILE)(1:4).EQ.'CORE') INCORE(NFILE) = 1
                     IF(FLMODE(NFILE)(1:1).EQ.'N'   ) IST   (NFILE) = 1
                     IF(FLMODE(NFILE)(1:1).EQ.'O'   ) IST   (NFILE) = 2
                     IF(FLMODE(NFILE)(1:1).EQ.'S'   ) IST   (NFILE) = 3
                     IF(IST(NFILE).EQ.0) THEN
                                         IST(NFILE) = 3
                             IF(IO.EQ.2) IST(NFILE) = 1
                                         ENDIF
                     IF(DATAKP(NFILE).EQ.' '.AND.IST(NFILE).EQ.3)
     +                                   INCORE(NFILE) = 1
                     IDISP  = IST(NFILE) + 1
                     WRITE(6,210) DDNAME(NFILE)(1:8),IO,FLDISP(IDISP)
                     DO 200  I  = 1,5
                     NC(I,NFILE)= 0
  200                CONTINUE
                     RETURN
                     ENDIF
C
  210 FORMAT(10X,'FILE DD=',A8,' OPENED BY MODE ',I1,' AS ',A)
C
  300 IF(ECODE.EQ.0) RETURN
      CALL PDSERR(ECODE,MEMBER,PATH,DDNAME(NFILE))
      RETURN
      END
