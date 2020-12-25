      SUBROUTINE OVRWRT(MEMBER,RAREA,N)
C
      INCLUDE  'READPINC'
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
      DIMENSION    RAREA(1)
C
C * OVERWRITE
C
      ISW   = 0
      PATH  = 5
      ECODE = 0
      MODE  = 0
      IPOS  = 0
C
      CALL FILSRC(NFILE,FILENM)
C
      IF(IOS(NFILE).EQ.0) THEN
                    WRITE(6,*) ' *** FILE NOT OPENED DD=',DDNAME(NFILE)
                    STOP
                    ENDIF
C
      IF(N.EQ.0)   THEN
       WRITE(6,*) ' **** MEMBER LENGTH 0 ORDERED TO BE OVERWRITTEN ',
     *            'FOR ',MEMBER,' ON DD=',DDNAME(NFILE),' ***'
                   ECODE=4
                   GO TO 3400
                   END IF
C
C
      IF(  INCORE(NFILE) .EQ. 1 ) THEN
           DO 3000 I=ICNTMX,1,-1
           IF( IPOSDD(I).EQ.NFILE .AND. CZMEMB(I).EQ.MEMBER) THEN
                                  MODE = 1
                                  IPOS = I
                                  GO TO 3100
                                  ENDIF
 3000      CONTINUE
           ENDIF
C
 3100 CONTINUE
      IF(IST(NFILE).EQ.3.AND.MODE.EQ.1)  GO TO 3200
CFACOMS
CM    CALL PDSSRC(DDNAME(NFILE),MEMBER,N1,ECODE)
CFACOME
CUNIXS
      N1 = 0
      CALL PDSSRC( PATHNM(NFILE),LENPAT(NFILE),MEMBER,N1,ECODE )
CUNIXE
C
      NC(5,NFILE)=NC(5,NFILE)+1
      IF(ECODE.EQ.8) ISW = 1
      IF(ECODE.GT.8) GO TO 3400
C
      IF(ISW.EQ.0) THEN
CFACOMS
CM                 CALL PDSDEL(DDNAME(NFILE),MEMBER,'        ',ECODE)
CFACOME
CUNIXS
              CALL PDSDEL( PATHNM(NFILE),LENPAT(NFILE),MEMBER,ECODE )
CUNIXE
                   IF(ECODE.GT.8)  GO TO 3400
                   ENDIF
C
      IF(MODE.EQ.0) THEN
                    PATH = 7
                    CALL   WRITE( MEMBER , RAREA , N )
                    RETURN
                    ENDIF
C
 3200 CONTINUE
      LENG  = IZDTLN(IPOS)
C
      IF(N.LE.LENG) THEN
                    CALL WRITEO( MEMBER , RAREA , N , IPOS , NFILE )
                    GO TO 3300
                    ENDIF
C
      IPOSDD(IPOS)= 0
CKSK  CZMEMB(IPOS)= 8H
      CZMEMB(IPOS)= '        '
CMOD  IZDTLN(IPOS)= 0
      ECODE       = 0
      PATH        = 7
      NC(5,NFILE) = NC(5,NFILE)+1
      CALL   WRITE( MEMBER , RAREA , N )
      RETURN
C
 3300 IF(ECODE.EQ.0) RETURN
 3400 CALL PDSERR(ECODE,MEMBER,PATH,DDNAME(NFILE))
      RETURN
      END
