C=======================================================================
      SUBROUTINE PONRED( SUBNAM, IPDS, MEMBER, WORK, LENG, IPRN, IOUT )
C=======================================================================
C  SEARCH & READ A MEMBER FROM VPDS OR PDS
C  IF THE MEMBER IS NOT FOUND IN VPDS OR MEMORY OVER, IT WILL BE READ 
C  FROM ACTUAL PDS BY FILE I/O
C                                      BY K.OKUMURA(JAERI),OCT/1999
C
C  I   SUBNAM : NAME OF SUBROUTINE WHICH CALLS THIS SYSTEM
C  I   IPDS   : DIRECTORY NUMBER
C  I   MEMBER : NAME OF MEMBER(A8)
C  O   WORK   : DATA ARRAY READ FROM THE MEMBER
C I/O  LENG   : INPUT
C               >=0 READ ALL DATA STORED IN THE MEMBER
C               < 0 READ ONLY -LENG NUMBER OF DATA FROM TOP
C               OUTPUT
C               =0 MEMBER NOT EXISTING IN VPDS AND PDS
C               >0 DATA LENGTH IN THE MEMBER
C  I   IPRN   : PRINT CONTROL FOR MEMBER ACCESS INFORMATION
C  I   IOUT   : OUTPUT DEVICE NUMBER FOR ERROR OR CAUTION MESSAGES
C
C--------1---------2---------3---------4---------5---------6---------7--
C
      INCLUDE 'INCPDS'
      COMMON  /USPDSC/ PDSDIR(MXPDS), MOACS(MXPDS), MOSTY(MXPDS)
      COMMON  /VPDSI1/ LMEMBR, NMEMBR, LASTAD, LOCUPY, ISTATS, NFAIL
      COMMON  /VPDSI2/ MLENG(MXMBR), MATTR(MXMBR), MADRS(MXMBR)
      COMMON  /VPDSC1/ MNAME(MXMBR), MDACS(MXPDS)
      COMMON  /VPDSR1/ VRF(MXVRF)
C
      CHARACTER*4    MOACS, MOSTY, MDACS
      CHARACTER*8    SUBNAM
      CHARACTER*8    MEMBER, MNAME
      CHARACTER*120  PDSDIR, DIRNAM
      DIMENSION WORK(*)
C
C***********************************************************************
C
C     LENGKP : ORIGINAL INPUT
C     LENGR  : REQUIRED DATA LENGTH
C     LENG   : OUTPUT : FULL LENGTH OF MEMBER OR 0 (NOT EXISTING)
C
      LENGKP = LENG
      DIRNAM = PDSDIR(IPDS)
      IF(MDACS(IPDS).NE.'CORE') GOTO 3000
C
C***********************************************************************
C     READ A MEMBER FROM VPDS IF MEMBER EXISTING
C***********************************************************************
C
C-----SERACH VPDS AND GET DATA LENGTH AND TOP ADRESS
      CALL PONSRC( MEMBER, IPDS, IEXT, LENG, IPOS, ITOP )
      IF(IEXT.NE.1) GOTO 1000
      IF(LENGKP .LT. 0) THEN
        LENGR = IABS(LENGKP)
      ELSE
        LENGR = LENG
      ENDIF
      IF(LENGR.GT.LENG) THEN
        WRITE(IOUT,6000) SUBNAM, LENGR, LENG, MEMBER, DIRNAM
        STOP 999
      ENDIF
C
C-----READ MEMBER FROM VPDS ONLY FOR REQUIRED LENGTH
      IT = ITOP-1
      DO 100 I=1,LENGR
        WORK(I)=VRF(IT+I)
  100 CONTINUE
C
      IF(IPRN.GE.2) THEN
        IF(LENGR .LT. LENG) THEN
          WRITE(IOUT,6100) '     MEMBER ',MEMBER,' WITH LENGTH ',
     &    LENG,' WAS PARTIALLY(',LENGR,') READ FROM CORE ',DIRNAM
        ELSE
          WRITE(IOUT,6200) '     MEMBER ',MEMBER,' WITH LENGTH ',
     &    LENG,' WAS READ FROM CORE ',DIRNAM
        ENDIF
      ENDIF
      GOTO 9999
C
C***********************************************************************
C     MEMBER NOT EXISTING IN VPDS
C     GET FULL LENGTH OF MEMBER IN PDS, AND CHECK IS IT POSSIBLE TO COPY
C     FULL LENGTH DATA TO VPDS OR NOT
C***********************************************************************
C
 1000 IF(LMEMBR.GE.MXMBR) GOTO 2000
      CALL PDSLN(DIRNAM,MEMBER,LENG,IRC)
      IF(IRC.EQ.1) THEN
        LENG = 0
        GOTO 9999
      ELSE
        CALL PDSER(SUBNAM,DIRNAM,MEMBER,IRC,IOUT)
      ENDIF
C
      IF(LENGKP .LT. 0) THEN
        LENGR = IABS(LENGKP)
      ELSE
        LENGR = LENG
      ENDIF
C
      LASTSP = MXVRF - LASTAD
      IF(LENG.GT.LASTSP)  GOTO 2000
C
C***********************************************************************
C     MEMBER NOT EXISTING IN VPDS
C     READ THE MEMBER FROM PDS AND SET AS A NEW MEMBER(FULL LENGTH)
C     IN THE LAST SPACE OF VPDS
C***********************************************************************
C
      CALL PDSIN(DIRNAM,MEMBER,VRF(LASTAD+1),LENG,IOUT,IPRN,IRC)
      IF(IRC.EQ.1) THEN
        LENG = 0
        GOTO 9999
      ELSE
        CALL PDSER(SUBNAM,DIRNAM,MEMBER,IRC,IOUT)
      ENDIF
C
      DO 200 I=1,LENGR
        WORK(I)=VRF(LASTAD+I)
  200 CONTINUE
C
      LMEMBR = LMEMBR + 1
      NMEMBR = NMEMBR + 1
      MNAME(LMEMBR) = MEMBER
      MATTR(LMEMBR) = IPDS
      MLENG(LMEMBR) = LENG
      MADRS(LMEMBR) = LASTAD + 1
      LOCUPY = LOCUPY + LENG
      LASTAD = LASTAD + LENG
      GOTO 9999
C
C***********************************************************************
C     MEMBER NOT EXISTING IN VPDS
C     READ THE MEMBER FROM PDS AND SET AS A NEW MEMBER(FULL LENGTH)
C     IN THE FIRST SPACE OF GHOST MEMBER IF POSSIBLE
C***********************************************************************
C
 2000 CALL PONGST( LENG, LENGO, IPOS, ITOP )
      IF( LENGO.GT.0 ) THEN
        CALL PDSIN(DIRNAM,MEMBER,VRF(ITOP),LENG,IOUT,IPRN,IRC)
        IF(IRC.EQ.1) THEN
          LENG = 0
          GOTO 9999
        ELSE
          CALL PDSER(SUBNAM,DIRNAM,MEMBER,IRC,IOUT)
        ENDIF
C
        IT = ITOP-1
        DO 300 I=1,LENGR
          WORK(I)=VRF(IT+I)
  300   CONTINUE
C
        NMEMBR = NMEMBR + 1
        LOCUPY = LOCUPY + LENG
        MNAME(IPOS) = MEMBER
        MATTR(IPOS) = IPDS
        MLENG(IPOS) = LENG
        MADRS(IPOS) = ITOP
        GOTO 9999
      ENDIF
      NFAIL = NFAIL + 1
C
C***********************************************************************
C     MEMBER NOT EXISTING IN VPDS, AND VPDS IS NOT AVAILABLE
C     READ THE MEMBER FROM PDS
C***********************************************************************
C
 3000 LENGW = LENGKP
      CALL PDSIN(DIRNAM,MEMBER,WORK,LENGW,IOUT,IPRN,IRC)
      IF(IRC.EQ.1) THEN
        LENG = 0
      ELSE
        LENG = LENGW
        CALL PDSER(SUBNAM,DIRNAM,MEMBER,IRC,IOUT)
      ENDIF
C
C=======================================================================
C
 6000 FORMAT(//1H ,'<<<  ERROR STOP (',A,')  >>>',/,1X,
     &'PDS FILE ACCESS ERROR IN READ MODE WITH SPECIFIED LENGTH',/,1X,
     &'REQUIRED LENGTH(=',I8,') GREATER THAN ACTUAL LENGTH(=',I8,')',/,
     & 1X,
     &'MEMBER    :',A8,/,1X,
     &'DIRECTORY :',A120)
C
 6100 FORMAT(A,A,A,I7,A,I7,A,/,5X,A)
 6200 FORMAT(A,A,A,I7,A,/,5X,A)
C
 9999 RETURN
      END
