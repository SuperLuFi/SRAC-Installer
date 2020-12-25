C-----------------------------------------------------------------------
C     MAIN PROGRAM TO
C     CONVERT TEXT-PDS TO PDS FOR SRAC95-UNIX (TXTTOPDS)
C     ASSUMPTION FOR TEXT FORMAT :
C      (1) FIRST LINE IS COMMENT
C      (2) COMMAND LINE FORMAT IS AS FOLLOWING:
C          *PUT  MENBER  TYPE  LENG
C          '*PUT' MUST BE LOCATED FROM THE FIRST COLUMN
C          '*PUT', MEMBER NAME(<A9), TYPE(ANY), LENG(<A11) ARE DIVIDED
C            BY SOME BLANKS
C      (3) DATA FORMAT (12A=1PE12.5) FOR FLOATING NUMBER
C      (4) DATA FORMAT (12A=11A,1X) FOR INTEGER NUMBER
C      (5) DATA FORMAT (12A=1X,A4,7X) FOR CHARACTER DATA
C      (6) 1-6 DATA(FLOATING OR INTEGER OR CHARACTER) IN A LINE
C      (7) FLOATING NUMBER ALWAYS INCLUDE '.' IN A DATA(A12)
C
C      SAMPLE :
C      COMMENT(1 LINE)
C      *PUT CU050000 N      9
C                0           0  1.00000E-01.........
C                1 -2.00000E-04        125 .........
C      *PUT MU050000 N     14
C       1.00000E+02 2.00000E-02-3.00000E-04.........
C       1.00000E+02 2.00000E-02-3.00000E-04.........
C      -1.00000E+02 2.00000E-02
C      ...........
C      ...........
C      *FIN
C
C   SPECIAL TREATMENT WILL BE DONE TO TREAT CHARACTER DATA FOR THE
C   MEMBERS : ----DN-T IN MACRO/MACROWRK
C             ----BNUP IN MACRO/MACROWRK
C             ----REST IN MACRO/MACROWRK
C-----------------------------------------------------------------------
      PARAMETER (MAXWK=300000)
      CHARACTER LINE*72,MEMBER*8,DIRNAM*72
      DIMENSION WORK(MAXWK)
      DIMENSION DATA(6)
      COMMON /SETDT/ NTNUC1,NTNUC2,NZON2,NZON3
C
      CALL UIOSET
      IOTXT = 10
      IOUT  = 6
C********************
C  READ INPUT DATA  *
C********************
C     DIRNAM : FULL NAME OF DIRECTORY FOR PDS
C     EX:/DG05/UFS02/J9347/SRAC95/LIB/PDS/PFAST/PFASTJ2
      READ(5,'(A72)') DIRNAM
      IF(DIRNAM(1:1).EQ.' ') THEN
        WRITE(6,*) ' ERROR(MAIN) : DIRECTORY NAME IS INVALID'
        WRITE(6,*) ' THE FIRST COLUMN SHOULD BE NON-BLANK'
        WRITE(6,*) ' DIRNAM = ',DIRNAM
        STOP
      ENDIF
C********************
C  READ TEXT PDS    *
C********************
      NMEM = 0
      REWIND IOTXT
C----- SKIP 1 RECORD
      READ (IOTXT,'(A72)') LINE
C----- SET MEMBER NAME & DATA LENGTH
 1000 READ (IOTXT,'(A72)') LINE
      IF(LINE(1:4).EQ.'*FIN') THEN
        WRITE(IOUT,*)
        WRITE(IOUT,*) ' NUMBER OF MEMBERS WRITTEN IN PDS=',NMEM
        WRITE(IOUT,*) ' ********** JOB END **********'
        GOTO 9999
      ENDIF
      CALL SETMEM(LINE,MEMBER,LENG)
      IF(LENG.GT.MAXWK) THEN
        WRITE(6,*) ' ERROR (MAIN) : WORK AREA(MAXWK=',MAXWK,
     &             ') IS LESS THAN REQUIRED SIZE(=',LENG,' IN MEMBER:',
     &             MEMBER
        STOP
      ENDIF
C----- SET NUMBER OF LINES TO BE READ FOR PDS DATA
      CALL TXTLIN(LENG,LDATA)
C----- SET PDS DATA IN WORK DIMENSION
      NTNUC1 = 0
      NTNUC2 = 0
      NZON2  = 0
      NZON3  = 0
      IPOS   = 0
      DO 100 L=1,LDATA
        READ (IOTXT,'(A72)') LINE
        KPOS = IPOS + 1
        IF(L.NE.LDATA) THEN
          LD = 6
          IF(MEMBER(5:6).EQ.'DN' .AND. MEMBER(8:8).EQ.'T') THEN
            CALL SETDA1(LINE,LD,DATA,KPOS)
          ELSEIF(MEMBER(5:8).EQ.'BNUP') THEN
            CALL SETDA2(LINE,LD,DATA,KPOS)
          ELSEIF(MEMBER(5:8).EQ.'REST') THEN
            CALL SETDA3(LINE,LD,DATA,KPOS)
          ELSE
            CALL SETDAT(LINE,LD,DATA)
          ENDIF
          DO 110 I=1,LD
            IPOS = IPOS + 1
            WORK(IPOS) = DATA(I)
  110     CONTINUE
        ELSE
          LD = LENG - 6*(LDATA-1)
          IF(MEMBER(5:6).EQ.'DN' .AND. MEMBER(8:8).EQ.'T') THEN
            CALL SETDA1(LINE,LD,DATA,KPOS)
          ELSEIF(MEMBER(5:8).EQ.'BNUP') THEN
            CALL SETDA2(LINE,LD,DATA,KPOS)
          ELSEIF(MEMBER(5:8).EQ.'REST') THEN
            CALL SETDA3(LINE,LD,DATA,KPOS)
          ELSE
            CALL SETDAT(LINE,LD,DATA)
          ENDIF
          DO 120 I=1,LD
            IPOS = IPOS + 1
            WORK(IPOS) = DATA(I)
  120     CONTINUE
        ENDIF
  100 CONTINUE
C**************************
C  WRITE WORK DATA IN PDS *
C**************************
      CALL PDSOUT(DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(6,*) ' ERROR(MAIN) : PDS ERROR, CODE =',IRC,' MEMBER =',
     &             MEMBER
        STOP
      ELSE
        NMEM = NMEM + 1
      ENDIF
      GOTO 1000
 9999 STOP
      END
C***********************************************************************
C
C  UIOUNT   : SET UNFORMATED(0) OR FORMATED(1)
C             FOR EACH I/O DEVICE
C
C***********************************************************************
C
      SUBROUTINE UIOUNT(IOFORM)
      DIMENSION IOFORM(100)
      DO 100 I=1,100
        IOFORM(I) = -1
  100 CONTINUE
      IOFORM(5)  = 1
      IOFORM(6)  = 1
      IOFORM(10) = 1
      RETURN
      END
