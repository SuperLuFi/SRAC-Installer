C***********************************************************************
C TERMINATE PROGRAM BECAUSE OF PDS ACCESS ERROR                        *
C***********************************************************************
C
      SUBROUTINE PDSER(SUBNAM,DIRNAM,MEMBER,IRC,IOUT)
C
C-----------------------------------------------------------------------
C     IRC : ERROR CODE
C         = 0 NORMAL END
C         = 1 MEMBER NOT FOUND IN READ MODE
C         = 2 MEMBER NAME IS EMPTY OR INVALID
C         = 3 DIRECTORY NAME IS EMPTY OR TOO LONG
C         = 4 MEMBER ALREADY EXIST IN WRITE MODE
C         = 5 PDS OPEN ERROR
C         = 6 PDS CLOSE ERROR
C         = 7 PDS READ ERROR
C         = 8 PDS WRITE ERROR
C-----------------------------------------------------------------------
C
      CHARACTER DIRNAM*120,MEMBER*8,SUBNAM*6
C
      IF(IRC.EQ.1) THEN
        WRITE(IOUT,6010) SUBNAM,MEMBER,DIRNAM
        STOP
      ELSEIF(IRC.EQ.2) THEN
        WRITE(IOUT,6020) SUBNAM,MEMBER,DIRNAM
        STOP
      ELSEIF(IRC.EQ.3) THEN
        WRITE(IOUT,6030) SUBNAM,MEMBER,DIRNAM
        STOP
      ELSEIF(IRC.EQ.4) THEN
        WRITE(IOUT,6040) SUBNAM,MEMBER,DIRNAM
        STOP
      ELSEIF(IRC.EQ.5) THEN
        WRITE(IOUT,6050) SUBNAM,MEMBER,DIRNAM
        STOP
      ELSEIF(IRC.EQ.6) THEN
        WRITE(IOUT,6060) SUBNAM,MEMBER,DIRNAM
        STOP
      ELSEIF(IRC.EQ.7) THEN
        WRITE(IOUT,6070) SUBNAM,MEMBER,DIRNAM
        STOP
      ELSEIF(IRC.EQ.8) THEN
        WRITE(IOUT,6080) SUBNAM,MEMBER,DIRNAM
        STOP
      ENDIF
C
 6010 FORMAT(//1H ,'<<<  ERROR STOP (',A6,')  >>>',/,1X,
     &'PDS FILE ACCESS ERROR, MEMBER NOT FOUND IN READ MODE',/,1X,
     &'MEMBER    :',A8,/,1X,
     &'DIRECTORY :',A120)
 6020 FORMAT(//1H ,'<<<  ERROR STOP (',A6,')  >>>',/,1X,
     &'PDS FILE ACCESS ERROR, MEMBER NAME IS EMPTY OR INVALID',/,1X,
     &'MEMBER    :',A8,/,1X,
     &'DIRECTORY :',A120)
 6030 FORMAT(//1H ,'<<<  ERROR STOP (',A6,')  >>>',/,1X,
     &'PDS FILE ACCESS ERROR, DIRECTORY NAME IS EMPTY OR TOO LONG',/,1X,
     &'MEMBER    :',A8,/,1X,
     &'DIRECTORY :',A120)
 6040 FORMAT(//1H ,'<<<  ERROR STOP (',A6,')  >>>',/,1X,
     &'PDS FILE ACCESS ERROR, MEMBER ALREADY EXIST IN WRITE MODE',/,1X,
     &'MEMBER    :',A8,/,1X,
     &'DIRECTORY :',A120)
 6050 FORMAT(//1H ,'<<<  ERROR STOP (',A6,')  >>>',/,1X,
     &'PDS FILE ACCESS ERROR, FILE OPEN ERROR',/,1X,
     &'MEMBER    :',A8,/,1X,
     &'DIRECTORY :',A120)
 6060 FORMAT(//1H ,'<<<  ERROR STOP (',A6,')  >>>',/,1X,
     &'PDS FILE ACCESS ERROR, FILE CLOSE ERROR',/,1X,
     &'MEMBER    :',A8,/,1X,
     &'DIRECTORY :',A120)
 6070 FORMAT(//1H ,'<<<  ERROR STOP (',A6,')  >>>',/,1X,
     &'PDS FILE ACCESS ERROR, FILE READ ERROR',/,1X,
     &'MEMBER    :',A8,/,1X,
     &'DIRECTORY :',A120)
 6080 FORMAT(//1H ,'<<<  ERROR STOP (',A6,')  >>>',/,1X,
     &'PDS FILE ACCESS ERROR, FILE WRITE ERROR',/,1X,
     &'MEMBER    :',A8,/,1X,
     &'DIRECTORY :',A120)
C
      RETURN
      END
