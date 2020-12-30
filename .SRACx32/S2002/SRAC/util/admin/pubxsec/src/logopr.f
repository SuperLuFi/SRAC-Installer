C***********************************************************************
C     PRINT DATE & TIME & CODE NAME & VERSION NO.
C     FOR MORE-MOSRA (UTILITY CODE FOR MOSRA CODE)
C***********************************************************************
      SUBROUTINE LOGOPR
C
      COMMON  /UNITIO/ NIN1, NOUT1
      CHARACTER*8     IDATE
C=======================================================================
C
      NOUT2 = NOUT1
      WRITE(NOUT2,*)
C+++  PRINT DATE & TIME
cksk  CALL UDATE(IDATE)
cksk  CALL UTIME(NTIME)
cksk  IH      = NTIME/3600000
cksk  IMINU   = (NTIME-IH*3600000)/60000
cksk  IS      = (NTIME-IH*3600000-IMINU*60000)/1000
cksk  WRITE(NOUT1,6000) IDATE,IH,IMINU,IS
 6000 FORMAT(1X,'======= MORE-MOSRA CODE RUN   DATE : ',A8,
     &       ' START TIME=',I2,':',I2,':',I2,/)
C
C+++  PRINT LOGO (MORE-SRAC)
      WRITE(NOUT2,6010)
 6010 FORMAT(1H ,4X,'LL    LL    LLLLL   LLLLLL    LLLLLL   ',/,
     &           5X,'L L  L L   L     L  L     L   L        ',/,
     &           5X,'L  LL  L   L     L  LLLLLL    LLLLL    ',/,
     &           5X,'L  LL  L   L     L  L   L     L        ',/,
     &           5X,'L      L    LLLLL   L    LL   LLLLLL   ',/)
C
      WRITE(NOUT2,6011)
      WRITE(NOUT2,6012)
      WRITE(NOUT2,6013)
      WRITE(NOUT2,6014)
      WRITE(NOUT2,6015)
      WRITE(NOUT2,6016)
      WRITE(NOUT2,6017)
      WRITE(NOUT2,6018)
      WRITE(NOUT2,6019)
      WRITE(NOUT2,6020)
      WRITE(NOUT2,6021)
      WRITE(NOUT2,6022)
      WRITE(NOUT2,6023)
      WRITE(NOUT2,6024)
      WRITE(NOUT2,6025)
 6011 FORMAT(1H ,5X,'         ***                  ***              ',
     &       '*********') 
 6012 FORMAT(1H ,5X,'         ***                  ',
     &       '***                  ******')
 6013 FORMAT(1H ,5X,'   ******************************************',
     &       '       ******')
 6014 FORMAT(1H ,5X,'    PubXsec  : UTILITY CODE OF SRAC          ')
 6015 FORMAT(1H ,5X,'   *****************************************',
     &       '********')
 6016 FORMAT(1H ,5X,'        ***                   ***            ***') 
 6017 FORMAT(1H ,5X,'        ***                ******            ***') 
 6018 FORMAT(1H ,5X,'   ************           *** ***            ',
     &       '*************')
 6019 FORMAT(1H ,5X,'  **************          **  ***             ',
     &       '**************')
 6020 FORMAT(1H ,5X,'       ***                **  ***',
     &       '                       *****')
 6021 FORMAT(1H ,5X,'      ****   **             *****',
     &       '                         ***')
 6022 FORMAT(1H ,5X,'      *********               ***',
     &       '            **************')
 6023 FORMAT(1H ,5X,'        * ***                **',
     &       '               ***********')
 6024 FORMAT(/,9X,   'LAST MODIFIED:(10 OCT 2001) ')
 6025 FORMAT(/,1H ,79(1H=),/)
C
      RETURN
      END
