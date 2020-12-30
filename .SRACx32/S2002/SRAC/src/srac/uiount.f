C***********************************************************************
C
C  UIOUNT   : SET UNFORMATED(0) OR FORMATED(1)
C             FOR EACH I/O DEVICE
C             IOFORM < 0 : NOT OPENED AT INITIALIZATION STEP, 
C             EVEN IF "SETENV FU??" IS SET.
C
C  THIS ROUTINE IS USED FOR SRAC95-EWS
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
      IOFORM(31) = 1
      IOFORM(50) = 1
      IOFORM(89) = 1
      IOFORM(91) = 1
      IOFORM(93) = 1
      IOFORM(95) = 1
      IOFORM(98) = 1
      IOFORM(99) = 1
C
      IOFORM(1)  = 0
      IOFORM(2)  = 0
      IOFORM(3)  = 0
      IOFORM(4)  = 0
      IOFORM(8)  = 0
      IOFORM(9)  = 0
      IOFORM(10) = 0
      IOFORM(11) = 0
      IOFORM(12) = 0
      IOFORM(13) = 0
      IOFORM(14) = 0
      IOFORM(15) = 0
      IOFORM(16) = 0
      IOFORM(17) = 0
      IOFORM(18) = 0
      IOFORM(19) = 0
      IOFORM(20) = 0
      IOFORM(21) = 0
      IOFORM(22) = 0
      IOFORM(26) = 0 
      IOFORM(28) = 0
      IOFORM(32) = 0
      IOFORM(33) = 0
      IOFORM(52) = 0
      IOFORM(81) = 0
      IOFORM(82) = 0
      IOFORM(83) = 0
      IOFORM(84) = 0
      IOFORM(85) = 0
      IOFORM(92) = 0
      IOFORM(96) = 0
      IOFORM(97) = 0
C
      IOFORM(49) = -1
      RETURN
      END
