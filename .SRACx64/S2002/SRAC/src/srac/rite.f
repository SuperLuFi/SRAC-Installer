C             RITE                LEVEL=1        DATE=81.11.14
      SUBROUTINE RITE (N,IREC,VECP,NWDS,MODE)
C
C     PERFORMS BINARY WRITE OPERATIONS
C
      DIMENSION IREC(1), VECP(1)
C
C     MODE IS OPERATION INDICATOR ( 1/2/3/4/5/6/7/8/9=
C     CORE TO LCM/BINARY WRITE/BINARY WRITE WITH COUNT/
C     END FILE AND REWIND/BINARY INTEGER WRITE/BINARY
C     INTEGER WRITE WITH COUNT/NOT USED/NOT USED/BINARY WRITE OF
C     EIGHT BYTE WORDS )
C
      GO TO (100,110,120,130,140,150,160,170,180), MODE
C
C     TRANSFERS INFORMATION FROM CORE TO LCM
C
C     N NOT USED
C     IREC LCM POINTER
C     VECP CORE VECTOR
C     NWDS NUMBER OF WORDS TO TRANSFER
C
  100 CONTINUE
      CALL ECWR (VECP,IREC,NWDS,IEREC)
      RETURN
C
C     WRITES BINARY RECORD
C
C     N IS UNIT NUMBER
C     IREC NOT USED
C     VECP IS CORE VECTOR
C     NWDS IS NUMBER OF WORDS
C
  110 CONTINUE
      WRITE (N) (VECP(I),I=1,NWDS)
      RETURN
C
C     WRITES BINARY RECORD WITH FIRST WORD AS BLOCK COUNT
C
C     N IS UNIT NUMBER
C     IREC NOT USED
C     VECP IS CORE VECTOR
C     NWDS IS THE NUMBER OF WORDS IN THE CORE VECTOR
C
  120 CONTINUE
      WRITE (N) NWDS,(VECP(I),I=1,NWDS)
      RETURN
C
C     WRITES AN END OF FILE AND REWINDS THE UNIT
C
C     N IS UNIT NUMBER
C     IREC NOT USED
C     VECP NOT USED
C     NWDS NOT USED
C
  130 CONTINUE
CNN   END FILE N
      REWIND N
      RETURN
C
C     WRITES BINARY INTEGER RECORD
C
C     N IS UNIT NUMBER
C     IREC IS INTEGER CORE VECTOR
C     VECP NOT USED
C     NWDS IS NUMBER OF WORDS
C
  140 CONTINUE
      WRITE (N) (IREC(I),I=1,NWDS)
      RETURN
C
C     WRITES BINARY INTEGER RECORD WITH FIRST WORD AS BLOCK COUNT
C
C     N IS UNIT NUMBER
C     IREC IS INTEGER CORE VECTOR
C     VECP NOT USED
C     NWDS IS NUMBER WORDS IN THE INTEGER CORE VECTOR
C
  150 CONTINUE
      WRITE (N) NWDS,(IREC(I),I=1,NWDS)
      RETURN
C
C     NOT USED
C
  160 CONTINUE
      RETURN
C
C     NOT USED
C
  170 CONTINUE
      RETURN
C
C     WRITES BINARY RECORD OF EIGHT BYTE WORDS
C
C     N IS UNIT NUMBER
C     IREC NOT USED
C     VECP IS CORE
C     NWDS IS NUMBER OF EIGHT BYTE WORDS
C
  180 CONTINUE
      IDX = 2 * NWDS
      WRITE ( N ) ( VECP ( I ), I = 1, IDX )
      RETURN
      END
