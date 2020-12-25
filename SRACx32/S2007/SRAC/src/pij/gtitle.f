C             GTITLE              LEVEL=1        DATE=84.03.06
C
C***********************************************************************
C                                                                      *
C       GTITLE    : WRITING TITLE OF REGION                            *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       CODER       : HATA KENICHROU                                   *
C       DATE        : 1982.01.20                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C       ORGSET                                                         *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C         KFIG    :  KIND OF REGION     = 1,2,3,4,5                    *
C                       IF  =0   WRITE DATA'S TITLE                    *
C         IFTYPE  :  FIGURE SCALE TYPE  =1,2,3                         *
C         TITLE   :  DATA'S TITLE                                      *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C         NCHAR   :  NUMBER OF CHARACTERS IN DATA'S TITLE              *
C         RTITLE  :  REGION TITLE                                      *
C         XPOS    :  X POSITION OF REGION TITLE                        *
C                                                                      *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE GTITLE (KFIG,IFTYPE,TITLE)
CMSASA
C     INTEGER   TITLE(18),TITLEW(18),RTITLE(3,5)
      CHARACTER*4   TITLE(18),TITLEW(18),RTITLE(3,5)
C
      DIMENSION  XPOS(3)
C
      DATA  XPOS  /  -33.01,  -27.08,  -30.47  /
      DATA  RTITLE  / ' SUB',' REG','ION ',    ' T -',' REG','ION ',
     @                ' R -',' REG','ION ',    'MATE','RIAL',' NO.',
     @                ' X -',' REG','ION '/
C
C*** CHANGE SMALL CHARACTERS TO LARGE CHARACTERS BECAUSE SYMBOL ROUTINE
C    DOES NOT SUPPORT SMALL CHARACTERS
C
      DO 5 I=1,18
        TITLEW(I)=TITLE(I)
    5 CONTINUE
      CALL SCTOLC(TITLEW,18)
C
      IF ( KFIG .NE. 0 )  GO TO 30
         DO  10  I = 1,17
CIBM        IF ( TITLEW(19-I) .NE. '    ' )   GO TO 20
CMSASA      IF ( TITLEW(19-I) .NE. 4H     )   GO TO 20
            IF ( TITLEW(19-I) .NE. '    ' )   GO TO 20
C
   10       CONTINUE
CK 20    CALL GSCHAR ( 13+I*6.77,  235.0,  4, TITLE, 111,(19-I)*4)
   20    CALL UGSCHA ( 13+I*6.77,  235.0,  4, TITLEW, 111,(19-I)*4)
         RETURN
C
   30 CONTINUE
CKSK     CALL GSCHAR ( XPOS(IFTYPE), -115. ,  6-IFTYPE,
         CALL UGSCHA ( XPOS(IFTYPE), -115. ,  6-IFTYPE,
     @                                        RTITLE(1,KFIG), 111,12)
      RETURN
C
      END
