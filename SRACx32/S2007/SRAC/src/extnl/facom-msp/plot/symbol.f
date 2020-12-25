C                                                                               
      SUBROUTINE    SYMBOL ( X, Y, HH, IBCDA, ANGLE, NCHAR )                    
C                                                                               
C/******************************************************************            
C *                                                                *            
C *    PIFLIB.H : HEADER FILE FOR PLOTTER ROUTINE ON WS            *            
C *                                                                *            
C ******************************************************************/           
C                                                                               
C    FLOAT XORG, YORG           /* X, Y ORIGIN ON X-WINDOW           */         
C    FLOAT PSXORG, PSYORG       /* X, Y ORIGIN ON POST SCRIPT        */         
C    FLOAT XPT0, YPT0           /* TERMINATE POINTS OF BEFORE STROKE */         
C    FLOAT PSSTRX, PSSTRY       /* TERMINATE POINTS OF PS STROKE     */         
C    FLOAT XPIXMM, YPIXMM       /* VALUE A PIXEL PER MM              */         
C    FLOAT FVAL                 /* FACTOR VALUE                      */         
C    FLOAT XSTR, YSTR           /* CURRENT POINT OF WHERE ROUTINE    */         
C                                                                               
C    CHAR IFLAG                 /* CONDITION FLAG FOR GDFILE         */         
C    CHAR IFPLT3                /* IF CALL PLOT(,,(+OR-)3)           */         
C    CHAR PSPLOT                /* IF DRAWING BEFORE IN POST SCRIPT  */         
C    CHAR IFDRAW                /* IF DRAWING BEFORE IN X-WINDOW     */         
C    LONG LTYPE                 /* CURRENT LINE TYPE                 */         
C    FLOAT PSCALE               /* SCALING FACTOR OF XPIF            */         
C    CHAR IFXPIF                /* IF CALLED FROM  XPIF              */         
C    LONG IPUNIT                /* PS FILE LOGICAL UNIT NUMBER       */         
C    UNSIGNED LONG COLOR[8]     /* COLOR VALUE                       */         
C                                                                               
      COMMON / BIFLIB /                                                         
     1    XORG    , YORG    , PSXORG  , PSYORG  , XPT0    , YPT0    ,           
     2    PSSTRX  , PSSTRY  , XPIXMM  , YPIXMM  , FVAL    ,                     
     3    XSTR    , YSTR    , IFLAG   , IFPLT3  , PSPLOT  ,                     
     4    IFDRAW  , LTYPE   , PSCALE  , IFXPIF  , IPUNIT  , COLOR(8)            
C                                                                               
C/******************************************************************            
C *                                                                *            
C *    GGSX.H : HEADER FILE FOR GGS ROUTINES ON EWS                *            
C *                                                                *            
C ******************************************************************/           
C                                                                               
      COMMON /GCOMXY/ JFILM, KPEN, JOPT, KERR                                   
      COMMON /GCOMXY/ FCT, HFCT, OLX, OLY, XRG, YRG, FF                         
C                                                                               
      COMMON /LTPCOM/ LTP001, LTP002                                            
C                                                                               
C                                                                               
C     INTEGER*2     IBCDA(1)                                                    
      CHARACTER*1   IBCDA(1)                                                    
      DIMENSION     XA(9),    YA(9)                                             
      CHARACTER*1   IBCD(255)                                                   
CITJ+                                                                           
      CHARACTER*1   IBCDW                                                       
CITJ-                                                                           
CMSASA                                                                          
C     INTEGER*2     IBCDC(126)                                                  
C     EQUIVALENCE  (IBCD(1), IBCDC(1))                                          
CEND.MSASA                                                                      
C                                                                               
      INTEGER       IDXTAB(128)                                                 
CMSASA ... G77 0.5.X DOES NOT ALLOW HALF INTEGER.                               
C     INTEGER*2     ITZ(768)                                                    
      INTEGER       ITZ(768)                                                    
C                                                                               
      DATA     OLDTH, OLDFCT   / 999.0, 9999.0 /                                
      DATA     RAD  , X0 , Y0  / 0.017453, 0.0, 0.0 /                           
      DATA     DDV             / 0.3 /                                          
      DATA     EPS             / 0.01 /                                         
      DATA     ICHART          /    0 /                                         
C                                                                               
CMSASA                                                                          
C     INTEGER       IDXTAB(128)                                                 
C     INTEGER*2     ITZ(768)                                                    
C                                                                               
      DATA (IDXTAB(I),I=1,95)/                                                  
     1       524289, 51118082,   393238, 34013201,   458771,                    
     2     34013189,   458759, 34078728, 17498122, 50790412,                    
     3     17694734, 34406417, 33947668, 33816593,   786454,                    
     4       327801, 50462799, 51249198, 33751126, 17301626,                    
     5     17105007, 34668668,   524407, 50856064, 16908410,                    
     6     16973954,   131265,   524419,   393349, 50528360,                    
     7       393352, 17629318, 34144393, 50921611,   589966,                    
     8     17236112,   786578,   786581, 34209947, 50790551,                    
     9     17367193,   393374, 34472060, 34341023, 34144418,                    
     A     50856100,   131188, 17957051, 50659442, 51249318,                    
     B     17170473, 17170535, 33816662, 33816643, 33816658,                    
     C       131171,   458797, 33882225, 50921540, 51183684,                    
     D     17105008, 17105066,   327790,   327789,        0,                    
     E     17367090,   786485,   524365, 34013236,   458808,                    
     F       393272, 51118137, 17170504, 50724924, 34275499,                    
     G       327765,   196650, 17039462, 50659407,   131168,                    
     H     34209908, 17170525,   393291,   196680, 17104970,                    
     I     50593865, 34209868,   458848, 51118156,   589920,                    
     D     50790464, 34275427, 51052623, 33816642, 34340947 /                   
      DATA (IDXTAB(I),I=96,128)/                                                
     1     50528379, 33685584, 16908386, 34406487, 50593882,                    
     2       458845, 50528350, 17104991, 17104994, 33882211,                    
     3     17236059, 17694894,   393301, 34472041, 50462897,                    
     4       196712, 17694782, 50921548,   327705, 17367066,                    
     5     17629218, 33816604, 34209821,   786462,   327713,                    
     6     17891362, 34275366, 34275411, 17498290,  1048757,                    
     7       262188, 50659370,   590009 /                                       
      DATA (ITZ(I),I=1,190)/                                                    
     1      34,  36,   4,   0,  64,  68,  36,  34,  36,  20,                    
     2       3,   1,  16,  48,  65,  67,  52,  36,  34,  36,                    
     3       2,  32,  66,  36,  34,   2,  36,  32,  36,  66,                    
     4      34,   4,  68,  34,   0,  34,  64,  34,  18,  50,                    
     5      34,  68,   4,  68,   0,  64,   0,  34,   4,  34,                    
     6      68,  34,  32,  34,  68,  51,  19,   4,  19,  17,                    
     7       0,  17,  49,  64,  49,  51,  34,  36,  32,  34,                    
     8       2,  66,  34,   4,  64,  34,   0,  68,  34,   4,                    
     9      68,   0,  64,  34,  34,  36,   1,  65,  36,  34,                    
     A      35,   3,  32,  67,  35,  34,  16,  48,  32,  39,                    
     B      22,   5,   6,  23,  55,  70,  69,   1,   0,  64,                    
     C      48,  55,   2,  66,  71,   7,   4,  52,  67,  65,                    
     D      48,  16,   1,   2,   6,  23,  55,  70,   6,   7,                    
     E      71,  70,  32,   6,  23,  55,  70,  69,  52,  20,                    
     F      52,  67,  65,  48,  16,   1,   3,  20,   5,   6,                    
     G       1,  16,  48,  65,  70,  55,  23,   6,   4,  19,                    
     H      67,   1,  65, 112,  66,   4,  70,   4,  68, 112,                    
     I      65,   1,  55,  53,  39,  55,   6,  23,  38,  32,                    
     J      38,  55,  70,   3,  19,  18,  20,  19,  35,  34 /                   
      DATA (ITZ(I),I=191,380)/                                                  
     1      36,  35,  51,  50,  52,  51,  67,   0,   3,  67,                    
     2       3,   6,  23,  55,  70,  64,  55,  70,  65,  48,                    
     3       0,   7,  55,  70,  69,  52,   4,  52,  67,  65,                    
     4      71,   7,   4,  52,   4,   0,  64,  83,  51,  67,                    
     5      65,  48,  16,   1,   6,  23,  55,  70,  69,  16,                    
     6      48,  32,  39,  23,  55,   5,   6,  23,  55,  70,                    
     7      69,  52,  36,  34, 112,  16,  48,  33,  16, 112,                    
     8      34,  39,  32,  49,  54,  39,  48,  16,  23,  55,                    
     9      16,  32,  33,  17,  49,  33,  38,  37,  21,  53,                    
     A      37,  35,  19,  51,  64,   0,   7,   4,  68,  71,                    
     B      64,   0,   7,  64,  71,  36,   7,   0,   3,  71,                    
     C      37,  64,  53,  70,  65,  48,  16,   1,   6,  23,                    
     D      55,  70, 112,  34,  64,  33,  37,  35,   3,  67,                    
     E      35,   1,  69,  35,   5,  65,  23,  55,  48,  16,                    
     F      37,  36,  52,  53,  37, 112,  49,  33,  34,  50,                    
     G      49,  32,   1,  37,  65,   1,   2,   1,  16,  48,                    
     H      65,  67,  52,  20,   5,   6,  23,  55,  70,  32,                    
     I      39,   7,  71,   0,  64, 112,  20,  52,   7,   2,                    
     J       1,  16,  48,  65,  71,   7,  32,  71,  64,  36 /                   
      DATA (ITZ(I),I=381,570)/                                                  
     1       0,   7,  55,  70,  69,  52,   4,  36,  64,   0,                    
     2      71, 112,   7,  64,   7,  36,  32,  36,  71, 112,                    
     3      51,  19, 112,  17,  49,  32,  17,  22,  39,   1,                    
     4      65, 112,   2,  68,   6,   5,  33,  69,  64,  48,                    
     5      49,  65,  64, 112,   0,  71, 112,   6,   7,  23,                    
     6      22,   6,  39,  32,  17,  49,  32,  39,  22,  54,                    
     7      39,   3,  67,  52,  50,  67,   3,  20,  18,   3,                    
     8       1,  18,  32,  39,  71,  80,   0,  36,   7,  87,                    
     9      39,  37,  64,  21,  22,  39,  54,   3,   2,  17,                    
     A      33,  66,   4,  68, 112,  65,   1, 112,  16,  53,                    
     B      19,  35,  36,  20,  19,   0,  64, 112,  70,   6,                    
     C     112,   3,  67,  66,  82,  66,  51,  34,  18,   3,                    
     D       4,  21,  37,  52,  51,  52,  69,  85, 100,  99,                    
     E      82,   1,  65, 112,  34,  38,  36,  68,   4,   5,                    
     F       1,  16,  32,  49,  54,  71,  87, 102,   2,  50,                    
     G      67,  68,  53,   5,  22,  38,  68,  84, 101, 112,                    
     H      99,  82,  66,  36,  20,   3,   0,  16,  33,  34,                    
     I      51,  36,  37,  22,   6,  70,  54,  37,  36,  19,                    
     J      34,  33,  48,  64,   0,  21,  18,  33,  49,  66 /                   
      DATA (ITZ(I),I=571,760)/                                                  
     1      69,  66,  81,  17,  21,   5,  69,  53,  49,  66,                    
     2      33,  17,   2,   4,  21,  53,  68,  66,  49,  33,                    
     3      32,  38,   1,   5,  22,  38,  53,  51,   3,  51,                    
     4      49,  32,  16,   1,  69, 112,  65,  49,  21,   5,                    
     5       2,  17,  34,  37,  34,  49,  66,  69,   4,  20,                    
     6      18,  33,  50,  52,  68, 112,  38,  32,   0,  35,                    
     7      65,  35,  21,   6,  36,  19,  18,  33,  49,  66,                    
     8      67,  52,  36,  21,  38,  54,  65,  33,  18,  20,                    
     9      37,  69, 112,  67,   3,   4,  21,  36,  18,  36,                    
     A      53,  68,  48,  33,  49,  50,  34,  33, 112,   3,                    
     B      83, 112,  36,  52,  53,  37,  36,   1,  69, 112,                    
     C       5,  65,  66,  49,  17,   2,   4,  21,  53,  68,                    
     D     112,  38,  32,   2,  17,  49,  66,  67,  52,  20,                    
     E       5,  22,  54,  69, 112,  39,  32,   3,  99,  17,                    
     F      21,  20,   4,  68,  52,  53,  49,  50,  66,   2,                    
     G      66,  49,  17,   2,   5,  22,  54,  69,  67,  50,                    
     H      34,  19,  20,  37,  53,  68,  55,  53,  39,  55,                    
     I     112,  23,  21,   7,  23,   0,  16,  54,  70, 112,                    
     J      68,  53,  21,   4,  19,  51,  66,  49,  17,   2 /                   
      DATA (ITZ(I),I=761,768)/                                                  
     1     112,  18,  52,   0,   0,   0,   0,   0 /                             
C                                                                               
      DO  100  I  =  1  ,  255                                                  
CMSASA   IBCD(I) = '\0'                                                         
         IBCD(I) = CHAR(0)                                                      
  100 CONTINUE                                                                  
C                                                                               
      IBIT8   =       256                                                       
      IBIT16  =     65536                                                       
      IBIT24  =  16777216                                                       
C                                                                               
      H       =  HH                                                             
CASASA H       =  SQRT( H*H )                                                   
      H       =  ABS( H )                                                       
      ICHART  =  0                                                              
      IPEN    =  3                                                              
      NT      =  NCHAR                                                          
      ICB     =  0                                                              
      DIV     =  7.0                                                            
C                                                                               
      IF ( NT .EQ. 0 )    THEN                                                  
         ICB     =  3                                                           
      ELSE IF ( NT .LT. 0 )    THEN                                             
         IF ( NT .NE. -1 )    IPEN    =  2                                      
         ICB     =  3                                                           
CMSASA ... THIS GIMICK DOES NOT WORK ON LITTLE-EIDIAN !!!                       
C        IBCD(1) =  IBCDA(3)                                                    
C        IBCD(2) =  IBCDA(4)                                                    
C        ICHART  =  IBCDC(1)                                                    
         CALL CNVC2I( ICHART, IBCDA )                                           
CEND.MSASA                                                                      
         IF ( ICHART .LE. 15 )   DIV     =  4.0                                 
      END IF                                                                    
C                                                                               
CITJ+97/07/15                                                                   
C     FCT     =  H / DIV                                                        
      FCTSY     =  H / DIV                                                      
CITJ+97/07/15                                                                   
      TH      =  ANGLE                                                          
C                                                                               
      IF ( TH .NE. OLDTH )    THEN                                              
         OLDTH   =  TH                                                          
         TH1     =  TH * RAD                                                    
         SINTH   =  SIN ( TH1 )                                                 
         COSTH   =  COS ( TH1 )                                                 
CITJ+97/07/15                                                                   
C        OLDFCT  =  FCT                                                         
         OLDFCT  =  FCTSY                                                       
CITJ-97/07/15                                                                   
         XA(1)   =  0.0                                                         
         YA(1)   =  0.0                                                         
CITJ+97/07/15                                                                   
C        XA(2)   =  FCT * COSTH                                                 
C        YA(2)   =  FCT * SINTH                                                 
         XA(2)   =  FCTSY * COSTH                                               
         YA(2)   =  FCTSY * SINTH                                               
CITJ-97/07/15                                                                   
         DO  120  I  =  3  ,  8                                                 
            XA(I)   =  XA(I-1) + XA(2)                                          
            YA(I)   =  YA(I-1) + YA(2)                                          
  120    CONTINUE                                                               
      END IF                                                                    
C                                                                               
CITJ+97/07/15                                                                   
C     IF ( FCT .NE. OLDFCT )    THEN                                            
C        OLDFCT  =  FCT                                                         
      IF ( FCTSY .NE. OLDFCT )    THEN                                          
         OLDFCT  =  FCTSY                                                       
CITJ-97/07/15                                                                   
         XA(1)   =  0.0                                                         
         YA(1)   =  0.0                                                         
CITJ+97/07/15                                                                   
C        XA(2)   =  FCT * COSTH                                                 
C        YA(2)   =  FCT * SINTH                                                 
         XA(2)   =  FCTSY * COSTH                                               
         YA(2)   =  FCTSY * SINTH                                               
CITJ-97/07/15                                                                   
         DO  130  I  =  3  ,  8                                                 
            XA(I)   =  XA(I-1) + XA(2)                                          
            YA(I)   =  YA(I-1) + YA(2)                                          
  130    CONTINUE                                                               
      END IF                                                                    
C                                                                               
      XX      =  X                                                              
      YY      =  Y                                                              
      TEMPX   =  SQRT( ( XX - 999.0 )*( XX - 999.0 ) )                          
      TEMPY   =  SQRT( ( YY - 999.0 )*( YY - 999.0 ) )                          
      IF ( TEMPX .GT. EPS )    X0      =  XX                                    
      IF ( TEMPY .GT. EPS )    Y0      =  YY                                    
      XX      =  X0                                                             
      YY      =  Y0                                                             
C                                                                               
C ********  VIRTICAL DIRECTION CHECK.  ********                                 
      IF ( NT .GE. 0 )    THEN                                                  
         IF ( HH .LT. 0.0 )    THEN                                             
            X0      =  X0 + H * SINTH                                           
            Y0      =  Y0 - H * COSTH                                           
            XA(9)   =   ( 1.0 + DDV ) * H * SINTH                               
            YA(9)   =  -( 1.0 + DDV ) * H * COSTH                               
            ISWV    =  1                                                        
         END IF                                                                 
      END IF                                                                    
C                                                                               
      IF ( NT .LT. 0 .AND. ICHART .LE. 15 )   THEN                              
         X0      =  X0 - ( XA(3) - YA(3) )                                      
         Y0      =  Y0 - ( XA(3) + YA(3) )                                      
      END IF                                                                    
C                                                                               
      NNT     =  ABS ( NT )                                                     
C                                                                               
      IF ( ICB .NE. 3 )    THEN                                                 
         DO  140  I  =  1  ,  NCHAR                                             
            IBCD(I)   =  IBCDA(I)                                               
  140    CONTINUE                                                               
CMSASA   IBCD(NCHAR+1) = '\0'                                                   
         IBCD(NCHAR+1) = CHAR(0)                                                
      END IF                                                                    
C                                                                               
      DO  300  K  =  1  ,  NNT                                                  
         IF ( ICB .EQ. 3 )    THEN                                              
CMSASA ... THIS GIMICK DOES NOT WORK ON LITTLE-EIDIAN !!!                       
C           IBCD(1)  =  IBCDA(3)                                                
C           IBCD(2)  =  IBCDA(4)                                                
C           ICHAR0   =  IBCDC(1)                                                
            CALL CNVC2I( ICHAR0, IBCDA )                                        
CEND.MSASA                                                                      
         ELSE                                                                   
CITJ+                                                                           
C           CALL  ASTOEB ( IBCD(K), IEBC )                                      
            IBCDW = IBCD(K)                                                     
            IEBC = ICHAR(IBCDW)                                                 
CITJ-                                                                           
            ICHAR0  =  MOD ( IEBC , 128 )                                       
         END IF                                                                 
         ICHAR1  =  ICHAR0 + 1                                                  
         IDATA   =  IDXTAB(ICHAR1)                                              
         IF ( IDATA .NE. 0 )    THEN                                            
            KPOS    =  IDATA / IBIT24                                           
            IMD16   =  IDATA / IBIT16                                           
            KNUM    =  MOD ( IMD16 , IBIT8 )                                    
            KTAB    =  MOD ( IDATA , IBIT16 )                                   
            KPT     =  ( KTAB - 1 )*4 + KPOS                                    
            DO  200  M  =  KPT+1  ,  KPT+KNUM                                   
               NX      =  ITZ(M) / 16 + 1                                       
               NY      =  MOD ( ITZ(M) , 16 ) + 1                               
               IF ( NX .EQ. 8 )    THEN                                         
                  IPEN    =  3                                                  
               ELSE                                                             
                  XT      =  X0 + XA(NX) - YA(NY)                               
                  YT      =  Y0 + YA(NX) + XA(NY)                               
                  CALL  PLOT ( XT , YT , IPEN )                                 
                  IPEN    =  2                                                  
               END IF                                                           
  200       CONTINUE                                                            
         END IF                                                                 
         X0      =  X0 + XA(8)                                                  
         Y0      =  Y0 + YA(8)                                                  
         IPEN    =  3                                                           
  300 CONTINUE                                                                  
C                                                                               
C ********  END OF SYMB4                                                        
C                                                                               
      IF ( HH .LT. 0.0 )    THEN                                                
         X0      =  X0 - H * SINTH                                              
         Y0      =  Y0 + H * COSTH                                              
      END IF                                                                    
C                                                                               
      IF ( NCHAR .LT. 0 )    THEN                                               
         X0      =  XX                                                          
         Y0      =  YY                                                          
      END IF                                                                    
C                                                                               
      IPEN    =  3                                                              
      CALL  PLOT ( X0 , Y0 , IPEN )                                             
C                                                                               
      RETURN                                                                    
      END                                                                       
