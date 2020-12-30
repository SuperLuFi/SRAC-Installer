C***********************************************************************
C  PROGRAM TO EDIT EFFECTIVE MICRO XS FROM MICREF FILE                 *
C  VERSION : SRAC95 FOR UNIX                                           *
C***********************************************************************
C
      SUBROUTINE MICEDT(DIRNAM, IOUT, IPRN, MICNM, NG, NGF, XSEC)
C
C=========================== FOR MAIN ==================================
CDEL  PARAMETER  (MAXNG=107, MAXXS=10)
      INCLUDE  'PARAMINC'
      DIMENSION  XSEC(MAXNG,MAXXS)
      CHARACTER*72  DIRNAM
      CHARACTER* 8  MICNM
C=======================================================================
CDEL  PARAMETER  (MAXWRK=15000)
      COMMON /WKPDS/ WORK(MAXWRK)  
      DIMENSION      IWORK(1)
      EQUIVALENCE ( WORK(1),IWORK(1) )
      CHARACTER* 8  MEMBER 
C
C---------------------------- INPUT ------------------------------------
C     DIRNAM     : DIRECTORY NAME (A72) OF PDS : /XXX/XXX/MICREF01
C     IOUT       : LOGICAL DEVICE FOR OUTPUT
C     IPRN       : =0(NO PRINT), =1(PRINT OUT IN DEVICE IOUT)
C     MICNM      : MEMBER NAME(A8) OF MICRO XS (XZZMCBFT)
C                   X  : INTERNALY SET
C                   ZZM: NUCLIDE-TAG
C                   C  : CHEMICAL-TAG (USUALY C='0')
C                   B  : BURNUP-TAG
C                   F  : MIXTURE-TAG
C                   T  : TEMPERATURE-TAG(NEAREST T-TAG TO MIXTURE TEMP)
C                        CAUTION:T-TAG IN SRAC INPUT IS INEFFECTIVE
C                   EXAMPLE MICNM='XU0804Z9'
C---------------------------- OUTPUT -----------------------------------
C     NGF        : NUMBER OF FAST ENERGY GROUPS
C     NG         : NUMBER OF TOTAL ENERGY GROUPS
C     XSEC(G,I)  : MICROSCOPIC CROSS SECTION
C                  I=1 : PRODUCTION
C                  I=2 : FISSION
C                  I=3 : CAPTURE
C                  I=4 : ABSORPTION
C                  I=5 : TOTAL
C                  I=6 : FISSION SPECTRUM
C-----------------------------------------------------------------------
***************************
*      ZERO SETTING       *--------------------------------------------
***************************
      DO 100  IG = 1,MAXNG
      DO 100   J = 1,MAXXS
        XSEC(IG,J) = 0.0
  100 CONTINUE
**************************************
*  READ NUMBER OF ENERGY GROUP       *---------------------------------
**************************************
      MEMBER = 'CONTF002'
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
      NGF = IWORK(1)
C
      MEMBER = 'CONTT002'
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        IF(IRC.EQ.1) THEN
          WRITE(IOUT,*) ' CAUTION(MICEDT): MEMBER CONTT000 NOT FOUND',
     &                  ' THEN, NGT=0 IS ASSUMED'
          if(iout.ne.6) then 
          WRITE(6,*)    ' CAUTION(MICEDT): MEMBER CONTT000 NOT FOUND',
     &                  ' THEN, NGT=0 IS ASSUMED'
          endif
          NGT=0
        ELSE
          WRITE(IOUT,*) ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
          if(iout.ne.6) then 
          WRITE(6,*)    ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
          endif
          STOP
        ENDIF
      ELSE
        NGT = IWORK(1)
      ENDIF
C
      NG = NGF + NGT
      IF (NG.GT.MAXNG) THEN
        WRITE(IOUT,*) ' ERROR(MICEDT): NUMBER OF ENERGY GROUPS(=', NG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNG, ')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MICEDT): NUMBER OF ENERGY GROUPS(=', NG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNG, ')'
        endif 
        STOP
      ENDIF     
**************************************
*  READ CONTROL MEMBER OF FAST GROUP *---------------------------------
*  CzzmFbxt or CzzmF000              *
**************************************
      MEMBER      = MICNM
      MEMBER(1:1) = 'C'
      MEMBER(5:5) = 'F'
      IEFF = 1
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.EQ.1) THEN
        MEMBER(6:8) = '000'
        IEFF = 0
        CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
        IF(IRC.NE.0) THEN
          WRITE(IOUT,*) ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
          if(iout.ne.6) then 
          WRITE(6,*)    ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
          endif
          STOP
        ENDIF
      ELSEIF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
C
      ICAP = IWORK(1)
      IFIS = IWORK(2)
********************************************
*  READ FAST GROUP XS FROM MICREF          *---------------------------
*  MzzmFbxt or MzzmF000                    *
********************************************
      MEMBER(1:1) = 'M'
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
        endif              
        STOP
      ENDIF
C ------- READ SIG-C -------
      ISW = 0
      IF(ICAP.EQ.1) THEN
        DO 200  J = 1,NGF
          XSEC(J,3) = WORK(J)
          XSEC(J,4) = WORK(J)
  200   CONTINUE
        ISW = NGF
      ENDIF
C ------- READ SIG-F AND SET SIG-P, SIG-A,
      IF(IFIS.EQ.1) THEN
        DO 210  J = 1,NGF
          XSEC(J,1) = WORK(ISW+J) * WORK(ISW+NGF+J)
          XSEC(J,2) = WORK(ISW+J)
          XSEC(J,4) = WORK(ISW+J) + XSEC(J,3)
          XSEC(J,6) = WORK(ISW+2*NGF+J)
  210   CONTINUE
      ENDIF
C ------- READ SIG-T -------
      ISW = ICAP*NGF + IFIS*3*NGF + NGF
      DO 220  J = 1 , NGF
        XSEC(J,5) = WORK(ISW+J)
  220 CONTINUE
*****************************************
*  READ CONTROL MEMBER OF THERMAL GROUP *-------------------------------
*  CzzmTbxt or CzzmT00t (Czzmc00t)      *
*****************************************
C  present SRAC do not support C-Tag in MICREF
      IF(NGT.LE.0) GO TO 1000
      MEMBER      = MICNM
      MEMBER(1:1) = 'C'
      MEMBER(5:5) = 'T'
      IEFF = 1
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.EQ.1) THEN
CKSK    MEMBER(5:5) = MICNM(5:5)
        MEMBER(6:7) = '00'
        IEFF = 0
        CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
        IF(IRC.NE.0) THEN
          WRITE(IOUT,*) ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
          if(iout.ne.6) then 
          WRITE(6,*)    ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
          endif
          STOP
        ENDIF
      ELSEIF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
C     IF(WORK(2).EQ.0) THEN 
C       ISCT=0
C     ELSE
C       ISCT=1
C     ENDIF
********************************
*  READ THERMAL XS FROM MICREF *----------------------------------------
*  KzzmTbxt or Kzzmc00t        *
********************************
      MEMBER(1:1) = 'K'
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MICEDT): PDSIN ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
C
      IF(LENG.EQ.NGT*5) THEN  
        ISCT = 0
      ELSEIF(LENG.EQ.NGT*(NGT+5)) THEN
        ISCT = 1
      ELSE
        WRITE(IOUT,*) ' ERROR(MICEDT): LENGTH(',LENG,') OF MEMBER:',
     &                MEMBER,' IS NOT CONSISTENT'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(MICEDT): LENGTH(',LENG,') OF MEMBER:',
     &                MEMBER,' IS NOT CONSISTENT'
        endif
      ENDIF
C
      NNC = ISCT*NGT*NGT + NGT
      NNF = NNC + 2*NGT
      NNP = NNC + 3*NGT
      NNT = NNC + 1*NGT
      DO 300 J = 1 , NGT
        XSEC(NGF+J,1) = WORK(NNF + J) * WORK(NNP + J)
        XSEC(NGF+J,2) = WORK(NNF + J)
        XSEC(NGF+J,3) = WORK(NNC + J)
        XSEC(NGF+J,4) = WORK(NNC + J) + WORK(NNF + J)
        XSEC(NGF+J,5) = WORK(NNT + J)
  300 CONTINUE
***************************
*     PRINT OUT           *--------------------------------------------
***************************
 1000 IF(IPRN.EQ.1) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,7000)
        WRITE(IOUT,*)
        WRITE(IOUT,*) '    ***** MICRO XS MEMBER NAME = ',MICNM,'*****'
        WRITE(IOUT,*)
        WRITE(IOUT,*) '    NGF = ',NGF
        WRITE(IOUT,*) '    NGT = ',NGT
        WRITE(IOUT,*) '    NG  = ',NG
        WRITE(IOUT,*)
        WRITE(IOUT,'(A)') '**** MICROSCOPIC CROSS SECTION ****'
        WRITE(IOUT,'(A/(1P10E12.5))')
     &                       ' PRODUCTION',(XSEC(IG,1),IG=1,NG)
        WRITE(IOUT,'(A/(1P10E12.5))')
     &                       ' FISSION   ',(XSEC(IG,2),IG=1,NG)
        WRITE(IOUT,'(A/(1P10E12.5))')
     &                       ' CAPTURE   ',(XSEC(IG,3),IG=1,NG)
        WRITE(IOUT,'(A/(1P10E12.5))')
     &                       ' ABSORPTION',(XSEC(IG,4),IG=1,NG)
        WRITE(IOUT,'(A/(1P10E12.5))')
     &                       ' TOTAL     ',(XSEC(IG,5),IG=1,NG)
        WRITE(IOUT,'(A/(1P10E12.5))')
     &                       ' FISS.SPECT',(XSEC(IG,6),IG=1,NG)
        WRITE(IOUT,*)
        WRITE(IOUT,7010)
        WRITE(IOUT,*)
      ENDIF
 7000 FORMAT(1H ,'MICEDT',114(1H=))
 7010 FORMAT(1H ,114(1H=),'MICEDT')
C
 9000 RETURN
      END
