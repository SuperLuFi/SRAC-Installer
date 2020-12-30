
/******************************************************************
 *  Fortran called C subroutine :
 *
 *   UNIX venders have their own convention or rule for external
 *  names of FORTRAN coded subprograms or commons.
 *
 *   We prepare equivalent sources for C coded routines
 * 
 *     1. Attach underscore to FORTRAN name. ( SUN & Other BSD UNIX)
 *     2. Same as FORTRAN name (lowercase).   (HP-UX,IBM-AIX etc.)
 *     3. Uppercase string of FORTRAN name.   ( Ncube etc. )
 *     4. "_" + uppercase string of FORTRAN name.   ( HI/OSF )
 *
 *******************************************************************/

/*******************************************************************
 *
 *    Get local time and day and pass them to Fortran program
 *    ( ANSI C & POSIX )
 *
 ******************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <time.h>

void cc_time_date();

void cctimdat( i_hour, i_minute, i_sec, i_day, i_month, i_year )
 int * i_hour ;	 	/* hour */
 int * i_minute ;	/* minuite */
 int * i_sec ;	 	/* second */
 int * i_day ; 		/* hour */
 int * i_month ;	/* month */
 int * i_year ; 	/* year */
{
     cc_time_date(i_hour, i_minute, i_sec, i_day, i_month, i_year );
}
void cctimdat_( i_hour, i_minute, i_sec, i_day, i_month, i_year )
 int * i_hour ;	 	/* hour */
 int * i_minute ;	/* minuite */
 int * i_sec ;	 	/* second */
 int * i_day ; 		/* hour */
 int * i_month ;	/* month */
 int * i_year ; 	/* year */
{
     cc_time_date(i_hour, i_minute, i_sec, i_day, i_month, i_year );
}
void CCTIMDAT( i_hour, i_minute, i_sec, i_day, i_month, i_year )
 int * i_hour ;	 	/* hour */
 int * i_minute ;	/* minuite */
 int * i_sec ;	 	/* second */
 int * i_day ; 		/* hour */
 int * i_month ;	/* month */
 int * i_year ; 	/* year */
{
     cc_time_date(i_hour, i_minute, i_sec, i_day, i_month, i_year );
}
void _CCTIMDAT( i_hour, i_minute, i_sec, i_day, i_month, i_year )
 int * i_hour ;	 	/* hour */
 int * i_minute ;	/* minuite */
 int * i_sec ;	 	/* second */
 int * i_day ; 		/* hour */
 int * i_month ;	/* month */
 int * i_year ; 	/* year */
{
     cc_time_date(i_hour, i_minute, i_sec, i_day, i_month, i_year );
}



void cc_time_date( i_hour, i_minute, i_sec, i_day, i_month, i_year )
 int * i_hour ;	 	/* hour */
 int * i_minute ;	/* minuite */
 int * i_sec ;	 	/* second */
 int * i_day ; 		/* hour */
 int * i_month ;	/* month */
 int * i_year ; 	/* year */
{
    time_t th ;
    struct tm *lt ;

    if( -1 != time( &th ) )
    {
       lt = localtime( &th ) ;

       *i_hour		= lt->tm_hour ;
       *i_minute	= lt->tm_min ;
       *i_sec		= lt->tm_sec ;
       *i_day		= lt->tm_mday ;
       *i_month		= lt->tm_mon + 1 ;
       *i_year		= lt->tm_year ;
    }
    else
    {
       *i_hour 		= 0 ;
       *i_minute	= 0 ;
       *i_sec		= 0 ;
       *i_day		= 0 ;
       *i_month		= 0 ;
       *i_year		= 0 ;
    }
}
