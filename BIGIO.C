/*
 *    **************************************************
 *    *                                                *
 *    *                    BIGIO.C                     *
 *    *                                                *
 *    *          Extended Precision Calculator         *
 *    *                                                *
 *    *             Low Level I/O Routines             *
 *    *                                                *
 *    *              Version 4.3 02-04-89              *
 *    *                                                *
 *    *              Judson D. McClendon               *
 *    *              329 37th Court N.E.               *
 *    *              Birmingham, AL 35215              *
 *    *                 205-853-8440                   *
 *    *            Compuserve [74415,1003]             *
 *    *                                                *
 *    **************************************************
 */



/*
 *    **************************************************
 *    *                                                *
 *    *                   Includes                     *
 *    *                                                *
 *    **************************************************
 */

#include <conio.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bigcalc.h"
#include "biggvar.h"

#ifdef REALMODE
#include <dos.h>
#else
#define INCL_BASE
#include <os2.h>
#endif


/*
 *    **************************************************
 *    *                                                *
 *    *                   Constants                    *
 *    *                                                *
 *    **************************************************
 */

#define CLOCKFREQ       1193180L    /* Timer frequency        */
#define SPEAKERMODE     0xB6        /* Set timer for speaker  */
#define TIMERMODEPORT   0x43        /* Timer mode port        */
#define FREQPORT        0x42        /* Frequency-control port */
#define SPEAKERPORT     0x61        /* Speaker port           */
#define SPEAKERON       0x03        /* Speaker-on bits        */
#define FREQ            400         /* A frequency            */
#define CFREQ  CLOCKFREQ / FREQ     /* Division frequency     */

#define SCRCOLOR        0x07        /* Grey on black          */
#define MONOSCREEN      0xB0000000L /* Mono buffer address    */
#define COLORSCREEN     0xB8000000L /* Color buffer address   */





/*
 *    **************************************************
 *    *                                                *
 *    *            Source Local Variables              *
 *    *                                                *
 *    **************************************************
 */

typedef struct {                 /* Screen char struct */
   char
      data,
      attr;
   } CHTYPE;

typedef struct {                 /* Screen memory structure */
   CHTYPE ch[25][80];
   } far *SCREENTYPE;

static SCREENTYPE                /* Screen pointer  */
   scr;

static int
   arow = 0,                     /* Absolute row    */
   acol = 0;                     /* Absolute column */

#ifdef REALMODE
static union REGS
   cpu;                          /* CPU registers for int86() */
static struct SREGS
   segs;
#endif

static unsigned short cbLVB;     /* length of LVB */




/*
 *    **************************************************
 *    *                                                *
 *    *              Keyboard Routines                 *
 *    *                                                *
 *    **************************************************
 */


/*
 *    **************************************************
 *    *                                                *
 *    *         Get decoded character from kbd         *
 *    *                                                *
 *    **************************************************
 */

extern int GetChar(void)

{
   int chr;

   CurPos(arow + 1, acol + 1);

#ifdef REALMODE                  /* Show logical video buffer */
   cpu.x.cx = cbLVB;
   segs.es = FP_SEG(scr);
   cpu.x.di = FP_OFF(scr);
   cpu.h.ah = 255;
   int86x(16, &cpu, &cpu, &segs);
#else
   VioShowBuf(0, cbLVB, 0);
#endif

   chr = getch();
   if (chr == 0)
      chr = 1000 + getch();      /* Non ASCII character */
   else
      if (isascii(chr) )
         chr = toupper(chr);

   return(chr);
}




/*
 *    **************************************************
 *    *                                                *
 *    *         Return character if key pressed        *
 *    *                                                *
 *    **************************************************
 */

extern int KeyPressed(void)

{
   if (kbhit())
      return(GetChar());
   else
      return(0);
}






/*
 *    **************************************************
 *    *                                                *
 *    *                Screen Routines                 *
 *    *                                                *
 *    **************************************************
 */


/*
 *    **************************************************
 *    *                                                *
 *    *            Initialize Screen Drivers           *
 *    *                                                *
 *    **************************************************
 */

extern void ScrInit(void)

{
            /* Get far pointer to screen buffer */

#ifdef REALMODE

   cpu.h.ah = 15;                /* Get video mode */
   int86(16, &cpu, &cpu);

   if ( (cpu.h.al != 2)          /* 80x25 b/w   */
         &&
        (cpu.h.al != 3)          /* 80x25 color */
         &&
        (cpu.h.al != 7) ) {      /* 80x25 mono  */
      puts("Display must be in 80x25 text mode");
      exit(1);
      }

   if (cpu.h.al == 7)
      scr = (SCREENTYPE)((void far *)MONOSCREEN);
   else
      scr = (SCREENTYPE)((void far *)COLORSCREEN);

  segs.es = FP_SEG(scr);
  cpu.x.di = FP_OFF(scr);

  cpu.h.ah = 254;
  int86x(16, &cpu, &cpu, &segs);

  FP_SEG(scr) = segs.es;
  FP_OFF(scr) = cpu.x.di;
  cbLVB = 25*80;

#else

   VIOMODEINFO viomiMode;
   viomiMode.cb = sizeof(viomiMode);
   VioGetMode(&viomiMode, 0);
   if ( (viomiMode.row != 25)
         ||
        (viomiMode.col != 80) ) {
      puts("Display must be in 80x25 text mode");
      exit(1);
      }

   VioGetBuf((PULONG) &scr, (PUSHORT) &cbLVB, 0);

#endif

}




/*
 *    **************************************************
 *    *                                                *
 *    *            End of Job Screen Cleanup           *
 *    *                                                *
 *    **************************************************
 */

extern void ScrTerm(void)

{

   if (printid[0] == 'B')        /* Close BIGCALC.PRN if open */
      fclose(printfile);

   CurPos(XSIGNROW + 1, 1);
   EraEop();

#ifdef REALMODE                  /* Show logical video buffer */
   cpu.x.cx = cbLVB;
   segs.es = FP_SEG(scr);
   cpu.x.di = FP_OFF(scr);
   cpu.h.ah = 255;
   int86x(16, &cpu, &cpu, &segs);
#else
   VioShowBuf(0, cbLVB, 0);
#endif

}




/*
 *    **************************************************
 *    *                                                *
 *    *                Clear the Screen                *
 *    *                                                *
 *    **************************************************
 */

extern void ScrClr(void)

{

   CurPos(1, 1);
   EraEop();

}




/*
 *    **************************************************
 *    *                                                *
 *    *              Erase to End of Line              *
 *    *                                                *
 *    **************************************************
 */

extern void EraEol(void)

{

   WNChar(' ', (80 - acol));

}




/*
 *    **************************************************
 *    *                                                *
 *    *              Erase to End of Page              *
 *    *                                                *
 *    **************************************************
 */

extern void EraEop(void)

{

   WNChar(' ', (80 * (24 - arow) + 80 - acol));

}




/*
 *    **************************************************
 *    *                                                *
 *    *         Position Cursor at row, column         *
 *    *                                                *
 *    **************************************************
 */

extern void CurPos(int row, int col)

{

   arow = row - 1;
   acol = col - 1;

#ifdef REALMODE
   cpu.h.ah = 2;                 /* Set cursor position */
   cpu.h.dh = (char) arow;
   cpu.h.dl = (char) acol;
   cpu.h.bh = 0;
   int86(16, &cpu, &cpu);
#else
   VioSetCurPos(arow, acol, 0);
#endif

}




/*
 *    **************************************************
 *    *                                                *
 *    *            Get Cursor row, column              *
 *    *                                                *
 *    **************************************************
 */

extern void CurGet(int *row, int *col)

{

   *row = arow + 1;
   *col = acol + 1;

}




/*
 *    **************************************************
 *    *                                                *
 *    *     Write Character count times to Screen      *
 *    *                                                *
 *    **************************************************
 */

static void WNChar(int chr, int count)

{

   CHTYPE byte, far *pos, far *pend;

   pos = &scr->ch[arow][acol];
   pend = pos + count;
   byte.data = (char)chr;
   byte.attr = SCRCOLOR;

   while (pos < pend)
      *pos++ = byte;

}




/*
 *    **************************************************
 *    *                                                *
 *    *            Write Character to Screen           *
 *    *                                                *
 *    **************************************************
 */

extern void WChar(int chr)

{

   scr->ch[arow][acol].data = (char)chr;
   if (++acol > 79) {
      arow++;
      acol = 0;
      }

}




/*
 *    **************************************************
 *    *                                                *
 *    *             Write String to Screen             *
 *    *                                                *
 *    **************************************************
 */

extern void WString(char *str)

{

   while (*str) {
      scr->ch[arow][acol].data = *(str++);
      if (++acol > 79) {
         arow++;
         acol = 0;
         }
      }

}




/*
 *    **************************************************
 *    *                                                *
 *    *             Write Integer to Screen            *
 *    *                                                *
 *    **************************************************
 */

extern void WInteger(long integer)

{

   long order;

   if (integer) {
      if (integer < 0L) {
         WChar('-');
         integer = - integer;
         }
      order = 1000000000L;
      while (order > integer) {
         order /= 10L;
         }
      while (order) {
         WChar((int)((integer / order) + (long)'0'));
         integer %= order;
         order /= 10L;
         }
      }
   else
      WChar('0');

}




/*
 *    **************************************************
 *    *                                                *
 *    *       Write String at row, col to Screen       *
 *    *                                                *
 *    **************************************************
 */

extern void WriteAt(int row, int col, char *str)

{

   CurPos(row, col);
   WString(str);

}




/*
 *    **************************************************
 *    *                                                *
 *    *     Display Message centered on row line       *
 *    *                                                *
 *    **************************************************
 */

extern void WriteCenter(int row, char *msg)

{

   CurPos(row, 1);
   EraEol();

   CurPos(row, ((80 - strlen(msg)) / 2));
   WString(msg);

}




/*
 *    **************************************************
 *    *                                                *
 *    *      Display Message centered on 25th line     *
 *    *                                                *
 *    **************************************************
 */

extern void Message(char *msg)

{

   WriteCenter(25, msg);

#ifdef REALMODE                  /* Show logical video buffer */
   cpu.x.cx = cbLVB;
   segs.es = FP_SEG(scr);
   cpu.x.di = FP_OFF(scr);
   cpu.h.ah = 255;
   int86x(16, &cpu, &cpu, &segs);
#else
   VioShowBuf(0, cbLVB, 0);
#endif

}




/*
 *    **************************************************
 *    *                                                *
 *    *      Write Message and Wait for keystroke      *
 *    *                                                *
 *    **************************************************
 */

extern void MessageWait(char *msg)

{

   char tmsg[81];

   strcpy(tmsg, msg);
   if (*tmsg)
      strcat(tmsg, "  ");
   strcat(tmsg, "(Press a key to continue)");

   Message(tmsg);
   GetChar();

}




/*
 *    **************************************************
 *    *                                                *
 *    *      Write Message and Prompt for Escape       *
 *    *                                                *
 *    **************************************************
 */

extern void MessageEsc(char *msg)

{

   char tmsg[81];

   strcpy(tmsg, msg);
   if (*tmsg)
      strcat(tmsg, "... ");
   strcat(tmsg, "(Press Esc to abort)");

   Message(tmsg);

}






/*
 *    **************************************************
 *    *                                                *
 *    *         Number Entry Support Routines          *
 *    *                                                *
 *    **************************************************
 */


/*
 *    **************************************************
 *    *                                                *
 *    *         Display Character at row, col          *
 *    *                                                *
 *    **************************************************
 */

extern void DisplayChar(int *row, int *col, int chr)

{

   CurPos(*row, *col);
   WChar(chr);

   if (*col < MAXDISPCOL)     /* Find position for next character */
      (*col)++;
   else {
      *col = MINDISPCOL;
      (*row)++;
      }
   CurPos(*row, *col);

}




/*
 *    **************************************************
 *    *                                                *
 *    *               Backspace Character              *
 *    *                                                *
 *    **************************************************
 */

extern void BackSpace(int *row, int *col)

{

   if (*col > MINDISPCOL)     /* find previous character position */
      (*col)--;
   else {
      *col = MAXDISPCOL;
      (*row)--;
      }

   CurPos(*row, *col);
   WChar(' ');
   CurPos(*row, *col);

}




/*
 *    **************************************************
 *    *                                                *
 *    *     Display Exponent Character at row, col     *
 *    *                                                *
 *    **************************************************
 */

extern void DisplayExpChar(int *row, int *col, int chr)

{

   CurPos(*row, *col);
   WChar(chr);

   if (*col < 80)             /* Find position for next character */
      (*col)++;
   else {
      *col = MINDISPCOL;
      (*row)++;
      }
   CurPos(*row, *col);

}




/*
 *    **************************************************
 *    *                                                *
 *    *          Backspace Exponent Character          *
 *    *                                                *
 *    **************************************************
 */

extern void BackSpaceExp(int *row, int *col)

{

   if (*col > MINDISPCOL)     /* find previous character position */
      (*col)--;
   else {
      *col = 80;
      (*row)--;
      }

   CurPos(*row, *col);
   WChar(' ');
   CurPos(*row, *col);

}




/*
 *    **************************************************
 *    *                                                *
 *    *          Display Exponent at row, col          *
 *    *                                                *
 *    **************************************************
 */

extern void DisplayExp(int *row, int *col, int exprow, int expcol, int expsign, long exponent)

{

   long order;


   *row = exprow;
   *col = expcol;
   CurPos(exprow, expcol);          /* Locate to beginning of exponent */
   EraEol();

   if (expsign == '-')
      DisplayExpChar(row, col, '-');

   if (exponent) {                  /* Write exponent value */
      if (exponent < 0L) {
         DisplayExpChar(row, col, '-');
         exponent = - exponent;
         }
      order = 1000000000L;
      while (order > exponent)
         order /= 10L;
      while (order) {
         DisplayExpChar(row, col, (int)((exponent / order) + (long) '0'));
         exponent %= order;
         order /= 10L;
         }
      }

}






/*
 *    **************************************************
 *    *                                                *
 *    *               Printer Routines                 *
 *    *                                                *
 *    **************************************************
 */


/*
 *    **************************************************
 *    *                                                *
 *    *           Print Character on Printer           *
 *    *                                                *
 *    **************************************************
 */

extern void PChar(int chr)

{

   if (!printtoscreen) {         /* If using printer or disk ... */
      fputc(chr, printfile);
      return;
      }

            /* Direct screen output */

   if (chr == CR) {              /* Carriage Return */
      acol = 0;
      }
   else if (chr == LF) {         /* Line Feed */
      arow++;
      acol = 0;
      }
   else if (chr == FF) {         /* Form Feed */
      }
   else {
      scr->ch[arow][acol].data = (char)chr;
      if (++acol > 79) {
         arow++;
         acol = 0;
         }
      }

}




/*
 *    **************************************************
 *    *                                                *
 *    *            Print String on Printer             *
 *    *                                                *
 *    **************************************************
 */

extern void PString(char *str)

{

   if (!printtoscreen) {            /* If using printer or disk ... */
      fputs(str, printfile);
      return;
      }

            /* Direct screen output */

   while (*str) {
      if (*str == CR) {             /* Carriage Return */
         acol = 0;
         }
      else if (*str == LF) {        /* Line Feed */
         arow++;
         acol = 0;
         }
      else if (*str == FF) {        /* Form Feed */
         }
      else {
         scr->ch[arow][acol].data = *(str);
         if (++acol > 79) {
            arow++;
            acol = 0;
            }
         }
      str++;
      }

}




/*
 *    **************************************************
 *    *                                                *
 *    *            Print Integer on Printer            *
 *    *                                                *
 *    **************************************************
 */

extern void PInteger(long integer)

{

   long order;

   if (integer) {
      if (integer < 0L) {
         PChar('-');
         integer = - integer;
         }
      order = 1000000000L;
      while (order > integer) {
         order /= 10L;
         }
      while (order) {
         PChar((int)((integer / order) + (long)'0'));
         integer %= order;
         order /= 10L;
         }
      }
   else
      PChar('0');

}




/*
 *    **************************************************
 *    *                                                *
 *    *              New Line on Printer               *
 *    *                                                *
 *    **************************************************
 */

extern void NewLine(void)

{

   PString("\r\n");

}




/*
 *    **************************************************
 *    *                                                *
 *    *              New Page on Printer               *
 *    *                                                *
 *    **************************************************
 */

extern void NewPage(void)

{

   PString("\r\f");

}






/*
 *    **************************************************
 *    *                                                *
 *    *          Miscellaneous I/O Routines            *
 *    *                                                *
 *    **************************************************
 */


/*
 *    **************************************************
 *    *                                                *
 *    *                 Beep speaker                   *
 *    *                                                *
 *    **************************************************
 */

extern void Beep(void)

{

#ifdef REALMODE
   static long endtime;
   static unsigned char saveport;

   outp(TIMERMODEPORT, SPEAKERMODE);
   saveport = (unsigned char) inp(SPEAKERPORT);

   outp(FREQPORT, CFREQ & 0xFF);          /* Start tone */
   outp(FREQPORT, CFREQ >> 8);
   outp(SPEAKERPORT, saveport | SPEAKERON);

   endtime = TimerTicks() + 2L;           /* Pause 2 clock ticks */
   while (TimerTicks() < endtime)
      ;

   outp(SPEAKERPORT, saveport);           /* Stop tone */
#else
   DosBeep(1000, 125);                    /* 1/8 second beep */
#endif

}




/*
 *    **************************************************
 *    *                                                *
 *    *         Fetch Timer Ticks since Midnight       *
 *    *                                                *
 *    **************************************************
 */
#ifdef REALMODE
extern long TimerTicks(void)
{

   static long ticks;

   cpu.h.ah = 0;
   int86(0x1A, &cpu, &cpu);

   ticks = (((long) cpu.x.cx) << 16) + ((long) cpu.x.dx);
   return(ticks);

}
#endif
