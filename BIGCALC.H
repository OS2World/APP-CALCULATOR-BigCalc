/*
 *    **************************************************
 *    *                                                *
 *    *                   BIGCALC.H                    *
 *    *                                                *
 *    *          Extended Precision Calculator         *
 *    *                                                *
 *    *     Global Constants & Function Prototypes     *
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
 *    *                   Constants                    *
 *    *                                                *
 *    **************************************************
 */

#define TITLE "Extended Precision Calculator 4.3"

#define REALMODE                          /* Remove for OS/2 protected mode */

#define MINPREC 3                         /* Min nbr of digits  */
#define DEFAULTPREC 50                    /* Default precision  */
#define MAXNORM 1075                      /* Max normal digits  */
#define MAXCOMP (MAXNORM + 9)             /* Max compute digits */
#define MAXWORK (MAXCOMP * 2 + 5)         /* Max work digits    */
#define COMPPREC (normprec + 9)           /* Compute digits     */
#define WORKPREC (compprec * 2 + 5)       /* Work digits        */
#define MAXEXDIGITS 9                     /* Max exponent digits */
#define MINEXP -999999999L                /* Min exponent */
#define MAXEXP  999999999L                /* Max exponent */
#define MINFLOATDSP -50L                  /* Min exp float decimal display */
#define MAXFLOATDSP 65L                   /* Max exp float decimal display */
#define XSIGNROW 18                       /* X sign row */
#define ENTRYSIGNROWBIG 4                 /* Sign row for big X */
#define ENTRYSIGNROWSMALL 18              /* Sign row for small X */
#define SIZEOFSMALL 453                   /* Max size for part screen entry */
#define SIGNDISPCOL 3                     /* Sign column for number display */
#define MINDISPCOL 4                      /* Min column for number display */
#define MAXDISPCOL 68                     /* Max column for number display */
#define SIGNPRTCOL 4                      /* Sign column for number printer */
#define MINPRTCOL 5                       /* Min column for number printer */
#define MAXPRTCOL 63                      /* Max column for number printer */
#define DISPDECIMAL '.'                   /* Display decimal point */
#define PRINTDECIMAL '*'                  /* Print decimal point */



/*
 *    **************************************************
 *    *                                                *
 *    *                    Defines                     *
 *    *                                                *
 *    **************************************************
 */

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef BOOLEAN
#define BOOLEAN int
#endif

#define GETNORMTEMP(n) malloc((n) * sizeof(NORMTYPE))
#define GETCOMPTEMP(n) malloc((n) * sizeof(COMPTYPE))




/*
 *    **************************************************
 *    *                                                *
 *    *                   Key Codes                    *
 *    *                                                *
 *    **************************************************
 */

#define ESCAPE     27   /* Escape          */
#define BACKSPACE  8    /* Back Space      */
#define CR         '\r' /* Carriage Return */
#define LF         '\n' /* Line Feed       */
#define FF         '\f' /* Form Feed       */

#define ADD        43   /*  +          */
#define SUBTRACT   45   /*  -          */
#define MULTIPLY   42   /*  *          */
#define DIVIDE     47   /*  /          */
#define LASTX      76   /*  L          */
#define CLEAR      67   /*  C          */
#define PRINT      80   /*  P          */
#define SCINOT     70   /*  F          */
#define XCHGXREG   88   /*  X          */
#define CHGSIGN    83   /*  S          */
#define PRINTDISK  68   /*  D          */
#define GROUPSIZE  71   /*  G          */
#define MENUROLL   77   /*  M          */
#define VIEWREG    86   /*  V          */
#define CLEARX     8    /* Back Space  */
#define STOREX     1073 /* PgUp        */
#define RECALLREG  1081 /* PgDn        */
#define ENTER      13   /* Enter       */
#define XCHGXY1    1075 /* Left Arrow  */
#define XCHGXY2    1077 /* Right Arrow */
#define ROLLDOWN   1080 /* Down Arrow  */
#define ROLLUP     1072 /* Up Arrow    */
#define HELP       1059 /* F1          */
#define POWER      1060 /* F2          */
#define SQUAREROOT 1061 /* F3          */
#define SQUARE     1062 /* F4          */
#define RECIPROCAL 1063 /* F5          */
#define FACTORIAL  1064 /* F6          */
#define INTEGER    1065 /* F7          */
#define FRACTION   1066 /* F8          */
#define RECALLPI   1067 /* F9          */
#define RECALLE    1068 /* F10         */
#define SIN        1094 /* CTRL-F1     */
#define ARCSIN     1095 /* CTRL-F2     */
#define COS        1096 /* CTRL-F3     */
#define ARCCOS     1097 /* CTRL-F4     */
#define TAN        1098 /* CTRL-F5     */
#define ARCTAN     1099 /* CTRL-F6     */
#define LOG        1100 /* CTRL-F7     */
#define EXP10      1101 /* CTRL-F8     */
#define LN         1102 /* CTRL-F9     */
#define EXPE       1103 /* CTRL-F10    */




/*
 *    **************************************************
 *    *                                                *
 *    *                  Data Types                    *
 *    *                                                *
 *    **************************************************
 */

typedef char NORMDIGIT;
typedef int  WORKDIGIT;

typedef struct {
   long exp;
   int sign;
   int digits;
   NORMDIGIT man[MAXNORM];
   } NORMTYPE;

typedef struct {
   long exp;
   int sign;
   int digits;
   WORKDIGIT man[MAXWORK];
   } WORKTYPE;

typedef struct {
   long exp;
   int sign;
   int digits;
   WORKDIGIT man[MAXCOMP];
   } COMPTYPE;



/*
 *    **************************************************
 *    *                                                *
 *    *        BIGCALC.C Function Declarations         *
 *    *                                                *
 *    **************************************************
 */

extern  void main(int argc,char * *argv);
static  void Initialize(int argc,char *argv);

static  void Add(void);
static  void Subtract(void);
static  void Multiply(void);
static  void Divide(void);
static  void Power(void);
static  void SquareRoot(void);
static  void Square(void);
static  void Reciprocal(void);
static  void Factorial(void);
static  void IntegerPart(void);
static  void FractionPart(void);
static  void Sin(void);
static  void ArcSin(void);
static  void Cos(void);
static  void ArcCos(void);
static  void Tan(void);
static  void ArcTan(void);
static  void Log(void);
static  void Exp10(void);
static  void Ln(void);
static  void ExpE(void);
static  void RecallPi(void);
static  void RecallE(void);
static  void RecallLastX(void);
static  void ChangeSign(void);
static  void PrintDisk(void);
static  void GroupSize(void);
static  void MenuRoll(void);
static  void ViewReg(void);
static  void AcceptX(void);
static  void ClearX(void);
static  void Enter(void);
static  void SciNotation(void);
static  void Clear(void);
static  void Print(void);
static  void StoreX(void);
static  void AddXReg(void);
static  void SubtractXReg(void);
static  void MultiplyXReg(void);
static  void DivideXReg(void);
static  void RecallReg(void);
static  void AddRegX(void);
static  void SubtractRegX(void);
static  void MultiplyRegX(void);
static  void DivideRegX(void);
static  void ExchangeXY(void);
static  void ExchangeXReg(void);
static  void RollDown(void);
static  void RollUp(void);
static  void PushStack(void);
static  void DropStack(void);



/*
 *    **************************************************
 *    *                                                *
 *    *         BIGIO.C Function Declarations          *
 *    *                                                *
 *    **************************************************
 */

extern  int GetChar(void);
extern  int KeyPressed(void);

extern  void ScrInit(void);
extern  void ScrTerm(void);
extern  void ScrClr(void);
extern  void EraEol(void);
extern  void EraEop(void);
extern  void CurPos(int row,int col);
extern  void CurGet(int *row,int *col);
static  void WNChar(int chr,int count);
extern  void WChar(int chr);
extern  void WString(char *str);
extern  void WInteger(long integer);
extern  void WriteAt(int row,int col,char *str);
extern  void WriteCenter(int row,char *msg);
extern  void Message(char *msg);
extern  void MessageWait(char *msg);
extern  void MessageEsc(char *msg);

extern  void DisplayChar(int *row,int *col,int chr);
extern  void BackSpace(int *row,int *col);
extern  void DisplayExpChar(int *row,int *col,int chr);
extern  void BackSpaceExp(int *row,int *col);
extern  void DisplayExp(int *row,int *col,int exprow,int expcol,int expsign,long exponent);

extern  void PChar(int chr);
extern  void PString(char *str);
extern  void PInteger(long integer);
extern  void NewLine(void);
extern  void NewPage(void);

extern  void Beep(void);
extern  long TimerTicks(void);



/*
 *    **************************************************
 *    *                                                *
 *    *        BIGMATH.C Function Declarations         *
 *    *                                                *
 *    **************************************************
 */

extern  int ExtendedAdd(void);
extern  int ExtendedSubtract(void);
extern  int ExtendedMultiply(void);
extern  int ExtendedDivide(void);
extern  int ExtendedSquareRoot(void);
extern  int ExtendedPower(void);
extern  int ExtendedSinCos(int scflag);
extern  int ExtendedTan(void);
extern  int ExtendedArcSinCos(int scflag);
extern  int ExtendedArcTan(void);
static  int ResolveAngle(void);
extern  int ExtendedLog(void);
extern  int ExtendedExp10(void);
extern  int ExtendedLn(void);
extern  int ExtendedExpE(void);
extern  int ExtendedReciprocal(void);
extern  int ExtendedFactorial(void);



/*
 *    **************************************************
 *    *                                                *
 *    *        BIGMISC.C Function Declarations         *
 *    *                                                *
 *    **************************************************
 */

extern  int Normalize(int w);
extern  int FlipSign(int sign);
extern  int ExtendedRound(int w);
extern  int ExtendedIntegerPart(void);
extern  int ExtendedFractionPart(void);
extern  int ExtendedGetX(void);

extern  void ExtendedRecallPi(int dest);
extern  void ExtendedRecallHalfPi(int dest);
extern  void ExtendedRecallE(int dest);
extern  void ExtendedRecallLn10(int dest);
extern  void ExtendedRecallLnP9(int dest);
extern  void ExtendedRecallLnP99(int dest);
extern  void ExtendedRecallLnP999(int dest);
extern  void ExtendedRecallLnP9999(int dest);
extern  void ExtendedRecallLnP99999(int dest);
extern  void ExtendedRecallSinP1(int dest);
extern  void ExtendedRecallSinP01(int dest);
extern  void ExtendedRecallSinP001(int dest);
extern  void ExtendedRecallCosP1(int dest);
extern  void ExtendedRecallCosP01(int dest);
extern  void ExtendedRecallCosP001(int dest);

extern  void ClearStack(int lo,int hi);
extern  void ClearReg(int lo,int hi);
extern  void ClearWork(int w);
extern  void ClearTemp(COMPTYPE *temp);
extern  void MoveStackWork(int source,int dest);
extern  void MoveRegWork(int source,int dest);
extern  void MoveWorkStack(int source,int dest);
extern  void MoveWorkReg(int source,int dest);
extern  void MoveWorkWork(int source,int dest);
extern  void MoveWorkTemp(int source,COMPTYPE *dest);
extern  void MoveTempWork(COMPTYPE *source,int dest);
extern  void MoveTempTemp(COMPTYPE *source,COMPTYPE *dest);
extern  void SetWorkInteger(int dest,long integer);
extern  void SetTempInteger(COMPTYPE *dest,long integer);



/*
 *    **************************************************
 *    *                                                *
 *    *       BIGPRINT.C Function Declarations         *
 *    *                                                *
 *    **************************************************
 */

extern  void PrintHeading(void);
extern  void PrintReg(int lo,int hi);
extern  void PrintStack(int lo,int hi);
extern  void DisplayReg(int r);
extern  void DisplayStack(int s);
extern  void PrintNumber(NORMTYPE *nbr);
static  void PCharWrap(int chr);
static  void PSign(int sign);
static  void PExponent(long exponent);

extern  void WriteReg(int lo,int hi);
extern  void WriteStack(int lo,int hi);
extern  void WriteNumber(NORMTYPE *nbr);
static  void WExponent(long exponent);
extern  int  GetPrompt(void);
extern  void Overflow(void);
extern  void DivideByZero(void);
extern  void ZeroArgument(void);
extern  void NegativeArgument(void);
extern  void ArgumentNotInteger(void);
extern  void ArgumentInvalid(void);
extern  void MemoryError(void);

extern  void OnScreenMenu(void);
extern  void InitialScreen(void);
extern  void HelpScreen(void);
extern  void Heading1(void);
extern  void Heading2(void);
static  void HelpScreen1(void);
static  void HelpScreen2(void);
static  void HelpScreen3(void);
extern  void WorkScreen(void);
