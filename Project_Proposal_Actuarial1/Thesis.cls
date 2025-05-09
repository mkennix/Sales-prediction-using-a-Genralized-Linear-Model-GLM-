%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\NeedsTeXFormat{LaTeX2e}[1996/12/01]
\ProvidesClass{Thesis}
              [2007/22/02 v1.0
   LaTeX document class]
\def\baseclass{book}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{\baseclass}}
\def\@checkoptions#1#2{
  \edef\@curroptions{\@ptionlist{\@currname.\@currext}}
  \@tempswafalse
  \@tfor\@this:=#2\do{
    \@expandtwoargs\in@{,\@this,}{,\@curroptions,}
    \ifin@ \@tempswatrue \@break@tfor \fi}
  \let\@this\@empty
  \if@tempswa \else \PassOptionsToClass{#1}{\baseclass}\fi
}
\@checkoptions{11pt}{{10pt}{11pt}{12pt}}
\PassOptionsToClass{a4paper}{\baseclass}
\ProcessOptions\relax
\LoadClass{\baseclass}
\newcommand\bhrule{\typeout{------------------------------------------------------------------------------}}

\newcommand\Certificate[1]{
\btypeout{Declaration by the Supervisor}
\addtotoc{Declaration by the Supervisor}
\thispagestyle{plain}
%\null\vfil
%\vskip 60\p@
\begin{center}{\huge\bf Declaration by the Supervisor\par}\end{center}
%\vskip 60\p@
{\normalsize #1}
\vfil\vfil\null
\cleardoublepage
}

\newcommand\Declaration[1]{
\btypeout{Declaration by the Students}
\addtotoc{Declaration by the Students}
\thispagestyle{plain}
%\null\vfil
%\vskip 60\p@
\begin{center}{\huge\bf Declaration by the Students\par}\end{center}
%\vskip 60\p@
{\normalsize #1}
\vfil\vfil\null
\cleardoublepage
}

\newcommand\btypeout[1]{\bhrule\typeout{\space #1}\bhrule}
\def\today{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space \number\year}
\usepackage{setspace}
\onehalfspacing
\setlength{\parindent}{0pt}
\setlength{\parskip}{2.0ex plus0.5ex minus0.2ex}
\usepackage{vmargin}

\RequirePackage[utf8]{inputenc} 

%----------------------------------------------------------------------------------------
%	MARGINS
%----------------------------------------------------------------------------------------
\setmarginsrb  { 2.5cm}  % left margin
               { 1.5cm}  % top margin
               { 1.5cm}  % right margin
               { 1.5cm}  % bottom margin
               {  20pt}  % head height
               {0.25in}  % head sep
               {   9pt}  % foot height
               { 0.3in}  % foot sep
%----------------------------------------------------------------------------------------
\raggedbottom
\setlength{\topskip}{1\topskip \@plus 5\p@}
\doublehyphendemerits=10000       % No consecutive line hyphens.
\brokenpenalty=10000              % No broken words across columns/pages.
\widowpenalty=9999                % Almost no widows at bottom of page.
\clubpenalty=9999                 % Almost no orphans at top of page.
\interfootnotelinepenalty=9999    % Almost never break footnotes.
\usepackage{fancyhdr}
\lhead[\rm\thepage]{\fancyplain{}{\sl{\rightmark}}}
\rhead[\fancyplain{}{\sl{\leftmark}}]{\rm\thepage}
\chead{}\lfoot{}\rfoot{}\cfoot{}
\pagestyle{fancy}
\renewcommand{\chaptermark}[1]{\btypeout{\thechapter\space #1}\markboth{\@chapapp\ \thechapter\ #1}{\@chapapp\ \thechapter\ #1}}
\renewcommand{\sectionmark}[1]{}
\renewcommand{\subsectionmark}[1]{}
\def\cleardoublepage{\clearpage\if@twoside \ifodd\c@page\else
\hbox{}
\thispagestyle{empty}
\newpage
\if@twocolumn\hbox{}\newpage\fi\fi\fi}
\usepackage{amsmath,amsfonts,amssymb,amscd,amsthm,xspace}
\theoremstyle{plain}
\newtheorem{example}{Example}[chapter]
\newtheorem{theorem}{Theorem}[chapter]
\newtheorem{corollary}[theorem]{Corollary}
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{proposition}[theorem]{Proposition}
\newtheorem{axiom}[theorem]{Axiom}
\theoremstyle{definition}
\newtheorem{definition}[theorem]{Definition}
\theoremstyle{remark}
\newtheorem{remark}[theorem]{Remark}
\usepackage[centerlast,small,sc]{caption}
\setlength{\captionmargin}{20pt}
\newcommand{\fref}[1]{Figure~\ref{#1}}
\newcommand{\tref}[1]{Table~\ref{#1}}
\newcommand{\eref}[1]{Equation~\ref{#1}}
\newcommand{\cref}[1]{Chapter~\ref{#1}}
\newcommand{\sref}[1]{Section~\ref{#1}}
\newcommand{\aref}[1]{Appendix~\ref{#1}}
\renewcommand{\topfraction}{0.85}
\renewcommand{\bottomfraction}{.85}
\renewcommand{\textfraction}{0.1}
\renewcommand{\dbltopfraction}{.85}
\renewcommand{\floatpagefraction}{0.75}
\renewcommand{\dblfloatpagefraction}{.75}
\setcounter{topnumber}{9}
\setcounter{bottomnumber}{9}
\setcounter{totalnumber}{20}
\setcounter{dbltopnumber}{9}
\usepackage{graphicx}
\usepackage{epstopdf}
\usepackage[scriptsize]{subfigure}
\usepackage{booktabs}
\usepackage{rotating}
\usepackage{listings}
\usepackage{lstpatch}
\lstset{captionpos=b,
        frame=tb,
        basicstyle=\scriptsize\ttfamily,
        showstringspaces=false,
        keepspaces=true}
\lstdefinestyle{matlab} {
        language=Matlab,
        keywordstyle=\color{blue},
        commentstyle=\color[rgb]{0.13,0.55,0.13}\em,
        stringstyle=\color[rgb]{0.7,0,0} }
\usepackage[pdfpagemode={UseOutlines},bookmarks=true,bookmarksopen=true,
   bookmarksopenlevel=0,bookmarksnumbered=true,hypertexnames=false,
   colorlinks,linkcolor={black},citecolor={blue},urlcolor={red},
   pdfstartview={FitV},unicode,breaklinks=true]{hyperref}
\pdfstringdefDisableCommands{
   \let\\\space
}
\newcommand*{\supervisor}[1]{\def\supname{#1}}
\newcommand*{\thesistitle}[1]{\def\ttitle{#1}}
\newcommand*{\examiner}[1]{\def\examname{#1}}
\newcommand*{\degree}[1]{\def\degreename{#1}}
\newcommand*{\authora}[1]{\def\authornamea{#1}}
\newcommand*{\authorb}[1]{\def\authornameb{#1}}
\newcommand*{\authorareg}[1]{\def\authornameareg{#1}}
\newcommand*{\authorbreg}[1]{\def\authornamebreg{#1}}
\newcommand*{\authoramail}[1]{\def\authornameamail{#1}}
\newcommand*{\authorbmail}[1]{\def\authornamebmail{#1}}
\newcommand*{\addresses}[1]{\def\addressnames{#1}}
\newcommand*{\university}[1]{\def\univname{#1}}
\newcommand*{\UNIVERSITY}[1]{\def\UNIVNAME{#1}}
\newcommand*{\department}[1]{\def\deptname{#1}}
\newcommand*{\DEPARTMENT}[1]{\def\DEPTNAME{#1}}
\newcommand*{\group}[1]{\def\groupname{#1}}
\newcommand*{\GROUP}[1]{\def\GROUPNAME{#1}}
\newcommand*{\faculty}[1]{\def\facname{#1}}
\newcommand*{\FACULTY}[1]{\def\FACNAME{#1}}
\newcommand*{\subject}[1]{\def\subjectname{#1}}
\newcommand*{\keywords}[1]{\def\keywordnames{#1}}
\newcommand*{\namesigdate}[2][5cm]{
  \begin{tabular}{@{}p{#1}@{}}
    #2 \\ [1cm] \hrule \vspace{0.1cm}
    { \textsl{Signature}} \\ [1cm] \hrule \vspace{0.1cm}
    { \textsl{Date}}
  \end{tabular}
}
%----------------------------------------------------------------------------------------
%	DOCUMENT VARIABLES
%----------------------------------------------------------------------------------------

\thesistitle{Forecasting Supermarket sales using a Generalized Linear Model} 
%-------------------------------------------------  
\supervisor{Mr. Andrew Kinyita}
%-------------------------------------------------   
\examiner{Dr. Simon Mundia} 
%-------------------------------------------------   
\degree{Bachelor of Science} 
%-------------------------------------------------   
\authora{Kennedy Mwangi}  \authorareg{S030-01-1572/2019} \authoramail{mail@dkut.ac.ke}
\authorb{Cynthia Chepkirui} \authorbreg{S030-01-1609/2019} \authorbmail{mail@dkut.ac.ke}
%-------------------------------------------------   
\addresses{}
%-------------------------------------------------   
\subject{Actuarial Science} 
%-------------------------------------------------   
\keywords{} 
%-------------------------------------------------   
\university{\texorpdfstring{\href{http://www.dkut.ac.ke} 
                {Dedan Kimathi University of Technology}} 
                {Dedan Kimathi University of Technology}}
%-------------------------------------------------   
\UNIVERSITY{\texorpdfstring{\href{http://www.dkut.ac.ke} 
                {DEDAN KIMATHI UNIVERSITY OF TECHNOLOGY}} 
                {DEDAN KIMATHI UNIVERSITY OF TECHNOLOGY}}
%-------------------------------------------------                
\department{\texorpdfstring{\href{http://www.dkut.ac.ke/index.php/department-homepage} 
             {Department of Statistics \& Actuarial Science}} 
             {Department of Statistics \& Actuarial Science}}
%-------------------------------------------------                
\DEPARTMENT{\texorpdfstring{\href{http://www.dkut.ac.ke/index.php/department-homepage}		 
			 {DEPARTMENT OF STATISTICS AND ACTUARIAL SCIENCE}}
			 {DEPARTMENT OF STATISTICS AND ACTUARIAL SCIENCE}}
%-------------------------------------------------                
\faculty{\texorpdfstring{\href{http://www.dkut.ac.ke/index.php/school-of-science}
			{School of Science}}
			{School of Science}}
%-------------------------------------------------                
\FACULTY{\texorpdfstring{\href{http://www.dkut.ac.ke/index.php/school-of-science} 
        {SCHOOL OF SCIENCE}}  
        {SCHOOL OF SCIENCE}}
%----------------------------------------------------------------------------------------    

%----------------------------------------------------------------------------------------
%	ABSTRACT PAGE DESIGN
%----------------------------------------------------------------------------------------
\newenvironment{abstract}
{
  \btypeout{Abstract Page}
  \addtotoc{Abstract} 
  \thispagestyle{plain}
  \begin{center}{\huge\bf Abstract\par}\end{center}
}
%----------------------------------------------------------------------------------------
{
  \vfil\vfil\vfil\null
  \cleardoublepage
}
\addtocounter{secnumdepth}{1}
\setcounter{tocdepth}{6}
\newcounter{dummy}
\newcommand\addtotoc[1]{
\refstepcounter{dummy}
\addcontentsline{toc}{chapter}{#1}}
\renewcommand\tableofcontents{
\btypeout{Table of Contents}
\addtotoc{Table of Contents}
\begin{spacing}{1}{
    \setlength{\parskip}{1pt}
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\contentsname
        \@mkboth{
           \MakeUppercase\contentsname}{\MakeUppercase\contentsname}}
    \@starttoc{toc}
    \if@restonecol\twocolumn\fi
   \cleardoublepage
}\end{spacing}
}
\renewcommand\listoffigures{
\btypeout{List of Figures}
\addtotoc{List of Figures}
\begin{spacing}{1}{
    \setlength{\parskip}{1pt}
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listfigurename
      \@mkboth{\MakeUppercase\listfigurename}
              {\MakeUppercase\listfigurename}}
    \@starttoc{lof}
    \if@restonecol\twocolumn\fi
    \cleardoublepage
}\end{spacing}
}
\renewcommand\listoftables{
\btypeout{List of Tables}
\addtotoc{List of Tables}
\begin{spacing}{1}{ 
    \setlength{\parskip}{1pt}
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listtablename
      \@mkboth{ 
          \MakeUppercase\listtablename}{\MakeUppercase\listtablename}}
    \@starttoc{lot}
    \if@restonecol\twocolumn\fi
    \cleardoublepage
}\end{spacing}
}
\newcommand\listsymbolname{Abbreviations}
\usepackage{longtable}
\newcommand\listofsymbols[2]{
\btypeout{\listsymbolname}
\addtotoc{\listsymbolname}
    \chapter*{\listsymbolname
      \@mkboth{
          \MakeUppercase\listsymbolname}{\MakeUppercase\listsymbolname}}
\begin{longtable}[c]{#1}#2\end{longtable}\par
    \cleardoublepage
}
\newcommand\listconstants{Physical Constants}
\usepackage{longtable}
\newcommand\listofconstants[2]{
\btypeout{\listconstants}
\addtotoc{\listconstants}
    \chapter*{\listconstants
      \@mkboth{
          \MakeUppercase\listconstants}{\MakeUppercase\listconstants}}
\begin{longtable}[c]{#1}#2\end{longtable}\par
    \cleardoublepage
}
\newcommand\listnomenclature{Symbols}
\usepackage{longtable}
\newcommand\listofnomenclature[2]{
\btypeout{\listnomenclature}
\addtotoc{\listnomenclature}
    \chapter*{\listnomenclature
      \@mkboth{
          \MakeUppercase\listnomenclature}{\MakeUppercase\listnomenclature}}
\begin{longtable}[l]{#1}#2\end{longtable}\par
    \cleardoublepage
}
\newcommand\acknowledgement[1]{
\btypeout{Acknowledgement}
\addtotoc{Acknowledgement}
\thispagestyle{plain}
\begin{center}{\huge{\textit{Acknowledgement}} \par}\end{center}
{\normalsize #1}
\vfil\vfil\null
}
\newcommand\dedicatory[1]{
\btypeout{Dedicatory}
\addtotoc{Dedication}
\begin{center}{\huge\bf Dedication\par}\end{center}
\thispagestyle{plain}
{#1}
\cleardoublepage
}
\newcommand\References{
  \addtotoc{References}
  \btypeout{References}
  }
\endinput
