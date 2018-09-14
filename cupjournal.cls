%% This is cupjournal.cls v1.5, 2017/06/05
%% updated for CUP by Overleaf
\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{cupjournal}[2017/06/05, v1.5]

\newif\if@cropmarkson \@cropmarksonfalse
\newif\if@doublespaced \@doublespacedfalse
\newif\if@usebibtex \@usebibtexfalse
\DeclareOption{crop}{\@cropmarksontrue}
\DeclareOption{doublespaced}{\@doublespacedtrue}
\DeclareOption{bibtex}{\@usebibtextrue}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass[a4paper,10pt,twoside,onecolumn,final]{article}

\RequirePackage[T1]{fontenc}
\RequirePackage{newtxtext,newtxmath}
\RequirePackage{lineno}
\RequirePackage{enumitem}
\if@usebibtex
\RequirePackage{natbib}
\else
\PassOptionsToPackage{natbib=true}{biblatex}
\RequirePackage[authordate,backend=biber]{biblatex-chicago}
\fi

\RequirePackage[font={small},justification=justified,singlelinecheck=false]{caption}
\RequirePackage[tracking=smallcaps]{microtype}

\renewcommand{\linenumberfont}{\normalfont\scriptsize}

\RequirePackage{etoolbox}
\RequirePackage{fancyhdr}
\RequirePackage[explicit]{titlesec}
\RequirePackage{ragged2e}
\RequirePackage{textcase}
\RequirePackage[style]{abstract}

\setcounter{secnumdepth}{0}

\RequirePackage[paperheight=247mm,paperwidth=174mm,top=2cm,left=18mm,bottom=25.8mm,right=22mm,footskip=24pt,headsep=\baselineskip,headheight=24pt]{geometry}

% \RequirePackage[a4paper,top=2cm,left=2.5cm,bottom=2.5cm,right=4cm,footskip=24pt,headsep=\baselineskip,headheight=24pt,footnotesep=24pt]{geometry}

\RequirePackage{setspace}
\RequirePackage[norule]{footmisc}
\if@doublespaced
  \doublespacing
  \renewcommand{\footnotelayout}{\footnotesize\doublespacing}
  \patchcmd{\thanks}{#1}{\protect\doublespacing\footnotesize#1}{}{}
\fi

\if@cropmarkson
 \RequirePackage[cam,a4,center,info]{crop}
\fi

\clubpenalty 10000
\widowpenalty 10000

\newcommand{\doinumber}[1]{\def\@doinumber{#1}}
\newcommand{\journalname}[1]{\def\@journalname{#1}}
\newcommand{\journalvolume}[1]{\def\@journalvolume{#1}}
\newcommand{\journalyear}[1]{\def\@journalyear{#1}}
\newcommand{\pagecount}[1]{\def\@pagecount{#1}}

% \patchcmd{\sectionmark}{\MakeUppercase}{}{}{\PackageWarning{etoolbox}{nope}}

\fancyhf{}
\renewcommand{\headrulewidth}{0pt}
\fancyhead[RO]{\emph{\rightmark}\hspace{2em}\thepage}
\fancyhead[LE]{\thepage\hspace{2em}\textsc{\nouppercase{\leftmark}}}
\pagestyle{fancy}

\fancypagestyle{journalinfo}{%
  \fancyhf{}
  \lhead{\footnotesize\emph{\@journalname} \space\textbf{\@journalvolume,}\space\@pagecount\space\emph{ Cambridge University Press, \@journalyear}\\doi:\@doinumber}
}

\patchcmd{\maketitle}{\thispagestyle{plain}}{\thispagestyle{journalinfo}}{}{}

\renewcommand{\@maketitle}{%
  \renewcommand{\and}{\textsc{and} }
  \clearpage\null\vskip2em{\raggedright\singlespacing
    {\Large\itshape\bfseries\sffamily\@title\par}
    \vskip1em
    {\@author\par}
  }
  \vskip1.5em%
}
\renewcommand{\abstitlestyle}[1]{}
\setlength{\absleftindent}{0pt}
\setlength{\absrightindent}{0pt}
\setlength{\absparindent}{0pt}
\renewcommand{\abstracttextfont}{\footnotesize\noindent}
\apptocmd{\abstract}{\ignorespaces}{}{}
\apptocmd{\endabstract}{\vskip1em}{}{}

\titleformat{\section}
  {\scshape\normalsize\mdseries}
  {\thesection}
  {0pt}
  {#1}

\titleformat{\subsection}
  {\itshape\mdseries\normalsize}
  {\thesubsection}
  {0pt}
  {#1}

\titleformat{\subsubsection}[runin]
  {\itshape\mdseries\normalsize}
  {\thesubsubsection}
  {0pt}
  {#1.}

\titlespacing*{\section}{0pt}{2em}{1.5em}

\titlespacing*{\subsection}{0pt}{1.5em}{1em}

\titlespacing*{\subsubsection}{0pt}{1em}{5pt}

\renewcommand{\sectionmark}[1]{}
\renewcommand{\subsectionmark}[1]{}
\renewcommand{\subsubsectionmark}[1]{}


% \newcommand{\fignote}[1]{\justifying\noindent\fontsize{8.5pt}{10pt}\selectfont {\emph{Note:~}}#1 \par}
\newcommand{\fignote}[1]{\justifying\noindent\small{\emph{Note:~}}#1\par}

% \renewcommand\tablebody{\fontsize{9.5}{11}\selectfont }
\captionsetup*[table]{labelfont={up,sc}}
\captionsetup*[figure]{labelfont={it}}
\renewcommand{\tablename}{table}
\renewcommand{\arraystretch}{1.15}
\AtBeginEnvironment{tabular}{\small}
% \newcommand{\tabnote}[1]{ \vskip5pt \fontsize{9.5pt}{11pt}\selectfont {\it Note:}~#1 \par}
\newcommand{\tabnote}[1]{\vskip5pt{\justifying\noindent\small {\emph{Note:~}#1\par}}}

\newlist{hypolist}{enumerate}{2}
\setlist[hypolist]{itemsep=0pt,leftmargin=10em,label={\textsc{Hypothesis} \arabic*:}}

\endinput
