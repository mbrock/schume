
\documentclass{report}
%include polycode.fmt

% %subst comment a = "\mbox{\textbf{\sf \small --- " a "}}"
%subst comment a = "\mbox{\small --- " a "}"
%subst keyword a = "\mbox{\bf " a "}"
%format {          = "\mskip5mu\{\mskip1.5mu "

\usepackage[latin1]{inputenc}
\usepackage[boldsans]{ccfonts}

\usepackage{parskip}

\begin{document}

\title{\Huge{Scheme Compiler}}
\date{May 2008}
\author{Mikael Brockman \\ {\tt <m@@htns.se>}}
\maketitle

\chapter{Compiler}

%include Schume/Compiler.lhs

\end{document}