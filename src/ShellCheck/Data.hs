module ShellCheck.Data where

import ShellCheck.Interface
import Data.Version (showVersion)
import Paths_ShellCheck (version)

shellcheckVersion = showVersion version

genericInternalVariables = [
    -- Generic
    "", "_", "rest", "REST",

    -- Bash
    "BASH", "BASHOPTS", "BASHPID", "BASH_ALIASES", "BASH_ARGC",
    "BASH_ARGV", "BASH_CMDS", "BASH_COMMAND", "BASH_EXECUTION_STRING",
    "BASH_LINENO", "BASH_REMATCH", "BASH_SOURCE", "BASH_SUBSHELL",
    "BASH_VERSINFO", "BASH_VERSION", "COMP_CWORD", "COMP_KEY",
    "COMP_LINE", "COMP_POINT", "COMP_TYPE", "COMP_WORDBREAKS",
    "COMP_WORDS", "COPROC", "DIRSTACK", "EUID", "FUNCNAME", "GROUPS",
    "HISTCMD", "HOSTNAME", "HOSTTYPE", "LINENO", "MACHTYPE", "MAPFILE",
    "OLDPWD", "OPTARG", "OPTIND", "OSTYPE", "PIPESTATUS", "PPID", "PWD",
    "RANDOM", "READLINE_LINE", "READLINE_POINT", "REPLY", "SECONDS",
    "SHELLOPTS", "SHLVL", "UID", "BASH_ENV", "BASH_XTRACEFD", "CDPATH",
    "COLUMNS", "COMPREPLY", "EMACS", "ENV", "FCEDIT", "FIGNORE",
    "FUNCNEST", "GLOBIGNORE", "HISTCONTROL", "HISTFILE", "HISTFILESIZE",
    "HISTIGNORE", "HISTSIZE", "HISTTIMEFORMAT", "HOME", "HOSTFILE", "IFS",
    "IGNOREEOF", "INPUTRC", "LANG", "LC_ALL", "LC_COLLATE", "LC_CTYPE",
    "LC_MESSAGES", "LC_MONETARY", "LC_NUMERIC", "LC_TIME", "LINES", "MAIL",
    "MAILCHECK", "MAILPATH", "OPTERR", "PATH", "POSIXLY_CORRECT",
    "PROMPT_COMMAND", "PROMPT_DIRTRIM", "PS1", "PS2", "PS3", "PS4", "SHELL",
    "TIMEFORMAT", "TMOUT", "TMPDIR", "auto_resume", "histchars", "COPROC",

    -- Other
    "USER", "TZ", "TERM", "LOGNAME", "LD_LIBRARY_PATH", "LANGUAGE", "DISPLAY",
    "HOSTNAME", "KRB5CCNAME", "XAUTHORITY"

    -- shflags
    , "FLAGS_ARGC", "FLAGS_ARGV", "FLAGS_ERROR", "FLAGS_FALSE", "FLAGS_HELP",
    "FLAGS_PARENT", "FLAGS_RESERVED", "FLAGS_TRUE", "FLAGS_VERSION",
    "flags_error", "flags_return"
  ]

kshInternalVariables = [
    ".sh.version"
  ]

portageInternalVariables = [
    "A", "BDEPEND", "BROOT", "CATEGORY", "D", "DEPEND", "DESCRIPTION",
    "DISTDIR", "DOCS", "EAPI", "ED", "EPREFIX", "EROOT", "ESYSROOT", "FILESDIR",
    "HOME", "HOMEPAGE", "HTML_DOCS", "IUSE", "KEYWORDS", "LICENSE", "P",
    "PATCHES", "PDEPEND", "PF", "PN", "PR", "PROPERTIES", "PV", "PVR",
    "QA_AM_MAINTAINER_MODE", "QA_CONFIGURE_OPTIONS", "QA_DESKTOP_FILE",
    "QA_DT_NEEDED", "QA_EXECSTACK", "QA_FLAGS_IGNORED", "QA_MULTILIB_PATHS",
    "QA_PREBUILT", "QA_PRESTRIPPED", "QA_SONAME", "QA_SONAME_NO_SYMLINK",
    "QA_TEXTRELS", "QA_WX_LOAD", "RDEPEND", "REQUIRED_USE", "RESTRICT", "ROOT",
    "S", "SLOT", "SRC_TEST", "SRC_URI", "STRIP_MASK", "SUBSLOT", "SYSROOT",
    "T", "WORKDIR"
  ]

specialVariablesWithoutSpaces = [
    "$", "-", "?", "!", "#"
  ]

shellVariablesWithoutSpaces = specialVariablesWithoutSpaces ++ [
    "BASHPID", "BASH_ARGC", "BASH_LINENO", "BASH_SUBSHELL", "EUID", "LINENO",
    "OPTIND", "PPID", "RANDOM", "SECONDS", "SHELLOPTS", "SHLVL", "UID",
    "COLUMNS", "HISTFILESIZE", "HISTSIZE", "LINES"

    -- shflags
    , "FLAGS_ERROR", "FLAGS_FALSE", "FLAGS_TRUE"
  ]

portageVariablesWithoutSpaces = [
    "EAPI", "P", "PF", "PN", "PR", "PV", "PVR", "SLOT"
  ]

specialVariables = specialVariablesWithoutSpaces ++ ["@", "*"]

unbracedVariables = specialVariables ++ [
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
  ]

shellArrayVariables = [
    "BASH_ALIASES", "BASH_ARGC", "BASH_ARGV", "BASH_CMDS", "BASH_LINENO",
    "BASH_REMATCH", "BASH_SOURCE", "BASH_VERSINFO", "COMP_WORDS", "COPROC",
    "DIRSTACK", "FUNCNAME", "GROUPS", "MAPFILE", "PIPESTATUS", "COMPREPLY"
  ]

portageArrayVariables = [
    "PATCHES"
  ]

commonCommands = [
    "admin", "alias", "ar", "asa", "at", "awk", "basename", "batch",
    "bc", "bg", "break", "c99", "cal", "cat", "cd", "cflow", "chgrp",
    "chmod", "chown", "cksum", "cmp", "colon", "comm", "command",
    "compress", "continue", "cp", "crontab", "csplit", "ctags", "cut",
    "cxref", "date", "dd", "delta", "df", "diff", "dirname", "dot",
    "du", "echo", "ed", "env", "eval", "ex", "exec", "exit", "expand",
    "export", "expr", "fc", "fg", "file", "find", "fold", "fort77",
    "fuser", "gencat", "get", "getconf", "getopts", "grep", "hash",
    "head", "iconv", "ipcrm", "ipcs", "jobs", "join", "kill", "lex",
    "link", "ln", "locale", "localedef", "logger", "logname", "lp",
    "ls", "m4", "mailx", "make", "man", "mesg", "mkdir", "mkfifo",
    "more", "mv", "newgrp", "nice", "nl", "nm", "nohup", "od", "paste",
    "patch", "pathchk", "pax", "pr", "printf", "prs", "ps", "pwd",
    "qalter", "qdel", "qhold", "qmove", "qmsg", "qrerun", "qrls",
    "qselect", "qsig", "qstat", "qsub", "read", "readonly", "renice",
    "return", "rm", "rmdel", "rmdir", "sact", "sccs", "sed", "set",
    "sh", "shift", "sleep", "sort", "split", "strings", "strip", "stty",
    "tabs", "tail", "talk", "tee", "test", "time", "times", "touch",
    "tput", "tr", "trap", "tsort", "tty", "type", "ulimit", "umask",
    "unalias", "uname", "uncompress", "unexpand", "unget", "uniq",
    "unlink", "unset", "uucp", "uudecode", "uuencode", "uustat", "uux",
    "val", "vi", "wait", "wc", "what", "who", "write", "xargs", "yacc",
    "zcat"
  ]

nonReadingCommands = [
    "alias", "basename", "bg", "cal", "cd", "chgrp", "chmod", "chown",
    "cp", "du", "echo", "export", "false", "fg", "fuser", "getconf",
    "getopt", "getopts", "ipcrm", "ipcs", "jobs", "kill", "ln", "ls",
    "locale", "mv", "printf", "ps", "pwd", "renice", "rm", "rmdir",
    "set", "sleep", "touch", "trap", "true", "ulimit", "unalias", "uname"
    ]

sampleWords = [
    "alpha", "bravo", "charlie", "delta", "echo", "foxtrot",
    "golf", "hotel", "india", "juliett", "kilo", "lima", "mike",
    "november", "oscar", "papa", "quebec", "romeo", "sierra",
    "tango", "uniform", "victor", "whiskey", "xray", "yankee",
    "zulu"
  ]

binaryTestOps = [
    "-nt", "-ot", "-ef", "==", "!=", "<=", ">=", "-eq", "-ne", "-lt", "-le",
    "-gt", "-ge", "=~", ">", "<", "=", "\\<", "\\>", "\\<=", "\\>="
  ]

unaryTestOps = [
    "!", "-a", "-b", "-c", "-d", "-e", "-f", "-g", "-h", "-L", "-k", "-p",
    "-r", "-s", "-S", "-t", "-u", "-w", "-x", "-O", "-G", "-N", "-z", "-n",
    "-o", "-v", "-R"
  ]

shellForExecutable :: String -> Maybe Shell
shellForExecutable name =
    case name of
        "sh"    -> return Sh
        "bash"  -> return Bash
        "bats"  -> return Bash
        "dash"  -> return Dash
        "ash"   -> return Dash -- There's also a warning for this.
        "ksh"   -> return Ksh
        "ksh88" -> return Ksh
        "ksh93" -> return Ksh
        otherwise -> Nothing
