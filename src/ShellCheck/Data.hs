module ShellCheck.Data where

import ShellCheck.Interface
import Data.Version (showVersion)


{-
If you are here because you saw an error about Paths_ShellCheck in this file,
simply comment out the import below and define the version as a constant string.

Instead of:

    import Paths_ShellCheck (version)
    shellcheckVersion = showVersion version

Use:

    -- import Paths_ShellCheck (version)
    shellcheckVersion = "kludge"

-}

import Paths_ShellCheck (version)
shellcheckVersion = showVersion version  -- VERSIONSTRING


internalVariables = [
    -- Generic
    "", "_", "rest", "REST",

    -- Bash
    "BASH", "BASHOPTS", "BASHPID", "BASH_ALIASES", "BASH_ARGC",
    "BASH_ARGV", "BASH_ARGV0", "BASH_CMDS", "BASH_COMMAND",
    "BASH_EXECUTION_STRING", "BASH_LINENO", "BASH_LOADABLES_PATH",
    "BASH_REMATCH", "BASH_SOURCE", "BASH_SUBSHELL", "BASH_VERSINFO",
    "BASH_VERSION", "COMP_CWORD", "COMP_KEY", "COMP_LINE", "COMP_POINT",
    "COMP_TYPE", "COMP_WORDBREAKS", "COMP_WORDS", "COPROC", "DIRSTACK",
    "EPOCHREALTIME", "EPOCHSECONDS", "EUID", "FUNCNAME", "GROUPS", "HISTCMD",
    "HOSTNAME", "HOSTTYPE", "LINENO", "MACHTYPE", "MAPFILE", "OLDPWD",
    "OPTARG", "OPTIND", "OSTYPE", "PIPESTATUS", "PPID", "PWD", "RANDOM",
    "READLINE_ARGUMENT", "READLINE_LINE", "READLINE_MARK", "READLINE_POINT",
    "REPLY", "SECONDS", "SHELLOPTS", "SHLVL", "SRANDOM", "UID", "BASH_COMPAT",
    "BASH_ENV", "BASH_XTRACEFD", "CDPATH", "CHILD_MAX", "COLUMNS",
    "COMPREPLY", "EMACS", "ENV", "EXECIGNORE", "FCEDIT", "FIGNORE",
    "FUNCNEST", "GLOBIGNORE", "HISTCONTROL", "HISTFILE", "HISTFILESIZE",
    "HISTIGNORE", "HISTSIZE", "HISTTIMEFORMAT", "HOME", "HOSTFILE", "IFS",
    "IGNOREEOF", "INPUTRC", "INSIDE_EMACS", "LANG", "LC_ALL", "LC_COLLATE",
    "LC_CTYPE", "LC_MESSAGES", "LC_MONETARY", "LC_NUMERIC", "LC_TIME",
    "LINES", "MAIL", "MAILCHECK", "MAILPATH", "OPTERR", "PATH",
    "POSIXLY_CORRECT", "PROMPT_COMMAND", "PROMPT_DIRTRIM", "PS0", "PS1",
    "PS2", "PS3", "PS4", "SHELL", "TIMEFORMAT", "TMOUT", "TMPDIR",
    "auto_resume", "histchars",

    -- Other
    "USER", "TZ", "TERM", "LOGNAME", "LD_LIBRARY_PATH", "LANGUAGE", "DISPLAY",
    "HOSTNAME", "KRB5CCNAME", "XAUTHORITY"

    -- Ksh
    , ".sh.version"

    -- shflags
    , "FLAGS_ARGC", "FLAGS_ARGV", "FLAGS_ERROR", "FLAGS_FALSE", "FLAGS_HELP",
    "FLAGS_PARENT", "FLAGS_RESERVED", "FLAGS_TRUE", "FLAGS_VERSION",
    "flags_error", "flags_return"

    -- Bats
    ,"stderr", "stderr_lines"
  ]

specialIntegerVariables = [
    "$", "?", "!", "#"
  ]

specialVariablesWithoutSpaces = "-" : specialIntegerVariables

variablesWithoutSpaces = specialVariablesWithoutSpaces ++ [
    "BASHPID", "BASH_ARGC", "BASH_LINENO", "BASH_SUBSHELL", "EUID",
    "EPOCHREALTIME", "EPOCHSECONDS", "LINENO", "OPTIND", "PPID", "RANDOM",
    "READLINE_ARGUMENT", "READLINE_MARK", "READLINE_POINT", "SECONDS",
    "SHELLOPTS", "SHLVL", "SRANDOM", "UID", "COLUMNS", "HISTFILESIZE",
    "HISTSIZE", "LINES"

    -- shflags
    , "FLAGS_ERROR", "FLAGS_FALSE", "FLAGS_TRUE"
  ]

specialVariables = specialVariablesWithoutSpaces ++ ["@", "*"]

unbracedVariables = specialVariables ++ [
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
  ]

arrayVariables = [
    "BASH_ALIASES", "BASH_ARGC", "BASH_ARGV", "BASH_CMDS", "BASH_LINENO",
    "BASH_REMATCH", "BASH_SOURCE", "BASH_VERSINFO", "COMP_WORDS", "COPROC",
    "DIRSTACK", "FUNCNAME", "GROUPS", "MAPFILE", "PIPESTATUS", "COMPREPLY"
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
    "cp", "du", "echo", "export", "fg", "fuser", "getconf",
    "getopt", "getopts", "ipcrm", "ipcs", "jobs", "kill", "ln", "ls",
    "locale", "mv", "printf", "ps", "pwd", "renice", "rm", "rmdir",
    "set", "sleep", "touch", "trap", "ulimit", "unalias", "uname"
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

arithmeticBinaryTestOps = [
    "-eq", "-ne", "-lt", "-le", "-gt", "-ge"
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
        "busybox"  -> return BusyboxSh -- Used for directives and --shell=busybox
        "busybox sh"  -> return BusyboxSh
        "busybox ash"  -> return BusyboxSh
        "dash"  -> return Dash
        "ash"   -> return Dash -- There's also a warning for this.
        "ksh"   -> return Ksh
        "ksh88" -> return Ksh
        "ksh93" -> return Ksh
        _ -> Nothing

flagsForRead = "sreu:n:N:i:p:a:t:"
flagsForMapfile = "d:n:O:s:u:C:c:t"

declaringCommands = ["local", "declare", "export", "readonly", "typeset", "let"]
