module ShellCheck.Data where

import Data.Version (showVersion)
import Paths_ShellCheck (version)

shellcheckVersion = showVersion version

internalVariables = [
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
    "LC_MESSAGES", "LC_NUMERIC", "LINES", "MAIL", "MAILCHECK", "MAILPATH",
    "OPTERR", "PATH", "POSIXLY_CORRECT", "PROMPT_COMMAND",
    "PROMPT_DIRTRIM", "PS1", "PS2", "PS3", "PS4", "SHELL", "TIMEFORMAT",
    "TMOUT", "TMPDIR", "auto_resume", "histchars", "COPROC",

    -- Other
    "USER", "TZ", "TERM"
  ]

variablesWithoutSpaces = [
    "$", "-", "?", "!",
    "BASHPID", "BASH_ARGC", "BASH_LINENO", "BASH_SUBSHELL", "EUID", "LINENO",
    "OPTIND", "PPID", "RANDOM", "SECONDS", "SHELLOPTS", "SHLVL", "UID",
    "COLUMNS", "HISTFILESIZE", "HISTSIZE", "LINES"
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

sampleWords = [
    "alpha", "bravo", "charlie", "delta", "echo", "foxtrot",
    "golf", "hotel", "india", "juliett", "kilo", "lima", "mike",
    "november", "oscar", "papa", "quebec", "romeo", "sierra",
    "tango", "uniform", "victor", "whiskey", "xray", "yankee",
    "zulu"
  ]
