module ShellCheck.Data where

import ShellCheck.AST (Annotation(ShellOverride))
import ShellCheck.Interface
import ShellCheck.Prelude (headOrDefault)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (listToMaybe)
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

    -- POSIX
    "CDPATH", "ENV", "FCEDIT", "HISTFILE", "HISTSIZE", "HOME", "IFS", "LANG",
    "LC_ALL", "LC_COLLATE", "LC_CTYPE", "LC_MESSAGES", "LC_MONETARY",
    "LC_NUMERIC", "LC_TIME", "MAIL", "MAILCHECK", "MAILPATH", "OLDPWD",
    "OPTARG", "OPTIND", "PATH", "PWD",

    -- Bash
    "BASH", "BASHOPTS", "BASHPID", "BASH_ALIASES", "BASH_ARGC",
    "BASH_ARGV", "BASH_ARGV0", "BASH_CMDS", "BASH_COMMAND",
    "BASH_EXECUTION_STRING", "BASH_LINENO", "BASH_LOADABLES_PATH",
    "BASH_REMATCH", "BASH_SOURCE", "BASH_SUBSHELL", "BASH_VERSINFO",
    "BASH_VERSION", "COMP_CWORD", "COMP_KEY", "COMP_LINE", "COMP_POINT",
    "COMP_TYPE", "COMP_WORDBREAKS", "COMP_WORDS", "COPROC", "DIRSTACK",
    "EPOCHREALTIME", "EPOCHSECONDS", "EUID", "FUNCNAME", "GROUPS", "HISTCMD",
    "HOSTNAME", "HOSTTYPE", "MACHTYPE", "MAPFILE", "OSTYPE", "PIPESTATUS",
    "RANDOM", "READLINE_ARGUMENT", "READLINE_LINE", "READLINE_MARK",
    "READLINE_POINT", "REPLY", "SECONDS", "SHELLOPTS", "SHLVL", "SRANDOM",
    "UID", "BASH_COMPAT", "BASH_ENV", "BASH_XTRACEFD", "CHILD_MAX", "COLUMNS",
    "COMPREPLY", "EMACS", "EXECIGNORE", "FIGNORE", "FUNCNEST", "GLOBIGNORE",
    "HISTCONTROL", "HISTFILESIZE", "HISTIGNORE", "HISTTIMEFORMAT", "HOSTFILE",
    "IGNOREEOF", "INPUTRC", "INSIDE_EMACS", "LINES", "OPTERR",
    "POSIXLY_CORRECT", "PROMPT_COMMAND", "PROMPT_DIRTRIM", "PS0", "PS1", "PS2",
    "PS3", "PS4", "SHELL", "TIMEFORMAT", "TMOUT", "BASH_MONOSECONDS",
    "BASH_TRAPSIG", "GLOBSORT", "auto_resume", "histchars",

    -- Other
    "USER", "TZ", "TERM", "LOGNAME", "LD_LIBRARY_PATH", "LANGUAGE", "DISPLAY",
    "HOSTNAME", "KRB5CCNAME", "LINENO", "PPID", "TMPDIR", "XAUTHORITY"

    -- Ksh
    , ".sh.version"

    -- shflags
    , "FLAGS_ARGC", "FLAGS_ARGV", "FLAGS_ERROR", "FLAGS_FALSE", "FLAGS_HELP",
    "FLAGS_PARENT", "FLAGS_RESERVED", "FLAGS_TRUE", "FLAGS_VERSION",
    "flags_error", "flags_return"
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
    "HISTSIZE", "LINES", "BASH_MONOSECONDS", "BASH_TRAPSIG"

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
    "export", "expr", "fc", "fg", "file", "find", "fold", "fuser",
    "gencat", "get", "getconf", "getopts", "gettext", "grep", "hash",
    "head", "iconv", "ipcrm", "ipcs", "jobs", "join", "kill", "lex",
    "link", "ln", "locale", "localedef", "logger", "logname", "lp",
    "ls", "m4", "mailx", "make", "man", "mesg", "mkdir", "mkfifo",
    "more", "msgfmt", "mv", "newgrp", "ngettext", "nice", "nl", "nm",
    "nohup", "od", "paste", "patch", "pathchk", "pax", "pr", "printf",
    "prs", "ps", "pwd", "read", "readlink", "readonly", "realpath",
    "renice", "return", "rm", "rmdel", "rmdir", "sact", "sccs", "sed",
    "set", "sh", "shift", "sleep", "sort", "split", "strings", "strip",
    "stty", "tabs", "tail", "talk", "tee", "test", "time", "timeout",
    "times", "touch", "tput", "tr", "trap", "tsort", "tty", "type",
    "ulimit", "umask", "unalias", "uname", "uncompress", "unexpand",
    "unget", "uniq", "unlink", "unset", "uucp", "uudecode", "uuencode",
    "uustat", "uux", "val", "vi", "wait", "wc", "what", "who", "write",
    "xargs", "xgettext", "yacc", "zcat"
  ]

nonReadingCommands = [
    "alias", "basename", "bg", "cal", "cd", "chgrp", "chmod", "chown",
    "cp", "du", "echo", "export", "fg", "fuser", "getconf",
    "getopt", "getopts", "ipcrm", "ipcs", "jobs", "kill", "ln", "ls",
    "locale", "mv", "printf", "ps", "pwd", "readlink", "realpath",
    "renice", "rm", "rmdir", "set", "sleep", "touch", "trap", "ulimit",
    "unalias", "uname"
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

supportedShellDialects = [
    "sh", "bash", "dash", "ksh", "busybox", "zsh"
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
        "oksh"  -> return Ksh
        "zsh"   -> return Zsh
        _ -> Nothing

shellFromAnnotations :: [Annotation] -> String -> Maybe Shell
shellFromAnnotations annotations executable =
    shellForExecutable $ headOrDefault executable [s | ShellOverride s <- annotations]

shellFromFilename :: String -> Maybe Shell
shellFromFilename filename = listToMaybe candidates
  where
    shellExtensions = [(".ksh", Ksh)
                      ,(".bash", Bash)
                      ,(".bats", Bash)
                      ,(".dash", Dash)
                      ,(".envrc", Bash)
                      ,(".zsh", Zsh)
                      ,(".zsh-theme", Zsh)
                      ,(".zshenv", Zsh)
                      ,(".zprofile", Zsh)
                      ,(".zshrc", Zsh)
                      ,(".zlogin", Zsh)
                      ,(".zlogout", Zsh)]
                      -- The `.sh` is too generic to determine the shell:
                      -- We fallback to Bash in this case and emit SC2148 if there is no shebang
    -- zsh startup files without a leading dot, as in /etc/zsh/zshrc
    zshStartupFiles = ["zshenv", "zprofile", "zshrc", "zlogin", "zlogout"]
    basename = reverse . takeWhile (/= '/') . reverse $ filename
    candidates =
        [sh | (ext,sh) <- shellExtensions, ext `isSuffixOf` filename]
        ++ [Zsh | basename `elem` zshStartupFiles]

-- zsh's completion system marks completion and autoloadable functions with
-- a first line of "#compdef ..." or "#autoload ...". Such files rarely have
-- a shebang or an extension, so the marker identifies them as zsh.
shellFromFirstLine :: String -> Maybe Shell
shellFromFirstLine script =
    if any isMarker ["#compdef", "#autoload"] then Just Zsh else Nothing
  where
    firstLine = takeWhile (/= '\n') script
    isMarker m = m `isPrefixOf` firstLine &&
        all (`elem` " \t\r") (take 1 $ drop (length m) firstLine)

-- zsh 5.9 parameters that zsh itself sets or reads, in addition to internalVariables.
-- Checked 26 Nov 2026 against zsh 5.9 (zsh -fc, all modules loaded) and zshparam(1),
-- zshmodules(1), zshmisc(1). Use only when the dialect is zsh.
zshInternalVariables = [
    -- Set by the shell itself (zsh -f; ${(k)parameters})
    "ARGC", "CPUTYPE", "EGID", "FPATH", "GID", "HISTCHARS", "HOST", "KEYBOARD_HACK",
    "KEYTIMEOUT", "LISTMAX", "MANPATH", "MODULE_PATH", "NULLCMD", "PROMPT", "PROMPT2",
    "PROMPT3", "PROMPT4", "PSVAR", "READNULLCMD", "SAVEHIST", "SPROMPT", "TIMEFMT",
    "TMPPREFIX", "TRY_BLOCK_ERROR", "TRY_BLOCK_INTERRUPT", "TTY", "TTYIDLE",
    "USERNAME", "VENDOR", "WATCH", "WORDCHARS", "ZSH_ARGZERO", "ZSH_EVAL_CONTEXT",
    "ZSH_EXECUTION_STRING", "ZSH_NAME", "ZSH_PATCHLEVEL", "ZSH_SUBSHELL",
    "ZSH_VERSION", "argv", "cdpath", "fignore", "fpath", "mailpath", "manpath",
    "module_path", "path", "pipestatus", "prompt", "psvar", "signals", "status",
    "watch", "zsh_eval_context",

    -- zsh/parameter module (autoloaded on first use)
    "aliases", "builtins", "commands", "dirstack", "dis_aliases", "dis_builtins",
    "dis_functions", "dis_functions_source", "dis_galiases", "dis_patchars",
    "dis_reswords", "dis_saliases", "funcfiletrace", "funcsourcetrace", "funcstack",
    "functions", "functions_source", "functrace", "galiases", "history",
    "historywords", "jobdirs", "jobstates", "jobtexts", "modules", "nameddirs",
    "options", "parameters", "patchars", "reswords", "saliases", "userdirs",
    "usergroups",

    -- Other modules: zle, zleparameter, termcap, terminfo, sched, datetime, watch, system,
    -- langinfo, mapfile, pcre, curses, complist, zftp, db/gdbm
    "keymaps", "widgets", "termcap", "terminfo", "zsh_scheduled_events", "epochtime",
    "LOGCHECK", "WATCHFMT", "errnos", "sysparams", "langinfo", "mapfile",
    "zle_bracketed_paste", "ZPCRE_OP", "ZCURSES_COLORS", "ZCURSES_COLOR_PAIRS",
    "zcurses_attrs", "zcurses_colors", "zcurses_keycodes", "zcurses_windows",
    "ZLS_COLORS", "ZLS_COLOURS", "LISTPROMPT", "MENUPROMPT", "MENUSCROLL",
    "MENUSELECT", "ZFTP_ACCOUNT", "ZFTP_CODE", "ZFTP_HOST", "ZFTP_IP", "ZFTP_PORT",
    "ZFTP_PREFS", "ZFTP_PWD", "ZFTP_REPLY", "ZFTP_SESSION", "ZFTP_SYSTEM",
    "ZFTP_TMOUT", "ZFTP_TYPE", "ZFTP_USER", "ZFTP_VERBOSE", "zgdbm_tied",

    -- Read by the shell when set, or set by matching (zshparam: Parameters Used By The Shell)
    "ERRNO", "ZSH_SCRIPT", "ARGV0", "BAUD", "CORRECT_IGNORE", "CORRECT_IGNORE_FILE",
    "DIRSTACKSIZE", "HISTORY_IGNORE", "MATCH", "MBEGIN", "MEND", "match", "mbegin",
    "mend", "POSTEDIT", "PROMPT_EOL_MARK", "REPORTMEMORY", "REPORTTIME", "reply",
    "RPROMPT", "RPS1", "RPROMPT2", "RPS2", "STTY", "TERMINFO", "TERMINFO_DIRS",
    "TMPSUFFIX", "ZBEEP", "ZDOTDIR", "zle_highlight", "ZLE_LINE_ABORTED",
    "ZLE_REMOVE_SUFFIX_CHARS", "ZLE_SPACE_SUFFIX_CHARS", "ZLE_RPROMPT_INDENT",

    -- Hook arrays (zshmisc: Hook Functions) and PERIOD
    "chpwd_functions", "periodic_functions", "precmd_functions", "preexec_functions",
    "zshaddhistory_functions", "zshexit_functions", "PERIOD"
  ] ++ zshZleVariables ++ zshCompletionVariables

-- Special only inside ZLE widget functions (zshzle(1), User-Defined Widgets).
-- Precedent: bash's READLINE_LINE is in internalVariables unconditionally.
zshZleVariables = [
    "BUFFER", "BUFFERLINES", "CONTEXT", "CURSOR", "CUTBUFFER", "HISTNO",
    "ISEARCHMATCH_ACTIVE", "ISEARCHMATCH_END", "ISEARCHMATCH_START", "KEYMAP", "KEYS",
    "KEYS_QUEUED_COUNT", "killring", "LASTABORTEDSEARCH", "LASTSEARCH", "LASTWIDGET",
    "LBUFFER", "MARK", "NUMERIC", "PENDING", "POSTDISPLAY", "PREBUFFER", "PREDISPLAY",
    "RBUFFER", "REGION_ACTIVE", "region_highlight", "registers", "SUFFIX_ACTIVE",
    "SUFFIX_END", "SUFFIX_START", "UNDO_CHANGE_NO", "UNDO_LIMIT_NO", "WIDGET",
    "WIDGETFUNC", "WIDGETSTYLE", "YANK_ACTIVE", "YANK_END", "YANK_START",
    "ZLE_RECURSIVE", "ZLE_STATE"
  ]

-- Special inside completion widgets (zshcompwid(1)), plus the variables the
-- completion system (compsys) sets for, or reads from, completion functions:
-- _arguments/_values set context state state_descr line opt_args val_args;
-- _main_complete sets curcontext; _dispatch sets service; compinit sets the _comp* globals.
-- Precedent: bash's COMP_WORDS and COMPREPLY are in internalVariables.
zshCompletionVariables = [
    -- completion widgets
    "CURRENT", "IPREFIX", "ISUFFIX", "PREFIX", "QIPREFIX", "QISUFFIX", "SUFFIX",
    "compstate", "words", "redirections",
    -- completion system
    "context", "state", "state_descr", "line", "opt_args", "val_args", "curcontext",
    "service", "compcontext", "_comp_caller_options", "_comp_priv_prefix",
    "compprefuncs", "comppostfuncs", "_compskip", "_comp_command", "_comp_command1",
    "_comp_command2", "_comps", "_services", "_patcomps", "_postpatcomps",
    "_compautos", "_comp_dumpfile", "_comp_options", "_comp_setup", "_lastcomp"
  ]

-- Globals created by functions shipped with zsh: `autoload -U colors && colors`
-- and `promptinit`. vcs_info sets vcs_info_msg_0_ .. vcs_info_msg_N_ (N from the
-- max-exports style, default 2): match the pattern vcs_info_msg_[0-9]+_ instead of listing.
zshBundledFunctionVariables = [
    "color", "colour", "fg", "fg_bold", "fg_no_bold", "bg", "bg_bold", "bg_no_bold",
    "reset_color", "bold_color", "prompt_theme", "prompt_themes", "prompt_newline"
  ]

-- Arrays (normal, not associative) among all zsh names above, from ${(t)name}.
-- Complete list: the existing arrayVariables is applied to bash only.
zshArrayVariables = [
    "argv", "cdpath", "fignore", "fpath", "mailpath", "manpath", "module_path", "path",
    "pipestatus", "psvar", "signals", "watch", "zsh_eval_context", "dirstack",
    "dis_patchars", "dis_reswords", "funcfiletrace", "funcsourcetrace", "funcstack",
    "functrace", "historywords", "patchars", "reswords", "keymaps",
    "zsh_scheduled_events", "epochtime", "errnos", "zle_bracketed_paste",
    "zcurses_attrs", "zcurses_colors", "zcurses_keycodes", "zcurses_windows",
    "zgdbm_tied", "match", "mbegin", "mend", "reply", "zle_highlight",
    "chpwd_functions", "periodic_functions", "precmd_functions", "preexec_functions",
    "zshaddhistory_functions", "zshexit_functions", "killring", "region_highlight",
    "words", "redirections", "context", "state", "state_descr", "line",
    "_comp_priv_prefix", "compprefuncs", "comppostfuncs", "_comp_options",
    "prompt_theme", "prompt_themes"
  ]

-- Associative arrays among the names above: their subscripts are string keys,
-- never arithmetic, e.g. compstate[insert]=1, functions[foo]=..., commands[ls].
zshAssociativeArrays = [
    "aliases", "builtins", "commands", "dis_aliases", "dis_builtins", "dis_functions",
    "dis_functions_source", "dis_galiases", "dis_saliases", "functions",
    "functions_source", "galiases", "history", "jobdirs", "jobstates", "jobtexts",
    "modules", "nameddirs", "options", "parameters", "saliases", "userdirs",
    "usergroups", "widgets", "termcap", "terminfo", "sysparams", "langinfo", "mapfile",
    "registers", "compstate", "opt_args", "val_args", "_comp_caller_options", "_comps",
    "_services", "_patcomps", "_postpatcomps", "_compautos", "_lastcomp", "color",
    "colour", "fg", "fg_bold", "fg_no_bold", "bg", "bg_bold", "bg_no_bold"
  ]

-- Integer or float zsh parameters (values never contain spaces), in addition to
-- variablesWithoutSpaces. Matters only under sh_word_split/emulate sh, since native
-- zsh does not split unquoted expansions.
zshVariablesWithoutSpaces = [
    "ARGC", "EGID", "FUNCNEST", "GID", "HISTCMD", "KEYTIMEOUT", "LISTMAX", "MAILCHECK",
    "SAVEHIST", "TRY_BLOCK_ERROR", "TRY_BLOCK_INTERRUPT", "TTYIDLE", "ZSH_SUBSHELL",
    "status", "LOGCHECK", "ZCURSES_COLORS", "ZCURSES_COLOR_PAIRS", "ZFTP_TMOUT",
    "ERRNO", "MBEGIN", "MEND", "ZLE_RPROMPT_INDENT", "BUFFERLINES", "CURSOR", "HISTNO",
    "ISEARCHMATCH_ACTIVE", "ISEARCHMATCH_END", "ISEARCHMATCH_START",
    "KEYS_QUEUED_COUNT", "MARK", "NUMERIC", "PENDING", "REGION_ACTIVE",
    "SUFFIX_ACTIVE", "SUFFIX_END", "SUFFIX_START", "UNDO_CHANGE_NO", "UNDO_LIMIT_NO",
    "YANK_ACTIVE", "YANK_END", "YANK_START", "ZLE_RECURSIVE", "CURRENT"
  ]

-- Prompt parameters: zsh expands them at prompt time (with PROMPT_SUBST, as
-- oh-my-zsh, prezto and most themes set), so $var inside their values are reads.
-- Extends specialReferences ("PS1".."PS4", "PROMPT_COMMAND") in getReferencedVariables.
zshPromptVariables = [
    "PS1", "PS2", "PS3", "PS4", "PROMPT", "PROMPT2", "PROMPT3", "PROMPT4", "prompt",
    "RPROMPT", "RPS1", "RPROMPT2", "RPS2", "SPROMPT"
  ]

flagsForRead = "sreu:n:N:i:p:a:t:"
flagsForMapfile = "d:n:O:s:u:C:c:t"

declaringCommands = ["local", "declare", "export", "readonly", "typeset", "let"]

privilegeElevationCommands = ["sudo", "doas", "run0"]
