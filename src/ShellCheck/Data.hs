module ShellCheck.Data where

import qualified Data.Map
import ShellCheck.Interface
import ShellCheck.PortageAutoInternalVariables
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


genericInternalVariables = [
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

    -- shflags
    , "FLAGS_ARGC", "FLAGS_ARGV", "FLAGS_ERROR", "FLAGS_FALSE", "FLAGS_HELP",
    "FLAGS_PARENT", "FLAGS_RESERVED", "FLAGS_TRUE", "FLAGS_VERSION",
    "flags_error", "flags_return"
  ]

kshInternalVariables = [
    ".sh.version"
  ]

portageManualInternalVariables = [
    -- toolchain settings
    "CFLAGS", "CXXFLAGS", "CPPFLAGS", "LDFLAGS", "FFLAGS", "FCFLAGS",
    "CBUILD", "CHOST", "MAKEOPTS"
    -- TODO: Delete these if we can handle `tc-export CC` implicit export.
    , "CC", "CPP", "CXX"

    -- portage internals
    , "EBUILD_PHASE", "EBUILD_SH_ARGS", "EMERGE_FROM", "FILESDIR",
    "MERGE_TYPE", "PM_EBUILD_HOOK_DIR", "PORTAGE_ACTUAL_DISTDIR",
    "PORTAGE_ARCHLIST", "PORTAGE_BASHRC", "PORTAGE_BINPKG_FILE",
    "PORTAGE_BINPKG_TAR_OPTS", "PORTAGE_BINPKG_TMPFILE", "PORTAGE_BIN_PATH",
    "PORTAGE_BUILDDIR", "PORTAGE_BUILD_GROUP", "PORTAGE_BUILD_USER",
    "PORTAGE_BUNZIP2_COMMAND", "PORTAGE_BZIP2_COMMAND", "PORTAGE_COLORMAP",
    "PORTAGE_CONFIGROOT", "PORTAGE_DEBUG", "PORTAGE_DEPCACHEDIR",
    "PORTAGE_EBUILD_EXIT_FILE", "PORTAGE_ECLASS_LOCATIONS", "PORTAGE_GID",
    "PORTAGE_GRPNAME", "PORTAGE_INST_GID", "PORTAGE_INST_UID",
    "PORTAGE_INTERNAL_CALLER", "PORTAGE_IPC_DAEMON", "PORTAGE_IUSE",
    "PORTAGE_LOG_FILE", "PORTAGE_MUTABLE_FILTERED_VARS",
    "PORTAGE_OVERRIDE_EPREFIX", "PORTAGE_PYM_PATH", "PORTAGE_PYTHON",
    "PORTAGE_PYTHONPATH", "PORTAGE_READONLY_METADATA", "PORTAGE_READONLY_VARS",
    "PORTAGE_REPO_NAME", "PORTAGE_REPOSITORIES", "PORTAGE_RESTRICT",
    "PORTAGE_SAVED_READONLY_VARS", "PORTAGE_SIGPIPE_STATUS", "PORTAGE_TMPDIR",
    "PORTAGE_UPDATE_ENV", "PORTAGE_USERNAME", "PORTAGE_VERBOSE",
    "PORTAGE_WORKDIR_MODE", "PORTAGE_XATTR_EXCLUDE", "REPLACING_VERSIONS",
    "REPLACED_BY_VERSION", "__PORTAGE_HELPER", "__PORTAGE_TEST_HARDLINK_LOCKS",

    -- generic ebuilds
    "A", "ARCH", "BDEPEND", "BOARD_USE", "BROOT", "CATEGORY", "D",
    "DEFINED_PHASES", "DEPEND", "DESCRIPTION", "DISTDIR", "DOCS", "EAPI",
    "ECLASS", "ED", "EPREFIX", "EROOT", "ESYSROOT", "EXTRA_ECONF",
    "EXTRA_EINSTALL", "EXTRA_MAKE", "FEATURES", "FILESDIR", "HOME", "HOMEPAGE",
    "HTML_DOCS", "INHERITED", "IUSE", "KEYWORDS", "LICENSE", "P", "PATCHES",
    "PDEPEND", "PF", "PKG_INSTALL_MASK", "PKGUSE", "PN", "PR", "PROPERTIES",
    "PROVIDES_EXCLUDE", "PV", "PVR", "QA_AM_MAINTAINER_MODE",
    "QA_CONFIGURE_OPTIONS", "QA_DESKTOP_FILE", "QA_DT_NEEDED", "QA_EXECSTACK",
    "QA_FLAGS_IGNORED", "QA_MULTILIB_PATHS", "QA_PREBUILT", "QA_PRESTRIPPED",
    "QA_SONAME", "QA_SONAME_NO_SYMLINK", "QA_TEXTRELS", "QA_WX_LOAD", "RDEPEND",
    "REPOSITORY", "REQUIRED_USE", "REQUIRES_EXCLUDE", "RESTRICT", "ROOT", "S",
    "SLOT", "SRC_TEST", "SRC_URI", "STRIP_MASK", "SUBSLOT", "SYSROOT", "T",
    "WORKDIR",

    -- autotest.eclass declared incorrectly
    "AUTOTEST_CLIENT_TESTS", "AUTOTEST_CLIENT_SITE_TESTS",
    "AUTOTEST_SERVER_TESTS", "AUTOTEST_SERVER_SITE_TESTS", "AUTOTEST_CONFIG",
    "AUTOTEST_DEPS", "AUTOTEST_PROFILERS", "AUTOTEST_CONFIG_LIST",
    "AUTOTEST_DEPS_LIST", "AUTOTEST_PROFILERS_LIST",

    -- cros-board.eclass declared incorrectly
    "CROS_BOARDS",

    -- Undeclared cros-kernel2 vars
    "AFDO_PROFILE_VERSION",

    -- haskell-cabal.eclass declared incorrectly
    "CABAL_FEATURES",

    -- Undeclared haskell-cabal.eclass vars
    "CABAL_CORE_LIB_GHC_PV",

    -- Undeclared readme.gentoo.eclass vars
    "DOC_CONTENTS",

    -- Backwards compatibility perl-module.eclass vars
    "MODULE_AUTHOR", "MODULE_VERSION",

    -- Undeclared perl-module.eclass vars
    "mydoc",

    -- python-utils-r1.eclass declared incorrectly
    "RESTRICT_PYTHON_ABIS", "PYTHON_MODNAME",

    -- ABI variables
    "ABI", "DEFAULT_ABI",

    -- AFDO variables
    "AFDO_LOCATION",

    -- Linguas
    "LINGUAS"
  ]

eclassVarsFromMap :: String -> [String]
eclassVarsFromMap eclass =
    Data.Map.findWithDefault []
                             eclass
                             portageAutoInternalVariables

portageInternalVariables inheritedEclasses =
    portageManualInternalVariables ++ concatMap eclassVarsFromMap
                                                inheritedEclasses

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

portageBuildFlagVariables = [
    "CFLAGS", "CXXFLAGS", "CPPFLAGS", "LDFLAGS"
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
        "dash"  -> return Dash
        "ash"   -> return Dash -- There's also a warning for this.
        "ksh"   -> return Ksh
        "ksh88" -> return Ksh
        "ksh93" -> return Ksh
        _ -> Nothing

flagsForRead = "sreu:n:N:i:p:a:t:"
flagsForMapfile = "d:n:O:s:u:C:c:t"

declaringCommands = ["local", "declare", "export", "readonly", "typeset", "let"]
