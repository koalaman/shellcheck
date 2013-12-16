% SHELLCHECK(1) Shell script analysis tool

# NAME

shellcheck - Shell script analysis tool

# SYNOPSIS

**shellcheck** [*OPTIONS*...] *FILES*...

# DESCRIPTION

ShellCheck is a static analysis and linting tool for sh/bash scripts. It's
mainly focused on handling typical beginner and intermediate level syntax
errors and pitfalls where the shell just gives a cryptic error message or
strange behavior, but it also reports on a few more advanced issues where
corner cases can cause delayed failures.

# OPTIONS

**-f** *FORMAT*, **--format=***FORMAT*

:    Specify the output format of shellcheck, which prints its results in the
     standard output. Subsequent **-f** options are ignored, see **FORMATS**
     below for more information.

**-e**\ *CODE1*[,*CODE2*...],\ **--exclude=***CODE1*[,*CODE2*...]

:    Explicitly exclude the specified codes from the report. Subsequent **-e**
     options are cumulative, but all the codes can be specified at once,
     coma-separated as a single argument.

# FORMATS

**tty**

:    This is the default format, a plain text output. If your terminal handles
     colors, messages are printed with a different color depending on their
     types (error, warnings etc). You get a pretty terminal frontend.

**gcc**

:    This format outputs messages similar to those from gcc.

     `<file>:<line>:<column>: <type>: <message>`

**checkstyle**

:    Checkstyle is a development tool to help programmers write Java code that
     adheres to a coding standard. Checkstyle reports are XML files that can
     be integrated with a wide range of tools from text editors to continuous
     integration platforms.

```
<?xml version='1.0' encoding='UTF-8'?>
<checkstyle version='4.3'>
  <file name='file'>
    <error
      line='line'
      column='column'
      severity='severity'
      message='message'
      source='ShellCheck.SC####' />
    ...
  </file>
  ...
</checkstyle>
```

**json**

:    Json is a popular serialization format that is more suitable for web
     applications. ShellCheck's json is compact and contains only the strict
     minimum.

```
[
  {
    "line": line,
    "column": column,
    "level": level,
    "code": ####,
    "message": message
  },
  ...
]
```

# SEE ALSO

sh(1) bash(1)
