% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coerceDT.R
\name{coerceDT}
\alias{coerceDT}
\alias{coerceDT.default}
\alias{coerceDT.character}
\alias{coerceDT.data.table}
\title{Coerce \code{data.table}s}
\usage{
coerceDT(
  data,
  select = NULL,
  drop = NULL,
  required = NULL,
  forbidden = NULL,
  copy = TRUE,
  NAerror = TRUE,
  warn = TRUE,
  ...
)

\method{coerceDT}{default}(data, ..., copy)

\method{coerceDT}{character}(
  data,
  select = NULL,
  drop = NULL,
  required = NULL,
  forbidden = NULL,
  copy = FALSE,
  NAerror = TRUE,
  warn = TRUE,
  ...
)

\method{coerceDT}{data.table}(
  data,
  select = NULL,
  drop = NULL,
  required = NULL,
  forbidden = NULL,
  copy = TRUE,
  NAerror = TRUE,
  warn = TRUE
)
}
\arguments{
\item{data}{Any of the types supported by \code{\link[data.table:as.data.table]{data.table::as.data.table()}} OR
a single character string, in which case \code{\link[data.table:fread]{data.table::fread()}} is used to
read in \code{data}.}

\item{select}{Optional; if \code{NULL} (the default), all columns of \code{data} are
returned. If a \code{character} vector, the corresponding columns are returned. If
a \code{j}-like expression (see \code{\link[data.table:data.table]{data.table::data.table()}}), the result of that
expression will be returned. \emph{N.B.} for the \code{j}-like version: \code{required}
will be considered \emph{before} selection occurs, and thus any coercion to
relevant classes will also happen pre-selection.}

\item{drop}{Optional; if \code{NULL}, ignored. If a character or integer vector,
the corresponding columns \emph{if present} will be dropped. If \emph{not} present,
\code{coerceDT} will warn about a request to drop columns that don't exist.}

\item{required}{Optional; if \code{NULL} (the default), there are no required
columns. If a \code{character} vector, \code{coerceDT} will produce an error indicating
which columns are not present. If a named \code{list}, the names will be required
columns and the \code{list} entries will be used to coerce the corresponding
columns. If those entries are themselves characters, those will be assumed to
be the class to coerce to via an \code{as.Class} method; otherwise, they should be
single argument functions that will be used to transform the column.}

\item{forbidden}{Optional; if \code{NULL}, ignored. If a character vector,
\code{coerceDT} will error if any of those columns are present.}

\item{copy}{Logical; if \code{TRUE} (default), a new \code{data.table} is returned;
if \code{FALSE}, \code{data} \emph{may} be modified in place, but is not \emph{guaranteed} to be
so. For example, selecting a subset of columns creates a new \code{data.table}
reference.}

\item{NAerror}{Logical; if column coercion results in any \code{NA}s, is that an
error (default: yes)?}

\item{warn}{Logical; issue a warning e.g. when \code{drop}ping columns that are
not present (default: yes)?}

\item{...}{
  Arguments passed on to \code{\link[data.table:fread]{data.table::fread}}
  \describe{
    \item{\code{input}}{ A single character string. The value is inspected and deferred to either \code{file=} (if no \\n present), \code{text=} (if at least one \\n is present) or \code{cmd=} (if no \\n is present, at least one space is present, and it isn't a file name). Exactly one of \code{input=}, \code{file=}, \code{text=}, or \code{cmd=} should be used in the same call. }
    \item{\code{file}}{ File name in working directory, path to file (passed through \code{\link[base]{path.expand}} for convenience), or a URL starting http://, file://, etc. Compressed files with extension \file{.gz} and \file{.bz2} are supported if the \code{R.utils} package is installed. }
    \item{\code{text}}{ The input data itself as a character vector of one or more lines, for example as returned by \code{readLines()}. }
    \item{\code{cmd}}{ A shell command that pre-processes the file; e.g. \code{fread(cmd=paste("grep",word,"filename"))}. See Details. }
    \item{\code{sep}}{ The separator between columns. Defaults to the character in the set \code{[,\\t |;:]} that separates the sample of rows into the most number of lines with the same number of fields. Use \code{NULL} or \code{""} to specify no separator; i.e. each line a single character column like \code{base::readLines} does.}
    \item{\code{sep2}}{ The separator \emph{within} columns. A \code{list} column will be returned where each cell is a vector of values. This is much faster using less working memory than \code{strsplit} afterwards or similar techniques. For each column \code{sep2} can be different and is the first character in the same set above [\code{,\\t |;}], other than \code{sep}, that exists inside each field outside quoted regions in the sample. NB: \code{sep2} is not yet implemented. }
    \item{\code{nrows}}{ The maximum number of rows to read. Unlike \code{read.table}, you do not need to set this to an estimate of the number of rows in the file for better speed because that is already automatically determined by \code{fread} almost instantly using the large sample of lines. \code{nrows=0} returns the column names and typed empty columns determined by the large sample; useful for a dry run of a large file or to quickly check format consistency of a set of files before starting to read any of them. }
    \item{\code{header}}{ Does the first data line contain column names? Defaults according to whether every non-empty field on the first data line is type character. If so, or TRUE is supplied, any empty column names are given a default name. }
    \item{\code{na.strings}}{ A character vector of strings which are to be interpreted as \code{NA} values. By default, \code{",,"} for columns of all types, including type \code{character} is read as \code{NA} for consistency. \code{,"",} is unambiguous and read as an empty string. To read \code{,NA,} as \code{NA}, set \code{na.strings="NA"}. To read \code{,,} as blank string \code{""}, set \code{na.strings=NULL}. When they occur in the file, the strings in \code{na.strings} should not appear quoted since that is how the string literal \code{,"NA",} is distinguished from \code{,NA,}, for example, when \code{na.strings="NA"}. }
    \item{\code{stringsAsFactors}}{ Convert all or some character columns to factors? Acceptable inputs are \code{TRUE}, \code{FALSE}, or a decimal value between 0.0 and 1.0. For \code{stringsAsFactors = FALSE}, all string columns are stored as \code{character} vs. all stored as \code{factor} when \code{TRUE}. When \code{stringsAsFactors = p} for \code{0 <= p <= 1}, string columns \code{col} are stored as \code{factor} if \code{uniqueN(col)/nrow < p}. 
  }
    \item{\code{verbose}}{ Be chatty and report timings? }
    \item{\code{skip}}{ If 0 (default) start on the first line and from there finds the first row with a consistent number of columns. This automatically avoids irregular header information before the column names row. \code{skip>0} means ignore the first \code{skip} rows manually. \code{skip="string"} searches for \code{"string"} in the file (e.g. a substring of the column names row) and starts on that line (inspired by read.xls in package gdata). }
    \item{\code{colClasses}}{ As in \code{\link[utils:read.table]{utils::read.csv}}; i.e., an unnamed vector of types corresponding to the columns in the file, or a named vector specifying types for a subset of the columns by name. The default, \code{NULL} means types are inferred from the data in the file. Further, \code{data.table} supports a named \code{list} of vectors of column names \emph{or numbers} where the \code{list} names are the class names; see examples. The \code{list} form makes it easier to set a batch of columns to be a particular class. When column numbers are used in the \code{list} form, they refer to the column number in the file not the column number after \code{select} or \code{drop} has been applied.
    If type coercion results in an error, introduces \code{NA}s, or would result in loss of accuracy, the coercion attempt is aborted for that column with warning and the column's type is left unchanged. If you really desire data loss (e.g. reading \code{3.14} as \code{integer}) you have to truncate such columns afterwards yourself explicitly so that this is clear to future readers of your code.
  }
    \item{\code{integer64}}{ "integer64" (default) reads columns detected as containing integers larger than 2^31 as type \code{bit64::integer64}. Alternatively, \code{"double"|"numeric"} reads as \code{utils::read.csv} does; i.e., possibly with loss of precision and if so silently. Or, "character". }
    \item{\code{dec}}{ The decimal separator as in \code{utils::read.csv}. If not "." (default) then usually ",". See details. }
    \item{\code{col.names}}{ A vector of optional names for the variables (columns). The default is to use the header column if present or detected, or if not "V" followed by the column number. This is applied after \code{check.names} and before \code{key} and \code{index}. }
    \item{\code{check.names}}{default is \code{FALSE}. If \code{TRUE} then the names of the variables in the \code{data.table} are checked to ensure that they are syntactically valid variable names. If necessary they are adjusted (by \code{\link{make.names}}) so that they are, and also to ensure that there are no duplicates.}
    \item{\code{encoding}}{ default is \code{"unknown"}. Other possible options are \code{"UTF-8"} and \code{"Latin-1"}.  Note: it is not used to re-encode the input, rather enables handling of encoded strings in their native encoding. }
    \item{\code{quote}}{ By default (\code{"\\""}), if a field starts with a double quote, \code{fread} handles embedded quotes robustly as explained under \code{Details}. If it fails, then another attempt is made to read the field \emph{as is}, i.e., as if quotes are disabled. By setting \code{quote=""}, the field is always read as if quotes are disabled. It is not expected to ever need to pass anything other than \\"\\" to quote; i.e., to turn it off. }
    \item{\code{strip.white}}{ default is \code{TRUE}. Strips leading and trailing whitespaces of unquoted fields. If \code{FALSE}, only header trailing spaces are removed. }
    \item{\code{fill}}{logical (default is \code{FALSE}). If \code{TRUE} then in case the rows have unequal length, blank fields are implicitly filled.}
    \item{\code{blank.lines.skip}}{\code{logical}, default is \code{FALSE}. If \code{TRUE} blank lines in the input are ignored.}
    \item{\code{key}}{Character vector of one or more column names which is passed to \code{\link[data.table]{setkey}}. It may be a single comma separated string such as \code{key="x,y,z"}, or a vector of names such as \code{key=c("x","y","z")}. Only valid when argument \code{data.table=TRUE}. Where applicable, this should refer to column names given in \code{col.names}. }
    \item{\code{index}}{ Character vector or list of character vectors of one or more column names which is passed to \code{\link[data.table]{setindexv}}. As with \code{key}, comma-separated notation like \code{index="x,y,z"} is accepted for convenience. Only valid when argument \code{data.table=TRUE}. Where applicable, this should refer to column names given in \code{col.names}. }
    \item{\code{showProgress}}{ \code{TRUE} displays progress on the console if the ETA is greater than 3 seconds. It is produced in fread's C code where the very nice (but R level) txtProgressBar and tkProgressBar are not easily available. }
    \item{\code{data.table}}{ TRUE returns a \code{data.table}. FALSE returns a \code{data.frame}. The default for this argument can be changed with \code{options(datatable.fread.datatable=FALSE)}.}
    \item{\code{nThread}}{The number of threads to use. Experiment to see what works best for your data on your hardware.}
    \item{\code{logical01}}{If TRUE a column containing only 0s and 1s will be read as logical, otherwise as integer.}
    \item{\code{keepLeadingZeros}}{If TRUE a column containing numeric data with leading zeros will be read as character, otherwise leading zeros will be removed and converted to numeric.}
    \item{\code{yaml}}{ If \code{TRUE}, \code{fread} will attempt to parse (using \code{\link[yaml]{yaml.load}}) the top of the input as YAML, and further to glean parameters relevant to improving the performance of \code{fread} on the data itself. The entire YAML section is returned as parsed into a \code{list} in the \code{yaml_metadata} attribute. See \code{Details}. }
    \item{\code{autostart}}{ Deprecated and ignored with warning. Please use \code{skip} instead. }
    \item{\code{tmpdir}}{ Directory to use as the \code{tmpdir} argument for any \code{tempfile} calls, e.g. when the input is a URL or a shell command. The default is \code{tempdir()} which can be controlled by setting \code{TMPDIR} before starting the R session; see \code{\link[base:tempfile]{base::tempdir}}. }
    \item{\code{tz}}{ Relevant to datetime values which have no Z or UTC-offset at the end, i.e. \emph{unmarked} datetime, as written by \code{\link[utils:write.table]{utils::write.csv}}. The default \code{tz="UTC"} reads unmarked datetime as UTC POSIXct efficiently. \code{tz=""} reads unmarked datetime as type character (slowly) so that \code{as.POSIXct} can interpret (slowly) the character datetimes in local timezone; e.g. by using \code{"POSIXct"} in \code{colClasses=}. Note that \code{fwrite()} by default writes datetime in UTC including the final Z and therefore \code{fwrite}'s output will be read by \code{fread} consistently and quickly without needing to use \code{tz=} or \code{colClasses=}. If the \code{TZ} environment variable is set to \code{"UTC"} (or \code{""} on non-Windows where unset vs `""` is significant) then the R session's timezone is already UTC and \code{tz=""} will result in unmarked datetimes being read as UTC POSIXct. For more information, please see the news items from v1.13.0 and v1.14.0. }
  }}
}
\value{
A \code{data.table}; the returned object will be a copy (default), unless
\code{copy = FALSE}, in which case modifications \emph{may} be made in-place, though
are not \emph{guaranteed} to have been so
}
\description{
Provides consistent coercion of \code{data} to \link{data.table}
with error handling.
}
\details{
This function provides a general-purpose tool for common, basic
checking and conversion tasks with \code{data.table}s. It's intended use is as
a simplifying, standardizing interface for raw input checking, not to perform
complex requirement checks or manipulations. It is not, e.g., able to answer
if one-and-only-one of some set of columns are present, or to coerce column
values to a new values based on anything other than their initial value.

This method is implemented as an S3 generic method, and dispatches according
to \code{class(data)}. The class-specific methods may be used directly; \emph{however},
that will skip some input checking, namely:
\itemize{
\item that \code{select} / \code{drop} are mutually exclusive
\item that \code{copy} must be a length 1 logical
\item that \code{required} / \code{forbidden} have the correct format
}
}
