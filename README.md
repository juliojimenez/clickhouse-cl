[![CI](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/ci.yml/badge.svg)](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/ci.yml) [![LINTER](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/linter.yml/badge.svg)](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/linter.yml) [![CRITIC](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/critic.yml/badge.svg)](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/critic.yml) [![Release](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/release.yml/badge.svg)](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/release.yml)

<img src="logo.png"  width="64" height="64">

# clickhouse-cl

Common Lisp ClickHouse Client Library

- [Install](#install)
  - [Ultralisp.org](#ultralisporg)
  - [git clone](#git-clone)
  - [Releases](#releases)
- [No Line Breaks](#no-line-breaks-emacs)
- [`database` Class](#database-class)
  - [Slots](#slots)
  - [Usage](#usage)
  - [Methods](#methods)
    - [ping](#ping)
    - [replicas-status](#replicas-status)
    - [query](#query)
  - [Console Option](#console-option)
  - [Timeouts](#timeouts)
- [Formats](#formats)
  - [Functions](#functions)
    - [jget](#jget)
- [Input Parameters](#input-parameters)
- [Examples](#examples)
  - [Connection to a local database](#connection-to-a-local-database)
  - [Query](#query)
  - [Connecting to ClickHouse Cloud](#connecting-to-clickhouse-cloud)
- [Bugs, Features, and Vulnerabilities Reporting](#bugs-features-and-vulnerabilities-reporting)

## Install

### Ultralisp.org

clickhouse-cl is on [Ultralisp.org](https://ultralisp.org)!

```
> (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
...
> (ql:quickload :clickhouse)
...
```

### git clone

Clone this repo wherever your quicklisp `local-projects` folder is configured.

```
~/quicklisp/local-projects/$ git clone https://github.com/juliojimenez/clickhouse-cl
~/quicklisp/local-projects/$ cd clickhouse-cl
~/quicklisp/local-projects/clickhouse-cl/$
```
Some dependencies are on [Ultralisp.org](https://ultralisp.org/), make sure you have it...

```lisp
> (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
...
```

In the emacs SLIME REPL or SBCL, load clickhouse-cl with...

```lisp
> (ql:quickload :clickhouse)
To load "clickhouse":
  Load 1 ASDF system:
    clickhouse
; Loading "clickhouse"
[package clickhouse]

(:CLICKHOUSE)
```

### Releases

You can also download a [release](https://github.com/juliojimenez/clickhouse-cl/releases), extract it into your `local-projects`, and follow the same steps above (minus the git clone, of course).

## No Line Breaks (Emacs)

To prevent line breaks, which makes query outputs with many fields difficult to read, issue the command `M-x toggle-truncate-lines RET` in the Emacs minibuffer.

If that doesn't work, checkout this [StackExchange](https://superuser.com/questions/592154/how-can-i-turn-off-emacss-auto-line-wrapping-for-the-current-session) post for other options.

## `database` Class

### Slots

| Name | Accessor | Default | Description |
| ---- | -------- | ------- | ----------- |
| host | y | localhost | Database hostname |
| port | y | 8123 | Database port, i.e. 8443 or 8123 |
| ssl | y | nil | SSL option, boolean, t or nil. |
| username | y | default | Database username |
| password | y | nil | Database password |

### Usage

Creating a instance of `database`.

```lisp
(make-instance 'clickhouse:database :host "clickhouse.example.com" :port "8123" :username "example" :password "1amAsecretPassWord")
```

The clickhouse-cl package nickname is `ch` and will be used throughout this README for brevity.

Binding an instance of `database`.

```lisp
(defparameter *db* (make-instance 'ch:database :host "localhost" :port "8123" :ssl nil :username "default" :password "1amAsecretPassWord"))
```

Reading and setting a slot.

```lisp
> (ch::password *db*)
"1amAsecretPassWord"
> (setf (ch::password *db*) "chang3m3plea5e")
"chang3m3plea5e"
```
### Methods

#### ping

ch:ping *obj* :ping *bool* :console *bool*

```lisp
> (ch::ping *db*)
"Ok."
```

The `:ping t` keyword parameter explicitly calls the instance `/ping` endpoint.

```lisp
> (ch::ping *db* :ping t)
"Ok."
```

#### replicas-status

ch:replicas-status *obj* :console *bool* :verbose *bool*

```lisp
> (ch::replicas-status *db*)
"Ok."
```

#### query

ch:query *obj* *query* :console *bool* :no-format *bool* :timeout *int*

```lisp
> (ch::query *db* "SELECT 1")
"1"
```

### Console Option

All methods can take the keyword parameter `:console t`, providing a cleaner output when interacting directly with the library in the REPL.

```lisp
> (ch:query *db* "SHOW DATABASES")
"INFORMATION_SCHEMA
default
information_schema
system"
```

```lisp
> (ch:query *db* "SHOW DATABASES" :console t)
INFORMATION_SCHEMA
default
information_schema
letsgetitstarted
system
NIL
```

### Timeouts

The default *query* method timeout is 60 seconds. Use the `:timeout seconds` keyword parameter to change the default for long running operations.

```lisp
(ch:query *db* "INSERT INTO crypto_prices 
                    SELECT 
                        trade_date,
                        crypto_name,
                        volume,
                        price,
                        market_cap,
                        change_1_day
                    FROM s3('https://learn-clickhouse.s3.us-east-2.amazonaws.com/crypto_prices.csv',
                            'CSVWithNames'
                     )
                    SETTINGS input_format_try_infer_integers=0" :timeout 300)
```

## Formats

> ClickHouse can accept and return data in various formats. A format supported for input can be used to parse the data provided to INSERTs, to perform SELECTs from a file-backed table such as File, URL or HDFS, or to read a dictionary. A format supported for output can be used to arrange the results of a SELECT, and to perform INSERTs into a file-backed table. ([Formats](https://clickhouse.com/docs/en/interfaces/formats/))

clickhouse-cl supports automatic input and output format processing for the formats below. If such processing is not desired, the keyword parameter `:no-format t` is added to the [**query**](#query) method.

| Format | Input | Output | Result | Functions |
| ------ | ----- | ------ | ------ | --------- |
| [TabSeparated](https://clickhouse.com/docs/en/interfaces/formats/#tabseparated) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| [TabSeparatedRaw](https://clickhouse.com/docs/en/interfaces/formats/#tabseparatedraw) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| [TabSeparatedWithNames](https://clickhouse.com/docs/en/interfaces/formats/#tabseparatedwithnames) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| [TabSeparatedWithNamesAndTypes](https://clickhouse.com/docs/en/interfaces/formats/#tabseparatedwithnamesandtypes) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| [TabSeparatedRawWithNames](https://clickhouse.com/docs/en/interfaces/formats/#tabseparatedrawwithnames) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| [TabSeparatedRawWithNamesAndTypes](https://clickhouse.com/docs/en/interfaces/formats/#tabseparatedrawwithnamesandtypes) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| [CSV](https://clickhouse.com/docs/en/interfaces/formats/#csv) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| [CSVWithNames](https://clickhouse.com/docs/en/interfaces/formats/#csvwithnames) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| [CSVWithNamesAndTypes](https://clickhouse.com/docs/en/interfaces/formats/#csvwithnamesandtypes) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| [SQLInsert](https://clickhouse.com/docs/en/interfaces/formats/#sqlinsert) || :heavy_check_mark: | string ||
| [JSON](https://clickhouse.com/docs/en/interfaces/formats/#json) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONAsString](https://clickhouse.com/docs/en/interfaces/formats/#jsonasstring) | :heavy_check_mark: ||||
| [JSONStrings](https://clickhouse.com/docs/en/interfaces/formats/#jsonstrings) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONColumns](https://clickhouse.com/docs/en/interfaces/formats/#jsoncolumns) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONColumnsWithMetadata](https://clickhouse.com/docs/en/interfaces/formats/#jsoncolumnswithmetadata) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONCompact](https://clickhouse.com/docs/en/interfaces/formats/#jsoncompact) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONCompactStrings](https://clickhouse.com/docs/en/interfaces/formats/#jsoncompactstrings) || :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONCompactColumns](https://clickhouse.com/docs/en/interfaces/formats/#jsoncompactcolumns) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONEachRow](https://clickhouse.com/docs/en/interfaces/formats/#jsoneachrow) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONEachRowWithProgress](https://clickhouse.com/docs/en/interfaces/formats/#jsoneachrowwithprogress) || :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONStringsEachRow](https://clickhouse.com/docs/en/interfaces/formats/#jsonstringseachrow) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONStringsEachRowWithProgress](https://clickhouse.com/docs/en/interfaces/formats/#jsonstringseachrowwithprogress) || :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONCompactEachRow](https://clickhouse.com/docs/en/interfaces/formats/#jsoncompacteachrow) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONCompactEachRowWithNames](https://clickhouse.com/docs/en/interfaces/formats/#jsoncompacteachrowwithnames) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONCompactEachRowWithNamesAndTypes](https://clickhouse.com/docs/en/interfaces/formats/#jsoncompacteachrowwithnamesandtypes) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONCompactStringsEachRow](https://clickhouse.com/docs/en/interfaces/formats/#jsoncompactstringseachrow) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONCompactStringsEachRowWithNames](https://clickhouse.com/docs/en/interfaces/formats/#jsoncompactstringseachrowwithnames) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONCompactStringsEachRowWithNamesAndTypes](https://clickhouse.com/docs/en/interfaces/formats/#jsoncompactstringseachrowwithnamesandtypes) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [JSONObjectEachRow](https://clickhouse.com/docs/en/interfaces/formats/#jsonobjecteachrow) | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| [TSKV](https://clickhouse.com/docs/en/interfaces/formats#tskv) | :heavy_check_mark: | :heavy_check_mark: | '('('(k . v)*)*) ||
| [Pretty](https://clickhouse.com/docs/en/interfaces/formats/#pretty) || :heavy_check_mark: || Best viewed with `:console t` |
| [PrettyNoEscapes](https://clickhouse.com/docs/en/interfaces/formats#prettynoescapes) || :heavy_check_mark: || Best viewed with `:console t` |
| [PrettyMonoBlock](https://clickhouse.com/docs/en/interfaces/formats#prettymonoblock) || :heavy_check_mark: || Best viewed with `:console t` |
| [PrettyNoEscapesMonoBlock](https://clickhouse.com/docs/en/interfaces/formats#prettynoescapesmonoblock) || :heavy_check_mark: || Best viewed with `:console t` |
| [PrettyCompact](https://clickhouse.com/docs/en/interfaces/formats#prettycompact) || :heavy_check_mark: || Best viewed with `:console t` |
| [PrettyCompactNoEscapes](https://clickhouse.com/docs/en/interfaces/formats/#prettycompactnoescapes) || :heavy_check_mark: || Best viewed with `:console t` |
| [PrettyCompactMonoBlock](https://clickhouse.com/docs/en/interfaces/formats/#prettycompactmonoblock) || :heavy_check_mark: || Best viewed with `:console t` |
| [PrettyCompactNoEscapesMonoBlock](https://clickhouse.com/docs/en/interfaces/formats/#prettycompactnoescapesmonoblock) || :heavy_check_mark: || Best viewed with `:console t` |
| [PrettySpace](https://clickhouse.com/docs/en/interfaces/formats/#prettyspace) || :heavy_check_mark: || Best viewed with `:console t` |
| [PrettySpaceNoEscapes](https://clickhouse.com/docs/en/interfaces/formats/#prettyspacenoescapes) || :heavy_check_mark: || Best viewed with `:console t` |
| [PrettySpaceMonoBlock](https://clickhouse.com/docs/en/interfaces/formats/#prettyspacemonoblock) || :heavy_check_mark: || Best viewed with `:console t` |
| [PrettySpaceNoEscapesMonoBlock](https://clickhouse.com/docs/en/interfaces/formats/#prettyspacenoescapesmonoblock) || :heavy_check_mark: || Best viewed with `:console t` |
| [LineAsString](https://clickhouse.com/docs/en/interfaces/formats/#lineasstring) | :heavy_check_mark: ||||
| [Markdown](https://clickhouse.com/docs/en/interfaces/formats/#markdown) || :heavy_check_mark: || Best viewed with `:console t` |

### Functions

#### jget

Helper function used to access key values in formats that result in a `BOOST-JSON:JSON-OBJECT`.

ch:jget *obj* *key*

```lisp
> (defparameter *db* (make-instance 'ch:database))
*DB*
> (defparameter *result* (ch:query *db* "SELECT trip_id, passenger_count FROM trips LIMIT 10 FORMAT JSON"))
*RESULT*
> *result*
#<BOOST-JSON:JSON-OBJECT {"meta":#,"data":#,"rows":10,"rows_before_limit_at_least":10,"statistics":#}>
> (ch:jget *result* "rows")
10
T
```

## Input Parameters

This feature is an oversimplification of input parameters as seen in clickhouse-client.

To interpolate inputs into a query, use the function `input-parameters` with the input marker `$i`.

ch:input-parameters query &rest input

```
 (ch:query *db* (ch:input-parameters "SELECT $i" "1") :console t)
```

## Examples

### Connecting to a local database

This would be applicable to a recently [installed](https://clickhouse.com/docs/en/getting-started/quick-start/) database, prior to applying a password and/or adding any users.

```lisp
(defparameter *db* (make-instance 'ch:database))
```

### Query

```
(ch:query *db* "SELECT 1")
```

### Connecting to ClickHouse Cloud

This example connects to a [ClickHouse Cloud](https://clickhouse.com/cloud) database loaded with the [NYC Taxi](https://clickhouse.com/docs/en/getting-started/example-datasets/nyc-taxi/) dataset.

```
> (ql:quickload :clickhouse)
> (defparameter *db* (make-instance 'clickhouse:database
				                      :host "iqr3flp7yf.us-east-1.aws.clickhouse.cloud"
				                      :port 8443
				                      :ssl t
				                      :username "default"
				                      :password ")UwB2oL|QQpi"))
> (ch:query *db* "SELECT count()
                  FROM nyc_taxi 
                  FORMAT PrettySpaceNoEscapes" :console t)

  count()

 20000000
NIL
> (ch:query *db* "SELECT 
                    trip_id,
                    total_amount,
                    trip_distance
                  FROM nyc_taxi
                  LIMIT 5 
                  FORMAT PrettySpaceNoEscapes" :console t)

    trip_id   total_amount   trip_distance

 1199999902          19.56            2.59 
 1199999919           10.3             2.4 
 1199999944           24.3            5.13 
 1199999969           9.95             1.2 
 1199999990            9.8            2.17 
NIL
```

# Bugs, Features, and Vulnerabilities Reporting

To report bugs, request a feature, or report a security vulnerability, please submit a new [issue](https://github.com/juliojimenez/clickhouse-cl/issues/new/choose).
