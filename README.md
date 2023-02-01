[![CI](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/ci.yml/badge.svg)](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/ci.yml) [![LINTER](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/linter.yml/badge.svg)](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/linter.yml) [![CRITIC](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/critic.yml/badge.svg)](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/critic.yml)

# clickhouse-cl

Common Lisp ClickHouse Client Library

- [Loading using QuickLisp](#loading-using-quicklisp)
- [No Line Breaks](#no-line-breaks-emacs)
- [`database` Class](#database-class)
  - [Slots](#slots)
  - [Usage](#usage)
  - [Methods](#methods)
    - [ping](#ping)
    - [replicas-status](#replicas-status)
    - [query](#query)
  - [Console Option](#console-option)
- [Formats](#formats)
  - [Functions](#functions)
    - [jget](#jget)
- [Common Forms](#common-forms)
  - [Connection to a local database](#connection-to-a-local-database)
  - [Query](#query)
- [To Do](#to-do)

## Loading using QuickLisp

Clone this repo wherever your quicklisp `local-projects` folder is configured.

```
~/quicklisp/local-projects/$ git clone https://github.com/juliojimenez/clickhouse-cl
~/quicklisp/local-projects/$ cd clickhouse-cl
~/quicklisp/local-projects/clickhouse-cl/$
```
Some dependencies live on the awesome [Ultralisp.org](https://ultralisp.org/) distribution, load it like this...

```lisp
CL-USER> (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
; Fetching #<URL "http://dist.ultralisp.org/">
...
#<QL-DIST:DIST ultralisp 20230130142500>
CL-USER> 
```

In the emacs SLIME REPL or SBCL (or however you Lisp :wink:, as long as you QuickLisp), load the library with...

```lisp
CL-USER> (ql:quickload :clickhouse)
To load "clickhouse":
  Load 1 ASDF system:
    clickhouse
; Loading "clickhouse"
[package clickhouse]

(:CLICKHOUSE)
CL-USER>
```

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
| password | y | | Database password |

### Usage

Creating a instance of `database`.

```lisp
(make-instance 'clickhouse:database :host "clickhouse.example.com" :port "8123" :username "example" :password "1amAsecretPassWord")
```

Binding an instance of `database`.

```lisp
(defparameter *db* (make-instance 'clickhouse:database :host "localhost" :port "8123" :ssl nil :username "default" :password "1amAsecretPassWord"))
```

Reading and setting a slot.

```lisp
CL-USER> (clickhouse::password *db*)
"1amAsecretPassWord"
CL-USER> (setf (clickhouse::password *db*) "chang3m3plea5e")
"chang3m3plea5e"
CL-USER>
```
### Methods

#### ping

clickhouse::ping *obj* :ping *bool* :console *bool*

```lisp
CL-USER> (clickhouse::ping *db*)
"Ok."
```

```lisp
CL-USER>  (clickhouse::ping *db* :ping t)
"Ok."
```

#### replicas-status

clickhouse::replicas-status *obj* :console *bool* 

```lisp
CL-USER> (clickhouse::replicas-status *db*)
"Ok."
```

#### query

clickhouse::query *obj* *query* :console *bool* :no-format *bool*

```lisp
CL-USER> (clickhouse::query *db* "SELECT 1")
"1"
```

### Console Option

All methods can take the keyword parameter `:console t`, providing a cleaner output when interacting directly with the library in the REPL.

```lisp
CL-USER> (clickhouse:query *db* "SHOW DATABASES")
"INFORMATION_SCHEMA
default
information_schema
system"
```

```lisp
CL-USER> (clickhouse:query *db* "SHOW DATABASES" :console t)
INFORMATION_SCHEMA
default
information_schema
letsgetitstarted
system
NIL
```

```lisp
CL-USER> (clickhouse:query *db* "SELECT trip_id, passenger_count, pickup_ntaname FROM trips LIMIT 10")
"1201746944	1	Upper West Side
1200864931	5	Midtown-Midtown South
1200018648	1	Airport
1201452450	5	East Village
1202368372	2	West Village
1201973571	2	Clinton
1200831168	1	Hudson Yards-Chelsea-Flatiron-Union Square
1201362116	1	Clinton
1203091619	1	Midtown-Midtown South
1200639419	1	Hudson Yards-Chelsea-Flatiron-Union Square"
```

```lisp
CL-USER> (clickhouse:query *db* "SELECT trip_id, passenger_count, pickup_ntaname FROM trips LIMIT 10" :console t)
1201746944	1	Upper West Side
1200864931	5	Midtown-Midtown South
1200018648	1	Airport
1201452450	5	East Village
1202368372	2	West Village
1201973571	2	Clinton
1200831168	1	Hudson Yards-Chelsea-Flatiron-Union Square
1201362116	1	Clinton
1203091619	1	Midtown-Midtown South
1200639419	1	Hudson Yards-Chelsea-Flatiron-Union Square
NIL
```

```lisp
CL-USER> (clickhouse:query *db* "SELECT trip_id, passenger_count, pickup_ntaname FROM trips LIMIT 10 FORMAT TabSeparatedWithName" :console t)
trip_id	passenger_count	pickup_ntaname
1201746944	1	Upper West Side
1200864931	5	Midtown-Midtown South
1200018648	1	Airport
1201452450	5	East Village
1202368372	2	West Village
1201973571	2	Clinton
1200831168	1	Hudson Yards-Chelsea-Flatiron-Union Square
1201362116	1	Clinton
1203091619	1	Midtown-Midtown South
1200639419	1	Hudson Yards-Chelsea-Flatiron-Union Square
NIL
```

## Formats

> ClickHouse can accept and return data in various formats. A format supported for input can be used to parse the data provided to INSERTs, to perform SELECTs from a file-backed table such as File, URL or HDFS, or to read a dictionary. A format supported for output can be used to arrange the results of a SELECT, and to perform INSERTs into a file-backed table. ([Formats](https://clickhouse.com/docs/en/interfaces/formats/))

clickhouse-cl supports automatic input and output format processing for the formats below. If such processing is not desired, the keyword parameter `:no-format t` is added to the [**query**](#query) method.

| Format | Input | Output | Functions |
| ------ | ----- | ------ | --------- |
| JSON || :heavy_check_mark: | jget *obj* *key* |

### Functions

#### jget

jget *obj* *key*

```lisp
CL-USER> (defparameter *db* (make-instance 'clickhouse:database))
*DB*
CL-USER> (defparameter *result* (clickhouse:query *db* "SELECT trip_id, passenger_count FROM trips LIMIT 10 FORMAT JSON"))
*RESULT*
CL-USER> *result*
#<BOOST-JSON:JSON-OBJECT {"meta":#,"data":#,"rows":10,"rows_before_limit_at_least":10,"statistics":#}>
CL-USER> (clickhouse:jget *result* "rows")
10
T
```

## Common Forms

### Connection to a local database

This would be applicable to a recently [installed](https://clickhouse.com/docs/en/getting-started/quick-start/) database, prior to applying a password and/or adding any users.

```lisp
(defparameter *db* (make-instance 'clickhouse:database))
```

### Query

```
(clickhouse::query *db* "SELECT 1")
```

## To Do

- [x] [HTTP Client](https://github.com/juliojimenez/clickhouse-cl/issues/9)
- [x] [SQL Generator](https://github.com/juliojimenez/clickhouse-cl/issues/10)
- [x] [Improve Output](https://github.com/juliojimenez/clickhouse-cl/issues/12)
- [x] [Set Up Tests](https://github.com/juliojimenez/clickhouse-cl/issues/17)
- [x] [JSON Parsing](https://github.com/juliojimenez/clickhouse-cl/issues/18)
- [x] []