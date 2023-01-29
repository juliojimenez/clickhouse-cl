[![CI](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/ci.yml/badge.svg)](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/ci.yml)

# clickhouse-cl

Common Lisp ClickHouse Client Library

## Loading using QuickLisp

Clone this repo wherever your quicklisp `local-projects` folder is configured.

```
~/quicklisp/local-projects/$ git clone https://github.com/juliojimenez/clickhouse-cl
~/quicklisp/local-projects/$ cd clickhouse-cl
~/quicklisp/local-projects/clickhouse-cl/$
```

In the emacs SLIME REPL or SBCL (or however you Lisp :wink:, as long as you QuickLisp), load the library with...

```lisp
CL-USER> (ql:quickload :clickhouse-cl)
To load "clickhouse-cl":
  Load 1 ASDF system:
    clickhouse-cl
; Loading "clickhouse-cl"
[package clickhouse]

(:CLICKHOUSE-CL)
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

clickhouse::ping *obj* :ping *bool*

```lisp
CL-USER> (clickhouse::ping *db*)
"Ok."
```

```lisp
CL-USER>  (clickhouse::ping *db* :ping t)
"Ok."
```

#### replicas-status

clickhouse::replicas-status *obj*

```lisp
CL-USER> (clickhouse::replicas-status *db*)
"Ok."
```

#### query

clickhouse::query *obj* *query*

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
- [ ] [JSONEachRow Parsing](https://github.com/juliojimenez/clickhouse-cl/issues/18)