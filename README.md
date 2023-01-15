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

## `database` Class

### Slots

| Name | Accessor | Default | Description |
| ---- | -------- | ------- | ----------- |
| host | y | | Database hostname |
| port | y | 8443 | Database port, i.e. 8443 or 8123 |
| ssl | y | t | SSL option, boolean, t or nil. |
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

clickhouse::ping obj :ping bool

```lisp
CL-USER> (clickhouse::ping *db*)
"Ok.
"
200
#<HASH-TABLE :TEST EQUAL :COUNT 6 {1004C22953}>
#<QURI.URI.HTTP:URI-HTTP http://localhost:8123>
NIL
```

```lisp
CL-USER>  (clickhouse::ping *db* :ping t)
"Ok.
"
200
#<HASH-TABLE :TEST EQUAL :COUNT 6 {1003529693}>
#<QURI.URI.HTTP:URI-HTTP http://localhost:8123/ping>
NIL
```

#### replicas-status

clickhouse::replicas-status obj

```lisp
CL-USER> (clickhouse::replicas-status *db*)
#(79 107 46 10)
200
#<HASH-TABLE :TEST EQUAL :COUNT 4 {10048F4503}>
#<QURI.URI.HTTP:URI-HTTP http://localhost:8123/replicas_status>
NIL
```

## Common Forms

### Connection to a local database

This would be applicable to a recently [installed](https://clickhouse.com/docs/en/getting-started/quick-start/) database, prior to applying a password and/or adding any users.

```lisp
(defparameter *db* (make-instance 'clickhouse:database))
```

## To

- [x] [HTTP Client](https://github.com/juliojimenez/clickhouse-cl/issues/9)
- [ ] [SQL Generator](https://github.com/juliojimenez/clickhouse-cl/issues/10)
- [ ] [Improve Output](https://github.com/juliojimenez/clickhouse-cl/issues/12)


