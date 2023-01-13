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

```
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

```
(make-instance 'clickhouse:database :host "clickhouse.example.com" :port "8123" :username "example" :password "1amAsecretPassWord")
```

Binding an instance of `database`.

```
(defparameter *db* (make-instance 'clickhouse:database :host "clickhouse.example.com" :port "8123" :username "example" :password "1amAsecretPassWord"))
```

Reading and setting a slot.

```
CL-USER> (clickhouse::password *db*)
"1amAsecretPassWord"
CL-USER> (setf (clickhouse::password *db*) "chang3m3plea5e")
"chang3m3plea5e"
CL-USER>
```
### Methods





