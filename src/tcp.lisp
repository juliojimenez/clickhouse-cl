(defpackage :clickhouse.tcp
  (:use :cl)
  (:import-from :usocket
                :socket-connect))
  
(in-package :clickhouse.tcp)

(defclass native (database)
  ())


