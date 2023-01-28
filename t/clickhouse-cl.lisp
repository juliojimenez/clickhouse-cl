(defpackage :clickhouse-test
  (:use :cl :fiveam)
  (:import-from :clickhouse.utils
                :format-url
                :ver))

(in-package :clickhouse-test)

(def-suite clickhouse-cl :description "clickhouse-cl test suite.")

(in-suite clickhouse-cl)

; clickhouse.utils

(test format-url-non-ssl
  (is (string= "http://localhost:8123" (format-url "localhost" 8123 nil ""))))

(test format-url-ssl
  (is (string= "https://localhost:8123" (format-url "localhost" 8123 t ""))))

(test format-url-ssl-uri
  (is (string= "https://localhost:8443/ping" (format-url "localhost" 8443 t "/ping"))))

(test ver-t
  (is (equalp t (ver "okie dokie"))))

(test ver-nil
  (is (equalp nil (ver nil))))

(test prettify-nil
  (is (string= "1." (clickhouse.utils:prettify "1." :console nil))))

(test prettify-console
  (is (equalp nil (clickhouse.utils:prettify "1." :console t))))
