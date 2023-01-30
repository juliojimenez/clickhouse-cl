(defpackage :clickhouse-cl/ci
  (:use :cl)
  (:import-from :40ants-ci/workflow
                :defworkflow)
  (:import-from :40ants-ci/jobs/linter)
  (:import-from :40ants-ci/jobs/critic)
  (:import-from :40ants-ci/jobs/docs))
(in-package :clickhouse-cl/ci)


(defvar *asdf-system* "clickhouse-cl")

(defworkflow linter
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter
          :asdf-systems *asdf-system*)))

(defworkflow critic
  :on-pull-request t
  :jobs ((40ants-ci/jobs/critic:critic
          :asdf-systems *asdf-system*)))

(defworkflow docs
  :on-push-to "main"
  ;; It is useful to build docs in pull requests, because
  ;; some documentation builders like 40ants-doc, have
  ;; a builtin linter which may suggest documentation
  ;; improvements:
  :on-pull-request t
  :jobs ((40ants-ci/jobs/docs:build-docs
          :asdf-system *asdf-system*)))
