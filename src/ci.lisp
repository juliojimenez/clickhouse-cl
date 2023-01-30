(defpackage :clickhouse/ci
  (:use :cl)
  (:import-from :40ants-ci/workflow
                :defworkflow)
  (:import-from :40ants-ci/jobs/linter)
  (:import-from :40ants-ci/jobs/critic)
  (:import-from :40ants-ci/jobs/docs))
(in-package :clickhouse/ci)


(defworkflow linter
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter)))

(defworkflow critic
  :on-pull-request t
  :jobs ((40ants-ci/jobs/critic:critic)))

(defworkflow docs
  :on-push-to "main"
  ;; It is useful to build docs in pull requests, because
  ;; some documentation builders like 40ants-doc, have
  ;; a builtin linter which may suggest documentation
  ;; improvements:
  :on-pull-request t
  :jobs ((40ants-ci/jobs/docs:build-docs)))
