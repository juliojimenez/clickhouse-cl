(defpackage :clickhouse.ci
  (:use :cl)
  (:import-from :40ants-ci/workflow
                :defworkflow)
  (:import-from :40ants-ci/jobs/linter)
  (:import-from :40ants-ci/jobs/critic)
  (:import-from :40ants-ci/jobs/docs))

(defworkflow linter
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter)))

(defworkflow critic
  :on-pull-request t
  :jobs ((40ants-ci/jobs/critic:critic)))

(defworkflow docs
  :on-push-to "main"
  :jobs ((40ants-ci/jobs/docs:build-docs)))
