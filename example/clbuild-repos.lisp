(require :launchpad)

(defparameter project "clbuild-repos")

(launchpad:get-token-and-login project)

(launchpad:get-all-bugs project)

(launchpad:open-a-bug project "test" "just testing")

(launchpad:update-a-bug 483637 :title "modify test" :description "modify just testing")

