(require :launchpad)

(defparameter project "clbuild-repos")

(defparameter bug-id 1)

(launchpad:get-token-and-login project)

(launchpad:get-all-bugs project)

(setf bug-id (launchpad:open-a-bug project "test" "just testing"))

(launchpad:update-a-bug bug-id :title "modify test" :description "modify just testing")

(launchpad:get-a-bug bug-id)

(launchpad:add-a-comment 483637 "test adding a comment")

