(require :launchpad)

(defparameter project "clbuild-repos")

(defparameter bug-id 1)

(launchpad:login project)

(launchpad:get-all-bugs project)

(setf bug-id (launchpad:open-bug project "test" "just testing"))

(launchpad:update-bug bug-id :title "modify test" :description "modify just testing")

(launchpad:get-bug bug-id)

(launchpad:add-comment 483637 "test adding a comment")

