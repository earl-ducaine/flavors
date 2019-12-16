
(asdf:defsystem :flavors
  :depends-on (:cffi :alexandria ::bordeaux-threads :closer-mop)
  :serial t
  :components
  ((:file "package")
   (:file "flavors-macros")
   (:file "lap-lab")
   (:file "pre-kkernel")
   (:file "kkernel")
   (:file "kernel")
   (:file "flavor-main-decls")
   (:file "flavor-main0")
   (:file "flavor-main1")
   (:file "flavor-main2")
   (:file "vanilla")
   (:file "last-file")))
