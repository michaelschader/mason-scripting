(def mt (proxy [Thread] [] (run [] (. System/out println "blah!")) (mds [] (println "yeah"))))
(.mds mt)
(.start mt)

