(ns neoffroad.swing
   (:import  [javax.swing SwingUtilities JFrame JLabel])
)

(defn hello []
  (prn "hello"))

(hello)

(defn create-and-show-gui
  []

  (let [my-frame (doto (JFrame. "My Frame")
                   (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
        my-label (JLabel. "Hello UI")
        content-pane (.getContentPane  my-frame)]

    (.add content-pane my-label)
    (.pack my-frame)
    (.setVisible my-frame true)))


(defn gui []
  (SwingUtilities/invokeLater create-and-show-gui))

(gui)