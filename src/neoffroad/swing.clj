(ns neoffroad.swing
   (:import  [javax.swing SwingUtilities JFrame JLabel])
)

(defn main-frame []
  (doto (JFrame. "Registration")
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setSize 800 800)))

(defn create-and-show-gui []

  (let [my-frame (main-frame)
        content-pane (.getContentPane  my-frame)]
    (.add content-pane (JLabel. "Hello U"))
    (.pack my-frame)
    (.setVisible my-frame true)))


(defn gui []
  (SwingUtilities/invokeLater create-and-show-gui))

(gui)