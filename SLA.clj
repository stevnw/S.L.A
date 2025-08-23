(ns language-game
  (:import [javax.swing JFrame JPanel JButton JLabel SwingUtilities JOptionPane JComboBox]
           [java.awt BorderLayout GridLayout CardLayout Dimension Color Font Insets GridBagLayout GridBagConstraints FlowLayout]
           [java.awt.event ActionListener KeyEvent KeyListener]
           [java.io File FileReader FileWriter BufferedReader BufferedWriter]
           [java.util ArrayList Collections Properties]
           [javax.sound.sampled AudioSystem Clip]))

(defrecord LanguageWord [language meaning sound-file])

(def words (atom []))
(def current-index (atom 0))
(def card-layout (CardLayout.))
(def card-panel (JPanel. card-layout))
(def tsv-files (atom []))
(def selected-tsv (atom ""))
(def current-card (atom "home"))
(def version "1.0")

; Default hotkeys
(def default-hotkeys
  {:menu "m"
   :learn-prev "p"
   :learn-reveal "r"
   :learn-shuffle "s"
   :learn-next "space"
   :match-1 "1"
   :match-2 "2"
   :match-3 "3"
   :match-4 "4"})

(def hotkeys (atom default-hotkeys)) ; Current hotkeys


(def config-file "config.txt") ; Config


(defn load-config []
  (try
    (let [props (Properties.)
          file (File. config-file)]
      (when (.exists file)
        (with-open [reader (BufferedReader. (FileReader. file))]
          (.load props reader)
          (doseq [[key default] default-hotkeys]
            (let [value (.getProperty props (name key) (str default))]
              (swap! hotkeys assoc key value))))))
    (catch Exception e
      (println "Error loading config:" (.getMessage e)))))

; Save config I hope ?? It seems to work at least on my machine... This seems like the proper way to do it.
(defn save-config []
  (try
    (with-open [writer (BufferedWriter. (FileWriter. config-file))]
      (doseq [[key value] @hotkeys]
        (.write writer (str (name key) "=" value "\n"))))
    (catch Exception e
      (println "Error saving config:" (.getMessage e)))))

(load-config)

(defn get-tsv-files []
  (let [dir (File. ".")
        files (.listFiles dir)
        tsv-files (filter #(and (.isFile %) (.endsWith (.getName %) ".tsv")) files)]
    (map #(.getName %) tsv-files)))

(defn parse-tsv [filename]
  (let [lines (-> filename slurp clojure.string/split-lines)
        words-list (ArrayList.)]
    (doseq [line (if (> (count lines) 1) (rest lines) lines)] ; Skip header
      (let [parts (clojure.string/split line #"\t")
            sound-tag (if (>= (count parts) 4) (nth parts 3 "") "")
            sound-file (when (clojure.string/starts-with? sound-tag "[sound:") ; I plan to figure out the sound later (famous last words...)
                         (-> sound-tag
                             (clojure.string/replace #"\[sound:(.*?)\]" "$1")
                             (clojure.string/trim)))]
        (when (>= (count parts) 2)
          (.add words-list (->LanguageWord (nth parts 0) (nth parts 1) sound-file)))))
    (vec words-list)))

;; UI
(defn create-button [text action-fn & [bg-color font-size button-width button-height]]
  (let [button (JButton. text)]
    (.addActionListener button
      (proxy [ActionListener] []
        (actionPerformed [event] (action-fn event))))
    (when bg-color
      (.setBackground button bg-color)
      (.setOpaque button true))
    (.setFont button (Font. "Arial" Font/BOLD (or font-size 14)))
    (.setMargin button (Insets. 5 15 5 15))
    (when (and button-width button-height)
      (.setPreferredSize button (Dimension. button-width button-height)))
    button))

(defn create-label [text & [font-size alignment]]
  (let [label (JLabel. text (or alignment JLabel/CENTER))]
    (when font-size
      (.setFont label (Font. "Arial" Font/BOLD font-size)))
    label))

;; Version
(defn create-version-bar []
  (let [version-label (JLabel. (str "S.L.A: v" version) JLabel/RIGHT)
        panel (JPanel. (BorderLayout.))]
    (.setFont version-label (Font. "Arial" Font/PLAIN 10))
    (.setForeground version-label Color/GRAY)
    (.setBorder panel (javax.swing.BorderFactory/createEmptyBorder 2 5 2 5))
    (.add panel version-label BorderLayout/EAST)
    panel))

;; Panels
(def home-panel (JPanel. (BorderLayout.)))
(def learn-panel (JPanel. (BorderLayout.)))
(def match-panel (JPanel. (BorderLayout.)))
(def options-panel (JPanel. (BorderLayout.)))

(defn add-menu-button [panel]
  (let [menu-button (create-button "Menu"
                                   (fn [_] 
                                     (reset! current-card "home")
                                     (.show card-layout card-panel "home")))]
    (.add panel menu-button BorderLayout/NORTH)))

;; Learn mode
(defn update-learn-panel []
  (let [index @current-index
        word (nth @words index)
        language-label (create-label (:language word) 32)
        meaning-label (create-label (:meaning word) 20)]
    (.removeAll learn-panel)
    (add-menu-button learn-panel)
    (let [content-panel (JPanel. (GridLayout. 2 1 10 10))
          meaning-panel (JPanel. (BorderLayout.))]
      (.add content-panel language-label)
      (.add meaning-panel meaning-label BorderLayout/CENTER)
      (.setVisible meaning-panel false)
      (.add content-panel meaning-panel)
      (.add learn-panel content-panel BorderLayout/CENTER))
    (let [button-panel (JPanel. (GridLayout. 1 4 10 10))]
      (.add button-panel
            (create-button "Previous"
                           (fn [_]
                             (swap! current-index #(max 0 (dec %)))
                             (update-learn-panel))))
      (.add button-panel
            (create-button "Reveal"
                           (fn [_]
                             (let [meaning-panel (.getComponent (.getComponent learn-panel 1) 1)]
                               (.setVisible meaning-panel true)
                               (.revalidate learn-panel)
                               (.repaint learn-panel)))))
      (.add button-panel
            (create-button "Shuffle"
                           (fn [_]
                             (reset! current-index 0)
                             (swap! words #(shuffle %))
                             (update-learn-panel))))
      (.add button-panel
            (create-button "Next"
                           (fn [_]
                             (swap! current-index #(min (dec (count @words)) (inc %)))
                             (update-learn-panel))))
      (.add learn-panel button-panel BorderLayout/SOUTH))
    (.revalidate learn-panel)
    (.repaint learn-panel)))

;; Match mode
(defn create-match-panel []
  (let [correct-index (rand-int (count @words))
        correct-word (nth @words correct-index)
        options (-> (take 3 (shuffle (remove #(= % correct-word) @words)))
                    (conj correct-word)
                    shuffle)
        correct-option-index (.indexOf options correct-word)
        feedback-label (create-label " " 18)]
    (.removeAll match-panel)
    (add-menu-button match-panel)

    (let [center-content-panel (JPanel. (BorderLayout. 20 20))
          language-label (create-label (:language correct-word) 28)
          options-panel (JPanel. (GridLayout. 2 2 10 10))]
      (.add center-content-panel language-label BorderLayout/CENTER)
      (.add center-content-panel feedback-label BorderLayout/SOUTH)

      (doseq [[idx option] (map-indexed vector options)]
        (.add options-panel
              (create-button (:meaning option)
                             (fn [_]
                               (if (= idx correct-option-index)
                                 (do
                                   (.setText feedback-label "Correct!")
                                   (.setForeground feedback-label Color/GREEN)
                                   (doseq [c (.getComponents options-panel)]
                                     (.setEnabled c false))
                                   (future
                                     (Thread/sleep 1500)
                                     (SwingUtilities/invokeLater create-match-panel)))
                                 (do
                                   (.setText feedback-label "Incorrect! Try again.")
                                   (.setForeground feedback-label Color/RED)
                                   (.setEnabled (nth (vec (.getComponents options-panel)) idx) false)))))))
      
      (.add match-panel center-content-panel BorderLayout/CENTER)
      (.add match-panel options-panel BorderLayout/SOUTH))
    
    (.revalidate match-panel)
    (.repaint match-panel)))

;; Options
(defn create-options-panel []
  (.removeAll options-panel)
  (add-menu-button options-panel)
  
  (let [content-panel (JPanel. (GridBagLayout.))
        gbc (GridBagConstraints.)
        hotkey-fields (atom {})]
    
    (set! (.gridwidth gbc) 2)
    (set! (.insets gbc) (Insets. 10 10 5 10))
    (set! (.anchor gbc) GridBagConstraints/WEST)
    
    (set! (.gridy gbc) 0)
    (.add content-panel (create-label "Hotkey Configuration" 20) gbc)
    
    (set! (.gridwidth gbc) 1)
    (set! (.gridy gbc) 1)
    
    (doseq [[i [label key]] (map-indexed vector 
                                        [["Menu" :menu]
                                         ["Learn - Previous" :learn-prev]
                                         ["Learn - Reveal" :learn-reveal]
                                         ["Learn - Shuffle" :learn-shuffle]
                                         ["Learn - Next" :learn-next]
                                         ["Match - Option 1" :match-1]
                                         ["Match - Option 2" :match-2]
                                         ["Match - Option 3" :match-3]
                                         ["Match - Option 4" :match-4]])]
      (set! (.gridy gbc) (+ i 1))
      
      (set! (.gridx gbc) 0)
      (.add content-panel (create-label (str label ":")) gbc)
      
      (set! (.gridx gbc) 1)
      (let [text-field (javax.swing.JTextField. (get @hotkeys key))]
        (.setPreferredSize text-field (Dimension. 100 25))
        (swap! hotkey-fields assoc key text-field)
        (.add content-panel text-field gbc)))
    
    (set! (.gridwidth gbc) 2)
    (set! (.gridx gbc) 0)
    (set! (.gridy gbc) 11)
    (set! (.anchor gbc) GridBagConstraints/CENTER)
    
    (let [button-panel (JPanel. (FlowLayout. FlowLayout/CENTER 10 10))]
      (.add button-panel
            (create-button "Save"
                           (fn [_]
                             (doseq [[key field] @hotkey-fields]
                               (swap! hotkeys assoc key (.getText field)))
                             (save-config)
                             (reset! current-card "home")
                             (.show card-layout card-panel "home"))))
      (.add button-panel
            (create-button "Cancel"
                           (fn [_]
                             (reset! current-card "home")
                             (.show card-layout card-panel "home"))))
      (.add button-panel
            (create-button "Reset to Defaults"
                           (fn [_]
                             (reset! hotkeys default-hotkeys)
                             (doseq [[key field] @hotkey-fields]
                               (.setText field (get default-hotkeys key ""))))))
      (.add content-panel button-panel gbc))
    
    (.add options-panel content-panel BorderLayout/CENTER)
    (.revalidate options-panel)
    (.repaint options-panel)))

;; Main menu stuff
(defn create-home-panel []
  (.removeAll home-panel)
  (let [title-label (create-label "S.L.A" 32)
        content-panel (JPanel. (GridBagLayout.))
        button-panel (JPanel. (GridLayout. 4 1 20 10))
        gbc (GridBagConstraints.)
        file-combo (JComboBox. (into-array @tsv-files))
        top-panel (JPanel. (FlowLayout. FlowLayout/CENTER 0 20))]
    
    (.addActionListener file-combo
      (proxy [ActionListener] []
        (actionPerformed [e]
          (reset! selected-tsv (.getSelectedItem file-combo)))))
    
    (when (seq @tsv-files)
      (.setSelectedItem file-combo (first @tsv-files))
      (reset! selected-tsv (first @tsv-files)))
    
    (.add top-panel file-combo)
    (.add home-panel top-panel BorderLayout/NORTH)
    
    (set! (.gridy gbc) 0)
    (set! (.insets gbc) (Insets. 0 0 10 0))
    (.add content-panel title-label gbc)
    
    (set! (.gridy gbc) 1)
    (set! (.insets gbc) (Insets. 0 0 0 0))
    (.add button-panel
          (create-button "Learn Mode"
                         (fn [_]
                           (when-let [filename @selected-tsv]
                             (try
                               (reset! words (parse-tsv filename))
                               (reset! current-card "learn")
                               (update-learn-panel)
                               (.show card-layout card-panel "learn")
                               (catch Exception e
                                 (JOptionPane/showMessageDialog home-panel
                                   (str "Error loading file: " filename)
                                   "Error"
                                   JOptionPane/ERROR_MESSAGE)))))))
    (.add button-panel
          (create-button "Match Mode"
                         (fn [_]
                           (when-let [filename @selected-tsv]
                             (try
                               (reset! words (parse-tsv filename))
                               (reset! current-card "match")
                               (create-match-panel)
                               (.show card-layout card-panel "match")
                               (catch Exception e
                                 (JOptionPane/showMessageDialog home-panel
                                   (str "Error loading file: " filename)
                                   "Error"
                                   JOptionPane/ERROR_MESSAGE)))))))
    (.add button-panel
          (create-button "Options"
                         (fn [_]
                           (reset! current-card "options")
                           (create-options-panel)
                           (.show card-layout card-panel "options"))))
    (.add button-panel
          (create-button "Exit"
                         (fn [_]
                           (System/exit 0))))
    
    (.add content-panel button-panel gbc)
    
    (.add home-panel content-panel BorderLayout/CENTER)
    
    (.revalidate home-panel)
    (.repaint home-panel)))

;; Hotkey listener
(defn add-key-listener [frame]
  (let [key-listener (proxy [KeyListener] []
                      (keyPressed [e]
                        (let [key-code (.getKeyCode e)
                              key-char (-> e .getKeyChar Character/toLowerCase)
                              key-str (cond
                                        (== key-code KeyEvent/VK_SPACE) "space"
                                        (and (>= key-code KeyEvent/VK_0) (<= key-code KeyEvent/VK_9)) 
                                        (str (- key-code KeyEvent/VK_0))
                                        (and (>= key-code KeyEvent/VK_A) (<= key-code KeyEvent/VK_Z))
                                        (str key-char)
                                        :else nil)]
                          (when key-str
                            (case @current-card
                              "learn" (cond
                                        (= key-str (@hotkeys :learn-prev)) 
                                        (do (swap! current-index #(max 0 (dec %)))
                                            (update-learn-panel))
                                        
                                        (= key-str (@hotkeys :learn-reveal))
                                        (let [meaning-panel (.getComponent (.getComponent learn-panel 1) 1)]
                                          (.setVisible meaning-panel true)
                                          (.revalidate learn-panel)
                                          (.repaint learn-panel))
                                        
                                        (= key-str (@hotkeys :learn-shuffle))
                                        (do (reset! current-index 0)
                                            (swap! words #(shuffle %))
                                            (update-learn-panel))
                                        
                                        (= key-str (@hotkeys :learn-next))
                                        (do (swap! current-index #(min (dec (count @words)) (inc %)))
                                            (update-learn-panel))
                                        
                                        (= key-str (@hotkeys :menu))
                                        (do (reset! current-card "home")
                                            (.show card-layout card-panel "home")))
                              
                              "match" (cond
                                        (= key-str (@hotkeys :match-1)) 
                                        (when (and (>= (.getComponentCount match-panel) 3)
                                                   (>= (.getComponentCount (.getComponent match-panel 2)) 1))
                                          (.doClick (nth (vec (.getComponents (.getComponent match-panel 2))) 0)))
                                        
                                        (= key-str (@hotkeys :match-2)) 
                                        (when (and (>= (.getComponentCount match-panel) 3)
                                                   (>= (.getComponentCount (.getComponent match-panel 2)) 2))
                                          (.doClick (nth (vec (.getComponents (.getComponent match-panel 2))) 1)))
                                        
                                        (= key-str (@hotkeys :match-3)) 
                                        (when (and (>= (.getComponentCount match-panel) 3)
                                                   (>= (.getComponentCount (.getComponent match-panel 2)) 3))
                                          (.doClick (nth (vec (.getComponents (.getComponent match-panel 2))) 2)))
                                        
                                        (= key-str (@hotkeys :match-4)) 
                                        (when (and (>= (.getComponentCount match-panel) 3)
                                                   (>= (.getComponentCount (.getComponent match-panel 2)) 4))
                                          (.doClick (nth (vec (.getComponents (.getComponent match-panel 2))) 3)))
                                        
                                        (= key-str (@hotkeys :menu))
                                        (do (reset! current-card "home")
                                            (.show card-layout card-panel "home")))
                              
                              "options" (when (= key-str (@hotkeys :menu))
                                          (do (reset! current-card "home")
                                              (.show card-layout card-panel "home")))
                              
                              "home" (when (= key-str (@hotkeys :menu))
                                       (.show card-layout card-panel "home"))))))
                      (keyReleased [e])
                      (keyTyped [e]))]
    (.addKeyListener frame key-listener)
    (.setFocusable frame true)))

;; Main 
(defn create-and-show-gui []
  (let [frame (JFrame. "S.L.A")
        main-panel (JPanel. (BorderLayout.))]
    (.setPreferredSize frame (Dimension. 800 600))
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (.add card-panel home-panel "home")
    (.add card-panel learn-panel "learn")
    (.add card-panel match-panel "match")
    (.add card-panel options-panel "options")
    
    (reset! tsv-files (get-tsv-files))
    (when (empty? @tsv-files)
      (JOptionPane/showMessageDialog frame
        "No TSV files found in the current directory."
        "Error"
        JOptionPane/ERROR_MESSAGE))
    
    (create-home-panel)
    (.add main-panel card-panel BorderLayout/CENTER)
    (.add main-panel (create-version-bar) BorderLayout/SOUTH)
    (.add frame main-panel)
    (.pack frame)
    (.setLocationRelativeTo frame nil)
    (.setVisible frame true)
    
    (add-key-listener frame)))

(SwingUtilities/invokeLater create-and-show-gui)
