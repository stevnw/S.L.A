(ns SLA.core
  (:gen-class)
  (:import [javax.swing JFrame JPanel JButton JLabel SwingUtilities JOptionPane JComboBox JTextField JScrollPane]
           [java.awt BorderLayout GridLayout CardLayout Dimension Color Font Insets GridBagLayout GridBagConstraints FlowLayout]
           [java.awt.event ActionListener KeyEvent KeyListener]
           [java.io File FileReader FileWriter BufferedReader BufferedWriter]
           [java.util ArrayList Collections Properties]
           [javax.sound.sampled AudioSystem Clip AudioInputStream]
           [java.net URL]
           [java.awt GraphicsEnvironment]
           [java.io FileInputStream]))

(defrecord LanguageWord [language meaning sound-file reading])

(def words (atom []))
(def current-index (atom 0))
(def card-layout (CardLayout.))
(def card-panel (JPanel. card-layout))
(def tsv-files (atom []))
(def selected-tsv (atom ""))
(def current-card (atom "home"))
(def version "1.2")
(def main-frame (atom nil))
(def targeted-start-index (atom 0))
(def targeted-end-index (atom 0))
(def targeted-mode (atom "learn"))

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

(def default-config
  (merge default-hotkeys
         {:theme "light"}))

(def config (atom default-config))

(def config-file "config.txt")

;; Themes and such, potentially add more in the future. The current "darkmode" seems okay... - added some more you'd find in IDEs
(def light-theme
  {:panel-bg (java.awt.Color. 245 245 245)
   :button-bg (java.awt.Color. 240 240 240)
   :button-fg (java.awt.Color. 0 0 0)
   :label-fg (java.awt.Color. 0 0 0)})

(def dark-theme
  {:panel-bg (java.awt.Color. 30 30 30)
   :button-bg (java.awt.Color. 50 50 50)
   :button-fg (java.awt.Color. 255 255 255)
   :label-fg (java.awt.Color. 255 255 255)})
   
(def sepia-theme
  {:panel-bg (java.awt.Color. 253 246 227)
   :button-bg (java.awt.Color. 210 180 140)
   :button-fg (java.awt.Color. 88 57 39)
   :label-fg (java.awt.Color. 88 57 39)})
   
(def solarized-theme
  {:panel-bg (java.awt.Color. 0 43 54)
   :button-bg (java.awt.Color. 7 54 66)
   :button-fg (java.awt.Color. 131 148 150)
   :label-fg (java.awt.Color. 131 148 150)})
  
(def sakura-theme
  {:panel-bg (java.awt.Color. 255 240 245)
   :button-bg (java.awt.Color. 255 218 230)
   :button-fg (java.awt.Color. 119 79 97)
   :label-fg (java.awt.Color. 119 79 97)})
  
(def themes
  {:light light-theme
   :dark dark-theme
   :sepia sepia-theme
   :solarized solarized-theme
   :sakura sakura-theme})

(def current-theme (atom :light))

(defn load-config []
  (try
    (let [props (Properties.)
          file (File. config-file)]
      (when (.exists file)
        (with-open [reader (BufferedReader. (FileReader. file))]
          (.load props reader)
          (doseq [[key default] default-config]
            (swap! config assoc key (or (.getProperty props (name key)) default))))))
    (catch Exception e
      (println "Error loading config:" (.getMessage e))))
  (reset! current-theme (keyword (get @config :theme))))

(defn save-config []
  (try
    (with-open [writer (BufferedWriter. (FileWriter. config-file))]
      (doseq [[key value] @config]
        (.write writer (str (name key) "=" value "\n"))))
    (catch Exception e
      (println "Error saving config:" (.getMessage e)))))

(load-config)

(defn get-sla-fonts []
  (let [font-dir (File. "user")
        font-files (atom [])]
    (when (.exists font-dir)
      (doseq [file (.listFiles font-dir)]
        (when (and (.isFile file)
                   (or (.endsWith (.getName file) ".ttf")
                       (.endsWith (.getName file) ".otf")))
          (try
            (let [font (Font/createFont Font/TRUETYPE_FONT file)]
              (-> (GraphicsEnvironment/getLocalGraphicsEnvironment)
                  (.registerFont font))
              (swap! font-files conj (.getFontName font)))
            (catch Exception e
              (println "Error loading font" (.getName file) ":" (.getMessage e)))))))
    (sort @font-files)))

(defn apply-theme []
  (let [theme-colors (get themes @current-theme)]
    (javax.swing.UIManager/put "Panel.background" (:panel-bg theme-colors))
    (javax.swing.UIManager/put "Button.background" (:button-bg theme-colors))
    (javax.swing.UIManager/put "Button.foreground" (:button-fg theme-colors))
    (javax.swing.UIManager/put "Label.foreground" (:label-fg theme-colors))
    (javax.swing.UIManager/put "ComboBox.background" (:button-bg theme-colors))
    (javax.swing.UIManager/put "ComboBox.foreground" (:button-fg theme-colors))
    (javax.swing.UIManager/put "TextField.background" (:button-bg theme-colors))
    (javax.swing.UIManager/put "TextField.foreground" (:button-fg theme-colors))
    (javax.swing.UIManager/put "TextField.caretForeground" (:button-fg theme-colors))))

(declare create-and-show-gui)

(defn restart-app [] ; this seems like a bit of a stupid way to do it - however it is the only way in which I could think of to make it work with the least amount of effort... lol
  (when @main-frame
    (.dispose @main-frame))
  (reset! main-frame nil)
  (future
    (Thread/sleep 100)
    (SwingUtilities/invokeLater create-and-show-gui)))

(defn get-tsv-files []
  (let [dir (File. ".")
        files (.listFiles dir)
        tsv-files (filter #(and (.isFile %) (.endsWith (.getName %) ".tsv")) files)]
    (map #(.getName %) tsv-files)))

(defn parse-tsv [filename]
  (let [lines (-> filename slurp clojure.string/split-lines)
        words-list (ArrayList.)]
    (doseq [line (if (> (count lines) 1) (rest lines) lines)]
      (let [parts (clojure.string/split line #"\t")
            sound-tag (if (>= (count parts) 4) (nth parts 3 "") "")
            sound-file (when (and (not (clojure.string/blank? sound-tag))
                                  (clojure.string/includes? sound-tag ".wav")
                                  (not (clojure.string/starts-with? sound-tag "[sound:")))
                         (clojure.string/trim sound-tag))
            reading (if (>= (count parts) 3) (nth parts 2) "")]
        (when (>= (count parts) 2)
          (.add words-list (->LanguageWord (nth parts 0) (nth parts 1) sound-file reading)))))
    (vec words-list)))

(defn play-audio [audio-file]
  (when (and audio-file (not (clojure.string/blank? audio-file)))
    (try
      (let [file (File. audio-file)]
        (when (.exists file)
          (let [audio-stream (AudioSystem/getAudioInputStream file)
                clip (AudioSystem/getClip)]
            (.open clip audio-stream)
            (.start clip)
            (.addLineListener clip
              (proxy [javax.sound.sampled.LineListener] []
                (update [event]
                  (when (= (.getType event) javax.sound.sampled.LineEvent$Type/STOP)
                    (.close clip)
                    (.close audio-stream))))))))
      (catch Exception e
        (println "Error playing audio:" (.getMessage e))))))

;; UI
(defn create-button [text action-fn & [font-size button-width button-height]]
  (let [button (JButton. text)
        theme-colors (get themes @current-theme)]
    (.addActionListener button
      (proxy [ActionListener] []
        (actionPerformed [event] (action-fn event))))
    (.setBackground button (:button-bg theme-colors))
    (.setForeground button (:button-fg theme-colors))
    (.setOpaque button true)
    (.setFont button (Font. "SansSerif" Font/BOLD (or font-size 14)))
    (.setMargin button (Insets. 5 15 5 15))
    (when (and button-width button-height)
      (.setPreferredSize button (Dimension. button-width button-height)))
    button))

(defn create-label [text & [font-size alignment]]
  (let [label (JLabel. text (or alignment JLabel/CENTER))
        theme-colors (get themes @current-theme)
        font-family (get @config :font-family)
        base-font-size (Integer/parseInt (or (get @config :font-size) "16"))
        final-font-size (or font-size base-font-size)]
    (when final-font-size
      (if font-family
        (.setFont label (Font. font-family Font/BOLD final-font-size))
        (.setFont label (Font. "SansSerif" Font/BOLD final-font-size))))
    (.setForeground label (:label-fg theme-colors))
    label))

;; Version
(defn create-version-bar []
  (let [version-label (JLabel. (str "S.L.A: v" version) JLabel/RIGHT)
        panel (JPanel. (BorderLayout.))
        font-family (get @config :font-family)]
    (if font-family
      (.setFont version-label (Font. font-family Font/PLAIN 10))
      (.setFont version-label (Font. "SansSerif" Font/PLAIN 10)))
    (.setForeground version-label Color/GRAY)
    (.setBorder panel (javax.swing.BorderFactory/createEmptyBorder 2 5 2 5))
    (.add panel version-label BorderLayout/EAST)
    panel))

;; Panels
(def home-panel (JPanel. (BorderLayout.)))
(def learn-panel (JPanel. (BorderLayout.)))
(def match-panel (JPanel. (BorderLayout.)))
(def options-panel (JPanel. (BorderLayout.)))
(def targeted-options-panel (JPanel. (BorderLayout.)))

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
        reading-label (when (not (clojure.string/blank? (:reading word)))
                       (create-label (:reading word) 16))
        meaning-label (create-label (:meaning word) 20)]
    
    (when (:sound-file word)
      (.setCursor language-label (java.awt.Cursor/getPredefinedCursor java.awt.Cursor/HAND_CURSOR))
      (.addMouseListener language-label
        (proxy [java.awt.event.MouseAdapter] []
          (mouseClicked [e]
            (play-audio (:sound-file word))))))
    
    (.removeAll learn-panel)
    (add-menu-button learn-panel)
    (let [content-panel (JPanel. (GridLayout. (if reading-label 3 2) 1 10 10))
          meaning-panel (JPanel. (BorderLayout.))]
      (.add content-panel language-label)
      (when reading-label
        (.add content-panel reading-label))
      (.add meaning-panel meaning-label BorderLayout/CENTER)
      (.setVisible meaning-panel false)
      (.add content-panel meaning-panel)
      (.add learn-panel content-panel BorderLayout/CENTER))
    (let [button-panel (JPanel. (GridLayout. 1 4 10 10))]
      (.add button-panel
            (create-button "Previous"
                           (fn [_]
                             ;(swap! current-index #(max 0 (dec %)))
                             (swap! current-index #(if (<= (dec %) 0) (dec (count @words)) (dec %)))
                             (update-learn-panel))))
      (.add button-panel
            (create-button "Reveal"
                           (fn [_]
                             (let [meaning-panel (.getComponent (.getComponent learn-panel 1) (if reading-label 2 1))]
                               (.setVisible meaning-panel true)
                               (.revalidate learn-panel)
                               (.repaint learn-panel)))))
      (.add button-panel
            (create-button "Shuffle"
                           (fn [_]
                             (swap! words shuffle)
                             (update-learn-panel))))
      (.add button-panel
            (create-button "Next"
                           (fn [_]
                             ;(swap! current-index #(min (dec (count @words)) (inc %)))
                             (swap! current-index #(if (>= (inc %) (count @words)) 0 (inc %)))
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
        feedback-label (create-label " " 18)
        language-label (create-label (:language correct-word) 28)
        reading-label (when (not (clojure.string/blank? (:reading correct-word)))
                       (create-label (:reading correct-word) 16))]
    
    (when (:sound-file correct-word)
      (.setCursor language-label (java.awt.Cursor/getPredefinedCursor java.awt.Cursor/HAND_CURSOR))
      (.addMouseListener language-label
        (proxy [java.awt.event.MouseAdapter] []
          (mouseClicked [e]
            (play-audio (:sound-file correct-word))))))
    
    (.removeAll match-panel)
    (add-menu-button match-panel)
    (let [center-content-panel (JPanel. (BorderLayout. 20 20))
          options-panel (JPanel. (GridLayout. 2 2 10 10))]
      (.add center-content-panel language-label BorderLayout/CENTER)
      (when reading-label
        (.add center-content-panel reading-label BorderLayout/SOUTH))
      (.add center-content-panel feedback-label BorderLayout/SOUTH)
      (doseq [[idx option] (map-indexed vector options)]
        (.add options-panel
              (create-button (:meaning option)
                             (fn [_]
                               (if (= idx correct-option-index)
                                 (do
                                   (.setText feedback-label "Correct!")
                                   (.setForeground feedback-label (java.awt.Color/GREEN))
                                   (doseq [c (.getComponents options-panel)]
                                     (.setEnabled c false))
                                   (future
                                     (Thread/sleep 1500)
                                     (SwingUtilities/invokeLater create-match-panel)))
                                 (do
                                   (.setText feedback-label "Incorrect! Try again.")
                                   (.setForeground feedback-label (java.awt.Color/RED))
                                   (.setEnabled (nth (vec (.getComponents options-panel)) idx) false)))))))
      (.add match-panel center-content-panel BorderLayout/CENTER)
      (.add match-panel options-panel BorderLayout/SOUTH))
    (.revalidate match-panel)
    (.repaint match-panel)))

;; Options
(defn create-options-panel []
  (.removeAll options-panel)
  (add-menu-button options-panel)
  (let [scroll-panel (JScrollPane.)
        content-panel (JPanel. (GridBagLayout.))
        gbc (GridBagConstraints.)
        hotkey-fields (atom {})
        theme-combo (JComboBox. (into-array ["light" "dark" "sepia" "solarized" "sakura"]))
        available-fonts (concat ["System Default"] (get-sla-fonts))
        font-combo (JComboBox. (into-array available-fonts))
        font-size-field (JTextField. (or (get @config :font-size) "16"))]
    
    (.setViewportView scroll-panel content-panel)
    (.setVerticalScrollBarPolicy scroll-panel JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED)
    (.setHorizontalScrollBarPolicy scroll-panel JScrollPane/HORIZONTAL_SCROLLBAR_NEVER)
    
    (set! (.gridwidth gbc) 2)
    (set! (.insets gbc) (Insets. 10 10 5 10))
    (set! (.anchor gbc) GridBagConstraints/WEST)
    (set! (.gridy gbc) 0)
    (.add content-panel (create-label "Settings" 20) gbc)
    
    (set! (.gridy gbc) 1)
    (set! (.gridx gbc) 0)
    (.add content-panel (create-label "Font Family:") gbc)
    (set! (.gridx gbc) 1)
    (.setSelectedItem font-combo (if-let [current-font (get @config :font-family)]
                                   current-font
                                   "System Default"))
    (.add content-panel font-combo gbc)
    
    (set! (.gridy gbc) 2)
    (set! (.gridx gbc) 0)
    (.add content-panel (create-label "Font Size:") gbc)
    (set! (.gridx gbc) 1)
    (.setPreferredSize font-size-field (Dimension. 100 25))
    (.add content-panel font-size-field gbc)
    
    (set! (.gridy gbc) 3)
    (set! (.gridx gbc) 0)
    (.add content-panel (create-label "Theme:") gbc)
    (set! (.gridx gbc) 1)
    (.setSelectedItem theme-combo (name @current-theme))
    (.add content-panel theme-combo gbc)
    
    ; Buttons at the bottom
    (set! (.gridwidth gbc) 1)
    (set! (.gridy gbc) 4)
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
      (set! (.gridy gbc) (+ i 4))
      (set! (.gridx gbc) 0)
      (.add content-panel (create-label (str label ":")) gbc)
      (set! (.gridx gbc) 1)
      (let [text-field (JTextField. (get @config key))]
        (.setPreferredSize text-field (Dimension. 100 25))
        (swap! hotkey-fields assoc key text-field)
        (.add content-panel text-field gbc)))
    
    (set! (.gridwidth gbc) 2)
    (set! (.gridx gbc) 0)
    (set! (.gridy gbc) 14)
    (set! (.anchor gbc) GridBagConstraints/CENTER)
    (let [button-panel (JPanel. (FlowLayout. FlowLayout/CENTER 10 10))]
      (.add button-panel
            (create-button "Save"
                           (fn [_]
                             (doseq [[key field] @hotkey-fields]
                               (swap! config assoc key (.getText field)))
                             (let [new-theme-str (.getSelectedItem theme-combo)
                                   new-font-family (if (= (.getSelectedItem font-combo) "System Default")
                                                     nil
                                                     (.getSelectedItem font-combo))
                                   new-font-size (.getText font-size-field)
                                   theme-changed? (not= (name @current-theme) new-theme-str)
                                   font-changed? (or (not= (get @config :font-family) new-font-family)
                                                     (not= (get @config :font-size) new-font-size))]
                               (swap! config assoc :theme new-theme-str)
                               (swap! config assoc :font-family new-font-family)
                               (swap! config assoc :font-size new-font-size)
                               (reset! current-theme (keyword new-theme-str))
                               (save-config)
                               (if (or theme-changed? font-changed?)
                                 (restart-app)
                                 (do
                                   (reset! current-card "home")
                                   (.show card-layout card-panel "home")))))))
      (.add button-panel
            (create-button "Cancel"
                           (fn [_]
                             (reset! current-card "home")
                             (.show card-layout card-panel "home"))))
      (.add button-panel
            (create-button "Reset to Defaults"
                           (fn [_]
                             (let [theme-changed? (not= @current-theme :light)
                                   font-changed? (or (not (nil? (get @config :font-family)))
                                                     (not= (get @config :font-size) "16"))]
                               (reset! config default-config)
                               (reset! current-theme :light)
                               (doseq [[key field] @hotkey-fields]
                                 (.setText field (get default-hotkeys key "")))
                               (.setSelectedItem theme-combo (name @current-theme))
                               (.setSelectedItem font-combo "System Default")
                               (.setText font-size-field "16")
                               (save-config)
                               (if (or theme-changed? font-changed?)
                                 (restart-app)
                                 (do
                                   (reset! current-card "home")
                                   (.show card-layout card-panel "home")))))))
      (.add content-panel button-panel gbc))
    
    (.add options-panel scroll-panel BorderLayout/CENTER)
    (.revalidate options-panel)
    (.repaint options-panel)))

;; Targeted mode
(defn create-targeted-options-panel []
  (.removeAll targeted-options-panel)
  (add-menu-button targeted-options-panel)
  
  (let [content-panel (JPanel. (GridBagLayout.))
        gbc (GridBagConstraints.)
        file-combo (JComboBox. (into-array @tsv-files))
        start-field (doto (JTextField. (str @targeted-start-index))
                      (.setPreferredSize (Dimension. 120 25)))
        end-field (doto (JTextField. (str (if (and @selected-tsv (not (empty? @selected-tsv)))
                                   (dec (count (parse-tsv @selected-tsv)))
                                   @targeted-end-index)))
            (.setPreferredSize (Dimension. 120 25)))
        mode-combo (JComboBox. (into-array ["Learn" "Match"]))]
        
    (.addActionListener file-combo
      (proxy [ActionListener] []
        (actionPerformed [e]
          (let [selected-file (.getSelectedItem file-combo)]
            (when selected-file
              (let [word-count (count (parse-tsv selected-file))
                    end-value (dec word-count)]
                (.setText end-field (str end-value))))))))
    
    (when (seq @tsv-files)
      (.setSelectedItem file-combo (first @tsv-files)))
    
    (.setSelectedItem mode-combo @targeted-mode)
    
    (set! (.gridwidth gbc) 2)
    (set! (.insets gbc) (Insets. 10 10 5 10))
    (set! (.anchor gbc) GridBagConstraints/WEST)
    
    (set! (.gridy gbc) 0)
    (.add content-panel (create-label "Select Deck:") gbc)
    (set! (.gridy gbc) 1)
    (.add content-panel file-combo gbc)
    
    (set! (.gridy gbc) 2)
    (.add content-panel (create-label "Word Range:") gbc)
    
    (set! (.gridwidth gbc) 1)
    (set! (.gridy gbc) 3)
    (set! (.gridx gbc) 0)
    (.add content-panel (create-label "Start Index:") gbc)
    (set! (.gridx gbc) 1)
    (.add content-panel start-field gbc)
    
    (set! (.gridy gbc) 4)
    (set! (.gridx gbc) 0)
    (.add content-panel (create-label "End Index:") gbc)
    (set! (.gridx gbc) 1)
    (.add content-panel end-field gbc)
    
    (set! (.gridwidth gbc) 2)
    (set! (.gridy gbc) 5)
    (.add content-panel (create-label "Mode:") gbc)
    (set! (.gridy gbc) 6)
    (.add content-panel mode-combo gbc)
    
    (set! (.gridy gbc) 7)
    (set! (.anchor gbc) GridBagConstraints/CENTER)
    (.add content-panel 
          (create-button "Start Targeted Mode"
                         (fn [_]
                           (try
                             (let [filename (.getSelectedItem file-combo)
                                   start (Integer/parseInt (.getText start-field))
                                   end (Integer/parseInt (.getText end-field))
                                   mode (.getSelectedItem mode-combo)]
                               (reset! selected-tsv filename)
                               (reset! targeted-start-index start)
                               (reset! targeted-end-index end)
                               (reset! targeted-mode mode)
                               
                               (when-let [filename @selected-tsv]
                                 (reset! words (parse-tsv filename))
                                 (swap! words 
                                        (fn [ws] 
                                          (subvec ws 
                                                  (min start (dec (count ws))) 
                                                  (min (inc end) (count ws)))))
                                 
                                 (cond
                                   (= mode "Learn")
                                   (do
                                     (reset! current-index 0)
                                     (reset! current-card "learn")
                                     (update-learn-panel)
                                     (.show card-layout card-panel "learn"))
                                   
                                   (= mode "Match")
                                   (do
                                     (reset! current-card "match")
                                     (create-match-panel)
                                     (.show card-layout card-panel "match")))))
                             (catch Exception e
                               (JOptionPane/showMessageDialog targeted-options-panel
                                                              "Please enter valid numbers for start and end indices."
                                                              "Input Error"
                                                              JOptionPane/ERROR_MESSAGE)))))
          gbc)
    
    (.add targeted-options-panel content-panel BorderLayout/CENTER)
    (.revalidate targeted-options-panel)
    (.repaint targeted-options-panel)))

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
                               (reset! current-index 0)
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
          (create-button "Targeted Mode"
                         (fn [_]
                           (reset! current-card "targeted-options")
                           (create-targeted-options-panel)
                           (.show card-layout card-panel "targeted-options"))))
    (.add button-panel
          (create-button "Options"
                         (fn [_]
                           (reset! current-card "options")
                           (create-options-panel)
                           (.show card-layout card-panel "options"))))
    (.add content-panel button-panel gbc)
    (.add home-panel content-panel BorderLayout/CENTER)
    
;    (let [exit-panel (JPanel. (FlowLayout. FlowLayout/CENTER))] ; Commented out for now - don't think I want an exit button anymore... keeping incase I re-add it
;      (.add exit-panel 
;            (create-button "Exit"
;                           (fn [_]
;                             (System/exit 0))))
;      (.add home-panel exit-panel BorderLayout/SOUTH))
    
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
                                         (= key-str (get @config :learn-prev))
                                         (do (swap! current-index #(max 0 (dec %)))
                                             (update-learn-panel))
                                         (= key-str (get @config :learn-reveal))
                                         (let [meaning-panel (.getComponent (.getComponent learn-panel 1) (if (not (clojure.string/blank? (:reading (nth @words @current-index)))) 2 1))]
                                           (.setVisible meaning-panel true)
                                           (.revalidate learn-panel)
                                           (.repaint learn-panel))
                                         (= key-str (get @config :learn-shuffle))
                                         (do (reset! current-index 0)
                                             (swap! words shuffle)
                                             (update-learn-panel))
                                         (= key-str (get @config :learn-next))
                                         (do (swap! current-index #(min (dec (count @words)) (inc %)))
                                             (update-learn-panel))
                                         (= key-str (get @config :menu))
                                         (do (reset! current-card "home")
                                             (.show card-layout card-panel "home")))
                               "match" (cond
                                         (= key-str (get @config :match-1))
                                         (when (and (>= (.getComponentCount match-panel) 3)
                                                    (>= (.getComponentCount (.getComponent match-panel 2)) 1))
                                           (.doClick (nth (vec (.getComponents (.getComponent match-panel 2))) 0)))
                                         (= key-str (get @config :match-2))
                                         (when (and (>= (.getComponentCount match-panel) 3)
                                                    (>= (.getComponentCount (.getComponent match-panel 2)) 2))
                                           (.doClick (nth (vec (.getComponents (.getComponent match-panel 2))) 1)))
                                         (= key-str (get @config :match-3))
                                         (when (and (>= (.getComponentCount match-panel) 3)
                                                    (>= (.getComponentCount (.getComponent match-panel 2)) 3))
                                           (.doClick (nth (vec (.getComponents (.getComponent match-panel 2))) 2)))
                                         (= key-str (get @config :match-4))
                                         (when (and (>= (.getComponentCount match-panel) 3)
                                                    (>= (.getComponentCount (.getComponent match-panel 2)) 4))
                                           (.doClick (nth (vec (.getComponents (.getComponent match-panel 2))) 3)))
                                         (= key-str (get @config :menu))
                                         (do (reset! current-card "home")
                                             (.show card-layout card-panel "home")))
                               "options" (when (= key-str (get @config :menu))
                                          (do (reset! current-card "home")
                                              (.show card-layout card-panel "home")))
                               "targeted-options" (when (= key-str (get @config :menu))
                                                    (do (reset! current-card "home")
                                                        (.show card-layout card-panel "home")))
                               "home" (when (= key-str (get @config :menu))
                                        (.show card-layout card-panel "home"))))))
                       (keyReleased [e])
                       (keyTyped [e]))]
    (.addKeyListener frame key-listener)
    (.setFocusable frame true)))

;; Main
(defn create-and-show-gui []
  (let [frame (JFrame. "S.L.A")
        main-panel (JPanel. (BorderLayout.))]
    (reset! main-frame frame)
    (apply-theme)
    (.setPreferredSize frame (Dimension. 800 600))
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (.add card-panel home-panel "home")
    (.add card-panel learn-panel "learn")
    (.add card-panel match-panel "match")
    (.add card-panel options-panel "options")
    (.add card-panel targeted-options-panel "targeted-options")
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
