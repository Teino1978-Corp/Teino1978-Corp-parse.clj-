(defn parse-form [down-tok?, up-tok?, [tok & toks]]
  (loop [frm [tok], toks toks]
    (if-let [[tok2 & toks2] (seq toks)]
      (cond
        (down-tok? tok2) (let [[res toks3] (parse-form down-tok?, up-tok?, toks)]
                          (recur (conj frm res) toks3))
        (up-tok? tok2) [(conj frm tok2) toks2]
        :else         (recur (conj frm tok2) toks2))
      [frm nil])))

(defn parse [down-tok?, up-tok?, toks]
  (loop [top [], toks toks]
    (if-let [[tok & rtoks] (seq toks)]
      (if (down-tok? tok)
        (let [[res toks2] (parse-form down-tok? up-tok? toks)]
          (recur (conj top res) toks2))
        (recur (conj top tok) rtoks))
      top)))