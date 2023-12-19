(ns batalha-naval.teste2)
(defn posicao-aleatoria []
  {:linha (rand-int 4)
   :coluna (char (+ (int \A) (rand-int 4)))})

(defn exibe-tabuleiro [tabuleiro]
  (doseq [linha tabuleiro]
    (println (apply str linha))))

(defn cria-tabuleiro []
  (vec (replicate 4 (vec (replicate 4 \.)))))

(defn coluna-letra->indice [letra]
  (- (int letra) (int \A)))

(defn atingiu-navio? [tabuleiro palpite]
  (= (get-in tabuleiro palpite) \N))

(defn atualiza-tabuleiro [tabuleiro palpite valor]
  (let [linha (Integer. (subs (str palpite) 1))
        coluna (coluna-letra->indice (str (first palpite)))]
    (assoc-in tabuleiro [linha coluna] valor)))


(defn main []
  (println "Bem-vindo ao Jogo de Batalha Naval!")
  (let [posicao-navio (posicao-aleatoria)
        linha-navio (-> posicao-navio :linha)
        coluna-navio (coluna-letra->indice (-> posicao-navio :coluna))
        tabuleiro (atualiza-tabuleiro (cria-tabuleiro) [linha-navio coluna-navio] \N)]
    (loop [tabuleiro tabuleiro]
      (println "Tabuleiro:")
      (exibe-tabuleiro tabuleiro)
      (println "Faça seu palpite (ex: A1): ")
      (let [palpite (re-matches #"\b([A-D])([0-3])\b" (read-line))]
        (if palpite
          (let [[_ letra numero] palpite
                linha (Integer. numero)
                coluna (coluna-letra->indice letra)]
            (if (and (<= 0 linha 3) (<= 0 coluna 3))
              (let [novo-tabuleiro (if (atingiu-navio? tabuleiro [linha coluna])
                                     (do (println "Você acertou o navio!")
                                         (atualiza-tabuleiro tabuleiro [linha coluna] \N))
                                     (do (println "Você errou.")
                                         (atualiza-tabuleiro tabuleiro [linha coluna] \O)))]
                (if (some #{\N} (flatten novo-tabuleiro))
                  (do (println "Parabéns! Você afundou o navio.")
                      (exibe-tabuleiro novo-tabuleiro))
                  (recur novo-tabuleiro)))
              (println "Coordenadas inválidas! Tente novamente.")))
          (println "Formato de entrada inválido! Tente novamente."))))))
