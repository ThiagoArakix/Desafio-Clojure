(ns batalha-naval.teste1
  (:require [clojure.java.io :as io]))

(defrecord Jogo [tabuleiro recorde])

(defn inicializar-tabuleiro []
  (vec (repeat 4 (vec (repeat 4 \~)))))

(defn imprimir-tabuleiro [tabuleiro]
  (doseq [linha tabuleiro]
    (println (apply str (map #(cond (= % :navio) \~ (= % :encontrado) "X" :else %) linha)))))


(defn posicionar-navios-aleatorios [tabuleiro num-navios]
  (loop [tabuleiro-atual tabuleiro
         navios-posicionados 0]
    (if (= navios-posicionados num-navios)
      tabuleiro-atual
      (let [linha (rand-int (count tabuleiro))
            coluna (rand-int (count (first tabuleiro)))
            novo-tabuleiro (assoc-in tabuleiro-atual [linha coluna] :navio)]
        (recur novo-tabuleiro (inc navios-posicionados))))))

(defn posicao-valida? [linha coluna]
  (and (>= linha 0) (< linha 4)
       (>= coluna 0) (< coluna 4)))

(defn jogada-valida? [tabuleiro linha coluna]
  (and (posicao-valida? linha coluna)
       (let [posicao (get-in tabuleiro [linha coluna])]
         (not (or (= posicao :encontrado) (= posicao nil))))))



(defn algum-navio-encontrado? [tabuleiro]
  (some #(= :encontrado %) (apply concat tabuleiro)))

(defn jogo-concluido [tabuleiro recorde jogadas]
  (println "Parabéns! Você encontrou um navio!")
  (let [tempo-jogo (System/currentTimeMillis)
        novo-recorde (atualizar-recorde recorde jogadas tempo-jogo)
        mensagem-recorde (if (= recorde novo-recorde)
                           "Recorde não alcançado."
                           "Você atingiu um novo recorde!")]
    (println mensagem-recorde)
    (spit "recorde.txt" (pr-str novo-recorde))
    (imprimir-tabuleiro tabuleiro)
    (println "Jogo Concluído!")
    (println "Recorde Atual: Jogadas: " (:jogadas novo-recorde) ", Tempo: " (formatted-time (:tempo novo-recorde)))))


(defn formatted-time [time]
  (let [seconds (/ time 1000.0)]
    (format "%.3fs" seconds)))

(defn atualizar-recorde [recorde jogadas tempo]
  (let [jogadas-recorde (or (:jogadas recorde) Integer/MAX_VALUE)
        tempo-recorde (or (:tempo recorde) Long/MAX_VALUE)
        novo-recorde {:jogadas jogadas :tempo tempo}
        quebrou-recorde? (or (< jogadas jogadas-recorde)
                             (and (= jogadas jogadas-recorde)
                                  (< tempo tempo-recorde)))]
    (println "Recorde Atual: Jogadas: " jogadas-recorde ", Tempo: " (formatted-time tempo-recorde))
    (when quebrou-recorde?
      (spit "recorde.txt" (pr-str novo-recorde))
      (println "Recorde Quebrado! Novo recorde: Jogadas: " jogadas ", Tempo: " (formatted-time tempo)))
    (if quebrou-recorde? novo-recorde recorde)))


(defn jogo-loop [jogadas tabuleiro recorde]
  (let [tabuleiro-visual (vec (for [linha tabuleiro]
                                (mapv #(if (= % :navio) \~ %) linha)))]
    (println "\nJogadas: " jogadas)
    (imprimir-tabuleiro tabuleiro-visual)

    (if (algum-navio-encontrado? tabuleiro)
      (do
        (jogo-concluido tabuleiro recorde jogadas)
        {:tabuleiro tabuleiro :encerrado true}) ; Retorna um mapa com o tabuleiro e a marcação de jogo encerrado
      (let [entrada (read-line)]
        (if (= "desistir" entrada)
          (do
            (println "Você desistiu do jogo. Até a próxima!")
            {:tabuleiro tabuleiro :encerrado true}) ; Retorna um mapa com o tabuleiro e a marcação de jogo encerrado
          (if-let [[linha coluna] (and (not (empty? entrada))
                                       (map read-string (clojure.string/split entrada #" ")))]
            (if (jogada-valida? tabuleiro linha coluna)
              (if (= :navio (get-in tabuleiro [linha coluna]))
                (do
                  (println "Você encontrou um navio!")
                  (recur (inc jogadas) (assoc-in tabuleiro [linha coluna] :encontrado) recorde)) ; Chamada recursiva usando recur
                (do
                  (println "Água! Tente novamente.")
                  (recur (inc jogadas) tabuleiro recorde))) ; Chamada recursiva usando recur
              (do
                (println "Posição inválida ou já tentada. Tente novamente.")
                (recur jogadas tabuleiro recorde))) ; Chamada recursiva usando recur
            (do
              (println "Entrada inválida. Tente novamente.")
              (recur jogadas tabuleiro recorde))))))))

(defn jogar-batalha-naval []
  (println "Bem-vindo ao Jogo de Batalha Naval!")
  (println "Objetivo: Encontrar pelo menos um navio em menos jogadas e menos tempo.")
  (println "Para desistir, digite 'desistir'.")
  (println "Para fazer uma jogada, digite as coordenadas no formato 'linha coluna'. Exemplo: '1 2'.\n") ; Linha adicionada para explicar como fazer uma jogada

  (let [tabuleiro-inicial (inicializar-tabuleiro)
        num-navios 1 ; Altere o número de navios conforme necessário
        tabuleiro-com-navios (posicionar-navios-aleatorios tabuleiro-inicial num-navios)
        caminho-arquivo "C:/Users/Administrador/batalha-naval/src/batalha_naval/recorde.txt"
        recorde (if (io/file caminho-arquivo)
                  (read-string (slurp caminho-arquivo))
                  (do (spit caminho-arquivo "nil") nil))
        estado-inicial {:tabuleiro tabuleiro-com-navios :encerrado false :jogadas 0}] ; Estado inicial do jogo
    (loop [estado estado-inicial]
      (if (:encerrado estado)
        (do
          (println "Jogo encerrado. Até a próxima!")
          (recur estado-inicial)) ; Reinicia o jogo
        (let [novo-estado (jogo-loop (:jogadas estado) (:tabuleiro estado) recorde)]
          (recur (assoc novo-estado :jogadas (inc (:jogadas novo-estado))))))))) ; Continua o loop com o novo estado
