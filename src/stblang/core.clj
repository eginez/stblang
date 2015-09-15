(ns stblang.core
  "Simple lang based on the following gramar
  Statement: Expr\n
           | 'print' Expr\n

  Expr     : Operand (Operator Operand)*
           | Assigment

  Assignment: ID '=' Expr
  Operand: Number | ID
  Operator: '+'|'-'
  Number : ('0-9')+
  ID     : ('a-z'|'A-Z')+
  "
  (:require [clojure.java.io :as io])
  (:gen-class))


(defn eof? [chr]
  (= chr -1)) 

(defn is-letter [chr]
  (and (not (nil? chr)) 
       (Character/isLetter chr)))

(defn is-number [chr]
  (and (not (nil? chr)) 
       (Character/isDigit chr)))


(defn consume-spaces [seq-tokens]
  (loop [string seq-tokens]
    (if (= (first string) \space)
      (recur (rest string))
      string)))


(defn make-ast-id-node [name]
  (if (not (nil? name))
    {:type :id :value name}
    nil))

(defmulti match (fn [token seq-tokens] 
                  (if (coll? token )
                    (class token)
                    token)))

(defmethod match :equals [token seq-tokens]
  (let [chr (first (consume-spaces seq-tokens))]
    (contains? #{\=} chr)))

(defmethod match :operator [token seq-tokens]
  (let [chr (first (consume-spaces seq-tokens))]
    (contains? #{\+ \-} chr)))

(defmethod match :id [token seq-tokens]
  (loop [string (consume-spaces seq-tokens)
         vrb []]
  (if (is-letter (first string))
    (recur (rest string) (conj vrb (first string)))
    (when (not (empty? vrb))
      {:type :id :node vrb :string string}))))

(defmethod match :number [token seq-tokens]
  (loop [string (consume-spaces seq-tokens)
         vrb []]
  (if (is-number (first string))
    (recur (rest string) (conj vrb (first string)))
    (when (not (empty? vrb))
      {:type :number :node vrb :string string}))))


(defmethod match clojure.lang.PersistentVector [token seq-tokens]
  (let [r (match (first token) seq-tokens)]
    (while (not (nil? r))
      (match (rest token) (:string r)))))
  
(defmethod match :operand [token seq-tokens]
  (or
    (match :number seq-tokens)
    (match :id seq-tokens)))


(defmethod match :assignment [token seq-tokens]
  (match [:id :equals :number] seq-tokens))
  

(defmethod match :expression [token seq-tokens]
  (or 
    (match :assigment seq-tokens)
    (match :operand seq-tokens)))


(defmethod match :statement [token seq-tokens]
  nil)




(defmethod match :default [token line start]
  (println "Error in parsing"))


(defn parse-statement [line]
  (let [start 0]
    (match :id  line start)
    ))

(defn parse-stream [stream]
  (parse-statement (.readLine stream)))



;;(defn ingest-file [stream]
;;  (loop [line (.readLine stream) ast []]
;;    (println chr)
;;    (if (eof? chr)
;;      ast
;;      (recur (.read stream) (conj ast (char chr))))))
;;

(defn main []
  (parse-stream *in*))

