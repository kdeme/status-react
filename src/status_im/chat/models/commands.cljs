(ns status-im.chat.models.commands
  (:require [status-im.chat.constants :as chat-consts]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

(defn- resolve-references
  [contacts name->ref]
  (reduce-kv (fn [acc name ref]
               (assoc acc name (get-in contacts ref)))
             {}
             name->ref))

(defn command-name [{:keys [name]}]
  (str chat-consts/command-char name))

(defn commands-responses [_ _ _ _ _])
#_(defn commands
  "Returns map of commands eligible for current chat."
  [id->command access-scope->command-id {:keys [chat-id group-chat public?]}]
  (let [global-access-scope (cond-> #{}
                              (not group-chat) (conj :personal-chats)
                              group-chat (conj :group-chats) 
                              public? (conj :public-chats)) 
        chat-access-scope   (conj global-access-scope chat-id)] 
    (reduce (fn [acc command-id]
              )
            {}
            (into (get-in id->command global-access-scope)
                  (get-in id->command chat-access-scope)))
    
    (reduce (fn [acc access-scope]
              (merge acc (resolve-references all-contacts
                                             (get-in access-scope->commands-responses [access-scope type]))))
            {}
            (cons global-access-scope member-access-scopes))))

(defn requested-responses
  "Returns map of requested command responses eligible for current chat."
  [access-scope->commands-responses account chat contacts requests]
  (let [requested-responses (map :response requests)
        responses-map (commands-responses :response access-scope->commands-responses account chat contacts)]
    (select-keys responses-map requested-responses)))
