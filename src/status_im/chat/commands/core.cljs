(ns status-im.chat.commands.core
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [status-im.chat.commands.protocol :as protocol]
            [status-im.chat.commands.impl.transactions :as transactions]
            [status-im.chat.models.input :as input-model]))

(def ^:private arg-wrapping-char "\"")
(def ^:private command-char "/")
(def ^:private space-char " ")

(def register
  "Register of all commands. Whenever implementing a new command,
  provide the implementation in the `status-im.chat.commands.impl.*` ns,
  and add its instance here."
  #{(transactions/PersonalSendCommand.)})

(defn validate-and-send
  "Validates and sends command in current chat"
  [command cofx]
  nil)

(defn send
  "Sends command with given arguments in particular chat"
  [command chat-id cofx]
  nil)

(def command-id (juxt protocol/id protocol/scope))

(defn- prepare-params
  "Prepares parameters sequence of command by providing suggestion components with
  selected-event injected with correct arg indexes and `last-arg?` flag."
  [command]
  (let [parameters     (protocol/parameters command)
        last-param-idx (dec (count parameters))]
    (into []
          (map-indexed (fn [idx {:keys [suggestions] :as param}]
                         (if suggestions
                           (update param :suggestions partial
                                   (fn [value]
                                     [:set-command-parameter
                                      (= idx last-param-idx) idx value]))
                           param))
                       parameters))))

(defn- add-exclusive-choices [initial-scope exclusive-choices]
  (reduce (fn [scopes-set exclusive-choices]
            (reduce (fn [scopes-set scope]
                      (let [exclusive-match (set/intersection scope exclusive-choices)]
                        (if (seq exclusive-match)
                          (reduce conj
                                  (disj scopes-set scope)
                                  (map (partial conj
                                                (set/difference scope exclusive-match))
                                       exclusive-match))
                          scopes-set)))
                    scopes-set
                    scopes-set))
          #{initial-scope}
          exclusive-choices))

(defn index-commands
  "Takes collecton of things implementing the command protocol, and
  correctly indexes them  by their composite ids and access scopes."
  [commands {:keys [db]}]
  (let [id->command              (reduce (fn [acc command]
                                           (assoc acc (command-id command)
                                                  {:type   command
                                                   :params (prepare-params command)}))
                                         {}
                                         commands)
        access-scope->command-id (reduce-kv (fn [acc command-id {:keys [type]}]
                                              (let [access-scopes (add-exclusive-choices
                                                                   (protocol/scope type)
                                                                   protocol/or-scopes)]
                                                (reduce (fn [acc access-scope]
                                                          (update acc
                                                                  access-scope
                                                                  (fnil conj #{})
                                                                  command-id))
                                                        acc
                                                        access-scopes)))
                                            {}
                                            id->command)]
    {:db (assoc db
                :id->command              id->command
                :access-scope->command-id access-scope->command-id)}))

(defn chat-commands
  "Takes `id->command`, `access-scope->command-id` and `chat` parameters and returns
  entries map of `id->command` map eligible for given chat.
  Note that the result map is keyed just by `protocol/id` of command entries,
  not the unique composite ids of the global `id->command` map.
  That's because this function is already returning local commands for particular
  chat and locally, they should always have unique `protocol/id`."
  [id->command access-scope->command-id {:keys [chat-id group-chat public?]}]
  (let [global-access-scope (cond-> #{}
                              (not group-chat) (conj :personal-chats)
                              (and group-chat (not public?)) (conj :group-chats) 
                              public? (conj :public-chats)) 
        chat-access-scope   #{chat-id}]
    (reduce (fn [acc command-id]
              (let [{:keys [type] :as command-props} (get id->command command-id)]
                (assoc acc (protocol/id type) command-props)))
            {} 
            (concat (get access-scope->command-id global-access-scope)
                    (get access-scope->command-id chat-access-scope)))))

(defn- current-param-position [input-text selection]
  (when selection
    (when-let [subs-input-text (subs input-text 0 selection)]
      (let [input-params   (input-model/split-command-args subs-input-text)
            param-index    (dec (count input-params))
            wrapping-count (get (frequencies subs-input-text) arg-wrapping-char 0)]
        (if (and (string/ends-with? subs-input-text space-char)
                 (even? wrapping-count))
          param-index
          (dec param-index))))))

(defn- command-completion [input-params params]
  (let [input-params-count (count input-params)
        params-count       (count params)]
    (cond
      (= input-params-count params-count) :complete
      (< input-params-count params-count) :less-then-needed
      (> input-params-count params-count) :more-than-needed)))

(defn selected-chat-command
  "Takes input text, text-selection and `protocol/id->command-props` map (result of 
  the `chat-commands` fn) and returns the corresponding `command-props` entry, 
  or nothing if input text doesn't match any available command.
  Besides keys `:params` and `:type`, the returned map contains: 
  * `:input-params` - parsed parameters from the input text as map of `param-id->entered-value`
  # `:current-param-position` - index of the parameter the user is currently focused on (cursor position
  in relation to parameters), could be nil if the input is not selected
  # `:command-completion` - indication of command completion, possible values are `:complete`,
  `:less-then-needed` and `more-then-needed`"
  [input-text text-selection id->command-props]
  (when (input-model/starts-as-command? input-text)
    (let [[command-name & input-params] (input-model/split-command-args input-text)]
      (when-let [{:keys [params] :as command-props} (get id->command-props (subs command-name 1))] ;; trim leading `/` for lookup
        command-props
        (let [input-params (into {}
                                 (keep-indexed (fn [idx input-value]
                                                 (when (not (string/blank? input-value))
                                                   (when-let [param-name (get-in params [idx :id])] 
                                                     [param-name input-value]))))
                                 input-params)]
          (assoc command-props
                 :input-params input-params 
                 :current-param-position (current-param-position input-text text-selection)
                 :command-completion (command-completion input-params params)))))))

(defn set-command-parameter
  "Set value as command parameter for the current chat"
  [last-param? param-index value {:keys [db]}]
  (let [{:keys [current-chat-id]} db
        [command & params]  (-> (get-in db [:chats current-chat-id :input-text])
                                input-model/split-command-args)
        param-count         (count params)
        ;; put the new value at the right place in parameters array
        new-params          (cond-> (into [] params)
                              (< param-index param-count) (assoc param-index value)
                              (>= param-index param-count) (conj value))
        ;; if the parameter is not the last one for the command, add space
        input-text                (cond-> (str command space-char
                                               (input-model/join-command-args
                                                new-params))
                                    (and (not last-param?)
                                         (or (= 0 param-count)
                                             (= param-index (dec param-count))))
                                    (str space-char))]
    {:db (assoc-in db [:chats current-chat-id :input-text]
                   (input-model/text->emoji input-text))}))
