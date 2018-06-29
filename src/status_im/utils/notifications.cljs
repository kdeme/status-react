(ns status-im.utils.notifications
  (:require [goog.object :as object]
            [re-frame.core :as re-frame]
            [status-im.utils.handlers :as handlers]
            [status-im.react-native.js-dependencies :as rn]
            [status-im.utils.platform :as platform]
            [status-im.ui.components.react :refer [copy-to-clipboard]]
            [taoensso.timbre :as log]))

;; Work in progress namespace responsible for push notifications and interacting
;; with Firebase Cloud Messaging.

(handlers/register-handler-db
 :update-fcm-token
 (fn [db [_ fcm-token]]
   (assoc-in db [:notifications :fcm-token] fcm-token)))

(handlers/register-handler-fx
 :request-notifications-granted
 (fn [_ _]
   (re-frame/dispatch [:show-mainnet-is-default-alert])))

(handlers/register-handler-fx
 :request-notifications-denied
 (fn [_ _]
   (re-frame/dispatch [:show-mainnet-is-default-alert])))

(def firebase (object/get rn/react-native-firebase "default"))

;; NOTE: Only need to explicitly request permissions on iOS.
(defn request-permissions []
  (-> (.requestPermission (.messaging firebase))
      (.then
       (fn [_]
         (log/debug "notifications-granted")
         (re-frame/dispatch [:request-notifications-granted {}]))
       (fn [_]
         (log/debug "notifications-denied")
         (re-frame/dispatch [:request-notifications-denied {}])))))

(defn get-fcm-token []
  (-> (.getToken (.messaging firebase))
      (.then (fn [x]
               (log/debug "get-fcm-token: " x)
               (re-frame/dispatch [:update-fcm-token x])))))

(defn on-refresh-fcm-token []
  (.onTokenRefresh (.messaging firebase)
                   (fn [x]
                     (log/debug "on-refresh-fcm-token: " x)
                     (re-frame/dispatch [:update-fcm-token x]))))

;; TODO(oskarth): Only called in background on iOS right now.
;; NOTE(oskarth): Hardcoded data keys :sum and :msg in status-go right now.
(defn on-notification []
  (.onNotification (.notifications firebase)
                   (fn [event-js]
                     (let [event (js->clj event-js :keywordize-keys true)
                           data  (select-keys event [:sum :msg])
                           aps   (:aps event)]
                       (log/debug "on-notification event: " (pr-str event))
                       (log/debug "on-notification aps: "   (pr-str aps))
                       (log/debug "on-notification data: "  (pr-str data))))))

(def channel-id "status-im")
(def channel-name "Status")
(def sound-name "bell.mp3")
(def group-id "im.status.ethereum.MESSAGE")
(def icon "ic_stat_status_notification")

(defn create-notification-channel []
  (let [channel (firebase.notifications.Android.Channel. channel-id
                                                         channel-name
                                                         firebase.notifications.Android.Importance.Max)]
    (.setSound channel sound-name)
    (.setShowBadge channel true)
    (.enableVibration channel true)
    (.. firebase
        notifications
        -android
        (createChannel channel)
        (then #(log/debug "Notification channel created:" channel-id)
              #(log/error "Notification channel creation error:" channel-id %)))))

(defn on-notification-opened []
  (.. firebase
      notifications
      (onNotificationOpened (fn [event]
                              (let [{:keys [chat-id msg] :as payload} (js->clj
                                                                       (.. event -notification -data)
                                                                       :keywordize-keys true)]
                                (log/debug "onNotificationOpened" payload)
                                (when (or chat-id msg)
                                  (re-frame/dispatch [:navigate-to-chat (or chat-id msg)])))))))

(defn init []
  (on-refresh-fcm-token)
  (on-notification)
  (on-notification-opened)
  (when platform/android?
    (create-notification-channel)))

(def notification (firebase.notifications.Notification.))

;; API reference https://rnfirebase.io/docs/v4.2.x/notifications/reference/AndroidNotification
(defn display-notification [{:keys [title body chat-id]}]
  (.. notification
      (setTitle title)
      (setBody body)
      (setData (clj->js {:chat-id chat-id}))
      (setSound sound-name)
      (-android.setChannelId channel-id)
      (-android.setAutoCancel true)
      (-android.setPriority firebase.notifications.Android.Priority.Max)
      (-android.setGroup group-id)
      (-android.setGroupSummary true)
      (-android.setSmallIcon icon))
  (.. firebase
      notifications
      (displayNotification notification)
      (then #(log/debug "Display Notification" title body))
      (then #(log/debug "Display Notification error" title body))))

(re-frame/reg-fx :display-notification-fx display-notification)
