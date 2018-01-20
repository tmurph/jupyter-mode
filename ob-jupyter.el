;;; ob-jupyter.el --- org-babel functions for Jupyter frontend  -*- lexical-binding: t; -*-

;; Author: Trevor Murphy <trevor.m.murphy@gmail.com>
;; Maintainer: Trevor Murphy <trevor.m.murphy@gmail.com>
;; Version: 0.1.0
;; Keywords: literate programming, reproducible research
;; URL: https://github.com/tmurph/ob-jupyter
;; Package-Requires: (dash emacs-ffi)

;; This file is not part of GNU Emacs.

;; Copyright (C) 2018 Trevor Murphy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for working with Jupyter servers.  This library
;; aims to make Emacs a full-fledged Jupyter client.

;; Much of the ZMQ FFI code has been copied without changes from John
;; Kitchin's work here:
;; http://kitchingroup.cheme.cmu.edu/blog/2017/07/13/An-Emacs-zeromq-library-using-an-ffi/
;; Used under a CC BY-AS 4.0 license.

;; The rest is very much inspired by (though not copied from) Greg
;; Sexton's ob-ipython.el here:
;; https://github.com/gregsexton/ob-ipython

;;; Code:

(require 'ffi)
(require 'hmac-def)
(require 'json)
(require 'dash)

;; Constants

(defconst ob-jupyter-delim "<IDS|MSG>"
  "The special delimiter used in the Jupyter wire protocol.")

(defconst ob-jupyter-protocol-version "5.2"
  "Messaging protocol implemented in this library.

The Jupyter message specification is versioned independently of
the packages that use it, e.g. jupyter servers and clients.

For full details see
http://jupyter-client.readthedocs.io/en/latest/messaging.html#versioning")

;; External functions

(autoload 'org-id-uuid "org-id")

;; ZMQ ffi

(define-ffi-library zmq "libzmq")

(define-ffi-function zmq-errno "zmq_errno"
  (:int "the C errno")
  nil zmq
  "retrieve the C errno as known to the 0MQ thread.
http://api.zeromq.org/4-2:zmq-errno")

(define-ffi-function zmq-strerror "zmq_strerror"
  (:pointer "Error message string")
  ((:int errnum "The C errno"))
  zmq
  "the error message string corresponding to the specified error number
http://api.zeromq.org/4-2:zmq-strerror")

(define-ffi-function zmq-ctx-new "zmq_ctx_new"
  (:pointer "Pointer to a context")
  nil zmq
  "create new ØMQ context.
http://api.zeromq.org/4-2:zmq-ctx-new")

(define-ffi-function zmq-ctx-destroy "zmq_ctx_destroy"
  (:int "status")
  ((:pointer *context)) zmq
  "terminate a ØMQ context.
http://api.zeromq.org/4-2:zmq-ctx-destroy")

(define-ffi-function zmq-socket "zmq_socket"
  (:pointer "Pointer to a socket.")
  ((:pointer *context "Created by `zmq-ctx-new '.")
   (:int type)) zmq
   "create ØMQ socket.
http://api.zeromq.org/4-2:zmq-socket")

(define-ffi-function zmq-close "zmq_close"
  (:int "Status")
  ((:pointer *socket "Socket pointer created by `zmq-socket'")) zmq
  "close ØMQ socket.
http://api.zeromq.org/4-2:zmq-close")

(define-ffi-function zmq-connect "zmq_connect"
  (:int "Status")
  ((:pointer *socket "Socket pointer created by `zmq-socket'")
   (:pointer *endpoint "Char pointer, e.g. (ffi-make-c-string \"tcp://localhost:5555\")"))
  zmq
  "create outgoing connection from socket.
http://api.zeromq.org/4-2:zmq-connect")

(define-ffi-function zmq-disconnect "zmq_disconnect"
  (:int "Status")
  ((:pointer *socket "Socket pointer created by `zmq-socket'")
   (:pointer *endpoint "Char pointer, e.g. (ffi-make-c-string \"tcp://localhost:5555\")"))
  zmq
  "disconnect from socket from endpoint.
http://api.zeromq.org/4-2:zmq-disconnect")

(define-ffi-function zmq-setsockopt "zmq_setsockopt"
  (:int "Status")
  ((:pointer *socket "Socket pointer created by `zmq-socket'")
   (:int optnam "Name of option to set")
   (:pointer *optval "Pointer to option value")
   (:size_t len "Option value length in bytes"))
  zmq
  "set socket option.
http://api.zeromq.org/4-2:zmq-setsockopt")

(define-ffi-function zmq-getsockopt "zmq_getsockopt"
  (:int "Status")
  ((:pointer *socket "Socket pointer created by `zmq-socket'")
   (:int optnam "Name of option to get")
   (:pointer *optval "Buffer to receive option value")
   (:pointer *len "Pointer to length of bytes written to OPTVAL."))
  zmq
  "get socket option.
http://api.zeromq.org/4-2:zmq-getsockopt")

(define-ffi-function zmq-send "zmq_send"
  (:int "Number of bytes sent or -1 on failure.")
  ((:pointer *socket "Pointer to a socket.")
   (:pointer *msg "Pointer to a C-string to send")
   (:size_t len "Number of bytes to send")
   (:int flags))
  zmq
  "send a message part on a socket.
http://api.zeromq.org/4-2:zmq-send")

(define-ffi-function zmq-recv "zmq_recv"
  (:int "Number of bytes received or -1 on failure.")
  ((:pointer *socket)
   (:pointer *buf "Pointer to c-string to put result in.")
   (:size_t len "Length to truncate message at.")
   (:int flags))
  zmq
  "receive a message part from a socket.
http://api.zeromq.org/4-2:zmq-recv")

;; We cannot get these through a ffi because the are #define'd for the
;; CPP and invisible in the library. They only exist in the zmq.h file.

;; socket types

(defconst ZMQ-SUB 2
  "ZMQ Subscriber socket type.

A socket of type ZMQ_SUB is used by a subscriber to subscribe to
data distributed by a publisher.  Initially a ZMQ_SUB socket is
not subscribed to any messages, use the ZMQ_SUBSCRIBE option of
zmq_setsockopt(3) to specify which messages to subscribe to.  The
zmq_send() function is not implemented for this socket type.")

(defconst ZMQ-REQ 3
  "ZMQ Request socket type.

A socket of type ZMQ_REQ is used by a client to send requests to
and receive replies from a service.  This socket type allows only
an alternating sequence of zmq_send(request) and subsequent
zmq_recv(reply) calls.  Each request sent is round-robined among
all services, and each reply received is matched with the last
issued request.")

(defconst ZMQ-DEALER 5
  "ZMQ Dealer socket type.

A socket of type ZMQ_DEALER is an advanced pattern used for
extending request/reply sockets.  Each message sent is
round-robined among all connected peers, and each message
received is fair-queued from all connected peers.")

;; socket options

(defconst ZMQ-SUBSCRIBE 6
  "ZMQ Subscriber socket option.

The ZMQ_SUBSCRIBE option shall establish a new message filter on
a ZMQ_SUB socket.  Newly created ZMQ_SUB sockets shall filter out
all incoming messages, therefore you should call this option to
establish an initial message filter.

An empty option_value of length zero shall subscribe to all
incoming messages.  A non-empty option_value shall subscribe to
all messages beginning with the specified prefix.  Multiple
filters may be attached to a single ZMQ_SUB socket, in which case
a message shall be accepted if it matches at least one filter.")

(defconst ZMQ-RCVMORE 13
  "ZMQ socket option.

The ZMQ_RCVMORE option shall return True (1) if the message part
last received from the socket was a data part with more parts to
follow.  If there are no data parts to follow, this option shall
return False (0).")

(defconst ZMQ-EVENTS 15
  "ZMQ socket option.

The ZMQ_EVENTS option shall retrieve the event state for the
specified socket.  The returned value is a bit mask constructed
by OR'ing a combination of the following event flags:

ZMQ_POLLIN --- indicates that at least one message may be
received from the specified socket without blocking.

ZMQ_POLLOUT --- indicates that at least one message may be sent
to the specified socket without blocking.")

(defconst ZMQ-POLLIN 1)
(defconst ZMQ-POLLOUT 2)

;; send/recv options

(defconst ZMQ-DONTWAIT 1
  "ZMQ send/recv option.

With send: for socket types (DEALER, PUSH) that block when there
are no available peers (or all peers have full high-water mark),
specifies that the operation should be performed in non-blocking
mode.  If the message cannot be queued on the socket, the
zmq_send() function shall fail with errno set to EAGAIN.

With recv: specifies that the operation should be performed in
non-blocking mode.  If there are no messages available on the
specified socket, the zmq_recv() function shall fail with errno
set to EAGAIN.")

(defconst ZMQ-SNDMORE 2
  "ZMQ send option.

Specifies that the message being sent is a multi-part message,
and that further message parts are to follow.  An application
that sends multi-part messages must use the ZMQ_SNDMORE flag when
sending each message part except the final one.")

;; ZMQ API

(defun zmq-error-string ()
  "Retrieve the error message string of the last ZMQ error."
  (ffi-get-c-string (zmq-strerror (zmq-errno))))

(defun zmq-receive (num socket)
  "Read a string (up to NUM bytes) from SOCKET.

May read fewer bytes if that's all that the socket has to give."
  (let ((status -1))
    (with-ffi-string (r (make-string num ? ))
      (while (< status 0)
        (setq status (zmq-recv socket r num ZMQ-DONTWAIT)))
      (substring (ffi-get-c-string r) 0 status))))

(defun zmq-receive-multi (num socket)
  "Read a multipart message (up to NUM bytes per message) from SOCKET.

Returns a list of the various parts."
  (let ((more t)
        num-bytes ret)
    (with-ffi-temporaries ((zmore :int)
                           (size :size_t))
      (with-ffi-string (recv-str (make-string num ? ))
        (ffi--mem-set size :size_t (ffi--type-size :int))
        (while more
          (setq num-bytes (zmq-recv socket recv-str num 0))
          (when (= -1 num-bytes)
            (error "Could not receive a message"))
          (push (substring (ffi-get-c-string recv-str) 0 num-bytes) ret)
          (zmq-getsockopt socket ZMQ-RCVMORE zmore size)
          (setq more (ffi--mem-ref zmore :bool)))))
    (nreverse ret)))

(defun zmq-send-multi (str-list socket)
  "Send STR-LIST as a multi-part message to SOCKET."
  (let (str flag)
    (while (setq flag 0
                 str (pop str-list))
      (when str-list
        (setq flag ZMQ-SNDMORE))
      (with-ffi-string (s str)
        (zmq-send socket s (length str) flag)))))

(defun zmq-check-for-receive (socket)
  "Check if a message may be received from SOCKET."
  (let (events)
    (with-ffi-temporaries ((zevents :int)
                           (size :size_t))
      (ffi--mem-set size :size_t (ffi--type-size :int))
      (zmq-getsockopt socket ZMQ-EVENTS zevents size)
      (setq events (ffi--mem-ref zevents :int)))
    (> (logand events ZMQ-POLLIN) 0)))

;; Authentication

(defun ob-jupyter-strings-to-unibyte (strings)
  "Convert STRINGS to UTF8 unibyte strings."
  (let (ret)
    (dolist (s strings (nreverse ret))
      (push (encode-coding-string s 'utf-8 t) ret))))

(defun ob-jupyter-hash-to-string (bytestring)
  "Convert BYTESTRING to ascii string of hex digits."
  (let (ret)
    (dolist (c (string-to-list bytestring)
               (apply #'concat (nreverse ret)))
      (push (format "%02x" c) ret))))

(defun ob-jupyter-sha256 (object)
  "Hash OBJECT with the sha256 algorithm."
  (secure-hash 'sha256 object nil nil t))

(define-hmac-function ob-jupyter-hmac-sha256
  ob-jupyter-sha256 64 32)

(advice-add 'ob-jupyter-hmac-sha256 :filter-args
            #'ob-jupyter-strings-to-unibyte)

(advice-add 'ob-jupyter-hmac-sha256 :filter-return
            #'ob-jupyter-hash-to-string)

;; Low level

(defun ob-jupyter-recv-message (socket)
  "Read a Jupyter protocol message from 0MQ SOCKET.

Returns a list of elements of the message."
  (let ((max-len 4096))
    (zmq-receive-multi max-len socket)))

(defun ob-jupyter-send-message (socket msg)
  "Send Jupyter protocol MSG to 0MQ SOCKET.

MSG is a list of elements of the message."
  (zmq-send-multi msg socket))

;; Mid level

(defun ob-jupyter-authenticate-message (key msg)
  "Error if MSG does not authenticate with and KEY.

Returns MSG unchanged if it authenticates.

Uses `ob-jupyter-hmac-sha256' to authenticate."
  (let ((orig-msg msg)
        hmac rest)
    (while (and msg (not (string= ob-jupyter-delim (pop msg)))))
    (setq hmac (pop msg)
          rest (apply #'concat msg))
    (unless (or (string= hmac "")
                (string= hmac (ob-jupyter-hmac-sha256 rest key)))
      (error (concat "Message failed to authenticate!\n"
                     "  msg: %.70s") msg))
    orig-msg))

(defun ob-jupyter-validate-header-alist (alist)
  "Error if ALIST is not a valid Jupyter protocol header section."
  (let ((keys '(msg_id username session date msg_type version))
        value)
    (dolist (key keys)
      (setq value (assq key alist))
      (unless value
        (error (concat "Header is missing a required key!\n"
                       "  key: %s") key))
      (unless (stringp (cdr value))
        (error (concat "Header value is not a string!\n"
                       "  key: %s\n"
                       "  value: %s") key value)))))

(defun ob-jupyter-validate-parent_header-alist (alist)
  "Error if ALIST is not a valid Jupyter protocol parent_header section."
  (when alist
    (ob-jupyter-validate-header-alist alist)))

(defun ob-jupyter-validate-metadata-alist (alist)
  "Error if ALIST is not a valid Jupyter protocol metadata section."
  (unless (json-encode-alist alist)
    (error (concat "Metadata is not a valid alist!\n"
                   "  meta: %.70s") alist)))

(defun ob-jupyter-validate-content-alist (alist)
  "Error if ALIST is not a valid Jupyter protocol content section."
  (unless (json-encode-alist alist)
    (error (concat "Content is not a valid alist!\n"
                   "  content: %.70s") alist)))

(defun ob-jupyter-validate-alist (alist)
  "Error if ALIST is not a valid Jupyter protocol representation.

Returns alist unchanged if it is valid.

ALIST may include the following keys:
 - ident (may be a list of IDs or just a single ID)

ALIST must include the following nested key structure:
 - header
   - msg_id
   - username
   - session
   - date
   - msg_type
   - version
 - parent_header
 - metadata
 - content

For additional details, see http://jupyter-client.readthedocs.io/en/latest/messaging.html#general-message-format"
  (let ((keys '(header parent_header metadata content)))
    (dolist (key keys alist)
      (funcall (symbol-function (intern (format
                                         "ob-jupyter-validate-%s-alist"
                                         key)))
               (cdr (assq key alist))))))

(defun ob-jupyter-signed-message-from-parts (key id-parts msg-parts)
  "Create a signed Jupyter protocol message from KEY, ID-PARTS, and MSG-PARTS.

ID-PARTS may be nil, a single string ident, or a list of string
idents.  MSG-PARTS must be a list of four strings encoding JSON
dictionaries as per the Jupyter protocol.

If KEY is nil or the empty string, don't actually sign the
message before returning."
  (let (ret)
    (if (stringp id-parts)
        (push id-parts ret)
      (dolist (elt id-parts) (push elt ret)))
    (push ob-jupyter-delim ret)
    (if (and key (not (string= key "")))
        (push (ob-jupyter-hmac-sha256 (apply #'concat msg-parts) key) ret)
      (push "" ret))
    (dolist (elt msg-parts) (push elt ret))
    (nreverse ret)))

(defun ob-jupyter-alist-from-message (msg)
  "Convert Jupyter protocol MSG (in list-of-json-str form) to alist."
  (let ((keys '(header parent_header metadata content))
        key json-str ret)
    (while (and msg (not (string= ob-jupyter-delim (pop msg)))))
    (pop msg)                             ; discard hmac
    (while (setq key (pop keys)
                 json-str (pop msg))
      (push (cons key (json-read-from-string json-str)) ret))
    (nreverse ret)))

(defun ob-jupyter-msg-parts-from-alist (alist)
  "Convert Jupyter protocol ALIST to lists of json str."
  (let ((keys '(header parent_header metadata content))
        ret)
    (dolist (key keys (nreverse ret))
      (push (json-encode-alist (cdr (assq key alist))) ret))))

(defun ob-jupyter-default-header (msg_type &optional session)
  "Create a Jupyter protocol header alist of type MSG_TYPE.

If SESSION is provided, use that as the session value.
Otherwise, generate a new session UUID."
  `((msg_id . ,(org-id-uuid))
    (username . ,(user-login-name))
    (session . ,(or session (org-id-uuid)))
    (date . ,(format-time-string "%FT%T.%6NZ" nil t))
    (msg_type . ,msg_type)
    (version . ,ob-jupyter-protocol-version)))

(defun ob-jupyter-kernel-info-request-alist ()
  "Return a Jupyter protocol request for kernel info."
  `((header ,@(ob-jupyter-default-header "kernel_info_request"))
    (parent_header)
    (metadata)
    (content)))

(defun ob-jupyter-execute-request-alist (code)
  "Return a Jupyter protocol request to execute CODE."
  `((header ,@(ob-jupyter-default-header "execute_request"))
    (parent_header)
    (metadata)
    (content
     (code . ,code)
     (silent . :json-false)
     (store_history . t)
     (user_expressions)
     (allow_stdin . :json-false)
     (stop_on_error . t))))

(defun ob-jupyter-inspect-request-alist (pos code)
  "Return a Jupyter protocol request to inspect the object at POS in CODE."
  `((header ,@(ob-jupyter-default-header "inspect_request"))
    (parent_header)
    (metadata)
    (content
     (code . ,code)
     (cursor_pos . ,pos)
     (detail_level . 0))))

(defun ob-jupyter-complete-request-alist (pos code)
  "Return a Jupyter protocol request to complete the CODE at POS."
  `((header ,@(ob-jupyter-default-header "complete_request"))
    (parent_header)
    (metadata)
    (content
     (code . ,code)
     (cursor_pos . ,pos))))

(defun ob-jupyter-shutdown-request-alist (&optional restart)
  "Return a Jupyter protocol request to shut down the kernel.

If RESTART, restart the kernel after the shutdown."
  `((header ,@(ob-jupyter-default-header "shutdown_request"))
    (parent_header)
    (metadata)
    (content
     (restart . ,(if restart t :json-false)))))

(provide 'ob-jupyter)
;;; ob-jupyter.el ends here
