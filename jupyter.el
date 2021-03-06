;;; jupyter.el --- interact with Jupyter kernels  -*- lexical-binding: t; -*-

;; Author: Trevor Murphy <trevor.m.murphy@gmail.com>
;; Maintainer: Trevor Murphy <trevor.m.murphy@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/tmurph/jupyter-mode
;; Package-Requires: (company deferred dash emacs-ffi)

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

;; Jupyter Minor Mode aims to make Emacs a full-fledged Jupyter client.
;; The mode provides commands to inspect available kernels, start
;; inferior kernel processes, and connect buffers to existing processes.

;; This library also supports Org Babel communication with Jupyter
;; kernels.  Refer to ob-jupyter.el for details.

;; This library also supports completion with Company.  Refer to
;; company-jupyter.el for details.

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
(require 'deferred)

;; Bullshit

;;; this is exactly like `assq-delete-all' except with `equal'
;;; for some reason that's not built in
(defun jupyter--assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignore."
  (while (and (consp (car alist))
              (equal (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
               (equal (car (car tail-cdr)) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail tail-cdr))))
  alist)

(defsubst jupyter--null-from-empty-string (s)
  "If S is the empty string, return nil, else S."
  (if (string= s "") nil s))

;; Constants

(defconst jupyter--delim "<IDS|MSG>"
  "The special delimiter used in the Jupyter wire protocol.")

(defconst jupyter--protocol-version "5.2"
  "Messaging protocol implemented in this library.

The Jupyter message specification is versioned independently of
the packages that use it, e.g. jupyter servers and clients.

For full details see
http://jupyter-client.readthedocs.io/en/latest/messaging.html#versioning")

(defconst jupyter--zmq-max-recv (expt 2 19)
  "The size, in bytes, allocated to read ZMQ messages.")

;; External Definitions

(autoload 'org-id-uuid "org-id")
(autoload 'ansi-color-apply-on-region "ansi-color")

;; Customize

(defgroup jupyter nil
  "Settings for Jupyter kernel interaction."
  :prefix "jupyter-"
  :group 'languages)

(defcustom jupyter-runtime-dir "~/Library/Jupyter/runtime"
  "The directory to look for runtime connection files."
  :type 'string)

(defcustom jupyter-command "jupyter-console"
  "Command to start the interactive interpreter."
  :type 'string)

(defcustom jupyter-command-args '("--simple-prompt")
  "Default arguments for the interactive interpreter."
  :type '(repeat string))

(defcustom jupyter-poll-msec 5
  "The wait time (in msec) between polls to Jupyter sockets.

A shorter wait time increases Emacs CPU load."
  :type 'integer)

(defcustom jupyter-default-timeout-msec 1000
  "The wait time (in msec) before timing out polls to Jupyter sockets.

WARNING: if you set this to nil, polls never time out.  If for
any reason the kernel does not respond to a request then Emacs
will poll forever, hogging resources.

To recover in that case, call `deferred:clear-queue'."
  :type '(choice (integer :tag "Timeout msec")
                 (const :tag "Never time out" nil)))

;; ZMQ ffi

(define-ffi-library zmq "libzmq")

(define-ffi-function zmq--errno "zmq_errno"
  (:int "the C errno")
  nil zmq
  "retrieve the C errno as known to the 0MQ thread.
http://api.zeromq.org/4-2:zmq-errno")

(define-ffi-function zmq--strerror "zmq_strerror"
  (:pointer "Error message string")
  ((:int errnum "The C errno"))
  zmq
  "the error message string corresponding to the specified error number
http://api.zeromq.org/4-2:zmq-strerror")

(define-ffi-function zmq--ctx-new "zmq_ctx_new"
  (:pointer "Pointer to a context")
  nil zmq
  "create new ØMQ context.
http://api.zeromq.org/4-2:zmq-ctx-new")

(define-ffi-function zmq--ctx-destroy "zmq_ctx_destroy"
  (:int "status")
  ((:pointer *context)) zmq
  "terminate a ØMQ context.
http://api.zeromq.org/4-2:zmq-ctx-destroy")

(define-ffi-function zmq--socket "zmq_socket"
  (:pointer "Pointer to a socket.")
  ((:pointer *context "Created by `zmq--ctx-new '.")
   (:int type)) zmq
   "create ØMQ socket.
http://api.zeromq.org/4-2:zmq-socket")

(define-ffi-function zmq--close "zmq_close"
  (:int "Status")
  ((:pointer *socket "Socket pointer created by `zmq--socket'")) zmq
  "close ØMQ socket.
http://api.zeromq.org/4-2:zmq-close")

(define-ffi-function zmq--connect "zmq_connect"
  (:int "Status")
  ((:pointer *socket "Socket pointer created by `zmq--socket'")
   (:pointer *endpoint "Char pointer, e.g. (ffi-make-c-string \"tcp://localhost:5555\")"))
  zmq
  "create outgoing connection from socket.
http://api.zeromq.org/4-2:zmq-connect")

(define-ffi-function zmq--disconnect "zmq_disconnect"
  (:int "Status")
  ((:pointer *socket "Socket pointer created by `zmq--socket'")
   (:pointer *endpoint "Char pointer, e.g. (ffi-make-c-string \"tcp://localhost:5555\")"))
  zmq
  "disconnect from socket from endpoint.
http://api.zeromq.org/4-2:zmq-disconnect")

(define-ffi-function zmq--setsockopt "zmq_setsockopt"
  (:int "Status")
  ((:pointer *socket "Socket pointer created by `zmq--socket'")
   (:int optnam "Name of option to set")
   (:pointer *optval "Pointer to option value")
   (:size_t len "Option value length in bytes"))
  zmq
  "set socket option.
http://api.zeromq.org/4-2:zmq-setsockopt")

(define-ffi-function zmq--getsockopt "zmq_getsockopt"
  (:int "Status")
  ((:pointer *socket "Socket pointer created by `zmq--socket'")
   (:int optnam "Name of option to get")
   (:pointer *optval "Buffer to receive option value")
   (:pointer *len "Pointer to length of bytes written to OPTVAL."))
  zmq
  "get socket option.
http://api.zeromq.org/4-2:zmq-getsockopt")

(define-ffi-function zmq--send "zmq_send"
  (:int "Number of bytes sent or -1 on failure.")
  ((:pointer *socket "Pointer to a socket.")
   (:pointer *msg "Pointer to a C-string to send")
   (:size_t len "Number of bytes to send")
   (:int flags))
  zmq
  "send a message part on a socket.
http://api.zeromq.org/4-2:zmq-send")

(define-ffi-function zmq--recv "zmq_recv"
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

(defun zmq--error-string ()
  "Retrieve the error message string of the last ZMQ error."
  (ffi-get-c-string (zmq--strerror (zmq--errno))))

(defun zmq--receive (num socket)
  "Read a string (up to NUM bytes) from SOCKET.

May read fewer bytes if that's all that the socket has to give."
  (let ((status -1))
    (with-ffi-string (r (make-string num ? ))
      (while (< status 0)
        (setq status (zmq--recv socket r num ZMQ-DONTWAIT)))
      (substring (ffi-get-c-string r) 0 status))))

(defun zmq--receive-multi (num socket)
  "Read a multipart message (up to NUM bytes per message) from SOCKET.

Returns a list of the various parts."
  (let ((more t)
        num-bytes ret)
    (with-ffi-temporaries ((zmore :int)
                           (size :size_t))
      (with-ffi-string (recv-str (make-string num ? ))
        (ffi--mem-set size :size_t (ffi--type-size :int))
        (while more
          (setq num-bytes (zmq--recv socket recv-str num 0))
          (when (= -1 num-bytes)
            (error (zmq--error-string)))
          (push (substring (ffi-get-c-string recv-str) 0 num-bytes) ret)
          (zmq--getsockopt socket ZMQ-RCVMORE zmore size)
          (setq more (ffi--mem-ref zmore :bool)))))
    (nreverse ret)))

(defun zmq--send-multi (str-list socket)
  "Send STR-LIST as a multi-part message to SOCKET."
  (let (str flag written)
    (while (setq flag 0
                 str (pop str-list))
      (when str-list
        (setq flag ZMQ-SNDMORE))
      (with-ffi-string (s str)
        (setq written (zmq--send socket s (length str) flag))
        (when (= written -1)
          (error (zmq--error-string)))))))

(defun zmq--check-for-receive (socket)
  "Check if a message may be received from SOCKET."
  (let (events)
    (with-ffi-temporaries ((zevents :int)
                           (size :size_t))
      (ffi--mem-set size :size_t (ffi--type-size :int))
      (zmq--getsockopt socket ZMQ-EVENTS zevents size)
      (setq events (ffi--mem-ref zevents :int)))
    (> (logand events ZMQ-POLLIN) 0)))

;; Authentication

(defun jupyter--strings-to-unibyte (strings)
  "Convert STRINGS to UTF8 unibyte strings."
  (let (ret)
    (dolist (s strings (nreverse ret))
      (push (encode-coding-string s 'utf-8 t) ret))))

(defun jupyter--hash-to-string (bytestring)
  "Convert BYTESTRING to ascii string of hex digits."
  (let (ret)
    (dolist (c (string-to-list bytestring)
               (apply #'concat (nreverse ret)))
      (push (format "%02x" c) ret))))

(defun jupyter--sha256 (object)
  "Hash OBJECT with the sha256 algorithm."
  (secure-hash 'sha256 object nil nil t))

(define-hmac-function jupyter--hmac-sha256
  jupyter--sha256 64 32)

(advice-add 'jupyter--hmac-sha256 :filter-args
            #'jupyter--strings-to-unibyte)

(advice-add 'jupyter--hmac-sha256 :filter-return
            #'jupyter--hash-to-string)

;; Process Management

(defvar jupyter--session-kernels-alist nil
  "Internal alist of (SESSION . KERNEL) pairs.")

(defvar jupyter--session-langs-alist nil
  "Internal alist of (SESSION . LANGUAGE) pairs.")

(cl-defstruct (jupyter-struct
               (:constructor jupyter-struct--create)
               (:copier jupyter-struct--copy))
  "Jupyter kernel management object.

`jupyter-struct-kernelspec' The name of the launched kernel.

`jupyter-struct-process' The Jupyter process started by Emacs.

`jupyter-struct-buffer' The comint REPL buffer.

`jupyter-struct-conn-file-name'

`jupyter-struct-conn-file-existed-p' Whether this kernel's
connection file existed before starting the process.

`jupyter-struct-ssh-server' The server name connected via ssh, or nil.

`jupyter-struct-iopub-url' The endpoint to dynamically connect
  ZMQ socket objects to the Jupyter IOPub port.

`jupyter-struct-iopub-wait-msec' The time to give subscription
messages to propagate down dynamically connected ZMQ sockets.  As
an aside, this is the dumbest thing.  ZMQ absolutely refuses to
push messages to new SUB sockets and offers absolutely no
mechanism to check when a SUB socket is ready to receive
messages.  Maybe it's just a design flaw in Jupyter to use SUB
sockets but uggggggh.

`jupyter-struct-shell' A ZMQ socket object connected to the
Jupyter Shell port.

`jupyter-struct-context' A ZMQ context object to manage the sockets.

`jupyter-struct-key' The HMAC-SHA256 key used to authenticate
to the Jupyter server."
  (kernelspec nil :read-only t)
  (process nil :read-only t)
  (buffer nil :read-only t)
  (conn-file-name nil :read-only t)
  (cf-existed-p nil :read-only t)
  (ssh-server nil :read-only t)
  (iopub-url nil :read-only t)
  (iopub-wait-msec nil :read-only t)
  (shell nil :read-only t)
  (context nil :read-only t)
  (key nil :read-only t))

(defun jupyter--initialize-iopub (kernel)
  "Connect a new `ZMQ-SUB' socket to the IOPub URL of KERNEL.

We pull this into its own function, rather than doing it within
`jupyter--initialize-kernel', because we use multiple short-lived
subscriber sockets when sending multiple requests to the kernel
at once.  This allows us to use ZMQ to multiplex replies, rather
than having to implement that ourselves."
  (let* ((ctx (jupyter-struct-context kernel))
         (iopub-socket (zmq--socket ctx ZMQ-SUB))
         (iopub-url (jupyter-struct-iopub-url kernel))
         (iopub-wait (jupyter-struct-iopub-wait-msec kernel)))
    (with-ffi-strings ((i iopub-url)
                       (z ""))
      (zmq--connect iopub-socket i)
      (zmq--setsockopt iopub-socket ZMQ-SUBSCRIBE z 0)
      ;; give the subscribe time to propagate
      (sleep-for 0 iopub-wait))
    iopub-socket))

(defun jupyter--finalize-iopub (iopub-socket)
  "Close IOPUB-SOCKET."
  (zmq--close iopub-socket))

(defun jupyter--initialize-kernel
    (kernelspec name
                &optional conn-filename ssh-server cmd-args kernel-args)
  "Start a Jupyter KERNELSPEC and associate a comint repl.

If KERNELSPEC is nil, just use the Jupyter default (python).

The process name and comint buffer name will derive from NAME.

If no CONN-FILENAME is provided, a Jupyter connection file name
will be derived from NAME instead.

If SSH-SERVER is provided, tunnel communications over ssh.

If provided, the CMD-ARGS and KERNEL-ARGS (which must be lists) will
be passed to `jupyter-command' like so:

$ `jupyter-command' `jupyter-command-args'
  [ --existing CONN-FILENAME | -f derived-connection-file ]
  [ --ssh SSH-SERVER ]
  CMD-ARGS --kernel KERNELSPEC KERNEL-ARGS

Returns a `jupyter-struct'."
  (let* ((proc-name (format "*jupyter-%s*" name))
         (proc-buffer-name (format "*Jupyter:%s*" name))
         conn-file conn-file-args cf-existed-p full-file full-args
         proc-buf json ctx iopub-url iopub-wait-msec shell)
    (if conn-filename
        (setq conn-file conn-filename
              conn-file-args (list "--existing" conn-filename)
              cf-existed-p t)
      (setq conn-file (format "emacs-%s.json" name)
            conn-file-args (list "-f" conn-file)
            cf-existed-p nil))
    (setq full-file (expand-file-name conn-file jupyter-runtime-dir))
    (if ssh-server
        (setq full-file (progn (string-match "\\.json\\'" full-file)
                               (replace-match "-ssh.json" nil nil full-file))
              cf-existed-p nil
              iopub-wait-msec 300)
      (setq iopub-wait-msec 100))
    (setq full-args (-flatten
                     (list jupyter-command-args
                           conn-file-args
                           (and ssh-server (list "--ssh" ssh-server))
                           cmd-args
                           (and kernelspec (list "--kernel" kernelspec))
                           kernel-args)))
    ;; this creates the conn-file in `jupyter-runtime-dir'
    (setq proc-buf (apply #'make-comint-in-buffer proc-name
                          proc-buffer-name jupyter-command
                          nil full-args))
    (set-process-query-on-exit-flag (get-buffer-process proc-buf) nil)
    (while (not (file-exists-p full-file)) (sleep-for 0 5))
    ;; so we can read the file here
    (setq json (json-read-file full-file)
          iopub-url (format "%s://%s:%s"
                            (alist-get 'transport json)
                            (alist-get 'ip json)
                            (alist-get 'iopub_port json))
          ctx (zmq--ctx-new)
          shell (zmq--socket ctx ZMQ-DEALER))
    (with-ffi-strings ((s (format "%s://%s:%s"
                                  (alist-get 'transport json)
                                  (alist-get 'ip json)
                                  (alist-get 'shell_port json))))
      (zmq--connect shell s))
    (jupyter-struct--create :kernelspec kernelspec
                            :process (get-buffer-process proc-buf)
                            :buffer proc-buf
                            :conn-file-name full-file
                            :cf-existed-p cf-existed-p
                            :ssh-server ssh-server
                            :iopub-url iopub-url
                            :iopub-wait-msec iopub-wait-msec
                            :shell shell
                            :context ctx
                            :key (alist-get 'key json))))

(defun jupyter--wait-for-ready (kernel)
  "Loop until KERNEL is ready to send and receive messages.

Per the Jupyter developers, a fresh kernel may take a while
before it's ready to send and receive messages.  They check by
simply sending kernel_info_requests until they get a valid
response."
  (while (not (jupyter--validate-kernel-info
               (deferred:sync!
                 (jupyter--kernel-info-deferred kernel))))))

(defun jupyter--finalize-kernel (kernel)
  "Forcibly stop KERNEL and clean up associated ZMQ objects."
  (let ((proc (jupyter-struct-process kernel)))
    (when (process-live-p proc)
      (kill-process proc)))
  (kill-buffer (jupyter-struct-buffer kernel))
  (zmq--close (jupyter-struct-shell kernel))
  (zmq--ctx-destroy (jupyter-struct-context kernel))
  (unless (jupyter-struct-cf-existed-p kernel)
    (delete-file (jupyter-struct-conn-file-name kernel))))

(defun jupyter--initialize-session (kernel session)
  "Ask KERNEL for info to set up Emacs objects for SESSION."
  (deferred:$
    (jupyter--kernel-info-deferred kernel)
    (deferred:nextc it #'jupyter--implementation)
    (deferred:nextc it
      (lambda (interpreter)
        (jupyter--setup-inferior
         interpreter (jupyter-struct-buffer kernel)))))
  (deferred:$
    (jupyter--kernel-info-deferred kernel)
    (deferred:nextc it #'jupyter--language)
    (deferred:nextc it
      (lambda (lang)
        (push (cons session lang) jupyter--session-langs-alist)))))

(defun jupyter--acquire-session
    (session
     &optional kernelspec conn-filename ssh-server cmd-args kernel-args)
  "Return the internal pair (SESSION . kernel-struct).

If no such pair exists yet, create one with
`jupyter--initialize-kernel' and update
`jupyter--session-kernels-alist'.

If KERNELSPEC, CONN-FILENAME, SSH-SERVER, CMD-ARGS, KERNEL-ARGS
are provided, pass them to `jupyter--initialize-kernel'."
  (let ((session-cons (assoc session jupyter--session-kernels-alist))
        kernel)
    (unless session-cons
      (setq kernel (jupyter--initialize-kernel
                    kernelspec session conn-filename ssh-server
                    cmd-args kernel-args)
            session-cons (cons session kernel))
      (push session-cons jupyter--session-kernels-alist)
      (jupyter--wait-for-ready kernel)
      (jupyter--initialize-session kernel session)
      (jupyter--wait-for-ready kernel))
    session-cons))

(defun jupyter--finalize-session (session)
  "Remove SESSION from internal alists and finalize the kernel."
  (let ((kernel (cdr (assoc session jupyter--session-kernels-alist))))
    (setq jupyter--session-kernels-alist
          (jupyter--assoc-delete-all
           session jupyter--session-kernels-alist)
          jupyter--session-langs-alist
          (jupyter--assoc-delete-all
           session jupyter--session-langs-alist))
    (jupyter--finalize-kernel kernel)))

(defun jupyter--setup-inferior (interp inf-buffer)
  "Set up the appropriate major mode in INF-BUFFER according to INTERP."
  (funcall (symbol-function
            (intern (format "jupyter--setup-inferior-%s" interp)))
           inf-buffer))

;; IPython specific

(defvar python-shell--interpreter)
(defvar python-shell--interpreter-args)
(declare-function inferior-python-mode "python" nil)

(defun jupyter--setup-inferior-ipython (inf-buffer)
  "Set up inferior IPython mode in INF-BUFFER."
  (let ((python-shell--interpreter "ipython")
        (python-shell--interpreter-args
         (mapconcat #'identity (cons "-i" jupyter-command-args) " ")))
    (with-current-buffer inf-buffer
      (inferior-python-mode))))

;; Low level

(defun jupyter--recv-message (socket)
  "Read a Jupyter protocol message from 0MQ SOCKET.

Returns a list of elements of the message."
  (zmq--receive-multi jupyter--zmq-max-recv socket))

(defun jupyter--send-message (socket msg)
  "Send Jupyter protocol MSG to 0MQ SOCKET.

MSG is a list of elements of the message."
  (zmq--send-multi msg socket))

(defun jupyter--poll-deferred (socket &optional timeout)
  "Defer polling SOCKET until a reply is ready.

If TIMEOUT is not nil, will time out after TIMEOUT msec.

Returns a deferred object that can be chained with `deferred:$'."
  (deferred:new
    (deferred:lambda (elapsed)
      (cond
       ((zmq--check-for-receive socket) t)
       ((and elapsed (or timeout jupyter-default-timeout-msec)
             (> elapsed (or timeout jupyter-default-timeout-msec)))
        (error "Socket poll timed out"))
       (t (deferred:next self (+ (or elapsed 0) jupyter-poll-msec)))))))

;; Mid level

(defun jupyter--authenticate-message (key msg)
  "Error if MSG does not authenticate with and KEY.

Returns MSG unchanged if it authenticates.

Uses `jupyter--hmac-sha256' to authenticate."
  (let ((orig-msg msg)
        hmac rest)
    (while (and msg (not (string= jupyter--delim (pop msg)))))
    (setq hmac (pop msg)
          rest (apply #'concat msg))
    (unless (or (string= hmac "")
                (string= hmac (jupyter--hmac-sha256 rest key)))
      (error (concat "Message failed to authenticate!\n"
                     "  msg: %.70s") msg))
    orig-msg))

(defun jupyter--validate-header-alist (alist)
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

(defun jupyter--validate-parent_header-alist (alist)
  "Error if ALIST is not a valid Jupyter protocol parent_header section."
  (when alist
    (jupyter--validate-header-alist alist)))

(defun jupyter--validate-metadata-alist (alist)
  "Error if ALIST is not a valid Jupyter protocol metadata section."
  (unless (json-encode-alist alist)
    (error (concat "Metadata is not a valid alist!\n"
                   "  meta: %.70s") alist)))

(defun jupyter--validate-content-alist (alist)
  "Error if ALIST is not a valid Jupyter protocol content section."
  (unless (json-encode-alist alist)
    (error (concat "Content is not a valid alist!\n"
                   "  content: %.70s") alist)))

(defun jupyter--validate-alist (alist)
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
                                         "jupyter--validate-%s-alist"
                                         key)))
               (alist-get key alist)))))

(defun jupyter--signed-message-from-parts (key id-parts msg-parts)
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
    (push jupyter--delim ret)
    (if (and key (not (string= key "")))
        (push (jupyter--hmac-sha256 (apply #'concat msg-parts) key) ret)
      (push "" ret))
    (dolist (elt msg-parts) (push elt ret))
    (nreverse ret)))

(defun jupyter--alist-from-message (msg)
  "Convert Jupyter protocol MSG (in list-of-json-str form) to alist."
  (let ((keys '(header parent_header metadata content))
        key json-str ret)
    (while (and msg (not (string= jupyter--delim (pop msg)))))
    (pop msg)                             ; discard hmac
    (while (setq key (pop keys)
                 json-str (pop msg))
      (push (cons key (json-read-from-string json-str)) ret))
    (nreverse ret)))

(defun jupyter--msg-parts-from-alist (alist)
  "Convert Jupyter protocol ALIST to lists of json str."
  (let ((keys '(header parent_header metadata content))
        ret)
    (dolist (key keys (nreverse ret))
      (push (json-encode-alist (alist-get key alist)) ret))))

(defun jupyter--msg-type-from-alist (alist)
  "Extract the \"msg_type\" value from Jupyter protocol ALIST."
  (->> alist
       (assoc 'header)
       (assoc 'msg_type)
       (cdr)))

(defun jupyter--default-header (msg_type &optional session)
  "Create a Jupyter protocol header alist of type MSG_TYPE.

If SESSION is provided, use that as the session value.
Otherwise, generate a new session UUID."
  `((msg_id . ,(org-id-uuid))
    (username . ,(user-login-name))
    (session . ,(or session (org-id-uuid)))
    (date . ,(format-time-string "%FT%T.%6NZ" nil t))
    (msg_type . ,msg_type)
    (version . ,jupyter--protocol-version)))

(defun jupyter--kernel-info-request-alist ()
  "Return a Jupyter protocol request for kernel info."
  `((header ,@(jupyter--default-header "kernel_info_request"))
    (parent_header)
    (metadata)
    (content)))

(defun jupyter--execute-request-alist (code)
  "Return a Jupyter protocol request to execute CODE."
  `((header ,@(jupyter--default-header "execute_request"))
    (parent_header)
    (metadata)
    (content
     (code . ,code)
     (silent . :json-false)
     (store_history . t)
     (user_expressions)
     (allow_stdin . :json-false)
     (stop_on_error . t))))

(defun jupyter--inspect-request-alist (pos code)
  "Return a Jupyter protocol request to inspect the object at POS in CODE."
  `((header ,@(jupyter--default-header "inspect_request"))
    (parent_header)
    (metadata)
    (content
     (code . ,code)
     (cursor_pos . ,pos)
     (detail_level . 0))))

(defun jupyter--complete-request-alist (pos code)
  "Return a Jupyter protocol request to complete the CODE at POS."
  `((header ,@(jupyter--default-header "complete_request"))
    (parent_header)
    (metadata)
    (content
     (code . ,code)
     (cursor_pos . ,pos))))

(defun jupyter--shutdown-request-alist (&optional restart)
  "Return a Jupyter protocol request to shut down the kernel.

If RESTART, restart the kernel after the shutdown."
  `((header ,@(jupyter--default-header "shutdown_request"))
    (parent_header)
    (metadata)
    (content
     (restart . ,(if restart t :json-false)))))

(defun jupyter--iopub-last-p (alist)
  "Return t if ALIST is the last expected message on the IOPub channel."
  (->> alist
       (assq 'content)
       (assq 'execution_state)
       cdr
       (string= "idle")))

(defun jupyter--shell-last-p (alist)
  "Return t if ALIST is the last expected message on the Shell channel."
  (->> alist
       (assq 'header)
       (assq 'msg_type)
       cdr
       (string-match-p "reply\\'")))

(defun jupyter--shell-content-from-alist (reply-alist)
  "Extract the \"content\" alist from the \"shell\" part of REPLY-ALIST."
  (->> reply-alist
       (assoc 'shell)
       (cadr)       ; assume shell reply is a list with just one message
       (assoc 'content)))

(defun jupyter--iopub-content-from-alist (msg-type reply-alist)
  "Extract the \"content\" alist from the first IOPub message of type MSG-TYPE in REPLY-ALIST."
  (let ((rest (cdr (assoc 'iopub reply-alist)))
        alist result)
    (while (and rest (not result))
      (setq alist (car rest)
            rest (cdr rest))
      (when (string= msg-type (jupyter--msg-type-from-alist alist))
        (setq result alist)))
    (assoc 'content result)))

(defun jupyter--msg-id (alist)
  "Extract the msg_id of the header of ALIST."
  (->> alist
       (assoc 'header)
       (assoc 'msg_id)
       (cdr)))

(defun jupyter--parent-id (alist)
  "Extract the msg_id of the parent_header of ALIST."
  (->> alist
       (assoc 'parent_header)
       (assoc 'msg_id)
       (cdr)))

(defun jupyter--language (kernel-info-reply-alist)
  "Extract the kernel language from KERNEL-INFO-REPLY-ALIST."
  (->> kernel-info-reply-alist
       (jupyter--shell-content-from-alist)
       (assoc 'language_info)
       (assoc 'name)
       (cdr)))

(defun jupyter--implementation (kernel-info-reply-alist)
  "Extract the kernel implementation from KERNEL-INFO-REPLY-ALIST."
  (->> kernel-info-reply-alist
       (jupyter--shell-content-from-alist)
       (assoc 'implementation)
       (cdr)))

(defun jupyter--validate-kernel-info (kernel-info-reply-alist)
  "Return t if KERNEL-INFO-REPLY-ALIST is a complete reply.

A reply is considered \"complete\" if
 1. it contains a non-trivial 'shell component
 2. its 'iopub component contains an idle execution state message"
  (let ((iopub-content (cdr (assoc 'iopub kernel-info-reply-alist))))
    (and (jupyter--shell-content-from-alist kernel-info-reply-alist)
         (cl-some #'jupyter--iopub-last-p iopub-content))))

(defun jupyter--status (execute-reply-alist)
  "Extract the execution status from EXECUTE-REPLY-ALIST.

Returns a string, either \"ok\", \"abort\", or \"error\"."
  (->> execute-reply-alist
       (jupyter--shell-content-from-alist)
       (assoc 'status)
       (cdr)))

(defun jupyter--execute-result (execute-reply-alist)
  "Extract the IOPub \"execute_result\" from EXECUTE-REPLY-ALIST.

Returns an alist of mimetypes and contents, so like:
 \((text/plain . \"this is always here\")
  \(text/html . \"maybe this is here\"))"
  (->> execute-reply-alist
       (jupyter--iopub-content-from-alist "execute_result")
       (assoc 'data)
       (cdr)))

(defun jupyter--stream (execute-reply-alist)
  "Extract the IOPub \"stream\" from EXECUTE-REPLY-ALIST.

Returns an alist of stream data like:
 \((name . \"stdout\")
  \(text . \"contents\"))"
  (->> execute-reply-alist
       (jupyter--iopub-content-from-alist "stream")
       (cdr)))

(defun jupyter--display-data (execute-reply-alist)
  "Extract the IOPub \"display_data\" from EXECUTE-REPLY-ALIST.

Returns an alist of mimetypes and contents, so like:
 \((text/plain . \"this is always here\")
  \(image/png . \"base 64 encoded string, maybe\"))"
  (->> execute-reply-alist
       (jupyter--iopub-content-from-alist "display_data")
       (assoc 'data)
       (cdr)))

(defun jupyter--error (execute-reply-alist)
  "Extract the IOPub \"error\" data from EXECUTE-REPLY-ALIST.

Returns an alist like:
 \((traceback . [\"error tb line 1\" \"error tb line 2\"])
  \(ename . \"error name\")
  \(evalue . \"error value\"))"
  (cdr (jupyter--iopub-content-from-alist "error" execute-reply-alist)))

(defun jupyter--error-traceback-buffer (error-alist)
  "Create a buffer with the traceback from ERROR-ALIST."
  (let ((buf (get-buffer-create "*ob-jupyter-traceback*"))
        (tb (cdr (assoc 'traceback error-alist))))
    (with-current-buffer buf
      (erase-buffer)
      (mapc (lambda (line) (insert (format "%s\n" line))) tb)
      (ansi-color-apply-on-region (point-min) (point-max))
      (current-buffer))))

(defun jupyter--error-string (error-alist)
  "Format ERROR-ALIST to a string."
  (format "%s: %s" (cdr (assoc 'ename error-alist))
          (cdr (assoc 'evalue error-alist))))

(defun jupyter--raise-error-maybe (execute-reply-alist)
  "Raise an Emacs error from EXECUTE-REPLY-ALIST if appropriate.

If the error contains a traceback, attempt to display that
traceback in another window.

Return EXECUTE-REPLY-ALIST unchanged if no error."
  (let ((status (jupyter--status execute-reply-alist))
        (tb-buffer (jupyter--error-traceback-buffer
                    (jupyter--error execute-reply-alist))))
    (cond
     ((string= status "ok") execute-reply-alist)
     ((string= status "error")
      (when tb-buffer
        (display-buffer tb-buffer 'display-in-other-window))
      (error (jupyter--error-string
              (jupyter--error execute-reply-alist))))
     ((string= status "abort")
      (error "Kernel execution aborted")))))

(defun jupyter--inspect-text (inspect-reply-alist)
  "Extract the plaintext description from INSPECT-REPLY-ALIST."
  (->> inspect-reply-alist
       (jupyter--shell-content-from-alist)
       (assoc 'data)
       (assoc 'text/plain)
       (cdr)))

(defun jupyter--cursor-pos (complete-reply-alist)
  "Extract a cons like (CURSOR_START . CURSOR_END) from COMPLETE-REPLY-ALIST."
  (let* ((content (jupyter--shell-content-from-alist
                   complete-reply-alist))
         (cursor-end (cdr (assoc 'cursor_end content)))
         (cursor-start (cdr (assoc 'cursor_start content))))
    (cons cursor-start cursor-end)))

(defun jupyter--matches (complete-reply-alist)
  "Extract the list of completions from COMPLETE-REPLY-ALIST."
  (let* ((content (jupyter--shell-content-from-alist
                   complete-reply-alist))
         (matches-vector (cdr (assoc 'matches content)))
         (matches-lst (append matches-vector nil)))
    matches-lst))

;; High level API

(defun jupyter--send-alist-sync (alist socket &optional key)
  "Send Jupyter request ALIST to SOCKET.

If KEY is provided, sign messages with HMAC-SHA256 and KEY.

Block until the send completes."
  (->> alist
       (jupyter--validate-alist)
       (jupyter--msg-parts-from-alist)
       (jupyter--signed-message-from-parts key nil)
       (jupyter--send-message socket)))

(defun jupyter--recv-alist-sync (socket &optional key)
  "Receive a Jupyter reply alist from SOCKET.

If KEY is provided, authenticate messages with HMAC-SHA256 and KEY.

Block until the receive completes."
  (->> (jupyter--recv-message socket)
       (jupyter--authenticate-message key)
       (jupyter--alist-from-message)))

(defun jupyter--recv-all-deferred
    (socket parent-id last-p &optional key timeout)
  "Defer receiving a list of Jupyter reply alists from SOCKET.

If PARENT-ID is not nil, only consider replies with parent_header
msg_id of PARENT-ID.  Otherwise, consider all replies.

Loop until (funcall LAST-P alist) is not nil.

If TIMEOUT is provided, terminate early if any receive takes
longer than TIMEOUT msec.

If KEY is provided, authenticate messages with HMAC-SHA256 and KEY.

Returns a deferred object that can be chained with `deferred:$'."
  (deferred:new
    (deferred:lambda (results)
      (deferred:$
        (deferred:callback-post
          (jupyter--poll-deferred socket timeout))
        (deferred:nextc it
          (lambda () (jupyter--recv-alist-sync socket key)))
        (deferred:nextc it
          (lambda (alist)
            (when (or (not parent-id)
                      (string= (jupyter--parent-id alist) parent-id))
              (push alist results)
              alist)))
        (deferred:set-next it
          (make-deferred
           :callback (lambda (alist)
                       (if (and alist (funcall last-p alist))
                           (nreverse results)
                         (deferred:next self results)))
           :errorback (lambda () (nreverse results))))))))

(defun jupyter--roundtrip-deferred-1
    (alist shell-socket io-socket &optional key timeout)
  "Fire a Jupyter roundtrip request / reply pattern.

Send ALIST to SHELL-SOCKET and collect all messages sent back on
SHELL-SOCKET and IO-SOCKET into an alist like ((shell
shell-socket-list) (iopub io-socket-list)).

If KEY is provided, authenticate messages with HMAC-SHA256 and KEY.

If TIMEOUT is provided, stop receiving from a socket if any
receive on that socket takes longer than TIMEOUT msec.

Queues the deferred request and reply behavior, then returns the
deferred reply object, which can be chained with `deferred:$'."
  (deferred:next
    (lambda () (jupyter--send-alist-sync alist shell-socket key)))
  (deferred:parallel
    `((shell . ,(deferred:callback-post
                  (jupyter--recv-all-deferred
                   shell-socket (jupyter--msg-id alist)
                   #'jupyter--shell-last-p key timeout)))
      (iopub . ,(deferred:callback-post
                  (jupyter--recv-all-deferred
                   io-socket (jupyter--msg-id alist)
                   #'jupyter--iopub-last-p key timeout))))))

(defun jupyter--roundtrip-deferred (alist kernel &optional timeout)
  "Fire a Jupyter roundtrip request / reply pattern.

Send request ALIST to KERNEL and collect the reply.

If TIMEOUT is provided, stop receiving from a socket if any
receive on that socket takes longer than TIMEOUT msec.

Returns a deferred object that can be chained with `deferred:$'."
  (let ((iopub-socket (jupyter--initialize-iopub kernel)))
    (deferred:$
      (jupyter--roundtrip-deferred-1
       alist
       (jupyter-struct-shell kernel)
       iopub-socket
       (jupyter-struct-key kernel)
       timeout)
      (deferred:set-next it
        (make-deferred
         :callback (lambda (reply)
                     (jupyter--finalize-iopub iopub-socket)
                     reply)
         :errorback (lambda (err)
                      (jupyter--finalize-iopub iopub-socket)
                      (deferred:resignal err)))))))

(defun jupyter--kernel-info-deferred (kernel &optional timeout)
  "Fire a Jupyter kernel info request / reply roundtrip.

Query KERNEL for basic info.

If TIMEOUT is provided, stop receiving from kernel socket if any
receive on that socket takes longer that TIMEOUT msec.

Returns a deferred object that can be chained with `deferred:$'."
  (jupyter--roundtrip-deferred
   (jupyter--kernel-info-request-alist)
   kernel timeout))

(defun jupyter--execute-deferred (kernel code &optional timeout)
  "Fire a Jupyter code execution request / reply roundtrip.

Execute CODE on KERNEL.

If TIMEOUT is provided, stop receiving replies from a kernel
socket if any receive on that socket takes longer than TIMEOUT
msec.

Returns a deferred object that can be chained with `deferred:$'."
  (jupyter--roundtrip-deferred
   (jupyter--execute-request-alist code)
   kernel timeout))

(defun jupyter--inspect-deferred (kernel pos code &optional timeout)
  "Fire a Jupyter inspection request / reply roundtrip.

Query KERNEL for info on the object at POS in CODE.

If TIMEOUT is provided, stop receiving from a kernel socket if
any receive on that socket takes longer than TIMEOUT msec.

Returns a deferred object that can be chained with `deferred:$'."
  (jupyter--roundtrip-deferred
   (jupyter--inspect-request-alist pos code)
   kernel timeout))

(defun jupyter--complete-deferred (kernel pos code &optional timeout)
  "Fire a Jupyter completion request / reply roundtrip.

Query KERNEL for completion info at POS in CODE.

If TIMEOUT is provided, stop receiving from a kernel socket if
any receive on that socket takes longer than TIMEOUT msec.

Returns a deferred object that can be chained with `deferred:$'."
  (jupyter--roundtrip-deferred
   (jupyter--complete-request-alist pos code)
   kernel timeout))

;;; wtf? why doesn't this actually shut things down?
(defun jupyter--shutdown-deferred (kernel &optional restart timeout)
  "Fire a Jupyter shutdown request / reply roundtrip.

Ask KERNEL to shutdown.

If RESTART is provided, ask KERNEL to restart after shutdown.

If TIMEOUT is provided, stop receiving from a kernel socket if
any receive on that socket takes longer than TIMEOUT msec.

Returns a deferred object that can be chained with `deferred:$'."
  (jupyter--roundtrip-deferred
   (jupyter--shutdown-request-alist restart)
   kernel timeout))

;; Debug

(defvar jupyter--deferred-result nil
  "A place to store the last async result.

Handy for debugging.  Set it with `jupyter--sync-deferred'.")

(defun jupyter--sync-deferred (d)
  "Fire deferred object D and save the result to `jupyter--deferred-result'."
  (deferred:watch d
    (lambda (reply) (setq jupyter--deferred-result reply)))
  (deferred:callback d))

(defun jupyter--flush-kernel (kernel)
  "Receive any backed-up messages from KERNEL.

Pretty prints the results to *jupyter-debug* buffer."
  (let ((shell-port (jupyter-struct-shell kernel))
        (iopub-port (jupyter--initialize-iopub kernel))
        (key (jupyter-struct-key kernel))
        (debug-buf (get-buffer-create "*jupyter-debug*")))
    (deferred:$
      (deferred:parallel
        `((shell . ,(deferred:callback-post
                      (jupyter--recv-all-deferred
                       shell-port nil
                       #'jupyter--shell-last-p key 1000)))
          (iopub . ,(deferred:callback-post
                      (jupyter--recv-all-deferred
                       iopub-port nil
                       #'jupyter--iopub-last-p key 1000)))))
      (deferred:nextc it
        (lambda (alist)
          (jupyter--finalize-iopub iopub-port)
          (with-current-buffer debug-buf (erase-buffer))
          (pp alist debug-buf)
          (display-buffer debug-buf))))))

;;; Emacs

;; Minor Mode

(defvar-local jupyter--current-kernel nil
  "The Jupyter kernel struct associated with the current buffer.")

(defun jupyter--spec-name (kernelspec-alist)
  "Extract the name from KERNELSPEC-ALIST."
  (symbol-name (car kernelspec-alist)))

(defun jupyter--list-kernelspecs ()
  "Return a list of available Jupyter kernelspecs."
  (mapcar #'jupyter--spec-name
          (cdar (json-read-from-string
                 (shell-command-to-string
                  "jupyter-kernelspec list --json")))))

;;;###autoload
(defun jupyter-initialize-session
    (session
     &optional kernelspec conn-filename ssh-server cmd-args kernel-args)
  "Initialize a new Jupyter SESSION.

Returns the internal pair (SESSION . kernel-struct).

If SESSION has already been initialized, return the pair but do
not start a new session.

If no session pair exists yet, create one with
`jupyter--initialize-kernel' and update
`jupyter--session-kernels-alist'.

If KERNELSPEC, CONN-FILENAME, SSH-SERVER, CMD-ARGS, KERNEL-ARGS
are provided, pass them to `jupyter--initialize-kernel'."
  (interactive (list
                (completing-read
                 "Session: " jupyter--session-kernels-alist nil 'confirm
                 nil nil "default")
                nil nil nil nil nil))
  (let ((kernel-cons (assoc session jupyter--session-kernels-alist)))
    (unless kernel-cons
      (setq kernelspec (or kernelspec
                           (completing-read
                            "Start new kernel: "
                            (jupyter--list-kernelspecs)
                            nil t))
            conn-filename (or conn-filename
                              (completing-read
                               "(Optional) Use an existing connection? "
                               (directory-files jupyter-runtime-dir nil
                                                "json\\'")
                               nil t))
            ssh-server (or ssh-server
                           (read-from-minibuffer "(Optional) SSH server? "))
            cmd-args (or cmd-args
                         (read-from-minibuffer "(Optional) Additional Jupyter command args? "))
            kernel-args (or kernel-args
                            (read-from-minibuffer "(Optional) Additional kernel args? "))
            kernel-cons (apply #'jupyter--acquire-session
                               session
                               (mapcar #'jupyter--null-from-empty-string
                                       (list kernelspec conn-filename
                                             ssh-server cmd-args
                                             kernel-args)))))
    (display-buffer (jupyter-struct-buffer (cdr kernel-cons)))
    kernel-cons))

(defalias 'jupyter-open-session 'jupyter-initialize-session)

;;;###autoload
(defun jupyter-connect (session)
  "Connect the current buffer to the kernel associated with SESSION.

If no kernel is currently associated with SESSION, initialize one."
  (interactive (list
                (completing-read
                 "Session: " jupyter--session-kernels-alist nil 'confirm)))
  (let* ((kernel-cons (jupyter-initialize-session session))
         (kernel (cdr kernel-cons)))
    (setq-local jupyter--current-kernel kernel)
    (jupyter-mode +1)
    (display-buffer (jupyter-struct-buffer kernel))))

(defalias 'jupyter-close-session 'jupyter-finalize-session)

(defun jupyter-finalize-session (session)
  "Finalize the kernel associated with SESSION."
  (interactive (list
                (completing-read
                 "Session: " jupyter--session-kernels-alist nil t)))
  (jupyter--finalize-session session))

(defalias 'jupyter-close-all 'jupyter-finalize-all)

(defun jupyter-finalize-all ()
  "Finalize all existing sessions."
  (interactive)
  (mapc #'jupyter--finalize-session
        (mapcar #'car jupyter--session-kernels-alist)))

(define-minor-mode jupyter-mode
  "Utilities for working with connected Jupyter kernels."
  nil " Jupyter" nil
  (let* ((lang (substring (symbol-name major-mode)
                          0 (- (length "-mode"))))
         (hook-var (intern (format "jupyter-%s-mode-hook" lang))))
    (if jupyter-mode
        (if jupyter--current-kernel
            (run-hooks hook-var)
          (jupyter-mode -1)
          (message "%s"
                   (concat "No active Jupyter kernel for current buffer"
                           "\n\n"
                           "Use `jupyter-connect' to connect to a "
                           "kernel session")))
      (setq jupyter--current-kernel nil)
      (run-hooks hook-var))))

;; Compatibility

;; `desktop-mode' tries to restore minor modes by default.  But
;; `jupyter-mode' is more of a hack, not always like Emacs minor modes.
;; In particular, it should never start up without manual user
;; intervention.  We can tell desktop to leave well enough alone with
;; `desktop-minor-mode-handlers'.
(defvar desktop-buffer-name)
(defun jupyter--desktop-handler ()
  (message (concat
            (format "`jupyter-mode' not restored in buffer %s."
                    desktop-buffer-name)
            "\n\n"
            "Use `jupyter-connect' to connect to a kernel session.")))
(add-to-list 'desktop-minor-mode-handlers
             '(jupyter-mode jupyter--desktop-handler))

;; Python specific

(defvar-local jupyter--original-python-shell-buffer-name nil)
(defvar-local jupyter--original-python-shell-interpreter nil)
(defvar-local jupyter--original-python-shell-interpreter-args nil)
(defvar python-shell-buffer-name)
(defvar python-shell-interpreter)
(defvar python-shell-interpreter-args)
(declare-function org-babel-python-without-earmuffs "ob-python" (session))

(defun jupyter--python-shell-buffer-name ()
  "Toggle `python-shell-buffer-name' in the current kernel buffer."
  (if jupyter-mode
      (progn
        (setq-local jupyter--original-python-shell-buffer-name
                    python-shell-buffer-name)
        (setq-local python-shell-buffer-name
                    (org-babel-python-without-earmuffs
                     (buffer-name
                      (jupyter-struct-buffer jupyter--current-kernel)))))
    (setq-local python-shell-buffer-name
                jupyter--original-python-shell-buffer-name)))

(defun jupyter--python-shell-interpreter ()
  "Toggle `python-shell-interpreter' in the current kernel buffer."
  (if jupyter-mode
      (progn
        (setq-local jupyter--original-python-shell-interpreter
                    python-shell-interpreter)
        (setq-local python-shell-interpreter
                    jupyter-command))
    (setq-local python-shell-interpreter
                jupyter--original-python-shell-interpreter)))

(defun jupyter--python-shell-interpreter-args ()
  "Toggle `python-shell-interpreter-args' in the current kernel buffer."
  (if jupyter-mode
      (progn
        (setq-local jupyter--original-python-shell-interpreter-args
                    python-shell-interpreter-args)
        (setq-local python-shell-interpreter-args
                    (format "%s"
                            (-flatten
                             (list
                              jupyter-command-args
                              "--existing" (jupyter-struct-conn-file-name
                                            jupyter--current-kernel))))))
    (setq-local python-shell-interpreter-args
                jupyter--original-python-shell-interpreter-args)))

(add-hook 'jupyter-python-mode-hook #'jupyter--python-shell-buffer-name)
(add-hook 'jupyter-python-mode-hook #'jupyter--python-shell-interpreter)
(add-hook 'jupyter-python-mode-hook #'jupyter--python-shell-interpreter-args)

(provide 'jupyter)
;;; jupyter.el ends here
