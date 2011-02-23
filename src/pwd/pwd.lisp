(in-package :clap-pwd)

(defclass passwd ()
  ((pw-name :initarg :pw-name
            :accessor pw-name)
   (pw-passwd :initarg :pw-passwd
              :accessor pw-passwd)
   (pw-uid :initarg :pw-uid
           :accessor pw-uid)
   (pw-gid :initarg :pw-gid
           :accessor pw-gid)
   (pw-gecos :initarg :pw-gecos
             :accessor pw-gecos)
   (pw-dir :initarg :pw-dir
           :accessor pw-dir)
   (pw-shell :initarg :pw-shell
             :accessor pw-shell))
  (:documentation
   "a class of password database entry"))

(defmethod print-object ((object passwd) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~S ~S ~S ~S ~S ~S"
            (pw-name object) (pw-passwd object)
            (pw-uid object) (pw-gid object)
            (pw-gecos object) (pw-dir object) (pw-shell object))))

(defun getpwuid (uid)
  "return the password database entry of the user specified by UID"
  (multiple-value-bind
        (name passwd uid gid gecos dir shell)
      (osicat-posix:getpwuid uid)
    (make-instance 'passwd
                   :pw-name name
                   :pw-passwd passwd
                   :pw-uid uid
                   :pw-gid gid
                   :pw-gecos gecos
                   :pw-dir dir
                   :pw-shell shell)))

(defun getpwnam (name)
  "return the password database entry of the specified by NAME string"
  (multiple-value-bind
        (name passwd uid gid gecos dir shell)
      (osicat-posix:getpwnam name)
    (make-instance 'passwd
                   :pw-name name
                   :pw-passwd passwd
                   :pw-uid uid
                   :pw-gid gid
                   :pw-gecos gecos
                   :pw-dir dir
                   :pw-shell shell)))

;; getpwent, setpwent, endpwent is not defined in osicat
(osicat-posix::defsyscall "setpwent" :void)
(osicat-posix::defsyscall "endpwent" :void)
(osicat-posix::defsyscall ("getpwent" %getpwent) :pointer)

(defun getpwent ()
  (osicat-posix:set-errno 0)
  (labels ((passwd-ref (ptr name)
             (cffi:foreign-slot-value ptr 'osicat-posix::passwd name)))
    (handler-case
      (let ((ptr (%getpwent)))
        (values (passwd-ref ptr 'osicat-posix::name)
                (passwd-ref ptr 'osicat-posix::passwd)
                (passwd-ref ptr 'osicat-posix::uid)
                (passwd-ref ptr 'osicat-posix::gid)
                (passwd-ref ptr 'osicat-posix::gecos)
                (passwd-ref ptr 'osicat-posix::dir)
                (passwd-ref ptr 'osicat-posix::shell)))
      (osicat-posix:posix-error (cond)
        (if (= (osicat-sys:system-error-code cond) 0) ;no error
            nil
            (error cond))))))

(defun getpwall ()
  "return a list of all the password database entries"
  (setpwent)
  (unwind-protect
       (let ((pws nil))
         (loop
            while
              (multiple-value-bind
                    (name passwd uid gid gecos dir shell)
                  (getpwent)
                (if (and (null name) (null passwd) (null uid) (null gid)
                         (null gecos) (null dir) (null shell))
                    nil
                    (push 
                     (make-instance 'passwd
                                    :pw-name name
                                    :pw-passwd passwd
                                    :pw-uid uid
                                    :pw-gid gid
                                    :pw-gecos gecos
                                    :pw-dir dir
                                    :pw-shell shell)
                     pws))))
         (nreverse pws))
    (getpwent)))
