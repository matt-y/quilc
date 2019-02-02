;;;; printers.lisp
;;;;
;;;; Author: Eric Peterson
;;;;
;;;; stuff responsible for writing quil out to something readable / processable
;;;; all goes in here.

(in-package #:quilc)


(declaim (special *without-pretty-printing*
                  *statistics-dictionary*
                  *human-readable-stream*
                  *json-stream*))

(defun first-column-operator= (mat1 mat2)
  (multiple-value-bind (mat1 mat2) (quil::matrix-rescale mat1 mat2)
    (setf mat1 (quil::scale-out-matrix-phases mat1 mat2))
    (quil::matrix-first-column-equality mat1 mat2)))

(defun operator= (mat1 mat2)
  (multiple-value-bind (mat1 mat2) (quil::matrix-rescale mat1 mat2)
    (setf mat1 (quil::scale-out-matrix-phases mat1 mat2))
    (quil::matrix-equality mat1 mat2)))

(defun matrix-equals-dwim (mat1 mat2)
  "Returns true if mat1 is equal to mat2, with the specific notion of equality
depending on whether *ENABLE-STATE-PREP-COMPRESSION* is enabled."
  (funcall (if quil::*enable-state-prep-compression*
               #'first-column-operator=
               #'operator=)
           mat1
           mat2))

(defun print-matrix-representations (processed-program original-matrix)
  (let* ((processed-program-matrix (quil::gate-applications-to-logical-matrix processed-program :compress-qubits t))
         (same-same-but-different (quil::scale-out-matrix-phases processed-program-matrix
                                                                 original-matrix)))
    (format *human-readable-stream* "~%#Matrix read off from input code~%")
    (print-matrix-with-comment-hashes original-matrix *human-readable-stream*)
    (format *human-readable-stream* "~%#Matrix read off from compiled code~%")
    (print-matrix-with-comment-hashes same-same-but-different *human-readable-stream*)
    (format *human-readable-stream* "~%")
    (format *human-readable-stream* "Matrices are~a equal~%" (if (matrix-equals-dwim original-matrix same-same-but-different) "" " "))
    (finish-output *human-readable-stream*)))

(defun print-gate-depth (lschedule)
  (let ((depth (quil::lscheduler-calculate-depth lschedule)))
    (setf (gethash "gate_depth" *statistics-dictionary*) depth)
    (format *human-readable-stream*
            "# Compiled gate depth: ~d~%"
            depth)))

(defun print-2Q-gate-depth (lschedule)
  (let ((depth (quil::lscheduler-calculate-depth lschedule)))
    (setf (gethash "multiqubit_gate_depth" *statistics-dictionary*) depth)
    (format *human-readable-stream*
            "# Compiled multiqubit gate depth: ~d~%"
            depth)))

(defun print-gate-volume (lschedule)
  (let ((volume (quil::lscheduler-calculate-volume lschedule)))
    (setf (gethash "gate_volume" *statistics-dictionary*) volume)
    (format *human-readable-stream*
            "# Compiled gate volume: ~d~%"
            volume)))

(defun print-unused-qubits (lschedule chip-specification)
  (let* ((lscheduler-resources
           (let ((collect (quil::make-null-resource)))
             (quil::lscheduler-walk-graph
              lschedule
              :bump-value (lambda (instr value)
                            (setf collect
                                  (quil::resource-union collect
                                                        (quil::instruction-resources instr)))
                            value))
             collect))
         (unused-qubits
           (loop :for i :below (length (quil::vnth 0 (quil::chip-specification-objects chip-specification)))
                 :unless (quil::resources-intersect-p (quil::make-qubit-resource i)
                                                      lscheduler-resources)
                   :collect i)))
    (setf (gethash "unused_qubits" *statistics-dictionary*) unused-qubits)
    (format *human-readable-stream*
            "# Unused qubit list: ~{~a~^, ~}~%"
            unused-qubits)))

(defun print-program-runtime (lschedule chip-specification)
  (let ((duration (quil::lscheduler-calculate-duration lschedule
                                                       chip-specification)))
    (setf (gethash "program_duration" *statistics-dictionary*) duration)
    (format *human-readable-stream*
            "# Compiled program duration: ~5d~%"
            duration)))

(defun print-program-fidelity (lschedule chip-specification)
  (let ((fidelity (quil::lscheduler-calculate-fidelity lschedule
                                                       chip-specification)))
    (setf (gethash "program_fidelity" *statistics-dictionary*) fidelity)
    (format *human-readable-stream*
            "# Estimated compiled program fidelity: ~5d~%"
            fidelity)))

(defun print-topological-swap-count (topological-swaps)
  (setf (gethash "topological_swaps" *statistics-dictionary*) topological-swaps)
  (format *human-readable-stream*
          "# SWAPs incurred by topological considerations: ~d~%"
          topological-swaps))

(defun print-program (processed-program &optional (stream *standard-output*))
  (let ((program-as-string
          (with-output-to-string (s)
            (let ((quil::*print-fractional-radians* (not *without-pretty-printing*)))
              (quil::print-parsed-program processed-program s)))))
    (setf (gethash "processed_program" *statistics-dictionary*)
          program-as-string)
    (write-string program-as-string stream)))

(defun publish-json-statistics ()
  (yason:encode *statistics-dictionary* *json-stream*)
  *statistics-dictionary*)


;; custom encoder for rewiring objects
(defmethod yason:encode ((object quil::rewiring) &optional (stream *standard-output*))
  (yason:encode (quil::rewiring-l2p object) stream))
