;;;; PAOS - Performance Optimization
;;;; Batching, caching, and performance monitoring

(in-package #:paos/performance)

;;; ============================================================================
;;; Caching Layer
;;; ============================================================================

(defclass cache-entry ()
  ((key :initarg :key :accessor cache-key)
   (value :initarg :value :accessor cache-value)
   (timestamp :initarg :timestamp :accessor cache-timestamp)
   (ttl :initarg :ttl :accessor cache-ttl :initform 60)))

(defvar *cache* (make-hash-table :test 'equal)
  "Global cache for expensive operations.")

(defvar *cache-hits* 0
  "Cache hit counter.")

(defvar *cache-misses* 0
  "Cache miss counter.")

(defun cache-get (key)
  "Get value from cache if not expired."
  (let ((entry (gethash key *cache*)))
    (if (and entry (not (cache-expired-p entry)))
        (progn
          (incf *cache-hits*)
          (cache-value entry))
        (progn
          (incf *cache-misses*)
          nil))))

(defun cache-set (key value &key (ttl 60))
  "Set cache value with TTL in seconds."
  (setf (gethash key *cache*)
        (make-instance 'cache-entry
                      :key key
                      :value value
                      :timestamp (get-universal-time)
                      :ttl ttl)))

(defun cache-expired-p (entry)
  "Check if cache entry is expired."
  (> (- (get-universal-time) (cache-timestamp entry))
     (cache-ttl entry)))

(defun cache-clear ()
  "Clear all cache entries."
  (clrhash *cache*)
  (setf *cache-hits* 0)
  (setf *cache-misses* 0)
  (log-info "Cache cleared"))

(defun cache-stats ()
  "Get cache statistics."
  (let ((total (+ *cache-hits* *cache-misses*)))
    (list :hits *cache-hits*
          :misses *cache-misses*
          :hit-rate (if (> total 0)
                       (* 100.0 (/ *cache-hits* total))
                       0)
          :entries (hash-table-count *cache*))))

;;; ============================================================================
;;; Batched Status Polling
;;; ============================================================================

(defmacro with-cached-results ((key &key ttl) &body body)
  "Execute body with caching."
  `(or (cache-get ,key)
       (let ((result (progn ,@body)))
         (cache-set ,key result :ttl ,(or ttl 60))
         result)))

(defun optimized-poll-statuses (tags)
  "Poll agent statuses with caching."
  (with-cached-results ("status-poll" :ttl 2)
    (paos/status:poll-agent-statuses tags)))

(defun batch-status-requests (tags &key (batch-size 5))
  "Poll statuses in batches to reduce overhead."
  (let ((results '()))
    (loop for batch in (batch-list tags batch-size)
          do (let ((batch-results (paos/status:poll-agent-statuses batch)))
               (setf results (append results batch-results))))
    results))

(defun batch-list (list batch-size)
  "Split list into batches."
  (loop for i from 0 below (length list) by batch-size
        collect (subseq list i (min (+ i batch-size) (length list)))))

;;; ============================================================================
;;; Analysis Caching
;;; ============================================================================

(defun cached-analysis (statuses &key (ttl 60))
  "Analyze statuses with caching."
  (let ((cache-key (format nil "analysis-~A" (hash-statuses statuses))))
    (with-cached-results (cache-key :ttl ttl)
      (paos/ai:analyze-agent-statuses statuses))))

(defun hash-statuses (statuses)
  "Create hash of statuses for cache key."
  (let ((hash 0))
    (dolist (status statuses)
      (incf hash (paos/status:status-last-update status)))
    hash))

;;; ============================================================================
;;; Performance Monitoring
;;; ============================================================================

(defvar *performance-metrics* (make-hash-table :test 'equal)
  "Performance metrics storage.")

(defun measure-latency (operation-name function)
  "Measure and record latency for an operation."
  (let ((start (get-internal-real-time)))
    (multiple-value-prog1
        (funcall function)
      (let* ((end (get-internal-real-time))
             (elapsed-ms (/ (- end start) 
                           (/ internal-time-units-per-second 1000.0))))
        (record-metric operation-name elapsed-ms)))))

(defun record-metric (name value)
  "Record a performance metric."
  (let ((metrics (or (gethash name *performance-metrics*) '())))
    (push (list :value value :timestamp (get-universal-time)) metrics)
    ;; Keep only last 100 measurements
    (setf (gethash name *performance-metrics*)
          (subseq metrics 0 (min 100 (length metrics))))))

(defun get-metric-stats (name)
  "Get statistics for a metric."
  (let ((metrics (gethash name *performance-metrics*)))
    (when metrics
      (let ((values (mapcar (lambda (m) (getf m :value)) metrics)))
        (list :count (length values)
              :min (reduce #'min values)
              :max (reduce #'max values)
              :avg (/ (reduce #'+ values) (length values))
              :latest (getf (car metrics) :value))))))

(defun performance-report ()
  "Generate performance report."
  (format t "~%~A=== PERFORMANCE REPORT ===~A~%~%"
          paos/dashboard::*ansi-cyan*
          paos/dashboard::*ansi-reset*)
  
  ;; Cache stats
  (let ((stats (cache-stats)))
    (format t "Cache Performance:~%")
    (format t "  Hits: ~A | Misses: ~A | Hit Rate: ~,1F%~%"
            (getf stats :hits)
            (getf stats :misses)
            (getf stats :hit-rate))
    (format t "  Entries: ~A~%~%" (getf stats :entries)))
  
  ;; Operation metrics
  (format t "Operation Latencies:~%")
  (maphash (lambda (name metrics)
             (declare (ignore metrics))
             (let ((stats (get-metric-stats name)))
               (format t "  ~A: avg=~,2Fms, min=~,2Fms, max=~,2Fms~%"
                      name
                      (getf stats :avg)
                      (getf stats :min)
                      (getf stats :max))))
           *performance-metrics*)
  (format t "~%"))

;;; ============================================================================
;;; CPU & Memory Monitoring
;;; ============================================================================

(defun get-cpu-usage ()
  "Get current CPU usage (simplified)."
  ;; Platform-specific implementation would go here
  0.0)

(defun get-memory-usage ()
  "Get current memory usage."
  #+sbcl
  (let ((bytes (sb-ext:dynamic-space-size)))
    (/ bytes 1048576.0))  ; Convert to MB
  #-sbcl
  0.0)

(defun monitor-resources ()
  "Monitor CPU and memory usage."
  (list :cpu (get-cpu-usage)
        :memory-mb (get-memory-usage)
        :timestamp (get-universal-time)))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun log-info (format-string &rest args)
  "Log info message."
  (format t "INFO: ~A~%" 
          (apply #'format nil format-string args)))
