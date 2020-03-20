(define ll '(1 3 9 5 7))

;; node constructor
(define (make-node index value)
  (cons index value))

(define (get-index node)
  (car node))

(define (get-value node)
  (cdr node))

;; node 的左子叶节点
(define (left-child-node l node)
  (let ((index (+ (* 2 (get-index node)) 1)))
    (node-by-index l index)))

;; node 的右子叶节点
(define (right-child-node l node)
  (if (and (even? (length l)) (= (get-index node) (get-index (last-parent-node l))))
      '()
      (let ((index (+ (* 2 (get-index node)) 2)))
        (node-by-index l index))))

(define (node-by-index l index)
  (make-node index (list-ref l index)))

(define (even? num)
  (= (remainder num 2) 0))

;; list 最后一个父节点
(define (last-parent-node l)
  (let ((len (length l)))
    (if (even? len)
        (make-node (- (/ len 2) 1) (list-ref l (- (/ len 2) 1)))
        (make-node (- (/ (- len 1) 2) 1) (list-ref l (- (/ (+ len 1) 2) 1))))))


;; 设置 list k v
(define (list-set! list k val)
  (if (zero? k)
      (set-car! list val)
      (list-set! (cdr list) (- k 1) val)))

;; 比较两个节点
(define (smaller? node1 node2)
  (< (get-value node1) (get-value node2)))

;; 取更大的节点
(define (bigger-node node1 node2)
  (cond ((null? node1) node2)
        ((null? node2) node1)
        (else
         (if (smaller? node1 node2)
             node2
             node1))))

;; 返回大的子节点
(define (bigger-child-node l p-node)
  (let ((node1 (left-child-node l p-node)) (node2 (right-child-node l p-node)))
    (bigger-node node1 node2)))

;; 是否根节点
(define (root-node? node)
  (= (get-index node) 0))

;; node 的父节点
(define (parent-node l node)
  (if (root-node? node)
      node
      (let ((node-index (get-index node)))
        (if (even? node-index)
            (node-by-index l (- (/ node-index 2) 1))
            (node-by-index l (- (/ (+ node-index 1) 2) 1))))))

;; 交换 l 的两个 node, l 被修改
(define (swap l node p-node)
  (begin
    (set! tmp (get-value node))
    (list-set! l (get-index node) (get-value p-node))
    (list-set! l (get-index p-node) tmp)))

;; 子叶节点递归上浮
(define (flow-up l node)
  (let ((p-node (parent-node l node)))
    (if (smaller? p-node (bigger-child-node l p-node))
        (begin
          (swap l p-node (bigger-child-node l p-node))
          (flow-up l (node-by-index l (get-index p-node)))))))

(define (swap-with-child? l p-node)
  (let ((bigger-child (bigger-child-node l p-node)))
    (smaller? p-node bigger-child)))

;; 建堆，最大堆
(define (make-max-heap l)
  (define (iter p-idx)
    (if (< p-idx 0)
        l
        (let ((p-node (node-by-index l p-idx)))
          (if (swap-with-child? l p-node)
              (let ((up-node (bigger-child-node l p-node)))
                (flow-up l up-node)
                (iter (- p-idx 1)))
            (iter (- p-idx 1))))))
  (iter (get-index (last-parent-node l))))

;; 获取堆的最大值
(define (get-max h)
  (car h))

(define l1 '(9 7 4 2 1))
(define l2 '(12 8 7 3 1))
(define l3 '(11 4 3 2 1))

(define list-bundle (list l1 l2 l3))

;; 初始化一个各数组指针位置的列表，初始值 '(0 0 0 ...)
(define (init-idx-l lists)
  (define (iter result n)
    (if (= (length lists) n)
        result
        (iter (append result '(0)) (+ n 1))))
  (iter '() 0))

;; 用多路 list 的首个元素组成一个 list
(define (get-first-els lists)
  (define (iter result idx)
    (if (= (length lists) (+ idx))
        result
        (iter (append result
                      (list (car (list-ref lists idx))))
              (+ idx 1))))
  (iter '() 0))

;; 用首个元素 list 建堆
(define (init-max-heap lists)
  (make-max-heap (get-first-els lists)))


;; (init-max-heap list-bundle)

;; (init-idx-l list-bundle)

;; (define (topK k lists))
