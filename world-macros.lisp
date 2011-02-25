(in-package #:detome)

(defmacro set-level (map)
  (let ((h (length map))
        (w (length (car map))))
    `(setf *level* (make-array '(,h ,w)
                               :initial-contents ',map))))

(defmacro place-monster (name x y)
  (with-gensyms (mt)
    `(let ((,mt (lookup-monster-type ,name)))
       (push (make-instance 'monster               
                            :x ,x :y ,y
                            :name (symbol-name (gensym (name ,mt)))
                            :image-name (image-name ,mt)
                            :mon-type ,mt
                            :level (level ,mt)
                            :hp (funcall (hp-gen ,mt))
                            :att-r (funcall (att-r-gen ,mt))
                            :dmg-r (funcall (dmg-r-gen ,mt))
                            :def-r (funcall (def-r-gen ,mt))
                            :update-cb (ai-cb ,mt)
                            :update-cb-control '(:turns 0)) 
             *monsters-in-level*))))

(defmacro make-inanimate (name x y image)
  (with-gensyms (obj)
    `(let ((,obj (make-object :name ,name 
                              :render-level "inanimate"
                              :render-cb #'inanimate-renderer)))
       (set-meta (:image ,obj) ,image)
       (set-meta (:x ,obj) ,x)
       (set-meta (:y ,obj) ,y))))
