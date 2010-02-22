(in-package #:detome)

(defclass item-type ()
  ((cls
	:initarg cls :accessor cls :type keyword :documentation
	"The class of this item-type. :weapon, :armor, etc. This
    essentially determines how the item can be used.")
   (cb
	:initarg cb :accessor cb :type function :documentation
	"This is a function whose arguments are determined by the
	item-type's class and is called whenever the item is 'used'.")
   (level
	:initarg level :accessor level :type fixnum :documentation
	"The item-type's level meaning the minimum required level to use
    the item and also affects which monsters may spawn this
    item-type.")
   (rarity
	:initarg rarity :accessor rarity :type single-float :documentation
	"The rarity of the item-type determines how likely it is to
	spawn.")))

(defclass item ()
  ((location 
	:initarg location :accessor x :type list :documentation
	"The first element of this list is the parent object, actor,
    etc. to which this item belongs. The rest of the elements in the
    list are parent specific. For example if the first element is a
    map object then the second and third may be the grid coordinates
    on the map where this item is located, if the parent is an actor
    the arguments may be where on the player the object is located
    including their inventory.")
   (item-type
	:accessor item-type :initarg item-type :type item-type :documentation
	"The type of this item.")))
	