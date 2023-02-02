open Ava
open Problem

test("a function that returns the tail of a list", t => {

    let mylist = list{"a", "b", "c", "d"}
    
    t->Assert.is(One.last(mylist), Some("d"), (),)
})

test("a function that returns the last two elements of a list", t => {

    let mylist = list{"a", "b", "c", "d"}
    
    t->Assert.deepEqual(Two.lasttwo(mylist), Some("c", "d"), (),)
})

test("a function that returns nth element of a list", t => {
    let mylist = list{"a", "b", "c", "d"}
    
    t->Assert.deepEqual(Three.nth(mylist, 10), None, (),)
    t->Assert.deepEqual(Three.nth(mylist, -5), None, (),)
    t->Assert.deepEqual(Three.nth(mylist, 4), None, (),)
    t->Assert.deepEqual(Three.nth(mylist, 2), Some("c"), (),)
    t->Assert.deepEqual(Three.nth(mylist, 0), Some("a"), (),)
})