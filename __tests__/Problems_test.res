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