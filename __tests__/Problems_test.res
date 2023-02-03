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
    
    t->Assert.is(Three.nth(mylist, 10), None, (),)
    t->Assert.is(Three.nth(mylist, -5), None, (),)
    t->Assert.is(Three.nth(mylist, 4), None, (),)
    t->Assert.is(Three.nth(mylist, 2), Some("c"), (),)
    t->Assert.is(Three.nth(mylist, 0), Some("a"), (),)
})

test("the length of a list", t => {
    let mylist = list{"a", "b", "c"}

    t->Assert.is(Four.length(mylist), 3, (),)
    t->Assert.is(Four.length(list{}), 0, (),)
})

test("reversing a list", t => {
    let mylist = list{"a", "b", "c"}

    t->Assert.deepEqual(Five.reverse(mylist), list{"c", "b", "a"}, (),)
})

test("is list a reverse of itself (palindrome)", t => {
    let subject = list{"a", "b", "c"}

    t->Assert.isFalse(Six.isReverse(~claim=list{}, subject), (),)
    t->Assert.isFalse(Six.isReverse(~claim=list{"c", "b"}, subject), (),)
    t->Assert.isTrue(Six.isReverse(~claim=list{"c", "b", "a"}, subject), (),)
})

test("flattening a nested list in order", t => {
    
    let tree = list{
        list{"a"},
        list{},
        list{"b", "c"}
    }
    
    let subject = list{"a", "b", "c"}

    t->Assert.deepEqual(Seven.flatten(tree), subject, (), )
})

test("eliminating consecutive duplicates in a list", t => {
    
    let duplicated = list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}
    
    let distinct = list{"a", "b", "c", "a", "d", "e"}

    t->Assert.deepEqual(Eight.uniquify(duplicated), distinct, (), )
})