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

test("eliminating _consecutive_ duplicates in a list", t => {
    
    let duplicated = list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}
    
    let distinct = list{"a", "b", "c", "a", "d", "e"}

    t->Assert.deepEqual(Eight.uniquify(duplicated), distinct, (), )
})

test("pack consecutive duplicates in a list to sublists", t => {
    
    let duplicated = list{"a", "a", "a", "a", "b", "c", "c", "a", "a"}
    let packed = list{list{"a", "a", "a", "a"}, list{"b"}, list{"c", "c"}, list{"a", "a"}}

    t->Assert.deepEqual(Nine.pack(duplicated), packed, (), )
})

test("run length encoding of consecutive duplicates in a list", t => {

    let list = list{"W","W","W","W","W","W","W","W","W","W","W","W","B","W","W","W","W","W","W","W","W","W","W","W","W","B","B","B","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","B","W","W","W","W","W","W","W","W","W","W","W","W","W","W"}
    let encoded = list{(12, "W"), (1, "B"), (12, "W"), (3, "B"), (24, "W"), (1, "B"), (14, "W")}

    t->Assert.deepEqual(Ten.run_length_encode(list), encoded, (), )
})

test("modify run length encoding", t => {
    open Eleven

    let list = list{"W","W","W","W","W","W","W","W","W","W","W","W","B","W","W","W","W","W","W","W","W","W","W","W","W","B","B","B","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","B","W","W","W","W","W","W","W","W","W","W","W","W","W","W"}
    let encoded = list{
        Many(12, "W"), 
        One("B"), 
        Many(12, "W"), 
        Many(3, "B"), 
        Many(24, "W"), 
        One("B"), 
        Many(14, "W")
    }
    t->Assert.deepEqual(run_length_encode_modified(list), encoded, (), )
})

test("decode a run length encoded list", t => {
    open Eleven
    open Twelve

    let list = list{"W","W","W","W","W","W","W","W","W","W","W","W","B","W","W","W","W","W","W","W","W","W","W","W","W","B","B","B","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","B","W","W","W","W","W","W","W","W","W","W","W","W","W","W"}

    let encoded = list{
        Many(12, "W"), 
        One("B"), 
        Many(12, "W"), 
        Many(3, "B"), 
        Many(24, "W"), 
        One("B"), 
        Many(14, "W")
    }

    t->Assert.deepEqual(decode_run_length(encoded), list, (), )
})

test("run length encode direct without creating sublists", t => {
    open Eleven

    let list = list{"W","W","W","W","W","W","W","W","W","W","W","W","B","W","W","W","W","W","W","W","W","W","W","W","W","B","B","B","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","B","W","W","W","W","W","W","W","W","W","W","W","W","W","W"}
    let encoded = list{
        Many(12, "W"), 
        One("B"), 
        Many(12, "W"), 
        Many(3, "B"), 
        Many(24, "W"), 
        One("B"), 
        Many(14, "W")
    }
    t->Assert.deepEqual(Thirteen.run_length_encode_direct(list), encoded, (), )
})

test("duplicate elements in a list", t => {
    let list = list{"a","b","c","d","e","f"}

    let duplicated = list{"a","a", "b", "b","c", "c","d", "d","e", "e","f", "f"}
    t->Assert.deepEqual(Fourteen.duplicate(list), duplicated, (), )
})

test("duplicate elements in a list x times", t => {
    let list = list{"a","b","c","d","e","f"}

    let duplicated = list{"a","a", "b", "b","c", "c","d", "d","e", "e","f", "f"}
    t->Assert.deepEqual(Fifteen.duplicate(list, 2), duplicated, (), )
})

test("drop every nth element from a list", t => {
    let list = list{"a","b","c","d","e","f"}
    let dropped = list{"a","b",/*"c",*/ "d","e","f"}

    t->Assert.deepEqual(Sixteen.drop(list, 3), dropped, (), )
})

test("split a list into two parts given the length of the first part", t => {
    let list = list{"a","b","c","d","e","f"}
    let split = list{list{"a","b","c", "d"}, list{"e", "f"}}

    t->Assert.deepEqual(Seventeen.split(list, 4), split, (), )

    let list = list{"a","b","c","d","e","f"}
    let split = list{list{"a","b","c", "d", "e", "f"}, list{}}

    t->Assert.deepEqual(Seventeen.split(list, 6), split, (), )
})

test("extract a slice from a list", t => {
    let list = list{"a","b","c","d","e","f"}

    t->Assert.deepEqual(Eighteen.slice(list, 0, 20), list, (), )
    t->Assert.deepEqual(Eighteen.slice(list, 0, 2), list{"a", "b", "c"}, (), )

    t->Assert.deepEqual(Eighteen.slice(list, -1, 2), list{}, (), )
})

test("shift a List n places to the left starting at index zero", t => {
    let list = list{"a","b","c","d","e","f"}

    t->Assert.deepEqual(Nineteen.shiftItemsToTheLeft(list, 20), list, (), )
    t->Assert.deepEqual(Nineteen.shiftItemsToTheLeft(list, 0), list, (), )
    t->Assert.deepEqual(Nineteen.shiftItemsToTheLeft(list, -1), list, (), )

    t->Assert.deepEqual(Nineteen.shiftItemsToTheLeft(list, 6), list, (), )
    t->Assert.deepEqual(Nineteen.shiftItemsToTheLeft(list, 5), list{"f", "a","b","c","d","e"}, (), )
    t->Assert.deepEqual(Nineteen.shiftItemsToTheLeft(list, 2), list{"c","d","e","f","a","b"}, (), )
})

test("insert an element at a given position in a list", t => {
    let list = list{"a","b","c","d","e","f"}

    t->Assert.deepEqual(Twenty.insertInPlace(list, ~pos=-1, ~item="z"), list, (), )
    t->Assert.deepEqual(Twenty.insertInPlace(list, ~pos=20, ~item="z"), list{"a","b","c","d","e","f","z"}, (), )
    t->Assert.deepEqual(Twenty.insertInPlace(list, ~pos=0, ~item="z"), list{"z","a", "b","c","d","e","f"}, (), )
    t->Assert.deepEqual(Twenty.insertInPlace(list, ~pos=5, ~item="z"), list{"a","b","c","d","e","z","f"}, (), )
    t->Assert.deepEqual(Twenty.insertInPlace(list, ~pos=6, ~item="z"), list{"a","b","c","d","e","f","z"}, (), )
    t->Assert.deepEqual(Twenty.insertInPlace(list, ~pos=1, ~item="z"), list{"a", "z", "b","c","d","e","f"}, (), )
})

test("create a list of integers within a given range", t => {
    t->Assert.deepEqual(TwentyOne.listRange(~start=4, ~end=0), list{}, (), )
    t->Assert.deepEqual(TwentyOne.listRange(~start=4, ~end=6), list{4, 5, 6}, (), )
    t->Assert.deepEqual(TwentyOne.listRange(~start=-21, ~end=-19), list{-21, -20, -19}, (), )
    t->Assert.deepEqual(TwentyOne.listRange(~start=-21, ~end=-19), list{-21, -20, -19}, (), )
})

test("lottery: draw n different random numbers from a set 1..k numbers", t => {
    let stack = [41, 82, 3, 4, 5, 6, 7, 8, 9, 19, 22]

    t->Assert.is(TwentyTwo.lottery(~count=5, ~stack)->Js.Array.length, 5, (), )
    t->Assert.is(TwentyTwo.lottery(~count=-5, ~stack)->Js.Array.length, 0, (), )
})

test("shuffle an array", t => {
    let stack = [41, 82, 3, 4, 5, 6, 7, 8, 9, 19, 22]

    t->Assert.deepEqual(TwentyThree.shuffle([]), [], (), )
    t->Assert.deepEqual(TwentyThree.shuffle(["a"]), ["a"], (), )
    t->Assert.is(TwentyThree.shuffle(stack)->Js.Array.length, 11, (), )
})

test("generate combinations of k _distinct_ objects chosen from n elements of a list", t => {

    let list = list{"a","b","c","d","e","f"}
    
    t->Assert.deepEqual(TwentyFour.combinations(~stack=list, ~count=10), list{list}, (), )
    t->Assert.deepEqual(TwentyFour.combinations(~stack=list, ~count=-5), list{list{}}, (), )
    t->Assert.deepEqual(TwentyFour.combinations(~stack=list, ~count=list->Prelude.List.length), list{list}, (), )
    t->Assert.is(TwentyFour.combinations(~stack=list, ~count=2)->Prelude.List.length, 15, (), )
    t->Assert.is(TwentyFour.combinations(~stack=list, ~count=5)->Prelude.List.length, 6, (), )
})